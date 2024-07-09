#include "Rcpp.h"

#include <SWI-cpp2.h>
#include <SWI-cpp2.cpp>

// #include "Rcpp.h"
using namespace Rcpp ;

// Translate prolog expression to R
//
// [] -> NULL
// real -> NumericVector
// #(r1, r2, r3) -> NumericVector (# is a default, see option realvec)
// ##(#(row1), #(row2), ...) -> Matrix
// integer -> IntegerVector
// %(i1, i2, i3) -> IntegerVector (see option intvec for the name)
// %%(%(row1), %(row2), ...) -> Matrix
// string -> CharacterVector
// $$(s1, s2, s3) CharacterVector
// $$$($$(row1), $$(row2), ...) -> Matrix
// na (atom) -> NA
// true, false (atoms) -> LogicalVector
// !(l1, l2, l3) -> LogicalVector (see option boolvec)
// !!(!(row1), !(row2), ...) -> Matrix
// the empty atom -> ""
// other atoms -> symbol/name
// variable -> expression(variable name)
// compound -> call (aka. "language")
// list -> list
//
RObject pl2r(PlTerm pl, CharacterVector& names, PlTerm& vars) ;

// Translate R expression to prolog
//
// NULL -> []
// numeric vector of length 1 -> real (unless rolog.scalar == FALSE)
// numeric vector of length > 1 -> e.g., #(1.0, 2.0, 3.0) (see rolog.realvec)
// integer vector of length 1 -> integer
// integer vector of length > 1 -> %(1, 2, 3)
// character vector of length 1 -> string
// character vector of length > 1 -> $("a", "b", "c")
// logical vector of length 1 -> the atoms true, false or na
// logical vector of length > 1 -> $(true, false, na)
// other symbols/name -> atom
// expression -> variable
// call/language -> compound
// list -> list
//
PlTerm r2pl(SEXP r, CharacterVector& names, PlTerm& vars) ;

// Consult one or more files. If something fails, the procedure stops, and
// will not try to consult the remaining files.
//
// [[Rcpp::export(.consult)]]
LogicalVector consult_(CharacterVector files)
{
  for(R_xlen_t i=0; i<files.size(); i++)
  {
    try 
    {
      PlCall("consult", PlTermv(PlTerm_string(files(i))));
    }
    
    catch(PlException& ex)
    { 
      String err(ex.as_string(PlEncoding::Locale));
      PL_clear_exception() ;
      stop("failed to consult %s: %s", (char*) files(i), err.get_cstring()) ;
    }
  }

  return true ;
}

// Prolog -> R
RObject pl2r_null()
{
  return R_NilValue ;
}

// TODO: use this and pl.eq_if_atom(ATOM_na) instead of
//       pl.is_atom() && pl == "na"
// PlAtom ATOM_na("na");

// This helper function checks for na and then translates an individual PlTerm 
// to a double.
double pl2r_double(PlTerm pl)
{
  if(pl.is_atom() && pl.as_string() == "na")
    return NA_REAL ;

  try 
  {
    return pl.as_float() ;
  }

  catch(PlException& ex)
  { 
    warning("cannot convert %s to float: %s",
            pl.as_string(PlEncoding::Locale).c_str(), ex.as_string(PlEncoding::Locale).c_str()) ;
    PL_clear_exception() ;
    return NA_REAL ;
  }
}

// Convert scalar real to DoubleVector of length 1
DoubleVector pl2r_real(PlTerm pl)
{
  return DoubleVector::create(pl2r_double(pl)) ;
}

// Convert vector of reals (e.g., #(1.0, 2.0, na)) to DoubleVector
DoubleVector pl2r_realvec(PlTerm pl)
{
  size_t arity = pl.arity() ;
  DoubleVector r(arity) ;
  for(size_t i=0; i<arity; i++)
    r(i) = pl2r_double(pl[i+1]) ;

  return r ;
}

// Convert matrix of reals (e.g., ##(#(1.0, 2.0), #(na, ...), ...))
NumericMatrix pl2r_realmat(PlTerm pl)
{
  size_t nrow = pl.arity() ;
  size_t ncol = 0 ;
  if(nrow > 0)
  {
    for(size_t i=0; i<nrow; i++)
      if(i == 0)
        ncol = pl[1].arity() ;
      else
      {
        if(ncol != pl[i+1].arity())
          stop("cannot convert PlTerm to Matrix, inconsistent rows") ;
      }
  }

  NumericMatrix r(nrow, ncol) ;
  for(size_t i=0; i<nrow; i++)
    r.row(i) = pl2r_realvec(pl[i+1]) ;

  return r ;
}

// See above for pl2r_double
long pl2r_int(PlTerm pl)
{
  if(pl.is_atom() && pl.as_string() == "na")
    return NA_INTEGER ;

  try 
  {
    return pl.as_long() ;
  }
  
  catch(PlException& ex)
  { 
    warning("Cannot convert %s to integer: %s",
            pl.as_string(PlEncoding::Locale).c_str(), ex.as_string(PlEncoding::Locale).c_str()) ;
    PL_clear_exception() ;
    return NA_INTEGER ;
  }
}

IntegerVector pl2r_integer(PlTerm pl)
{
  return IntegerVector::create(pl2r_int(pl)) ;
}

IntegerVector pl2r_intvec(PlTerm pl)
{
  size_t arity = pl.arity() ;
  IntegerVector r(arity) ;
  for(size_t i=0; i<arity; i++)
    r(i) = pl2r_int(pl[i+1]) ;

  return r ;
}

IntegerMatrix pl2r_intmat(PlTerm pl)
{
  size_t nrow = pl.arity() ;
  size_t ncol = 0 ;
  if(nrow > 0)
  {
    for(size_t i=0; i<nrow; i++)
      if(i == 0)
        ncol = pl[1].arity() ;
      else
      {
        if(pl[i+1].arity() != ncol)
          stop("cannot convert PlTerm to Matrix, inconsistent rows") ;
      }
  }

  IntegerMatrix r(nrow, ncol) ;
  for(size_t i=0; i<nrow; i++)
    r.row(i) = pl2r_intvec(pl[i+1]) ;

  return r ;
}

// See above for pl2r_double
String pl2r_string(PlTerm pl)
{
  if(pl.is_atom() && pl.as_string() == "na")
    return NA_STRING ;
  
  return pl.as_string(PlEncoding::Locale) ;
}

CharacterVector pl2r_char(PlTerm pl)
{
  return CharacterVector::create(pl2r_string(pl)) ;
}

CharacterVector pl2r_charvec(PlTerm pl)
{
  size_t arity = pl.arity() ;
  CharacterVector r(arity) ;
  for(size_t i=0; i<arity; i++)
    r(i) = pl2r_string(pl[i+1]) ;

  return r ;
}

CharacterMatrix pl2r_charmat(PlTerm pl)
{
  size_t nrow = pl.arity() ;
  size_t ncol = 0 ;
  if(nrow > 0)
  {
    for(size_t i=0; i<nrow; i++)
      if(i == 0)
        nrow = pl[1].arity() ;
      else
      {
        if(pl[i+1].arity() != ncol)
          stop("cannot convert PlTerm to Matrix, inconsistent rows") ;
      }
  }

  CharacterMatrix r(nrow, ncol) ;
  for(size_t i=0; i<nrow; i++)
    r.row(i) = pl2r_charvec(pl[i+1]) ;

  return r ;
}

// Convert prolog atom to R symbol (handle na, true, false)
RObject pl2r_symbol(PlTerm pl)
{
  if(pl.as_string() == "na")
    return wrap(NA_LOGICAL) ;
  
  if(pl.as_string() == "true")
    return wrap(true) ;
  
  if(pl.as_string() == "false")
    return wrap(false) ;

  // Empty symbols
  if(pl.as_string() == "")
    return Function("substitute")() ;

  return as<RObject>(Symbol(pl.as_string(PlEncoding::UTF8))) ; // TODO: PlEncoding::Locale?
}

// Forward declaration, needed below
LogicalVector pl2r_boolvec(PlTerm pl)
{
  size_t arity = pl.arity() ;
  LogicalVector r(arity) ;
  for(size_t i=0; i<arity; i++)
  {
    PlTerm t = pl[i+1] ;
    if(t.is_atom())
    {
      if(t.as_string() == "na")
      {
        r(i) = NA_LOGICAL ;
        continue ;
      }
      
      if(t.as_string() == "true")
      {
        r(i) = 1 ;
        continue ;
      }
      
      if(t.as_string() == "false")
      {
        r(i) = 0 ;
        continue ;
      }
    }
    
    warning("pl2r_logical: invalid item %s, returning NA", t.as_string(PlEncoding::Locale).c_str()) ;
    r(i) = NA_LOGICAL ;
  }

  return r ;
}

LogicalMatrix pl2r_boolmat(PlTerm pl)
{
  size_t nrow = pl.arity() ;
  size_t ncol = 0 ;
  if(nrow > 0)
  {
    for(size_t i=0; i<nrow; i++)
      if(i == 0)
        ncol = pl[1].arity() ;
      else
      {
        if(pl[i+1].arity() != ncol)
          stop("cannot convert PlTerm to Matrix, inconsistent rows") ;
      }
  }

  LogicalMatrix r(nrow, ncol) ;
  for(size_t i=0; i<nrow; i++)
    r.row(i) = pl2r_boolvec(pl[i+1]) ;

  return r ;
}

// Translate prolog variables to R expressions.
RObject pl2r_variable(PlTerm pl, CharacterVector& names, PlTerm& vars)
{
  // names and vars is a list of all the variables from the R query,
  // a typical member of names is something like X, a member of vars 
  // is something like _1545.
  //
  // Search for the variable (e.g., _1545) in names and return its R name as an
  // expression (say, X).
  PlTerm_tail tail(vars) ;
  PlTerm_var v ;
  for(int i=0 ; i<names.length() ; i++)
  {
    PlCheckFail(tail.next(v)) ;
    if(v == pl)
      return ExpressionVector::create(Symbol(names(i))) ;
  }
  
  // If the variable is not found, it's a new one created by Prolog, e.g., in
  // queries like member(1, Y), Y is unified with [1 | _NewVar ]. This variable
  // cannot be translated to a human-readable name, so it is returned as _1545.
  return ExpressionVector::create(Symbol(pl.as_string(PlEncoding::UTF8))) ; // TODO: PlEncoding::Locale?
}

RObject pl2r(PlTerm pl, CharacterVector& names, PlTerm& vars)
{
  if(pl.type() == PL_NIL)
    return pl2r_null() ;
  
  if(pl.is_integer())
    return pl2r_integer(pl) ;
  
  if(pl.is_float())
    return pl2r_real(pl) ;
  
  if(pl.is_string())
    return pl2r_char(pl) ;
  
  if(pl.is_atom())
    return pl2r_symbol(pl) ;
  
  if(pl.is_variable())
    return pl2r_variable(pl, names, vars) ;
  
  stop("pl2r: Cannot convert %s", pl.as_string(PlEncoding::Locale).c_str()) ;
}

// Translate R expression to prolog
//
// Forward declarations
PlTerm r2pl_real(NumericVector r) ;
PlTerm r2pl_logical(LogicalVector r) ;
PlTerm r2pl_integer(IntegerVector r) ;
PlTerm r2pl_string(CharacterVector r) ;

// This returns an empty list
PlTerm r2pl_null()
{
  PlTerm_var pl ;
  PlTerm_tail tail(pl) ;
  PlCheckFail(tail.close()) ;
  return tail ;
}

// Prolog representation of R's NA.
PlTerm r2pl_na()
{
  return PlTerm_atom("na") ;
}

// Translate to matrix ###(##(1.0, 2.0, 3.0), ##(4.0, 5.0, 6.0))
PlTerm r2pl_matrix(Matrix<REALSXP> r)
{
  PlTermv rows(r.nrow()) ;
  for(int i=0 ; i<r.nrow() ; i++)
    PlCheckFail(rows[i].unify_term(r2pl_real(r.row(i)))) ;

  return PlCompound("###", rows) ;
}

// Translate to (scalar) real or compounds like ##(1.0, 2.0, 3.0)
PlTerm r2pl_real(NumericVector r)
{
  if(Rf_isMatrix(r))
    return r2pl_matrix(as<Matrix<REALSXP>>(r)) ;

  if(r.length() == 0)
    return r2pl_null() ;

  LogicalVector nan = is_nan(r) ;
  LogicalVector na = is_na(r) ;
  
  // Translate to scalar
  if(r.length() == 1)
  {
    if(na[0] && !nan[0])
      return r2pl_na() ;
    
    return PlTerm_float(r[0]);
  }

  // Translate to vector ##(1.0, 2.0, 3.0)
  size_t len = (size_t) r.length() ;
  PlTermv args(len) ;
  for(size_t i=0 ; i<len ; i++)
  {
    if(na[i] && !nan[i])
      PlCheckFail(args[i].unify_term(r2pl_na())) ;
    else
      PlCheckFail(args[i].unify_float(r[i])) ;
  }

  return PlCompound("##", args) ;
}

// Translate to matrix !!!(!!(true, false), !(false, true))
PlTerm r2pl_matrix(Matrix<LGLSXP> r)
{
  PlTermv rows(r.nrow()) ;
  for(int i=0 ; i<r.nrow() ; i++)
    PlCheckFail(rows[i].unify_term(r2pl_logical(r.row(i)))) ;

  return PlCompound("!!!", rows) ;
}

// Translate to (scalar) boolean or compounds like !!(true, false, na)
PlTerm r2pl_logical(LogicalVector r)
{
  if(Rf_isMatrix(r))
    return r2pl_matrix(as<Matrix<LGLSXP>>(r)) ;

  if(r.length() == 0)
    return r2pl_null() ;
  
  LogicalVector na = is_na(r) ;
  
  // scalar boolean
  if(r.length() == 1)
  {
    if(na[0])
      return r2pl_na() ;
    
    return PlTerm_atom(r[0] ? "true" : "false") ;
  }

  // LogicalVector !!(true, false, na)
  size_t len = (size_t) r.length() ;
  PlTermv args(len) ;
  for(size_t i=0 ; i<len ; i++)
  {
    if(na[i])
      PlCheckFail(args[i].unify_term(r2pl_na())) ;
    else
      PlCheckFail(args[i].unify_atom(r[i] ? "true" : "false")) ;  // TODO: unify_bool()
  }

  return PlCompound("!!", args) ;
}

// Translate to matrix %%%(%%(1, 2), %(3, 4))
PlTerm r2pl_matrix(Matrix<INTSXP> r)
{
  PlTermv rows(r.nrow()) ;
  for(int i=0 ; i<r.nrow() ; i++)
    PlCheckFail(rows[i].unify_term(r2pl_integer(r.row(i)))) ;

  return PlCompound("%%%", rows) ;
}

// Translate to (scalar) integer or compounds like %%(1, 2, 3)
PlTerm r2pl_integer(IntegerVector r)
{
  if(Rf_isMatrix(r))
    return r2pl_matrix(as<Matrix<INTSXP>>(r)) ;

  if(r.length() == 0)
    return r2pl_null() ;
  
  LogicalVector na = is_na(r) ;
  
  // scalar integer
  if(r.length() == 1)
  {
    if(na[0])
      return r2pl_na() ;
    
    return PlTerm_integer(r(0)) ;
  }
  
  // IntegerVector %%(1, 2, 3)
  size_t len = (size_t) r.length() ;
  PlTermv args(len) ;
  for(size_t i=0 ; i<len ; i++)
  {
    if(na[i])
      PlCheckFail(args[i].unify_term(r2pl_na())) ;
    else
      PlCheckFail(args[i].unify_integer(r[i])) ;
  }
  
  return PlCompound("%%", args) ;
}

// Translate R expression to prolog variable
//
// This function keeps a record of the names of the variables in 
// use (e.g., _1545) as well as the corresponding R names (e.g., X). If a new
// variable is encountered, its name is looked up in the list of known 
// variables, and it is unified with it if the name is found. Otherwise, a new
// variable is created.
//
PlTerm r2pl_var(ExpressionVector r, CharacterVector& names, PlTerm& vars)
{
  // Variable name in R
  Symbol n = as<Symbol>(r[0]) ;
  
  // Do not map the anonymous variable to a known variable name
  if(n == "_")
    return PlTerm_var() ;

  // Unify with existing variable of the same name
  PlTerm_tail tail(vars) ;
  PlTerm_var v ;
  for(R_xlen_t i=0 ; i<names.length() ; i++)
  {
    PlCheckFail(tail.next(v)) ;
    if(n == names(i))
      return v ;
  }

  // If no such variable exists, create a new one and remember the name
  names.push_back(n.c_str()) ;
  PlTerm_var pl ;
  PlCheckFail(tail.append(pl)) ;
  return pl ;
}

// Translate R symbol to prolog atom
PlTerm r2pl_atom(Symbol r)
{
  return PlTerm_atom(r.c_str()) ;
}

// Translate to matrix $$$($$(1, 2), $$(3, 4))
PlTerm r2pl_matrix(Matrix<STRSXP> r)
{
  PlTermv rows(r.nrow()) ;
  for(int i=0 ; i<r.nrow() ; i++)
    PlCheckFail(rows[i].unify_term(r2pl_string(r.row(i)))) ;

  return PlCompound("$$$", rows) ;
}

// Translate CharacterVector to (scalar) string or things like $("a", "b", "c")
PlTerm r2pl_string(CharacterVector r)
{
  if(Rf_isMatrix(r))
    return r2pl_matrix(as<Matrix<STRSXP>>(r)) ;

  if(r.length() == 0)
    return r2pl_null() ;
  
  LogicalVector na = is_na(r) ;
  
  // scalar string
  if(r.length() == 1)
  {
    if(na[0])
      return r2pl_na() ;
    
    return PlTerm_string(r(0)) ;
  }

  // compound like $("a", "b", "c")
  size_t len = (size_t) r.length() ;
  PlTermv args(len) ;
  for(size_t i=0 ; i<len ; i++)
  {
    if(na[i])
      PlCheckFail(args[i].unify_term(r2pl_na())) ;
    else
      PlCheckFail(args[i].unify_term(PlTerm_string(r(i)))) ; // DO NOT SUBMIT - unify_string()
  }

  return PlCompound("$$", args) ;
}

// Translate R list to prolog list, taking into account the names of the
// elements, e.g., list(a=1, b=2) -> [a-1, b-2]. This may change, since the
// minus sign is a bit specific to prolog, and the conversion in the reverse
// direction may be ambiguous.
//
PlTerm r2pl_list(List r, CharacterVector& names, PlTerm& vars)
{
  // Names of list elements (empty vector if r.names() == NULL)  
  CharacterVector n ;
  if(TYPEOF(r.names()) == STRSXP)
    n = as<CharacterVector>(r.names()) ;
  
  PlTerm_var pl ;
  PlTerm_tail tail(pl) ;
  for(R_xlen_t i=0; i<r.size() ; i++)
  {
    PlTerm arg = r2pl(r(i), names, vars) ;
    
    // Convert named argument to prolog pair a-X.
    if(n.length() && n(i) != "")
      PlCheckFail(tail.append(PlCompound("-", PlTermv(PlTerm_atom(n(i)), arg)))) ;
    else
      PlCheckFail(tail.append(arg)) ; // no name
  }
  
  PlCheckFail(tail.close()) ;
  return pl ;
}

PlTerm r2pl(SEXP r, CharacterVector& names, PlTerm& vars)
{
  if(TYPEOF(r) == REALSXP)
    return r2pl_real(r) ;
  
  if(TYPEOF(r) == LGLSXP)
    return r2pl_logical(r) ;
  
  if(TYPEOF(r) == INTSXP)
    return r2pl_integer(r) ;
  
  if(TYPEOF(r) == EXPRSXP)
    return r2pl_var(r, names, vars) ;

  if(TYPEOF(r) == SYMSXP)
    return r2pl_atom(r) ;

  if(TYPEOF(r) == STRSXP)
    return r2pl_string(r) ;

  if(TYPEOF(r) == VECSXP)
    return r2pl_list(r, names, vars) ;
  
  if(TYPEOF(r) == NILSXP)
    return r2pl_null() ;
  
  return r2pl_na() ;
}

class RlQuery  
{
  CharacterVector names ;
  PlTerm_var vars ;
  PlQuery* qid ;
  
public:
  RlQuery(RObject aquery) ;
  ~RlQuery() ;
  
  int next_solution() ;
  List bindings() ;
} ;

RlQuery::RlQuery(RObject aquery)
  : names(),
    vars(),
    qid(NULL)
{
  PlTerm pl = r2pl(aquery, names, vars) ;
  qid = new PlQuery("call", PlTermv(PlTerm(pl))) ;
}

RlQuery::~RlQuery()
{
  if(qid)
    delete qid ;
}

int RlQuery::next_solution()
{
  if(qid == NULL)
    stop("next_solution: no open query.") ;
    
  int q ;
  try
  {
    q = qid->next_solution() ;
  }

  catch(PlException& ex)
  {
    warning(ex.as_string(PlEncoding::Locale).c_str()) ;
    PL_clear_exception() ;
    stop("Query failed") ;
  }

  return q ;
}

List RlQuery::bindings()
{
  List l ;

  PlTerm_tail tail(vars) ;
  PlTerm_var v ;
  for(int i=0 ; i<names.length() ; i++)
  {
    PlCheckFail(tail.next(v)) ;
    RObject r = pl2r(v, names, vars) ;
    if(TYPEOF(r) == EXPRSXP && names[i] == as<Symbol>(as<ExpressionVector>(r)[0]).c_str())
    continue ;

    l.push_back(r, (const char*) names[i]) ;
  }

  return l ;
}

RlQuery* query_id = NULL ;

// Open a query for later use.
// [[Rcpp::export(.query)]]
RObject query_(RObject q)
{
  if(PL_current_query() != 0)
  {
    warning("Cannot raise simultaneous queries. Please invoke clear()") ;
    return wrap(false) ;
  }

  query_id = new RlQuery(q) ;
  return wrap(true) ;
}

// Clear query (and invoke cleanup handlers, see PL_close_query)
// [[Rcpp::export(.clear)]]
RObject clear_()
{
  if(query_id)
    delete query_id ;
  query_id = NULL ;

  return wrap(true) ;
}

// Submit query
// [[Rcpp::export(.submit)]]
RObject submit_()
{
  if(query_id == NULL)
  {
    warning("submit: no open query.") ;
    return wrap(false) ;
  }

  if(!query_id->next_solution())
  {
    delete query_id ;
    query_id = NULL ;
    return wrap(false) ;
  }
  
  return query_id->bindings() ;
}

// The SWI system should not be initialized twice; therefore, we keep track of
// its status.
bool pl_initialized = false ;
const char** pl_argv = NULL ;

// Initialize SWI-Prolog. This needs a list of the command-line arguments of 
// the calling program, the most important being the name of the main 
// executable, argv[0]. I added "-q" to suppress SWI prolog's welcome message
// which is shown in .onAttach anyway.
// [[Rcpp::export(.init)]]
LogicalVector init_(String argv0)
{
  if(pl_initialized)
    warning("Please do not initialize SWI-Prolog twice in the same session.") ;
  
  // Prolog documentation requires that argv is accessible during the entire 
  // session. I assume that this pointer is valid during the whole R session,
  // and that I can safely cast it to const.
  const int argc = 2 ; // 4 ; // forthcoming
  pl_argv = new const char*[argc] ;
  pl_argv[0] = argv0.get_cstring() ;
  pl_argv[1] = "-q" ;
//  pl_argv[2] = "-D embedded=true" ;
//  pl_argv[3] = "--sigalert=0" ;
  if(!PL_initialise(argc, (char**) pl_argv))
    stop("rswipl_init: initialization failed.") ;

  pl_initialized = true ;  
  return true ;
}

// Run a swipl session from R. This is needed for unit tests. Instead of
// swipl -g goal, invoke R -e 'library(rswipl)' --no-echo -q --args -g goal
// [[Rcpp::export(.swipl)]]
LogicalVector swipl_(String argv0, CharacterVector& arglist)
{
  if(pl_initialized)
    warning("Please do not initialize SWI-Prolog twice in the same session.") ;

  R_xlen_t argc = 1 + arglist.size() ;
  pl_argv = new const char*[argc] ;
  pl_argv[0] = argv0.get_cstring() ;
  for(R_xlen_t i=1 ; i<argc ; i++)
    pl_argv[i] = arglist(i-1) ;
  if(!PL_initialise(argc, (char**) pl_argv))
    stop("rswipl_init_swipl: initialization failed.") ;

  pl_initialized = true ;
  return true ;
}

// [[Rcpp::export(.done)]]
LogicalVector done_()
{
  if(!pl_initialized)
  {
    warning("rswipl_done: swipl has not been initialized") ;
    return true ;
  }

  // Just in case there are open queries
  clear_() ;

  PL_cleanup(0) ;
  pl_initialized = false ;
  delete [] pl_argv ;
  return true ;
}
