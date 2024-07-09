#' Create a query
#'
#' @param q
#' an R call. The R call consists of symbols, integers and real numbers, 
#' character strings, boolean values, expressions, lists, and other calls.
#' Vectors of booleans, integers, floating point numbers, and strings with
#' length _N_ > 1 are translated to prolog compounds !/N, %/N, #/N and $$/N,
#' respectively.
#'
#' @return
#' If the creation of the query succeeds, `TRUE`
#'
#' @md
#'
#' @seealso [submit()]
#' for submitting a query
#'
#' @seealso [clear()]
#' to close the currently open query
#'
#' @details
#' SWI-Prolog does not allow multiple open queries. If another query is open, it
#' it is closed and a warning is shown.
#'
#' @examples
#' q <- query(call("member", 1, expression(X)))
#' submit()
#' submit()
#' clear()
#'
query <- function(
    q=call("member", expression(X), list(quote(a), "b", 3L, 4, TRUE, expression(Y))))
{
    .query(q)
}

#' Clear current query
#'
#' @return
#' TRUE (invisible)
#'
#' @md
#'
#' @seealso [query()]
#' for opening a query
#'
#' @seealso [submit()]
#' for submitting a query
clear <- function()
{
    invisible(.clear())
}

#' Submit a query that has been opened with [query()] before.
#'
#' @return
#' If the query fails, `FALSE` is returned. If the query succeeds, a
#' (possibly empty) list is returned that includes the bindings required to
#' satisfy the query.
#'   
#' @md
#'
#' @seealso [query()]
#' for a opening a query.
#' 
#' @seealso [clear()]
#' for a clearing a query.
submit <- function()
{
    .submit()
}

