# Load rswipl.dll/rswipl.so on startup
# 
# This cannot be delegated to a useDynLib directive in NAMESPACE (at least not
# under linux). The reason is that rswipl.so itself is able to load other 
# packages (i.e. prolog libraries), and therefore exports a number of 
# prolog-specific symbols. The additional option local=FALSE makes sure these
# symbols are imported on startup. This option is not available in if we use
# useDynLib in NAMESPACE.
#
.onLoad <- function(libname, pkgname)
{
  libswipl = character(0)
  home <- Sys.getenv("SWI_HOME_DIR")
  msg <- ""
  rswipl.ok <- FALSE

  if(.Platform$OS.type == "windows")
  {
    pl0 <- file.path(libname, pkgname)
    home <- dir(pl0, pattern="swipl$", full.names=TRUE)
    lib = file.path(home, "bin")
    libswipl <- dir(lib,
      pattern=paste("libswipl", .Platform$dynlib.ext, "$", sep=""),
      full.names=TRUE)

    if(length(libswipl))
      rswipl.ok <- TRUE
  }
  
  if(.Platform$OS.type == "unix")
  {
    pl0 <- file.path(libname, pkgname)
    home <- file.path(pl0, "swipl", "lib", "swipl")
    lib <- file.path(pl0, "swipl", "lib")
    if(grepl("darwin", R.version$os))
      libswipl <- dir(lib, pattern="libswipl.dylib$", full.names=TRUE)
    else
      libswipl <- dir(lib, pattern="libswipl.so$", full.names=TRUE)

    if(length(libswipl) == 1)
      s <- dyn.load(libswipl, local=FALSE)
    
    # Even if there isn't any .so (we also have static builds)
    rswipl.ok <- TRUE
  }
  
  if(!rswipl.ok)
    msg <- "Unable to locate the SWI-Prolog runtime."

  op.rswipl <- list(
    rswipl.swi_home_dir = home,  # restore on .onUnload
    rswipl.home         = home,
    rswipl.ok           = rswipl.ok,
    rswipl.lib          = libswipl,
    rswipl.message      = msg)

  set <- !(names(op.rswipl) %in% names(options()))
  if(any(set))
    options(op.rswipl[set])

  if(!options()$rswipl.ok)
    return(FALSE)

  library.dynam(chname="rswipl", package=pkgname, lib.loc=libname, DLLpath=lib,
    local=FALSE)
  invisible()
}

.onUnload <- function(libpath)
{
  # See .onLoad for details
  library.dynam.unload("rswipl", libpath=libpath)

  if(options()$rswipl.ok & .Platform$OS.type == "unix")
  {
    libswipl <- options()$rswipl.lib
    if(length(libswipl))
      dyn.unload(libswipl)
  }

  invisible()
}

.onAttach <- function(libname, pkgname)
{
  if(!options()$rswipl.ok)
    return(FALSE)

  Sys.setenv(SWI_HOME_DIR=options()$rswipl.home)
  if(!.init(commandArgs()[1]))
  {
    warning("rswipl: initialization of swipl failed.")  
    return(FALSE)
  }

  msg <- options()$rswipl.message
  if(msg != "")
     packageStartupMessage(msg)
  invisible()
}

.onDetach <- function(libpath)
{
  # Clear any open queries
  clear() 
  if(!.done())
    stop("rswipl: not initialized.")

  home = options()$rswipl.swi_home_dir
  if(home == "")
    Sys.unsetenv("SWI_HOME_DIR")
  else
    Sys.setenv(SWI_HOME_DIR=home)
}
