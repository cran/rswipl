# Load libswipl.dll/so on startup
# 
# This cannot be delegated to a useDynLib directive in NAMESPACE (at least not
# under linux). The reason is that rolog.so itself is able to load other 
# packages (i.e. prolog libraries), and therefore exports a number of 
# prolog-specific symbols. The additional option local=FALSE makes sure these
# symbols are imported on startup. This option is not available in if we use
# useDynLib in NAMESPACE.
#
.onLoad <- function(libname, pkgname)
{
  # Load libswipl.dylib under macOS (linux: static lib)
  if(.Platform$OS.type == "unix" & R.version$os != "linux-gnu")
  {
    fp <- file.path(libname, pkgname, "swipl", "lib", "swipl", "lib")
    arch <- R.version$arch
    if(arch == 'aarch64')
      arch <- 'arm64'
    folder <- dir(fp, pattern=arch, full.names=TRUE)
    if(!length(folder) & arch == "arm64")
      folder <- dir(fp, pattern="aarch64-linux", full.names=TRUE)

    # Preload libswipl.dll
    if(R.version$os == "linux-gnu")
      dyn.load(file.path(folder, paste("libswipl", .Platform$dynlib.ext, sep="")))
    else
      dyn.load(file.path(folder, "libswipl.dylib")) # macOS
  }

  if(.Platform$OS.type == "windows")
  {
    folder <- file.path(libname, pkgname, "swipl", "bin")
    dyn.load(file.path(folder, paste("libswipl", .Platform$dynlib.ext, sep="")))
  }

  # Load rolog.dll/so
  library.dynam(chname="rswipl", package=pkgname, lib.loc=libname, local=FALSE)
  invisible()
}

.onUnload <- function(libpath)
{
  # See .onLoad for details
  library.dynam.unload("rswipl", libpath=libpath)

  if(.Platform$OS.type == "unix" & R.version$os != "linux-gnu")
  {
    fp <- file.path(libpath, "swipl", "lib", "swipl", "lib")
    arch <- R.version$arch
    if(arch == 'aarch64')
      arch <- 'arm64'
    folder <- dir(fp, pattern=arch, full.names=TRUE)
    if(!length(folder) & arch == "arm64")
      folder <- dir(fp, pattern="aarch64-linux", full.names=TRUE)	

    if(R.version$os == "linux-gnu")
      dyn.unload(file.path(folder, paste("libswipl", .Platform$dynlib.ext, sep="")))
    else
      dyn.unload(file.path(folder, "libswipl.dylib")) # macOS
  }
	
  if(.Platform$OS.type == "windows")
  {
    folder <- file.path(libpath, "swipl", "bin")
    dyn.unload(file.path(folder, paste("libswipl", .Platform$dynlib.ext, sep="")))
  }
}

.onAttach <- function(libname, pkgname)
{
  argv1 <- commandArgs()[1]

  if(.Platform$OS.type == "unix")
  {
    Sys.setenv(SWI_HOME_DIR=file.path(libname, pkgname, "swipl", "lib", "swipl"))
    if(!.init(argv1))
      stop("rswipl: initialization of Prolog failed.")  
  }

  # This is a bit of a mystery.
  #
  # Initialization of the SWI-Prolog works fine under linux, under Windows using
  # RStudio.exe, under Windows using RTerm.exe, but fails under RGui.exe (aka.
  # "blue R"). Even stranger, it works in the second attempt. 
  #
  # For this reason, I invoke rolog_init twice here if needed. Any hint to a
  # cleaner solution is appreciated.
  if(.Platform$OS.type == "windows")
  {
    Sys.setenv(SWI_HOME_DIR=file.path(libname, pkgname, "swipl"))

    if(!.init(argv1) && !.init(argv1))
      stop("rswipl: initialization of Prolog failed.")  
  }
  
  # SWI startup message
  query(call("message_to_string", quote(welcome), expression(W)))
  W <- submit()
  clear()
  packageStartupMessage(W$W)
  invisible()
}

.onDetach <- function(libpath)
{
  clear()
  if(!.done())
    warning("rswipl: Prolog not initialized.")

  Sys.unsetenv("SWI_HOME_DIR")
}

