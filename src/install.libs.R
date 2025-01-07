files <- Sys.glob(paste0("*", SHLIB_EXT))
dest <- file.path(R_PACKAGE_DIR, paste0("libs", R_ARCH))
dir.create(dest, recursive=TRUE, showWarnings=FALSE)
file.copy(files, dest, overwrite=TRUE)
if(file.exists("symbols.rds"))
    file.copy("symbols.rds", dest, overwrite=TRUE)

# The below is needed because file.copy does not copy symbolic links
if(SHLIB_EXT == ".so")
{
  unlink(file.path("inst", "swipl", "lib", "libswipl.so"))
  unlink(file.path("inst", "swipl", "lib", "libswipl.so.9"))
  dir.create(file.path(R_PACKAGE_DIR, "swipl", "lib"), recursive=TRUE, showWarnings=FALSE)
  file.symlink("libswipl.so.9", file.path(R_PACKAGE_DIR, "swipl", "lib", "libswipl.so"))
  file.symlink("libswipl.so.9.3.18", file.path(R_PACKAGE_DIR, "swipl", "lib", "libswipl.so.9"))
}
