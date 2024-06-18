.onLoad <- function(libname, pkgname) {
  
  packageStartupMessage(
    sprintf(
      'Loading dmcognigen Version %s',
      utils::packageVersion('dmcognigen')
    )
  )
  
}
