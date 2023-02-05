.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\n---> CASTLab.org: Version ", packageVersion(pkgname),
                        " of ", pkgname, " <---\n")
}
