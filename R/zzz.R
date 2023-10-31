#' @import rJava
#' @import rjd3filters

.onLoad <- function(libname, pkgname) {
  if (! requireNamespace("rjd3filters", quietly=T)) stop("Loading rjd3 libraries failed")

  result <- rJava::.jpackage(pkgname, lib.loc=libname)
  if (!result) stop("Loading java packages failed")

  #proto.dir <- system.file("proto", package = pkgname)
  #RProtoBuf::readProtoFiles2(protoPath = proto.dir)

  # reload extractors
  #.jcall("demetra/information/InformationExtractors", "V", "reloadExtractors")
}
