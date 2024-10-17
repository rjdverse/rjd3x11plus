

#' Get Macurves Filters
#'
#' @param seas_filter the filter to extract.
#' @param period period of the filter.
#' @examples
#' macurves("S3X3")
#' @export
macurves <- function(seas_filter = c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"), period = 12) {
  seas_filter <- match.arg(toupper(seas_filter),
                          c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"))
  seas_opt <- .jcall("jdplus/x12plus/base/api/SeasonalFilterOption",
                     "Ljdplus/x12plus/base/api/SeasonalFilterOption;",
                     "valueOf",
                     seas_filter)
  P <- .jcast(new(J("java.lang.Double"), as.character(period)),
             "java.lang.Number")
  seasFilter <- .jcall("jdplus/x12plus/base/core/X11SeasonalFiltersFactory",
                       "Ljdplus/toolkit/base/core/math/linearfilters/ISymmetricFiltering;",
                       "filter",
                       P, seas_opt)
  rjd3filters::.jd2r_finitefilters(seasFilter)
}
