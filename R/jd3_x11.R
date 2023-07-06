
#' Perform an X-11 like decomposition with any (non integer) periodicity.
#'
#' @param y input time-series.
#' @param period Period of the seasonal component, any positive real number.
#' @param mul Boolean indicating if the decomposition mode is multiplicative (TRUE).
#' @param trend.horizon bandwidth of trend filters.
#' @param trend.degree polynomial order in local trend model.
#' @param trend.kernel kernel weights in objective function.
#' @param trend.asymmetric truncation type for symmetric filter.
#' @param seas.s0 Seasonal filter for B5, C5, D5.
#' @param seas.s1 seasonal filter for B10, C10, D10.
#' @param extreme.lsig lower boundary used for outlier correction in irregular.
#' @param extreme.usig upper boundary used for outlier correction in irregular.
#'
#' @return
#' @export
#'
#' @examples
x11plus<-function(y, period, mul=TRUE, trend.horizon=6, trend.degree=2,
                  trend.kernel=c("Henderson", "BiWeight", "TriWeight", "TriCube", "Uniform", "Triangular", "Epanechnikov", "Trapezoidal"),
                  trend.asymmetric=c("CutAndNormalize", "Direct", "MMSRE"),
                  seas.s0=c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"),
                  seas.s1=c("S3X5", "S3X3", "S3X1", "S3X9", "S3X15"),
                  extreme.lsig=1.5, extreme.usig=2.5){
  seas0=match.arg(seas.s0)
  seas1=match.arg(seas.s1)
  tkernel=match.arg(trend.kernel)
  asym=match.arg(trend.asymmetric)
  jrslt<-.jcall("jdplus/x11plus/base/r/X11Decomposition", "Ljdplus/x11plus/base/r/X11Decomposition$Results;", "process", as.numeric(y), period, mul
                , as.integer(trend.horizon), as.integer(trend.degree),
                tkernel, asym, seas0, seas1, extreme.lsig, extreme.usig)
  decomposition<-list(
    y=as.numeric(y),
    t=rjd3toolkit::.proc_vector(jrslt, "d12"),
    sa=rjd3toolkit::.proc_vector(jrslt, "d11"),
    s=rjd3toolkit::.proc_vector(jrslt, "d10"),
    i=rjd3toolkit::.proc_vector(jrslt, "d13")
  )
  parameters<-list(
    multiplicative=mul,
    trend.horizon=trend.horizon,
    trend.degree=trend.degree,
    trend.kernel=trend.kernel,
    trend.asymmetric=trend.asymmetric,
    extreme.lsig=extreme.lsig,
    extreme.usig=extreme.usig
  )

  return(structure(list(
    decomposition=decomposition,
    parameters=parameters),
    class="JD3_X11PLUS"))
}

#' Apply Henderson linear filter
#'
#' @param y input time-series.
#' @param length length of the Henderson filter.
#' @param musgrave Boolean indicating if Musgrave asymmetric filters should be used.
#' @param ic ic ratio: irregular/trend-cycle.
#'
#' @return
#' @export
#'
#' @examples
henderson<-function(y, length, musgrave=TRUE, ic=4.5){
  return (.jcall("jdplus/x11plus/base/r/X11Decomposition", "[D", "henderson", as.numeric(y), as.integer(length), musgrave, ic))
}

