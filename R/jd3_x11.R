JD3_X11PLUS<-"JD3_x11PLUS"
JD3_X12PLUS<-"JD3_x12PLUS"
JD3<-"JD3"


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
#' @return An object of the class 'JD3_X11PLUS', containg the decomposition and the parameters
#' @export
#'
#' @examples
#' q<-x11plus(rjd3toolkit::ABS$X0.2.09.10.M, 12)
#'
x11plus<-function(y, period, mul=TRUE, trend.horizon=6, trend.degree=2,
                  trend.kernel=c("Henderson", "BiWeight", "TriWeight", "TriCube", "Uniform", "Triangular", "Epanechnikov", "Trapezoidal"),
                  trend.asymmetric=c("CutAndNormalize", "Direct", "MMSRE"),
                  seas.s0=c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"),
                  seas.s1=c("S3X5", "S3X3", "S3X1", "S3X9", "S3X15"),
                  extreme.lsig=1.5, extreme.usig=2.5){
  seas0 <- match.arg(seas.s0)
  seas1 <- match.arg(seas.s1)
  tkernel <- match.arg(trend.kernel)
  asym <- match.arg(trend.asymmetric)
  jrslt<-.jcall("jdplus/x12plus/base/r/X11Decomposition", "Ljdplus/x12plus/base/r/X11Decomposition$Results;", "process",
                as.numeric(y), period, mul,
                as.integer(trend.horizon), as.integer(trend.degree),
                tkernel, asym, seas0, seas1, extreme.lsig, extreme.usig)
  decomposition<-list(
    y=as.numeric(y),
    t=rjd3toolkit::.proc_vector(jrslt, "d12"),
    sa=rjd3toolkit::.proc_vector(jrslt, "d11"),
    s=rjd3toolkit::.proc_vector(jrslt, "d10"),
    i=rjd3toolkit::.proc_vector(jrslt, "d13")
  )
  parameters<-list(
    period=period,
    multiplicative=mul,
    trend.horizon=trend.horizon,
    trend.degree=trend.degree,
    trend.kernel=trend.kernel,
    trend.asymmetric=trend.asymmetric,
    extreme.lsig=extreme.lsig,
    extreme.usig=extreme.usig,
    seas.s0=seas.s0,
    seas.s1=seas.s1
  )

  return(structure(list(
    decomposition=decomposition,
    parameters=parameters),
    class=c(JD3_X11PLUS, JD3)))
}

#' Apply Henderson linear filter
#'
#' @param x input time-series.
#' @param length length of the Henderson filter.
#' @param musgrave Boolean indicating if Musgrave asymmetric filters should be used.
#' @param ic ic ratio: irregular/trend-cycle.
#'
#' @return A numeric array corresponding to the the trend
#' @export
#'
#' @examples
#' q<-x11plus(rjd3toolkit::ABS$X0.2.09.10.M, 12)
#'
#' henderson(q$decomposition$sa, 13)
henderson<-function(x, length, musgrave=TRUE, ic=4.5){
  result <- .jcall("jdplus/x12plus/base/r/X11Decomposition", "[D", "henderson",
                   as.numeric(x), as.integer(length), musgrave, ic)
  if (is.ts(x))
    result <- ts(result,start = start(x), frequency = frequency(x))
  return (result)
}
