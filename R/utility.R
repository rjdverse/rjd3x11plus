

.r2jd_doubleseq <- function(x) {
  .jcall("jdplus/toolkit/base/api/data/DoubleSeq",
         "Ljdplus/toolkit/base/api/data/DoubleSeq;",
         "of", as.numeric(x))
}
