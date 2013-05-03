################################################
## S+Proteome charge detection functions.
##
##  msCharge
##
################################################

#########################################################
##  Mother charge detection function: msCharge
#########################################################

"msCharge" <- function(x, ncharge=2:3, mz.precision=0.003,
  event="Charge Detection", ...)
{
  # check inputs
  if (!is(x,"msSet"))
    stop("Primary input must be of class msSet")

  if (!is.element("peak.class",names(x))){
    stop("peak.class is missing in primary object\n",
      "and is needed for charge detection:\n",
      "Call msAlign() first to fill the void")
  }

	checkVectorType(ncharge, "integer")
	if (!all(ncharge>1))
		stop("ncharge must be an integer vector with all elements larger than 1")

  # perform charge detection
  mz <- x[["peak.class"]][,"mass.loc"]
	out <- mz %o% ncharge
	dif <- outer(out, mz, function(a, b) {(a-b)/b})
	ind <- which(abs(dif)<mz.precision, arr.ind=TRUE)
	z <- rbind(mz[ind[,1]], mz[ind[,3]])
	dimnames(z) <- list(c("multiply-charged", "singly-charged"),
		paste(ncharge[ind[, 2]], "+", sep=""))

  # add charge estimate to the msSet object
  x <- msSet(x, peak.charge=z)

  # update history information
  eventHistory(x, msCharge=list(ncharge=ncharge, mz.precision=mz.precision))
}
