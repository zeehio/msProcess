# This file shows how to implement the algorithm proposed in the following papers 
# using the S-PLUS software package S+Proteome:
#
# 1. Y. Yasui, D. McLerran, B. L. Adam, M. Winget, M. Thornquist, and Z. Feng, 
# "An automated peak identification/calibration procedure for 
# high-dimensional protein measures from mass spectrometers," 
# Journal of Biomedicine and Biotechnology, 2003(4):242-248, 2003.
#
# 2. Y. Yasui, M. Pepe, M. L. Thompson, B. L. Adam, G. L. Wright, Jr., Y. Qu, 
# J. D. Potter, M. Winget, M. Thornquist, and Z. Feng, 
# "A data-analytic strategy for protein biomarker discovery: 
# Profiling of high-dimensional proteomic data for cancer detection," 
# Biostatistics, 4(3):449-63, 2003.
#
# NOTE: The parameter setting for the following processing functions 
# might not be optimal for the dataset used. A user is encouraged to 
# use this file as a template and try out different parameter settings. 

#==============================================================================
# load the msProcess package and the msBreast data package
  library("msProcess")
  library("msBreast")

#------------------------------------------------------------------------------
# select a few spectra for the following processing
  data(Breast2003QC, package="msBreast")
  z <- Breast2003QC[, 1:24]
  
#------------------------------------------------------------------------------  
  # denoising 
  # the output will be an msSet object with an additional element "noise"
  # NOTE: we are overwriting the input with the output  
  z <- msDenoise(z, FUN="wavelet", thresh.scale=2)

#------------------------------------------------------------------------------	
  # local noise estimation
  #	the output will be an msSet object with an additional element "noise.local"
  z <- msNoise(z, FUN="mean")

#------------------------------------------------------------------------------				
  # baseline correction
  #	the output will be an msSet object with an additional element "baseline"
  z <- msDetrend(z, FUN="monotone")

#------------------------------------------------------------------------------			
  # intensity normalization
  #	the output will be an msSet object with an additional element "tic"
  z <- msNormalize(z)

#------------------------------------------------------------------------------
  # peak detection
  #	the output will be an msSet object with additional elements "peak.list" and "use.mean"
  z <- msPeak(z, FUN="search", span=41, span.supsmu=0.05, snr=2)
  
  # visualize how the peak detection algorithm performed within a certain mass range    
  plot(z, process="msPeak", subset=NULL, xlim=c(13000, 17000))

#------------------------------------------------------------------------------			
  # peak alignment	
  #	the output will be an msSet object with an additional element "peak.class"
  z <- msAlign(z, FUN="vote", snr.thresh=10, mz.precision=0.002)	
  
  # visualize how the peak alignment algorithm performed within a certain mass range   
  plot(z, process="msAlign", subset=NULL, xlim=c(13000, 17000), lty=c(1,4))

#------------------------------------------------------------------------------			
  # peak quantification
  #	the output will be an msSet object with an additional element "peak.matrix"
  z <- msQuantify(z, measure="count")
  
  # display the peak matrix as an image    
  image(z, what="peak.matrix")
  
#------------------------------------------------------------------------------
  # check out what we have done to this dataset
  summary(z)  

#==============================================================================
  # explore the peaks detected in the first 24 spectra
  count.matrix.z <- z$peak.matrix
  
  # the total number of peaks detected  
  ncol(count.matrix.z)
  
  # histogram showing the number of peaks found in multiple spectra
  # Note: axis style "e" unimplemented in R
  barplot(tabulate(colSums(count.matrix.z>0)), names=as.character(1:24), #yaxs="e", 
    main="peak distribution", xlab="number of spectra", ylab="number of peaks")
    
#==============================================================================

