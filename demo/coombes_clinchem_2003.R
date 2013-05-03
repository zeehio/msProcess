# This file shows how to implement the algorithm proposed in the following paper 
# using the S-PLUS software package S+Proteome:
#
# Coombes KR, Fritsche HA Jr., Clarke C, Chen JN, Baggerly KA, Morris JS, 
# Xiao LC, Hung MC, and Kuerer HM, 
# "Quality control and peak finding for proteomics data collected from nipple 
# aspirate fluid by surface-enhanced laser desorption and ionization," 
# Clinical Chemistry, 49(10):1615-23, 2003.
#
# NOTE: The parameter setting for the following processing functions 
# might not be optimal for the dataset used. A user is encouraged to 
# use this file as a template and try out different parameter settings. 

#==============================================================================
# load the msProcess package and the msBreast data package
  library("msProcess")
  library("msBreast")
  
#==============================================================================
# use the first 24 spectra to establish a reproducible proteomics profile
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
  z <- msPeak(z, FUN="simple", use.mean=FALSE, snr=2)
  
  # visualize how the peak detection algorithm performed within a certain mass range    
  plot(z, process="msPeak", subset=NULL, xlim=c(13000, 17000))

#------------------------------------------------------------------------------			
  # peak alignment	
  #	the output will be an msSet object with an additional element "peak.class"
  z <- msAlign(z, FUN="gap", snr.thresh=10, mz.precision=0.003)	
  
  # visualize how the peak alignment algorithm performed within a certain mass range   
  plot(z, process="msAlign", subset=NULL, xlim=c(13000, 17000), lty=c(1,4))

#------------------------------------------------------------------------------			
  # peak quantification using peak counts
  #	the output will be an msSet object with an additional element "peak.matrix"
  z <- msQuantify(z, measure="count")
  
  # display the peak matrix as an image    
  image(z, what="peak.matrix")
   
  # save the peak count matrix for later use
  count.matrix.z <- z$peak.matrix 
  
#------------------------------------------------------------------------------			
  # peak quantification using spectrum intensity
  #	the output will be an msSet object with an additional element "peak.matrix"
  z <- msQuantify(z, measure="intensity")
  
  # display the peak matrix as an image    
  image(z, what="peak.matrix")
  
  # save the peak intensity matrix for later use
  intensity.matrix.z <- z$peak.matrix

#------------------------------------------------------------------------------
  # check out what we have done to the dataset
  summary(z)  
  
#==============================================================================
# use the other 72 spectra for quality assessment
  zz <- Breast2003QC[, -(1:24)]
  
#------------------------------------------------------------------------------  
  # denoising 
  # the output will be an msSet object with an additional element "noise"
  zz <- msDenoise(zz, FUN="wavelet", n.level=10, thresh.scale=2)

#------------------------------------------------------------------------------				
  # baseline correction
  #	the output will be an msSet object with an additional element "baseline"
  zz <- msDetrend(zz, FUN="monotone")

#------------------------------------------------------------------------------			
  # intensity normalization
  #	the output will be an msSet object with an additional element "tic"
  zz <- msNormalize(zz)
  
#------------------------------------------------------------------------------			
  # peak quantification using the peak classes define by the first 24 spectra
  #	the output will be an msSet object with an additional element "peak.matrix"
  zz <- msQuantify(z, xnew=zz, measure="intensity")
  
  # display the peak matrix as an image    
  image(zz, what="peak.matrix")
  
  # save the peak intensity matrix for later use
  intensity.matrix.zz <- zz$peak.matrix
    
#------------------------------------------------------------------------------
  # check out what we have done to the dataset
  summary(zz)  
  
#==============================================================================
  # explore the peaks detected in the first 24 spectra
  # the total number of peaks detected
  ncol(count.matrix.z)
  
  # histogram showing the number of peaks found in multiple spectra
  # Note: axis style "e" unimplemented in R
  barplot(tabulate(colSums(count.matrix.z>0)), names=as.character(1:24), #yaxs="e", 
    main="peak distribution", xlab="number of spectra", ylab="number of peaks")

  # quantile-quantile plots comparing the heights of the detected peaks
  # to the standard normal distribution after four data transformations
  par(mfrow=c(2,2))
  trans1 <- intensity.matrix.z
  qqnorm(trans1, ylab="original");    qqline(trans1)
  trans2 <- log(intensity.matrix.z + .Machine$double.eps)
  qqnorm(trans2, ylab="log");         qqline(trans2)  
  trans3 <- sqrt(intensity.matrix.z)
  qqnorm(trans3, ylab="square root"); qqline(trans3)
  trans4 <- intensity.matrix.z^(1/3)
  qqnorm(trans4, ylab="cube root");   qqline(trans4)  
  par(mfrow=c(1,1))
  
  
  # the peak intensity variance depends on the mean peak height and
  # cube root transformation can be used to stabilize the peak intensity variance
  # independently of the mean peak height   
  par(mfrow=c(1,2))  
  plot(colMeans(intensity.matrix.z), colStdevs(intensity.matrix.z), 
    xlab="mean peak height", ylab="standard deviation")  
  plot(colMeans(intensity.matrix.z^(1/3)), colStdevs(intensity.matrix.z^(1/3)),
    xlab="mean peak height", ylab="standard deviation") 
  par(mfrow=c(1,1))
     
#------------------------------------------------------------------------------
# perform quality assessment using the previously saved peak intensity/count matrices
  # identify "good" peaks that are detected in more than 1 spectrum 
  peaks.good <- which(colSums(count.matrix.z>0)>1)

  # perform PCA on the "good" peaks detected in the first 24 spectra
  pca <- msQualify(intensity.matrix.z[, peaks.good]^(1/3))

  # produce a barplot of the variances of the principal components
  plot(pca$prc)

  # predict the quality of the rest 72 spectra 
  quality <- predict(pca, intensity.matrix.zz[, peaks.good]^(1/3))
  
  # reorganize the test results for better visualization
  matrix(quality$pass, nrow=2)
  
#==============================================================================
