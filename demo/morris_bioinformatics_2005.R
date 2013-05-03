# This file shows how to implement the algorithm proposed in the following paper 
# using the S-PLUS software package S+Proteome:
#
# Morris, J.S., Coombes, K.R., Koomen, J., Baggerly, K.A., and Kobayashi, R.,
# "Feature extraction and quantification for mass spectrometry 
# in biomedical applications using the mean spectrum," 
# Bioinformatics, 21(9):1764-75, 2005.
#
# NOTE: The parameter setting for the following processing functions 
# might not be optimal for the dataset used. A user is encouraged to 
# use this file as a template and try out different parameter settings. 

#==============================================================================
# load the msProcess package and the msBreast data package
  library("msProcess")
  library("msBreast")

#------------------------------------------------------------------------------
# select the entire set of spectra for the following processing
  data(Breast2003QC, package="msBreast") 
  z <- Breast2003QC
  
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
  #	the output will be an msSet object with additional elements: "peak.class", 
  # "intensity.mean", "noise.mean", "noise.local.mean", and "use.mean".
  # NOTE: we are detecting the peaks on the mean spectrum by setting use.mean=TRUE 
  z <- msPeak(z, FUN="simple", use.mean=TRUE, snr=2)
  
  # visualize how the peak detection algorithm performed within a certain mass range    
  plot(z, process="msPeak", xlim=c(13000, 17000))

#------------------------------------------------------------------------------			
  # peak alignment	
  #	we are skipping the peak alignment step because
  # we can use the peaks detected on the mean spectrum to define the peak classes.
  
  # visualize how the mean spectrum peaks perform within a certain mass range   
  plot(z, process="msAlign", subset=seq(1,96,8), xlim=c(13000, 17000), lty=c(1,4))

#------------------------------------------------------------------------------			
  # peak quantification
  #	the output will be an msSet object with an additional element "peak.matrix"
  z <- msQuantify(z, measure="intensity")
  
  # display the peak matrix as an image    
  image(z, what="peak.matrix")
  
#------------------------------------------------------------------------------
  # check out what we have done to this dataset
  summary(z)
  
#==============================================================================
