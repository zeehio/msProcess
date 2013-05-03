# This file shows how to implement the algorithm proposed in the following paper 
# using the S-PLUS software package S+Proteome:
#
# Coombes, K.R., Tsavachidis, S., Morris, J.S., Baggerly, K.A., and Kuerer, H.M., 
# "Improved peak detection and quantification of mass spectrometry data 
# acquired from surface-enhanced laser desorption and ionization 
# by denoising spectra with the undecimated discrete wavelet transform," 
# Proteomics, 5:4107-17, 2005.
#
# NOTE: The parameter setting for the following processing functions 
# might not be optimal for the dataset used. A user is encouraged to 
# use this file as a template and try out different parameter settings. 

#==============================================================================
# load the msProcess package and the msBreast data package
  library("msProcess")
  library("msBreast")
    
# display and browse the online help files
  if (!is.R()) {
  	help(library="msProcess")	# or msHelp(section="proteome")
  } else {
  	help(topic="msProcess")
  }
  
#------------------------------------------------------------------------------ 
# explore the build-in dataset Breast2003QC
  # print out the summary of Breast2003QC  
  data(Breast2003QC, package="msBreast")          
  Breast2003QC           
          
  # plot the entire set of spectra by setting the interspectrum offset manually       
  plot(Breast2003QC, subset=NULL, offset=1000)
  
  # plot a few spectra by computing the offset automatically for better visualization
  # it might take some time to compute the offset for the entire set of spectra      
  plot(Breast2003QC, subset=1:8) 
   
  # visualize the entire set of spectra as an image
  image(Breast2003QC)

#------------------------------------------------------------------------------
# select a few spectra to demonstrate the pipeline processing functionalities
  z <- Breast2003QC[, 1:8]
  
#------------------------------------------------------------------------------  
  # denoising 
  # the output will be an msSet object with an additional element "noise"
  # NOTE: we are overwriting the input with the output  
  z <- msDenoise(z, FUN="wavelet", thresh.scale=2)
  
  # visualize how the denoising algorithm performed within a certain mass range
  plot(z, process="msDenoise", subset=1:4, xlim=c(13000, 17000), lwd=c(2,1))
  
  # display the noise removed as an image
  image(z, what="noise")

#------------------------------------------------------------------------------	
  # local noise estimation
  #	the output will be an msSet object with an additional element "noise.local"
  z <- msNoise(z, FUN="mean")
  
  # visualize how the local noise estimation algorithm performed within a certain mass range
  plot(z, process="msNoise", subset=NULL, offset=30, xlim=c(13000, 17000), lwd=c(3,1))
  
  # display the local noise estimated as an image  
  image(z, what="noise.local")

#------------------------------------------------------------------------------				
  # baseline correction
  #	the output will be an msSet object with an additional element "baseline"
  z <- msDetrend(z, FUN="monotone")
  
  # visualize how the baseline correction algorithm performed within a certain mass range  
  plot(z, process="msDetrend", subset=NULL,  xlim=c(13000, 17000), lty=c(1,4))
  
  # display the baseline estimated as an image    
  image(z, what="baseline")

#------------------------------------------------------------------------------			
  # intensity normalization
  #	the output will be an msSet object with an additional element "tic"
  z <- msNormalize(z)
  
  # visualize how the intensity normalization algorithm performed within a certain mass range   
  plot(z, process="msNormalize", subset=NULL, xlim=c(13000, 17000), lty=c(1,4))
	
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
  # peak quantification
  #	the output will be an msSet object with an additional element "peak.matrix"
  z <- msQuantify(z, measure="intensity")
  
  # display the peak matrix as an image    
  image(z, what="peak.matrix")
  
#------------------------------------------------------------------------------
  # check out what we have done to this dataset
  summary(z)  
  
#==============================================================================
