# This file shows how to implement the algorithm proposed in the following papers 
# using the S-PLUS software package S+Proteome:
#
# 1. Randolph TW and Yasui Y, 
# "Multiscale processing of mass spectrometry data," 
# Biometrics, 62:589-597, 2006.
#
# 2. Randolph TW, Mitchell BL, McLerran DF, Lampe PD, and Feng Z, 
# "Quantifying peptide signal in MALDI-TOF mass spectrometry data," 
# Molecular & Cellular Proteomics, 4(12):1990-1999, 2005.
#
# 3. Randolph TW, 
# "Scale-based normalization of spectral data," 
# Cancer Biomarkers, 2(3-4):135-144, 2006.
#
# NOTE: The parameter setting for the following processing functions 
# might not be optimal for the dataset used. A user is encouraged to 
# use this file as a template and try out different parameter settings. 

#==============================================================================
# load the msProcess package and the msBreast data package
  library("msProcess")
  library("msDilution")
  
#------------------------------------------------------------------------------ 
# explore the build-in dataset Dilution2005Raw
  # print out the summary of Dilution2005Raw   
  data(Dilution2005Raw, package="msDilution")
  Dilution2005Raw           
          
  # plot the peptide mixture spectra by setting the interspectrum offset manually       
  plot(Dilution2005Raw, subset=Dilution2005Raw$coding$pep.ind, offset=1000)
  
  # plot serum-only spectra by computing the offset automatically for better visualization
  # it might take some time to compute the offset for the entire set of spectra      
  plot(Dilution2005Raw, subset=Dilution2005Raw$coding$ser.ind) 
   
  # visualize half of the serum + peptide mixture spectra as an image
  image(Dilution2005Raw, subset=Dilution2005Raw$coding$mix.ind[1:125])

#------------------------------------------------------------------------------
# select a few spectra to demonstrate the pipeline processing functionalities
  z <- Dilution2005Raw[, Dilution2005Raw$coding$mix.ind[1:8]]
  
#------------------------------------------------------------------------------  
  # denoising 
  # the output will be an msSet object with an additional element "noise"
  # NOTE: we are overwriting the input with the output
  z <- msDenoise(z, FUN="mrd", wavelet="s8", levels=6, keep.smooth=FALSE, keep.details=TRUE)

  # visualize how the denoising algorithm performed within a certain mass range
  plot(z, process="msDenoise", subset=NULL, xlim=c(25000, 28000), lwd=c(2,1))
  
  # display the noise removed as an image
  image(z, what="noise")

#------------------------------------------------------------------------------	
#  # local noise estimation is not needed for MRD approach 
#  # as only detail of a specific scale is kept.

#------------------------------------------------------------------------------				
#  # baseline correction is not needed for MRD approach 
#  # as smooth was removed when denoising

#------------------------------------------------------------------------------			
  # intensity normalization
  #	the output will be an msSet object with an additional element "tic"
  z <- msNormalize(z, FUN="snv")
  
  # visualize how the intensity normalization algorithm performed within a certain mass range   
  plot(z, process="msNormalize", subset=NULL, xlim=c(25000, 28000), lty=c(1,4))
	
#------------------------------------------------------------------------------
  # peak detection
  #	the output will be an msSet object with additional elements "peak.list" and "use.mean"
  z <- msPeak(z, FUN="mrd", use.mean=FALSE, snr=2)

  # visualize how the peak detection algorithm performed within a certain mass range    
  plot(z, process="msPeak", subset=NULL, xlim=c(25000, 28000))

#------------------------------------------------------------------------------			
  # peak alignment	
  #	the output will be an msSet object with an additional element "peak.class"
  z <- msAlign(z, FUN="mrd", snr.thresh=2)	

  # visualize how the peak alignment algorithm performed within a certain mass range   
  plot(z, process="msAlign", subset=NULL, xlim=c(25000, 28000), lty=c(1,4))

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
