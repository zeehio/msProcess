## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/loadMSMenu.q#12 $
## $DateTime: 2008/08/27 12:07:31 $

loadMSMenu = function(){
  statMenuLoc <-
    guiGetPropertyValue("MenuItem",
                        Name = paste(guiGetMenuBar(), "Statistics", sep = "$"),
                        PropName = "Index")
                        
  MSProcessMenuName = "SPlusMenuBar$SplusMSProcess"
 
  guiCreate("MenuItem",
            Name = MSProcessMenuName,
            Type = "Menu", 
            Action = "None",
            MenuItemText = "&msProcess",
            StatusBarText = "Menu for msProcess",
            Index = as.numeric(statMenuLoc) + 1,
            OverWrite = F,
            EnableMenuItem = T)

##################################################
####  Menu for help, manual and demo scripts  ####
####      originally in .First.lib            ####
##################################################

	"menu.path" <- function(...) paste(..., sep="$")
		  
	"createMenu" <- function(menuPath, menuText, Type="MenuItem", ...)
	    invisible(guiCreate("MenuItem", Name=menuPath, Type=Type, MenuItemText=menuText, ...))

	"checkPath" <- function(x,type="dir"){
	    type <- match.arg(type,c("dir","file"))
	    if ((type == "dir" && !is.dir(x)) || (type == "file" && !file.exists(x)))
	      stop(x, " does not exist")
	    invisible(NULL)
	} ## end function def
	
	"createScriptMenus" <- function(x, baseMenuPath, open=TRUE, run=FALSE, type="appexams"){
	    # check data.frame structure
	    if (!is.data.frame(x))
	      stop("Input must be a data.frame")
	    required <- c("brief","filename")
	    if (!all(is.element(required, names(x))))
	      stop("Input data frame must contain the following columns: ", required)
	    
	    for (i in seq(numRows(x))){
	
	      example <- x[i,]
	      fname   <- strip.blanks(example$filename)
	      path    <- paste(baseMenuPath, paste("eg", i, sep=""), sep="$")
	      cmd     <- paste("msLaunchExample(\"", fname, "\", run=", run,
	        ", open=", open, ", type=\"", type, "\")", sep="")
	
	      guiCreate("MenuItem", Name=path, Type="MenuItem", Action="Expression", Command=cmd,
	        MenuItemText=example$brief, StatusBarText=strip.blanks(example$brief))
	    }
	
	    invisible(NULL)
	}  ## end function def
	
	"smartOrder" <- function(x){
		w       <- regexpr("[0-9]+", x)
	    nums    <- as.numeric(substring(x, w, w+attr(w, "match.length")-1))
	    good    <- (!is.na(nums))
	    z       <- rep("", length(x))
	    z[good] <- sprintf("%03d", nums[good])
	    order(sub("[0-9]+", z, x))
	}

	# initialize variables
	chapter <- "msProcess"
	  
	baseMenuPath     <- MSProcessMenuName		# menu.path(MSProcessMenuName, chapter)
	helpMenuPath     <- menu.path(baseMenuPath, "Help")
	demoMenuPath     <- menu.path(baseMenuPath, "Demo") 	        
	exampleMenuPath  <- menu.path(baseMenuPath, "R-ex")
	manualsMenuPath  <- menu.path(baseMenuPath, "Manuals")
	    
	# create file and directory paths
	chapterPath      <- .find.package(package=chapter)[1]
	
	# check existence of file and directory paths
	checkPath(chapterPath)

	# help submenu
	createMenu(helpMenuPath, "&Help", Type="Menu")
	helpcmd <- paste("\"", file.path(chapterPath, paste(chapter,".chm", sep=""), fsep="/"), "\"", sep="")
	helptext <- paste(chapter, " Help", sep="")
	createMenu(menu.path(helpMenuPath,"chm_help"), helptext, Action="Open", StatusBarText=helptext, Command=helpcmd)
	    
	# scan lists
	RexDir   <- file.path(chapterPath, "R-ex")	    
	Rex <- file.listing(dir=RexDir, pattern=".R")
	briefs    <- gsub("_", " ", gsub(".R$", "", gsub("group-","", Rex), ignore.case=T))
	ix        <- smartOrder(briefs)
	Rex <- data.frame(filename=Rex[ix], brief=briefs[ix], stringsAsFactors=FALSE)
	    
	demoDir   <- file.path(chapterPath, "demo")
	demofiles  <- file.listing(dir=demoDir, pattern=".R")
	demoBriefs <- gsub("_", " ", gsub(".R$", "", gsub("group-","", demofiles), ignore.case=T))
	ix         <- smartOrder(demoBriefs)
	demo      <- data.frame(filename=demofiles[ix], brief=demoBriefs[ix], stringsAsFactors=FALSE)
	    
	# demo submenu
	createMenu(demoMenuPath, "&Demo Scripts", Type="Menu")   
	createScriptMenus(demo, demoMenuPath, type="demo")
	        
	# examples submenu
	createMenu(exampleMenuPath, "Helpfile Examples", Type="Menu")
	createScriptMenus(Rex, exampleMenuPath, type="R-ex")
	    
	# manuals submenu
	createMenu(manualsMenuPath , "&Manual" , Type="Menu")
	helpcmd <- paste("\"", file.path(chapterPath, paste("doc/", chapter,"-manual.pdf", sep=""), fsep="/"), "\"", sep="")
	helptext <- paste(chapter, " Manual", sep="")
	createMenu(menu.path(manualsMenuPath,"pdf_manual"), helptext, Action="Open", 
	       		StatusBarText=helptext, Command=helpcmd)    

#####################################
####  Menu for processing steps  ####
#####################################

  guiCreate("MenuItem",
            Name = paste(MSProcessMenuName, "Separator1", sep = "$"),
            Type = "Separator",
            Action = "None")

  guiCreate("MenuItem",
            Name = paste(MSProcessMenuName, "MSImport", sep = "$"),
            Type = "MenuItem",
            DocumentType = "Any Documents",
            Action = "Function",
            Command = "menuMSImport",
            ShowDialogOnRun = T,
            MenuItemText = "&Import Spectra...",
            StatusBarText = "Import mass spectrum files from directory",
			EnableMenuItem = T)


  guiCreate("MenuItem",
            Name = paste(MSProcessMenuName, "PlotMSList", sep = "$"),
            Type = "MenuItem",
            DocumentType = "Any Documents",
            Action = "Function",
            Command = "menuMSDisplayMSList",
            ShowDialogOnRun = T,
            MenuItemText = "&Display msList...",
            StatusBarText = "Plot individual spectra from msList object",
			EnableMenuItem = T)


  guiCreate("MenuItem",
            Name = paste(MSProcessMenuName, "MSFilterMSList", sep = "$"),
            Type = "MenuItem",
            DocumentType = "Any Documents",
            Action = "Function",
            Command = "menuMSFilterMSList",
            ShowDialogOnRun = T,
            MenuItemText = "&Filter msList Object...",
            StatusBarText = "Filter/subset msList object",
			EnableMenuItem = T)


  guiCreate("MenuItem",
            Name = paste(MSProcessMenuName, "MSMergeMSList", sep = "$"),
            Type = "MenuItem",
            DocumentType = "Any Documents",
            Action = "Function",
            Command = "menuMSMergeMSList",
            ShowDialogOnRun = T,
            MenuItemText = "&Merge msList Objects...",
            StatusBarText = "Merge msList objects",
			EnableMenuItem = T)

  guiCreate("MenuItem",
            Name = paste(MSProcessMenuName, "Separator2", sep = "$"),
            Type = "Separator",
            Action = "None")
            
            		
  guiCreate("MenuItem",
            Name = paste(MSProcessMenuName, "Prepare", sep = "$"),
            Type = "MenuItem",
            DocumentType = "Any Documents",
            Action = "Function",
            Command = "menuMSPrepare",
            ShowDialogOnRun = T,
            MenuItemText = "&Prepare msSet...",
            StatusBarText = "Prepare mass spec data",
			EnableMenuItem = T)

  guiCreate("MenuItem",
            Name = paste(MSProcessMenuName, "MSFilterMSSet", sep = "$"),
            Type = "MenuItem",
            DocumentType = "Any Documents",
            Action = "Function",
            Command = "menuMSFilterMSSet",
            ShowDialogOnRun = T,
            MenuItemText = "&Filter msSet Object...",
            StatusBarText = "Filter/subset msSet object",
			EnableMenuItem = T)


  guiCreate("MenuItem",
            Name = paste(MSProcessMenuName, "Separator3", sep = "$"),
            Type = "Separator",
            Action = "None")

  guiCreate( "MenuItem", 
  		    Name = paste(MSProcessMenuName, "Denoise", sep="$"),
		    Type = "MenuItem",
		    DocumentType = "Any Documents",
		    Action = "Function",
		    MenuItemText = "Noise Reduction...",
		    StatusBarText = "Denoise mass spec data",
		    Command = "menuMSDenoise",
		    ShowDialogOnRun = T,
		    Deletable = T,
		    Overwrite = F,
		    EnableMenuItem = T)

  guiCreate( "MenuItem", 
  		    Name = paste(MSProcessMenuName, "Noise", sep="$"),
		    Type = "MenuItem",
		    DocumentType = "Any Documents",
		    Action = "Function",
		    MenuItemText = "Noise Estimation...",
		    StatusBarText = "Local noise estimation",
		    Command = "menuMSNoise",
		    ShowDialogOnRun = T,
		    Deletable = T,
		    Overwrite = F,
		    EnableMenuItem = T)

 guiCreate( "MenuItem", Name = paste(MSProcessMenuName, "Detrend", sep="$"),
		    Type = "MenuItem",
		    DocumentType = "Any Documents",
		    Action = "Function",
		    MenuItemText = "Detrend...",
		    StatusBarText = "Baseline correction",
		    Command = "menuMSDetrend",
		    CommandParameters = "",
		    ShowDialogOnRun = T,
		    AlwaysUseDefaults = T,
		    Hide = F,
		    Deletable = T,
		    Overwrite = F,
		    EnableMenuItem = T)
		    		    		    		    		    		    
 guiCreate( "MenuItem", Name = paste(MSProcessMenuName, "Normalize", sep="$"),
		    Type = "MenuItem",
		    DocumentType = "Any Documents",
		    Action = "Function",
		    MenuItemText = "Normalize...",
		    StatusBarText = "Intensity Normalization",
		    Command = "menuMSNormalize",
		    CommandParameters = "",
		    ShowDialogOnRun = T,
		    AlwaysUseDefaults = T,
		    Hide = F,
		    Deletable = T,
		    Overwrite = F,
		    EnableMenuItem = T)

 guiCreate( "MenuItem", Name = paste(MSProcessMenuName, "Peak", sep="$"),
		    Type = "MenuItem",
		    DocumentType = "Any Documents",
		    Action = "Function",
		    MenuItemText = "Peak Detection...",
		    StatusBarText = "Peak detection",
		    Command = "menuMSPeak",
		    CommandParameters = "",
		    ShowDialogOnRun = T,
		    AlwaysUseDefaults = T,
		    Hide = F,
		    Deletable = T,
		    Overwrite = F,
		    EnableMenuItem = T)

 guiCreate( "MenuItem", Name = paste(MSProcessMenuName, "Align", sep="$"),
		    Type = "MenuItem",
		    DocumentType = "Any Documents",
		    Action = "Function",
		    MenuItemText = "Peak Alignment...",
		    StatusBarText = "Peak alignment",
		    Command = "menuMSAlign",
		    CommandParameters = "",
		    ShowDialogOnRun = T,
		    AlwaysUseDefaults = T,
		    Hide = F,
		    Deletable = T,
		    Overwrite = F,
		    EnableMenuItem = T)		    

 guiCreate( "MenuItem", Name = paste(MSProcessMenuName, "Quantify", sep="$"),
		    Type = "MenuItem",
		    DocumentType = "Any Documents",
		    Action = "Function",
		    MenuItemText = "Peak Quantification...",
		    StatusBarText = "Peak quantification",
		    Command = "menuMSQuantify",
		    CommandParameters = "",
		    ShowDialogOnRun = T,
		    AlwaysUseDefaults = T,
		    Hide = F,
		    Deletable = T,
		    Overwrite = F,
		    EnableMenuItem = T)	

  guiCreate("MenuItem",
            Name = paste(MSProcessMenuName, "Separator4", sep = "$"),
            Type = "Separator",
            Action = "None")

  guiCreate("MenuItem",
            Name = paste(MSProcessMenuName, "DisplayMSSet", sep = "$"),
            Type = "MenuItem",
            DocumentType = "Any Documents",
            Action = "Function",
            MenuItemText = "&Display msSet...",
            StatusBarText = "Display an msSet object",
            Command = "menuMSDisplayMSSet",
            ShowDialogOnRun = T,
			EnableMenuItem = T)           

#  guiCreate("MenuItem",
#            Name = paste(MSProcessMenuName, "ImageMSSet", sep = "$"),
#            Type = "MenuItem",
#            DocumentType = "Any Documents",
#            Action = "Function",
#            MenuItemText = "&Image msSet...",
#            StatusBarText = "Image an msSet object",
#            Command = "menuMSImageMSSet",
#            ShowDialogOnRun = T,
#			EnableMenuItem = T)                       
            		    
 invisible()
}	    
