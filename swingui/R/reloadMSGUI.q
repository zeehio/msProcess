## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/reloadMSGUI.q#3 $
## $DateTime: 2008/05/15 15:42:19 $

guiPath = "d:\\Research\\packages_s\\msProcess\\swingui"
# loadPath = "d:\\Research\\Proteome"

reloadMSGUI = function(){
	removeMSGUI()
	
	funs.files = files.in.dir(paste(guiPath, "R", sep = "\\"))
	funs.files = funs.files[grep("q$", funs.files)]
	for(i in funs.files){
		source(paste(guiPath, "R", i, sep = "\\"))
	}
	gui.files = files.in.dir(paste(guiPath, "guicreate", sep = "\\"))
	gui.files = gui.files[grep("ssc$", gui.files)]
	for(i in gui.files){
		source(paste(guiPath, "guicreate", i, sep = "\\"))
	}
	loadMSMenu()
}

