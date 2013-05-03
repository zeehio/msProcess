## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/backMSImport.q#3 $
## $DateTime: 2008/08/28 16:00:53 $

backMSImport = function(data){

	initialmsg = cbIsInitDialogMessage(data)
	rollbackmsg = cbIsRollbackMessage(data)
	activeprop = cbGetActiveProp(data)

	## check dir
	if(activeprop == "MSImportDirName"){
		if(!is.dir(cbGetCurrValue(data, "MSImportDirName"))){
      		guiDisplayMessageBox(
      			paste("directory '", cbGetCurrValue(data, "MSImportDirName"), "'",
      			"does not exist. Please enter another directory name for data import."),
      			button = c("Ok"),
				icon = c("error"))
			return(data)		
		}
	}
			
	## browse for directory
	if(activeprop == "MSImportBrowseDirButton"){
#		dir = guiShowStandardFileDialog(dialogType = "folder") 
		if(length(file <- guiShowStandardFileDialog())!=0){ 
			dir = unpaste(file, sep = dirSeparator())
			file = unlist(dir[length(dir)])	
			dir = paste(unlist(dir[-length(dir)]), collapse = dirSeparator())
			data = cbSetCurrValue(data, "MSImportDirName", dir)
			data = cbSetOptionList(data, "MSImportFileList", paste(files.in.dir(dir), collapse = ","))
			data = cbSetCurrValue(data, "MSImportFileList", file)
		}
	}

	## check MSImportUseFileFilter
	if(activeprop == "MSImportUseFileFilter"){
		checked = (cbGetCurrValue(data, "MSImportUseFileFilter")=="T")
      	data = cbSetEnableFlag(data, "MSImportFilterPattern", checked)
      	data = cbSetEnableFlag(data, "MSImportFileList", !checked)
	}
	
	data
}
