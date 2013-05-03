## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/loadMSProps.q#3 $
## $DateTime: 2008/06/12 09:34:09 $

loadMSProps = function()
{
	package.path = system.file(package = "msProcess")
	prop.path <- paste(package.path, ".Prefs/package.prp", sep = "/")
	info.path <- paste(package.path, ".Prefs/package.fni", sep = "/")
	guiLoadDefaultObjects("Property", FileName = prop.path)
	guiLoadDefaultObjects("FunctionInfo", FileName = info.path)
}
