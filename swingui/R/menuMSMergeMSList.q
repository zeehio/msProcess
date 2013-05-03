## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSMergeMSList.q#3 $
## $DateTime: 2008/07/02 14:57:21 $

menuMSMergeMSList = function(objs, 
						 	printObj = T, 
						 	printSummary = T,
						 	saveAs = "merged.msList")
{


	objs = parseVectorString(objs)
	objs = lapply(objs, get)
	
	out = do.call("merge", objs)
	
	assign(saveAs, out, where = 1)
	
	if(printObj) print(out)
	
	if(printSummary) print(summary(out))
	invisible()
}
