## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSDisplayMSList.q#3 $
## $DateTime: 2008/08/28 16:00:53 $

menuMSDisplayMSList = function(x,				#1
							subset = 1, 		#2
							doPlot = T,			#3
							printObj = T,		#4
							printSummary = T, 	#5
							type = "l", 		#6
							row.layout = 1,		#7
							col.layout = 1)		#8
{
	#subset
	if(is.null(subset) || is.all.white(subset) || subset == "<All>"){ 
		subset = names(x)[seq(length(x))]
	} else {
		subset = parseVectorString(subset)
	}
	
	Names = names(x)
	x.subset = x[seq(Names)[match(subset, Names)]]

	#type
	type = substring(type, 1, 1)
	
	#plot
	if(doPlot){		
		#layout	
		opar = par(mfrow = c(row.layout, col.layout))
		on.exit(par(opar))
			
		for(i in subset){	
			plot(x = x.subset, index = i, type = type, add = T, main = i)
		}
	}
	
	#print
	if(printObj) print(x.subset)
	
	#summary
	if(printSummary) print(summary(x.subset))
	
	invisible()
}
