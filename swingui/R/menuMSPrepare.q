## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/menuMSPrepare.q#3 $
## $DateTime: 2008/04/29 16:30:51 $

menuMSPrepare = function(x, 
						 mass.min=1500, 
						 transform=NULL,
						 data.name=NULL, 
						 printObj = T, 
						 saveAs = paste(deparse(substitute(x)), ".prep", sep = "")){

	if(!is.null(transform) && is.all.white(transform))transform = NULL
	if(!is.null(transform)){
		transform = switch(transform, 
							"<None>" = NULL,
							"log" = log,
							"log10" = log10,
							"sqrt" = sqrt,
							"cubert" = function(x)x^(1/3),
							get(transform))
	}
	out = msPrepare(x = x, data.name = data.name, mass.min = mass.min, transform = transform)
	assign(saveAs, out, where = 1)
	
	if(printObj) print(out)
	invisible()
}

# menuMSProcessSelectPrepare = function()


