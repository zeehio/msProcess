## $Id: //depot/Research/msProcess/pkg/msProcess/swingui/R/removeMSGUI.q#1 $
## $DateTime: 2008/04/23 13:35:19 $

"removeMSGUI" <-
function()
{
  if(!exists("guiRemove"))
    stop("Not running under the Windows GUI")

  ## Remove the Properties for MS
  itemNames <- guiGetObjectNames("Property")
  msItems <- rev(itemNames[grep("MS", itemNames)])
  for(i in msItems)
    guiRemove("Property", name = i)

  ## Remove the FunctionInfo for MS
  itemNames <- guiGetObjectNames("FunctionInfo")
  msItems <- rev(itemNames[grep("menuMS", itemNames)])
  for(i in msItems)
    guiRemove("FunctionInfo", name = i)

  ## Remove the MenuItems for MS
  itemNames <- guiGetObjectNames("MenuItem")
  msItems <- rev(itemNames[grep("MS", itemNames)])
  for(i in msItems)
    guiRemove("MenuItem", name = i)

  return(invisible())
}
