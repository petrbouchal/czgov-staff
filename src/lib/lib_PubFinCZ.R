source('./src/lib/load_packages.R')

# Design elements to be picked up by libraries ----------------------------

fontfamily='Helvetica'
basecols <- c('#37424a','#00ccff','#d40072','#83389b','#7a9393')
mycols <- TintShade(basecols,tints=c(.5,.25),hexin=T,)
dimnames(colours) <- NULL
rm(basecols)
#font_import()
loadfonts(device='postscript',quiet=TRUE)
loadfonts(quiet=TRUE)
if(Sys.info()[['sysname']]=='Darwin') {mysys='mac'} else {mysys='win'}
if(mysys=='win') {loadfonts(device='win',quiet=TRUE)}

# Load functions / libraries ----------------------------------------------

source('./src/lib/LoadDataforPlotting.R')
source('./src/lib/SavePlot.R')
source('./src/lib/SortGroups.R')
source('./src/lib/TintShade.R')
source('./src/lib/GetColorTable.R')
source('./src/lib/rgb2col.R')
source('./src/lib/custom_themes.R')