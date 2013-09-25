source('./src/lib/lib_PubFinCZ.R')

require('XML')

xmlfile='/Users/petrbouchal/Downloads/Pokladna/Ciselniky/CUS0004_20100615_Smerna_uctova_osnova.xml'
tree <- xmlTreeParse(xmlfile)
xmltop <- xmlRoot(tree)
xmldata <- xmlSApply(xmltop[[8]][[1]], function(x) xmlSApply(x, xmlValue));str(xmldata)
xmldata_df <- data.frame(t(xmldata),row.names=NULL)

xmlfile='/Users/petrbouchal/Downloads/Pokladna/Ciselniky/CUS0003_20100615_Ciselnik_vykazu.xml'
tree <- xmlTreeParse(xmlfile)
xmltop <- xmlRoot(tree)
xmldata <- xmlSApply(xmltop[[8]][[1]], function(x) xmlSApply(x, xmlValue));str(xmldata)
xmldata_df <- data.frame(t(xmldata),row.names=NULL)

xmlfile='/Users/petrbouchal/Downloads/Pokladna/Ciselniky/RISRE0002_20130828_Rozpoctova_polozka.XML'
tree <- xmlTreeParse(xmlfile)
xmltop <- xmlRoot(tree)
xmldata <- xmlSApply(xmltop[[1]], function(x) xmlSApply(x, xmlValue));str(xmldata)
xmldata_df <- data.frame(t(xmldata),row.names=NULL)