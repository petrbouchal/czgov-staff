source('./src/lib/lib_PubFinCZ.R')

icomatch <- read.csv('./data-output/ICOnazev.csv',colClasses='character')
icomatch$ZC_ICO.ZC.ICO <- icomatch$orgid
icomatch$orgid <- NULL
finm201 <- read.csv('/Users/petrbouchal/Downloads/Pokladna/2012/2012_12_Data_CSUIS_FINM/FINM201.csv',
                    colClasses='character')

finm201x <- merge(finm201, icomatch, )