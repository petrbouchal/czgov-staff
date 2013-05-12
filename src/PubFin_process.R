isdata <- read.csv('../data-input/VydajeVysledovka_20130511_2309.csv')
bsdata <- read.csv('../data-input/VydajeDruhove_20130511_2309.csv')

bsdata$podil <- bsdata$pozmenach/bsdata$skutecnost
hist(bsdata$podil[bsdata$orgyear==2012],breaks=1000000000,xlim=c(0,2))