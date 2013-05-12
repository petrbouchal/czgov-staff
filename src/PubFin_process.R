inc <- read.csv('../data-input/VydajeVysledovka_20130511_2309.csv')
bud <- read.csv('../data-input/VydajeDruhove_20130511_2309.csv')

bud$podil <- bud$pozmenach/bud$skutecnost
hist(bud$podil[bud$orgyear==2012],breaks=1000,xlim=c(0.1,1.9))