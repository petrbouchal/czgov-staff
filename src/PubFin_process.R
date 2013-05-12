inc <- read.csv('./data-input/VydajeVysledovka_20130511_2309.csv')
bud <- read.csv('./data-input/VydajeDruhove_20130511_2309.csv')

bud$podil <- bud$pozmenach/bud$skutecnost
hist(bud$podil[bud$rowname=="Platy zaměstnanců v pracovním poměru" & bud$orgtype=='OSS'],
     breaks=10000,xlim=c(0.85,1.2), ylim=c(0,20))

bud$overspend <- (bud$podil-1)*bud$skutecnost
sum(bud$overspend[bud$orgtype=="OSS" & bud$rowlevel==0],na.rm=TRUE)
mean(bud$overspend[bud$orgtype=="OSS" & bud$rowlevel==0],na.rm=TRUE)
mean(bud$podil[bud$orgtype=="OSS" & bud$rowlevel==0],na.rm=TRUE)
sum(bud$overspend[bud$orgtype=="OSS" & bud$rowname=="Platy zaměstnanců v pracovním poměru"],na.rm=TRUE)
mean(bud$overspend[bud$orgtype=="OSS" & bud$rowname=="Platy zaměstnanců v pracovním poměru"],na.rm=TRUE)
mean(bud$podil[bud$orgtype=="OSS" & bud$rowname=="Platy zaměstnanců v pracovním poměru"],na.rm=TRUE)

yearsub=2011
bud_yr <- subset(bud, bud$orgyear==yearsub)

sumoverspend=sum(bud_yr$overspend[bud_yr$orgtype=="OSS" & bud_yr$rowname=="Platy zaměstnanců v pracovním poměru"],na.rm=TRUE)
sumtotal=sum(bud_yr$skutecnost[bud_yr$orgtype=="OSS" & bud_yr$rowname=="Platy zaměstnanců v pracovním poměru"],na.rm=TRUE)
sumoverspend
sumtotal
sumoverspend/sumtotal