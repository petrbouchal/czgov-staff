source('./src/lib/lib_PubFinCZ.R')
#uu0 <- LoadDataforPlotting('chapters')

uu <- uu0

uu$grouping <- as.character(uu$sheetname)
uu$grouping[uu$sheetname=='UO' & uu$Ministerstvo==TRUE]  <- 'UO - Ministerstva'
uu$grouping[uu$sheetname=='UO' & uu$Ministerstvo==FALSE]  <- 'UO - Ostatní'

uu <- uu[uu$Udaj=='PlatyOPPP' | uu$Udaj=='Platy' | uu$Udaj=='OPPP' | uu$Udaj=='Zam',]
uu$UdajStage <- paste0(uu$Udaj,'_',uu$BudgetStage)
uu$Udaj <- NULL
uu$BudgetStage <- NULL
uu$rok <- as.character(uu$Year)
uu$grouping <- as.character(uu$sheetname)
uu$grouping[uu$sheetname=='UO' & uu$Ministerstvo==TRUE]  <- 'UO - Ministerstva'
uu$grouping[uu$sheetname=='UO' & uu$Ministerstvo==FALSE]  <- 'UO - Ostatní'

#create wide dataset
uuw <- cast(uu,KapNum+KapAbb+KapName+grouping+Year~UdajStage, .progress='text')

# calculate means and indices
uuw$AvgSal_upraveny <- uuw$Platy_upraveny/uuw$Zam_upraveny*1000/12
uuw$AvgSal_skutecnost <- uuw$Platy_skutecnost/uuw$Zam_skutecnost*1000/12
uuw$Zam_upr2skut <- uuw$Zam_skutecnost/uuw$Zam_upraveny
uuw$AvgSal_skutMinusupr <- uuw$AvgSal_skutecnost-uuw$AvgSal_upraveny
uuw$AvgSal_skutPercupr <- uuw$AvgSal_skutMinusupr/uuw$AvgSal_upraveny

# melt this back into long format
uuw <- as.data.frame(uuw)
uu <- melt(uuw,id.vars=c('KapNum','KapAbb','KapName','Year','grouping'))

# create marker for index and pure budget data
# HERE

# separate 'variable' back into stage and udaj, aka create stage markers 
# HERE

# add base value value
uu <- uu[,c(4,1:3,5:7)]
uu <- AddBaseValue(uu,'2003-01-01',c(2:7))

# add lagged value - assuming 11 time periods
#uu <- ddply(uu, .(KapNum, grouping, UdajStage),
#            transform, valuelag = c(NA, value[-11]))

# add deflator data
uu <- AddDeflators(uu)

# calculate nominal and real changes in pay as budgeted and as turned out
uu$realchange <- uu$perc_base/uu$Infl2003Base

plot <- ggplot(uu[uu$variable=='AvgSal_skutecnost' & uu$grouping=='UO - Ministerstva',],
               aes(x=Year,y=realchange)) +
  geom_bar(stat='identity') +
  facet_wrap(~KapAbb) + theme_classic()
plot

plot <- ggplot(uuw[uuw$Platy_skutecnost!=0 & uuw$Year=='2012-01-01',],
               aes(x=grouping, fill=Zam_skutecnost)) +
  geom_bar(stat='identity', aes(y=AvgSal_skutMinusupr)) +
  scale_fill_continuous(low='yellow',high='red') +
  scale_y_continuous(limits=c(-2000,10000)) +
  facet_wrap(~KapAbb) + theme_classic() + coord_flip()
plot

plot <- ggplot(uuw[uuw$Platy_skutecnost!=0 & uuw$Year=='2012-01-01',],
               aes(x=grouping, fill=Zam_skutecnost)) +
  geom_bar(stat='identity', aes(y=Zam_upr2skut-1)) +
  scale_fill_continuous(low='yellow',high='red') +
  facet_wrap(~KapAbb) + theme_gray() + coord_flip()
plot

uuw2 <- uuw[uuw$grouping!='StatniSprava' & uuw$grouping!='ROPO celkem' & 
              uuw$grouping!='OSS-RO',]
plot <- ggplot(uuw2,
               aes(y=Zam_schvaleny,x=as.character(Year), fill=grouping)) +
  geom_bar(stat='identity',position='stack') +
  facet_wrap(~KapAbb,scales='free_y') + theme_gray()
plot

