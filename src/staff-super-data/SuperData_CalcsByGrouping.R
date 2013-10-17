source('./src/lib/lib_PubFinCZ.R')
uu0 <- LoadDataforPlotting('chapters')
uu <- uu0

# summarise by layer of civil service
hh <- ddply(uu,.(Year,grouping,Udaj,BudgetStage), summarise,
            value=sum(value, na.rm=T),
            .progress = "text")

# add variable for base value and change from base vars
hh <- AddBaseValue(hh,'2003-01-01',2:7)

hh$YearDate <- hh$Year
hh$Year <- as.character(hh$Year)
hh <- AddDeflators(hh)

plot <- ggplot(hh[hh$Udaj=='Zam' & hh$BudgetStage=='schvaleny',],
               aes(YearDate, value, colour=grouping)) +
  geom_line() +
  scale_y_continuous(labels=comma)
plot

plot <- ggplot(hh[hh$Udaj=='Zam' & hh$value!=0,],
               aes(Year, perc_base, colour=BudgetStage)) +
  geom_line(aes(group=BudgetStage),size=1) +
  scale_y_continuous(labels=percent) +
  facet_wrap(~ grouping,scales='free_y') +
  theme(legend.key.width=unit(2,'cm'))
plot

hh <- hh[hh$Udaj=='PlatyOPPP' | hh$Udaj=='Platy' | hh$Udaj=='OPPP' | hh$Udaj=='Zam',]
hh$UdajStage <- paste0(hh$Udaj,'_',hh$BudgetStage)
hh$Udaj <- NULL
hh$BudgetStage <- NULL

# reshape
hhw <- cast(hh,grouping+Year~UdajStage)
hhw$Platy_upr2skut <- hhw$Platy_upraveny/hhw$Platy_skutecnost
hhw$Zam_upr2skut <- hhw$Zam_upraveny/hhw$Zam_skutecnost
hhw$OPPP_upr2skut <- hhw$OPPP_upraveny/hhw$OPPP_skutecnost

# calculate average pay and indices
hhw$AvgSal_upraveny <- hhw$Platy_upraveny/hhw$Zam_upraveny*1000/12
hhw$AvgSal_skutecnost <- hhw$Platy_skutecnost/hhw$Zam_skutecnost*1000/12
hhw$AvgSal_upr2skut <- hhw$AvgSal_upraveny/hhw$AvgSal_skutecnost
hhw$AvgSal_uprMinusskut <- hhw$AvgSal_upraveny-hhw$AvgSal_skutecnost

# plots
plot <- ggplot(hhw, aes(x=Year, y=-AvgSal_uprMinusskut, fill=Zam_skutecnost)) +
  geom_bar(stat='identity') +
  scale_fill_continuous(low='yellow',high='red') +
  facet_wrap(~grouping) + theme_classic()
plot

plot <- ggplot(hhw, aes(x=Year, y=AvgSal_skutecnost, fill=Zam_skutecnost)) +
  geom_line(size=1,aes(y=AvgSal_skutecnost),colour='red') +
  geom_line(size=1, aes(y=AvgSal_upraveny),colour='yellow') +
  scale_fill_continuous(low='yellow',high='red') +
  facet_wrap(~grouping) + theme_classic()
plot

plot <- ggplot(hhw[hhw$Platy_skutecnost!=0,], aes(x=Year, fill=Zam_skutecnost)) +
  geom_line(size=1,aes(y=Platy_skutecnost),colour='red') +
  geom_line(size=1, aes(y=Platy_upraveny),colour='yellow') +
  scale_fill_continuous(low='yellow',high='red') +
  facet_wrap(~grouping) + theme_classic()
plot