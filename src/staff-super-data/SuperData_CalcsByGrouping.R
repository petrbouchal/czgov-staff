# Load and prep -----------------------------------------------------------

source('./src/lib/lib_PubFinCZ.R')
uu0 <- LoadDataforPlotting('chapters')
uu <- uu0

# MV causes distortions (2011-12 reclass of police in core MV) - can exclude
uu$todiscard <- FALSE
uu$todiscard[uu$KapAbb=='MV' & uu$sheetname=='UO'] <- TRUE
uu <- uu[uu$todiscard==FALSE,]

source('./src/staff-super-data/SuperData_FirstReshapeAndCalcs.R')

# summarise by layer of civil service
hh <- ddply(uu,.(Year,grouping,groupingsum,Udaj,BudgetStage), summarise,
            value=sum(value, na.rm=T),
            .progress = "text")

# Calculate indices and changes between budget stages ---------------------

# reshape to wide - expand variables
hh$UdajStage <- paste0(hh$Udaj,'_',hh$BudgetStage)
hh$Udaj <- NULL
hh$BudgetStage <- NULL

hhw2 <- cast(hh,grouping+Year+groupingsum~UdajStage)

# calculate average pay and indices
hhw2 <- CalcAvgsAndIndices(hhw2)

# turn back into long
hhw2 <- as.data.frame(hhw2)
hh <- melt(hhw2, id.vars=c('Year','grouping','groupingsum'))

hh <- AddBaseValue(hh,'2003-01-01')
hh <- AddEconIndicators(hh)
hh$realchange <- hh$perc_base/hh$Infl2003Base

# create markers for budget stages and type of data (budget x index)
# HERE

# create markers for groups of groupings
hh$exekutiva <- FALSE
hh$exekutiva[hh$grouping=='UO - Ministerstva' | hh$grouping=='UO - Ostatní' | 
               hh$grouping=='OSS-SS']  <- TRUE
hh$UO <- FALSE
hh$UO[hh$grouping=='UO - Ministerstva' | hh$grouping=='UO - Ostatní'] <- TRUE


# Plots -------------------------------------------------------------------

# % changes to nominal average salary by grouping
title='Plat ve státní správě jako podíl průměrného platu, podle typu organizace od roku 2003: rozpočet a skutečnost\n'
ylab='% rozdíl od průměrné mzdy v ČR (v Praze pro ústřední orgány)'
xlab=''
hh2 <- hh[hh$groupingsum==FALSE & hh$Year!='2013-01-01' & hh$variable=='Zam_skutecnost',]
plot <- ggplot(hh2,aes(Year, perc_base, colour=grouping, group=grouping)) +
  geom_line(size=1) +
  scale_y_continuous(labels=percent) + 
  labs(title=title, x=xlab, y=ylab)
plot

# % changes to real average salary by grouping
title='Plat ve státní správě jako podíl průměrného platu, podle typu organizace od roku 2003: rozpočet a skutečnost\n'
ylab='% rozdíl od průměrné mzdy v ČR (v Praze pro ústřední orgány)'
xlab=''
hh2 <- hh[hh$groupingsum==FALSE & hh$variable=='Platy_upraveny',]
plot <- ggplot(hh2, aes(Year, realchange, colour=grouping, group=grouping)) +
  geom_line(size=1) +
  scale_y_continuous(labels=percent) + 
  labs(title=title, x=xlab, y=ylab)
plot

#paybill turnout
title='Plat ve státní správě jako podíl průměrného platu, podle typu organizace od roku 2003: rozpočet a skutečnost\n'
ylab='% rozdíl od průměrné mzdy v ČR (v Praze pro ústřední orgány)'
xlab=''
hh2 <- hh[hh$groupingsum==FALSE & hh$Year!='2013-01-01' & hh$variable=='Platy_skutecnost',]
plot <- ggplot(hh2,aes(Year, value, fill=grouping, group=grouping)) +
  geom_area(stat='identity',position='stack') +
  scale_y_continuous(labels=comma) + 
  labs(title=title, x=xlab, y=ylab)
plot

# paybill adjusted for inflation by grouping, turnout
title='Plat ve státní správě jako podíl průměrného platu, podle typu organizace od roku 2003: rozpočet a skutečnost\n'
ylab='% rozdíl od průměrné mzdy v ČR (v Praze pro ústřední orgány)'
xlab=''
hh2 <- hh[hh$groupingsum==FALSE & hh$Year!='2013-01-01' & hh$variable=='Platy_skutecnost',]
plot <- ggplot(hh2,aes(Year, value/Infl2003Base, fill=grouping, group=grouping)) +
  geom_area(stat='identity',position='stack') +
  scale_y_continuous(labels=comma) + 
  labs(title=title, x=xlab, y=ylab)
plot

# staff numbers by grouping, as budgeted
title='Plat ve státní správě jako podíl průměrného platu, podle typu organizace od roku 2003: rozpočet a skutečnost\n'
ylab='% rozdíl od průměrné mzdy v ČR (v Praze pro ústřední orgány)'
xlab=''
hh2 <- hh[hh$groupingsum==FALSE & hh$Year!='2013-01-01',]
hh2 <- hh2[with(hh2, order(grouping)), ]
plot <- ggplot(hh2[hh2$variable=='Zam_upraveny',],
               aes(Year, value, fill=grouping, group=grouping)) +
  geom_area(stat='identity',position='stack') +
  scale_y_continuous(labels=comma) + 
  labs(title=title, x=xlab, y=ylab)
plot

# staff gap by grouping, as budgeted
title='Plat ve státní správě jako podíl průměrného platu, podle typu organizace od roku 2003: rozpočet a skutečnost\n'
ylab='% rozdíl od průměrné mzdy v ČR (v Praze pro ústřední orgány)'
xlab=''
hh2 <- hh[hh$groupingsum==FALSE & hh$Year!='2013-01-01' & hh$variable=='Zam_upr2skut',]
hh2 <- hh2[with(hh2, order(grouping)), ]
plot <- ggplot(hh2, aes(Year, value-1, fill=grouping, group=grouping)) +
  geom_bar(stat='identity',position='dodge') +
  scale_y_continuous(labels=comma) + facet_wrap(~grouping) + 
  labs(title=title, x=xlab, y=ylab)
plot

# salary 'raise' budgeting effect, by grouping
title='Plat ve státní správě jako podíl průměrného platu, podle typu organizace od roku 2003: rozpočet a skutečnost\n'
ylab='% rozdíl od průměrné mzdy v ČR (v Praze pro ústřední orgány)'
xlab=''
hh2 <- hh[hh$variable=='AvgSal_uprMinusskut' & hh$groupingsum==F,]
plot <- ggplot(hh2, aes(x=Year, y=-value, fill=grouping)) +
  geom_bar(stat='identity') +
  facet_wrap(~grouping) + 
  labs(title=title, x=xlab, y=ylab)
plot

title='Plat ve státní správě jako podíl průměrného platu, podle typu organizace od roku 2003: rozpočet a skutečnost\n'
ylab='% rozdíl od průměrné mzdy v ČR (v Praze pro ústřední orgány)'
xlab=''
hh2 <- hh[hh$variable=='AvgSal_skutecnost',]
plot <- ggplot(hh2,aes(x=Year, y=perc_base/Infl2003Base,group=grouping)) +
  geom_line(size=1,aes(colour=grouping)) + theme_classic() +
  scale_y_continuous(labels=percent) + 
  labs(title=title, x=xlab, y=ylab)
plot

# average salary as budgeted, as % of national average salary
title='Plat ve státní správě jako podíl průměrného platu, podle typu organizace od roku 2003: rozpočet a skutečnost\n'
ylab='% rozdíl od průměrné mzdy v ČR (v Praze pro ústřední orgány)'
xlab=''
hh2 <- hh[hh$variable=='AvgSal_schvaleny',]
plot <- ggplot(hh2,aes(x=Year, y=value/czsal_all,group=grouping)) +
  geom_line(size=1,aes(colour=grouping)) + theme_classic() +
  scale_y_continuous(labels=percent) + 
  labs(title=title, x=xlab, y=ylab)
plot

# average salary, as budgeted and turnout, real change from 2003
#in this one the adjustment for top managers is incorrectly deflated
title='Plat ve státní správě jako podíl průměrného platu, podle typu organizace od roku 2003: rozpočet a skutečnost\n'
ylab='% rozdíl od průměrné mzdy v ČR (v Praze pro ústřední orgány)'
xlab=''
hh2 <- hh[hh$variable=='AvgSal_skutecnost' | hh$variable=='AvgSal_upraveny',]
plot <- ggplot(hh2,
               aes(x=Year, y=value, group=variable)) +
  geom_line(size=1, aes(y=(value-1120)/(value_base-1120)/Infl2003Base,colour=variable)) +
  geom_line(size=1, aes(y=czsal_all/czsal_all_2003/Infl2003Base),colour='yellow') +
  geom_line(data=hh2[hh2$grouping=='UO - Ministerstva',],size=1,
            aes(y=phasal_all/phasal_all_2003/Infl2003Base,),colour='black') +
  facet_wrap(~grouping) + theme_classic() + 
  labs(title=title, x=xlab, y=ylab)
plot

#in this one the adjustment for top managers is incorrectly deflated
title='Plat ve státní správě jako podíl průměrného platu, podle typu organizace od roku 2003: rozpočet a skutečnost\n'
ylab='% rozdíl od průměrné mzdy v ČR (v Praze pro ústřední orgány)'
xlab=''
hh2 <- hh[hh$variable=='AvgSal_skutecnost' | hh$variable=='AvgSal_upraveny',]
plot <- ggplot(hh2,aes(x=Year, y=value, group=variable)) +
  geom_line(size=1,data=hh2[hh2$grouping!='UO - Ministerstva',],
            aes(y=value/czsal_all-1,colour=variable)) +
  geom_line(size=1,data=hh2[hh2$grouping=='UO - Ministerstva',],
            aes(y=value/phasal_all-1,colour=variable)) +
  geom_line(size=1,data=hh2[hh2$grouping=='UO - Ministerstva',],
            aes(y=(value-500/Infl2013Base)/phasal_all-1,colour=variable),
            linetype='dashed') +
  scale_y_continuous(labels=percent) +
  facet_wrap(~grouping) + 
  labs(title=title, x=xlab, y=ylab)
plot
