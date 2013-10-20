# Load and prep -----------------------------------------------------------

source('./src/lib/lib_PubFinCZ.R')
uu0 <- LoadDataforPlotting('chapters')
uu <- uu0

source('./src/staff-super-data/SuperData_FirstReshapeAndCalcs.R')

# Calculate indices and changes between budget stages ---------------------

# reshape to wide - expand variables
uu$UdajStage <- paste0(uu$Udaj,'_',uu$BudgetStage)
uu$Udaj <- NULL
uu$BudgetStage <- NULL

uuw2 <- cast(uu,grouping+Year+KapAbb+KapNum+KapName+Ministerstvo+groupingsum~UdajStage)

# calculate average pay and indices
uuw2 <- CalcAvgsAndIndices(uuw2)

# turn back into long
uuw2 <- as.data.frame(uuw2)
uu <- melt(uuw2, id.vars=c('Year','grouping','KapAbb','KapName','KapNum','groupingsum','Ministerstvo'))

uu <- AddBaseValue(uu,'2003-01-01')
uu <- AddEconIndicators(uu)

# create markers for budget stages and type of data (budget x index)
# HERE

# create markers for groups of groupings
uu$exekutiva <- FALSE
uu$exekutiva[uu$grouping=='UO - Ministerstva' | uu$grouping=='UO - Ostatní' | 
               uu$grouping=='OSS-SS']  <- TRUE
uu$UO <- FALSE
uu$UO[uu$grouping=='UO - Ministerstva' | uu$grouping=='UO - Ostatní'] <- TRUE

# calculate real changes
uu$realchange <- uu$perc_base/uu$Infl2003Base

# Plots -------------------------------------------------------------------

# simple composition in terms of additive categories of organisation
uu2 <- uu[uu$groupingsum==F & uu$variable=='Zam_skutecnost' & uu$UO & uu$value!=0,]
uu2 <- uu2[with(uu2, order(grouping)), ]
uu2$value[uu2$value==0] <- NA
plot <- ggplot(uu2,aes(y=value,x=Year,fill=grouping,group=grouping)) +
  geom_bar(stat='identity',position='stack') +
  facet_wrap(~KapAbb,scales='free_y')
plot

# real change in pay since 2003
uu2 <- uu[uu$variable=='AvgSal_skutecnost' & uu$grouping=='UO - Ministerstva' & 
            uu$Ministerstvo,]
plot <- ggplot(uu2,aes(x=Year,y=realchange)) +
  geom_line(size=1) +
  scale_y_continuous(labels=percent) +
  facet_wrap(~KapAbb) + theme_classic()
plot

# differential between budgeted and actual salary, by layer and chapter in 2012
uu2 <- uu[uu$Year=='2012-01-01' & uu$variable=='AvgSal_uprMinusskut' & !uu$groupingsum,]
plot <- ggplot(uu2,aes(x=grouping, fill=grouping)) +
  geom_bar(stat='identity', aes(y=-value)) +
  scale_y_continuous(limits=c(-2000,10000)) +
  facet_wrap(~KapAbb) + theme_igray() + coord_flip()
plot

# same thing in percentage terms
uu2 <- uu[uu$variable=='AvgSal_upr2skut' & uu$Year=='2012-01-01' & !uu$groupingsum,]
plot <- ggplot(uu2,aes(x=grouping, fill=grouping)) +
  geom_bar(stat='identity', aes(y=1-value)) +
  scale_y_continuous(labels=percent) +
  facet_wrap(~KapAbb) + coord_flip()
plot

# composition of jobs gap
uu2 <- uu[uu$variable=='Zam_uprMinusskut' & uu$groupingsum==F,]
uu2 <- uu2[with(uu2, order(grouping)), ]
plot <- ggplot(uu2,aes(x=Year, fill=grouping)) +
  geom_bar(stat='identity', aes(y=value)) +
  facet_wrap(~KapAbb, scales='free_y')
plot

# central body pay 
uu2 <- uu[(uu$variable=='AvgSal_skutecnost' | uu$variable=='AvgSal_upraveny') &
            uu$grouping=='UO',]
plot <- ggplot(uu2,aes(x=Year, group='KapAbb', colour=variable)) +
  geom_line() +
  facet_wrap(~KapAbb, scales='free_y')
plot
