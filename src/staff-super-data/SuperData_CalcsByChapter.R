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
uu$UO[uu$grouping=='UO - Ministerstva' | uu$grouping=='UO - Ostatní' | 
        uu$grouping=='UO'] <- TRUE

# calculate real changes
uu$realchange <- uu$perc_base/uu$Infl2003Base

# Plots -------------------------------------------------------------------

# simple composition in terms of additive categories of organisation
title='Zaměstnanci kapitol podle typu organizace (rozpočty)'
ylab='Počet zaměstnanců'
xlab=''
uu2 <- uu[uu$groupingsum==F & uu$variable=='Zam_schvaleny' & uu$value!=0,]
uu2 <- uu2[with(uu2, order(grouping)), ]
uu2$value[uu2$value==0] <- NA
plot <- ggplot(uu2,aes(y=value,x=Year,fill=grouping,group=grouping)) +
  geom_bar(stat='identity',position='stack') +
  scale_y_continuous(labels=comma) +
  facet_wrap(~KapAbb,scales='free_y') +
  labs(title=title, x=xlab, y=ylab)
plot

# real change in pay since 2003
title='Změna průměrné hrubé mzdy očistěná o inflaci\nMinisterstva, skutečný stav, 2003=100%'
ylab='% změna od 2003, očištěno o inflaci'
xlab=''
uu2 <- uu[uu$variable=='AvgSal_skutecnost' & uu$grouping=='UO - Ministerstva' & 
            uu$Ministerstvo,]
plot <- ggplot(uu2,aes(x=Year,y=realchange)) +
  geom_line(size=1) +
  scale_y_continuous(labels=percent) +
  facet_wrap(~KapAbb) + 
  labs(title=title, x=xlab, y=ylab)
plot

# differential between budgeted and actual salary, by layer and chapter in 2012
title='Průměrný plat: rozdíl mezi rozpočtem a výsledkem podle kapitoly a typu organizace'
ylab='Rozdíl v Kč'
xlab=''
uu2 <- uu[uu$Year=='2012-01-01' & uu$variable=='AvgSal_uprMinusskut' & !uu$groupingsum,]
plot <- ggplot(uu2,aes(x=grouping, fill=grouping)) +
  geom_bar(stat='identity', aes(y=-value)) +
  scale_y_continuous(limits=c(-2000,10000)) +
  facet_wrap(~KapAbb) +
  labs(title=title, x=xlab, y=ylab)
plot

# same thing in percentage terms
title='Průměrný plat: rozdíl mezi rozpočtem a výsledkem podle kapitoly a typu organizace'
ylab='Rozdíl v %'
xlab=''
uu2 <- uu[uu$variable=='AvgSal_upr2skut' & uu$Year=='2012-01-01' & !uu$groupingsum,]
plot <- ggplot(uu2,aes(x=grouping, fill=grouping)) +
  geom_bar(stat='identity', aes(y=1-value)) +
  scale_y_continuous(labels=percent) +
  facet_wrap(~KapAbb) +
  labs(title=title, x=xlab, y=ylab)
plot

# composition of jobs gap
title='Zaměstnanci: rozdíl mezi rozpočtem a výsledkem podle kapitoly a typu organizace'
ylab='Rozdíl v počtech zaměstnanců'
xlab=''
uu2 <- uu[uu$variable=='Zam_uprMinusskut' & uu$groupingsum==F,]
uu2 <- uu2[with(uu2, order(grouping)), ]
plot <- ggplot(uu2,aes(x=Year, fill=grouping)) +
  geom_bar(stat='identity', aes(y=value)) +
  facet_wrap(~KapAbb, scales='free_y') +
  labs(title=title, x=xlab, y=ylab)
plot

# central body pay 
title='Reálný růst platů v ústředních orgánech státu: rozpočet a skutečnost\n
Srovnání s reálným růstem průměrného platu v Praze'
ylab='Změna od roku 2003 očištěná o inflaci'
xlab=''
uu2 <- uu[(uu$variable=='AvgSal_skutecnost' | uu$variable=='AvgSal_upraveny') &
            uu$grouping=='UO' & uu$UO & !is.na(uu$value),]
uu2$ggroup <- paste0(uu2$variable,'_',uu2$KapAbb)
plot <- ggplot(uu2,aes(x=Year, y=realchange, colour=variable)) +
  geom_line(size=1) +
  geom_line(aes(y=phasal_all/phasal_all_2003/Infl2003Base)) +
  facet_wrap(~KapAbb) +
  scale_y_continuous(labels=percent) +
  labs(title=title, x=xlab, y=ylab)
plot

# central body pay 
title='Průměrný plat v centrálních orgánech států: rozpočet a skutečnost\n
Srovnání s průměrným platem v Praze'
ylab='\'000 Kč'
xlab=''
uu2 <- uu[(uu$variable=='AvgSal_skutecnost' | uu$variable=='AvgSal_upraveny') &
            uu$grouping=='UO' & uu$UO & !is.na(uu$value),]
uu2$ggroup <- paste0(uu2$variable,'_',uu2$KapAbb)
plot <- ggplot(uu2,aes(x=Year, y=value/1000, colour=variable)) +
  geom_line(size=1) +
  geom_line(aes(y=phasal_all/1000),colour='grey',linetype='dashed') +
  facet_wrap(~KapAbb) +
  scale_y_continuous(labels=comma) +
  labs(title=title, x=xlab, y=ylab)
plot

# central body staff numbers 
title='Počet zaměstnanců v ústředních orgánech státu: rozpočet a skutečnost'
ylab='Počet zaměstnanců'
xlab=''
uu2 <- uu[(uu$variable=='Zam_skutecnost' | uu$variable=='Zam_upraveny') &
            uu$grouping=='UO' & !is.na(uu$value) & uu$value!=0 & uu$UO,]
uu2$ggroup <- paste0(uu2$variable,'_',uu2$KapAbb)
plot <- ggplot(uu2,aes(x=Year, y=value, colour=variable)) +
  geom_line(size=1) +
  facet_wrap(~KapAbb, scales='free_y') +
  labs(title=title, x=xlab, y=ylab)
plot
