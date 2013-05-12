library(stringr)
# source('./src/PubFin_process.R')

bud$realitypodil <- bud$skutecnost/bud$pozmenach
bud$overspend <- bud$skutecnost - bud$pozmenach
rowsum <- ddply(bud, .(orgyear, rowname, rowlevel, orgtype), summarise,
                      overspend=sum(overspend)/1e9,
                      pozmenach=sum(pozmenach)/1e9,
                      rozpocet=sum(rozpocet)/1e9,
                      skutecnost=sum(skutecnost)/1e9)
rowsum_ch <- subset(rowsum,rowsum$orgtype=='chapter' & rowsum$rowlevel=='tree-depth-3')
rowsum_ch$pcoverspend <- rowsum_ch$overspend/rowsum_ch$pozmenach
# plot <- ggplot(rowsum_ch, aes(x=rowname, y=overspend, group=orgyear, fill=orgyear)) +
#   geom_bar(position="dodge", stat='identity') +
#   coord_flip() +
#   scale_y_continuous(limits=c(-10,10))
# plot

# calculate indices from year to year and ratios between plan and actual
budw$revisionpodil2010 = budw$pozmenach2010/budw$rozpocet2010
budw$realitypodil2010 = budw$skutecnost2010/budw$rozpocet2010
budw$realityindex2010 = 1
budw$budgetindex2010 = 1

budw$budgetindex2011 = budw$rozpocet2011/budw$rozpocet2010
budw$revisionpodil2011 = budw$pozmenach2011/budw$rozpocet2011
budw$realitypodil2011 = budw$skutecnost2011/budw$rozpocet2011
budw$realityindex2011 = budw$skutecnost2011/budw$skutecnost2010

budw$budgetindex2012 = budw$rozpocet2012/budw$rozpocet2011
budw$revisionpodil2012 = budw$pozmenach2012/budw$rozpocet2012
budw$realitypodil2012 = budw$skutecnost2012/budw$rozpocet2012
budw$realityindex2012 = budw$skutecnost2012/budw$skutecnost2011

# indexc means 'cumulative': total change from 2010
budw$realityindexc2010 = 1
budw$budgetindexc2010 = 1
budw$budgetindexc2011 = budw$rozpocet2011/budw$rozpocet2010
budw$budgetindexc2012 = budw$rozpocet2012/budw$rozpocet2010
budw$realityindexc2012 = budw$skutecnost2012/budw$skutecnost2010
budw$realityindexc2011 = budw$skutecnost2011/budw$skutecnost2010

# create long file from wide
budl <- melt(budw)

# subset for pay data only, for both chapters and orgs
# then subset for index variables only and create appropriate year variable
budl_platy_OSS <- subset(budl,budl$rowname=="Platy zaměstnanců v pracovním poměru" & budw$orgtype=="OSS")
budl_platyindex_OSS <- subset(budl_platy_OSS,grepl('indexc', budl_platy_OSS$variable))
budl_platyindex_OSS$year <- str_sub(budl_platyindex_OSS$variable,-4,-1)
budl_platyindex_OSS$variable <- sub('[01234]{4}', '', budl_platyindex_OSS$variable,)
budl_platyindex_OSS$group <- paste(budl_platyindex_OSS$variable,
                                   budl_platyindex_OSS$orgid, sep='_')
# same as above for chapters
budl_platy_ch <- subset(budl,budl$rowname=="Platy zaměstnanců v pracovním poměru" & budw$orgtype=="chapter")
budl_platyindex_ch <- subset(budl_platy_ch,grepl('indexc', budl_platy_ch$variable))
budl_platyindex_ch$year <- str_sub(budl_platyindex_ch$variable,-4,-1)
budl_platyindex_ch$variable <- sub('[01234]{4}', '', budl_platyindex_ch$variable,)

# create group variable to allow ggplot to group points correctly for drawing lines
# this plot shows one line for each org's plan and one line for each org's outturn,
# differentiated by colour and grouped by chapter
budl_platyindex_ch$group <- paste(budl_platyindex_ch$variable,
                                   budl_platyindex_ch$orgid, sep='_')
# plot this. Note ggplot drops whole line if even one point is outside scale limits.
plot <- ggplot(data=budl_platyindex_OSS, aes(x=year, y=value,
                                             group=group, colour=variable))+
  geom_line(alpha=1) +
  facet_wrap(~ orgchaptername, scales='free')
plot

# same plot but for chapters only
plot <- ggplot(data=budl_platyindex_ch, aes(x=year, y=value,
                                             group=group, colour=variable))+
  geom_line(alpha=1) +
  facet_wrap(~ orgchaptername, scales='free')
plot

# for chapter only - subset pay data from original long file
bud_platy_ch <- subset(bud,bud$rowname=="Platy zaměstnanců v pracovním poměru" & bud$orgtype=="chapter")
# calculate absolute and percentage over/underspend
bud_platy_ch$overspend <- bud_platy_ch$skutecnost-bud_platy_ch$rozpocet
bud_platy_ch$pcoverspend <- bud_platy_ch$overspend/bud_platy_ch$rozpocet
# reshape into long to allow charting
budl_platy_ch <- melt(bud_platy_ch)

# chart: shows overspends per chapter by year. Can select absolute or % in subset
plot <- ggplot(subset(budl_platy_ch, variable=='pcoverspend'),
               aes(x=orgyear, y=value, fill=orgyear,
                   group=orgchaptername))+
  geom_bar(stat='identity',position='dodge') +
  facet_wrap(~ orgchaptername)
plot

# chart total overspend per year
# summarise manually and then chart

overspendsum <- ddply(bud_platy_ch, .(orgyear), summarise,
                      overspend = sum(overspend),
                      rozpocet = sum(rozpocet))
overspendsum$pcoverspend <- overspendsum$overspend/overspendsum$rozpocet
overspendsuml <- melt(overspendsum)

plot <- ggplot(subset(overspendsuml, variable=='pcoverspend'),
               aes(x=orgyear, y=value, fill=orgyear, group=1))+
  geom_bar(stat='identity',position='dodge')
plot