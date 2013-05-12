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

budw$realityindexc2010 = 1
budw$budgetindexc2010 = 1
budw$budgetindexc2011 = budw$rozpocet2011/budw$rozpocet2010
budw$budgetindexc2012 = budw$rozpocet2012/budw$rozpocet2010
budw$realityindexc2012 = budw$skutecnost2012/budw$skutecnost2010
budw$realityindexc2011 = budw$skutecnost2011/budw$skutecnost2010

budl <- melt(budw)

budl_platy_OSS <- subset(budl,budl$rowname=="Platy zaměstnanců v pracovním poměru" & budw$orgtype=="OSS")
budl_platyindex_OSS <- subset(budl_platy_OSS,grepl('indexc', budl_platy_OSS$variable))
budl_platyindex_OSS$year <- str_sub(budl_platyindex_OSS$variable,-4,-1)
budl_platyindex_OSS$variable <- sub('[01234]{4}', '', budl_platyindex_OSS$variable,)
budl_platyindex_OSS$group <- paste(budl_platyindex_OSS$variable,
                                   budl_platyindex_OSS$orgid, sep='_')

budl_platy_ch <- subset(budl,budl$rowname=="Platy zaměstnanců v pracovním poměru" & budw$orgtype=="chapter")
budl_platyindex_ch <- subset(budl_platy_ch,grepl('indexc', budl_platy_ch$variable))
budl_platyindex_ch$year <- str_sub(budl_platyindex_ch$variable,-4,-1)
budl_platyindex_ch$variable <- sub('[01234]{4}', '', budl_platyindex_ch$variable,)

budl_platyindex_ch$group <- paste(budl_platyindex_ch$variable,
                                   budl_platyindex_ch$orgid, sep='_')
plot <- ggplot(data=budl_platyindex_OSS, aes(x=year, y=value,
                                             group=group, colour=variable))+
  geom_line(alpha=1) +
  facet_wrap(~ orgchaptername, scales='free')
plot

plot <- ggplot(data=budl_platyindex_ch, aes(x=year, y=value,
                                             group=group, colour=variable))+
  geom_line(alpha=1) +
  facet_wrap(~ orgchaptername, scales='free')
plot

bud_platy_ch <- subset(bud,bud$rowname=="Platy zaměstnanců v pracovním poměru" & bud$orgtype=="chapter")
bud_platy_ch$overspend <- bud_platy_ch$skutecnost-bud_platy_ch$rozpocet
bud_platy_ch$pcoverspend <- bud_platy_ch$overspend/bud_platy_ch$rozpocet
budl_platy_ch <- melt(bud_platy_ch)

plot <- ggplot(subset(budl_platy_ch, variable=='pcoverspend'),
               aes(x=orgyear, y=value, fill=orgyear,
                   group=orgchaptername))+
  geom_bar(stat='identity',position='dodge') +
  facet_wrap(~ orgchaptername)
plot

plot <- ggplot(subset(budl_platy_ch, variable=='overspend'),
               aes(x=orgyear, y=value, fill=orgyear, group=1))+
  geom_bar(stat='sum',position='dodge')
plot

hist(budw_platy_OSS$budgetindex1012,breaks=400,xlim=c(0.5,1.5))
plot(budw_platy_OSS$rozpocet2010,budw_platy_OSS$budgetindex1012,ylim=c(0.8,1.2),pch=19)