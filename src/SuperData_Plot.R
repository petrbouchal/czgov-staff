# source('./src/SuperData_ReadXLS.R')
sdatal <- read.csv('./data-output/SuperData.csv')[,2:7]
data <- sdatal
labels <- read.csv('./data-output/KapitolyNames.txt')[,2:5]
uu <- merge(data,labels)
deflators <- read.csv('./data-input/deflators.txt')
deflators$Year <- as.Date(deflators$Year,format='%Y-%m-%d')
uu$Year <- as.Date(uu$Year,format='%Y-%m-%d')
uu <- merge(uu,deflators,by='Year',all.y=FALSE,all.x=TRUE)

uu <- uu[uu$Udaj=='AvgSal' & uu$Year!='2013-01-01',]
#uu <- uu[uu$sheetname=='ROPO celkem' | uu$sheetname=='UO'| uu$sheetname=='StatniSprava',]
uu <- uu[uu$sheetname=='UO' & uu$Ministerstvo==TRUE,]

uu$value[uu$value==0] <- NA
#uu$value <- uu$value/uu$Infl2003Base
plot <- ggplot(uu, aes(Year, value, col=BudgetStage)) +
  geom_line() +
  geom_segment(x=-Inf,xend=Inf,y=100,yend=100,colour='black',size=1) +
  #scale_y_continuous(limits=c(20000,45000))+
  scale_x_date(breaks='1 year',labels=date_format('%Y'))+
  facet_wrap( ~ KapAbb,ncol=4,scales='fixed') +
  theme_economist_white() +
  theme(axis.text.x=element_text(angle=90))
plot