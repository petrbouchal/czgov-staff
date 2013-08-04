# source('./src/SuperData_ReadXLS.R')
data <- read.csv('./data-output/SuperData.csv')
labels <- read.csv('./data-output/KapitolyNames.csv')[,2:5]
uu <- merge(data,labels)
uu <- uu[sdatal$Udaj=='AvgSal' & sdatal$Year!='2013-01-01',]
#uu <- uu[uu$sheetname=='ROPO celkem' | uu$sheetname=='UO'| uu$sheetname=='StatniSprava',]
uu <- uu[uu$sheetname=='UO' & uu$Ministerstvo==TRUE,]

plot <- ggplot(uu, aes(Year, value, col=BudgetStage)) +
  geom_line() +
  geom_segment(x=-Inf,xend=Inf,y=100,yend=100,colour='black',size=1) +
  scale_y_continuous(limits=c(20000,45000))+
  scale_x_date(breaks='1 year',labels=date_format('%Y'))+
  facet_wrap( ~ KapAbb,ncol=6,scales='fixed') +
  theme(axis.text.x=element_text(angle=90))
plot