LoadDataforPlotting <- function () {
  sdatal <- read.csv('./data-output/SuperData.csv')[,2:7]
  data <- sdatal
  labels <- read.csv('./data-output/KapitolyNames.txt')[,2:5]
  uu <- merge(data,labels)
  deflators <- read.csv('./data-input/deflators.txt')
  deflators$Year <- as.Date(deflators$Year,format='%Y-%m-%d')
  uu$Year <- as.Date(uu$Year,format='%Y-%m-%d')
  uu <- merge(uu,deflators,by='Year',all.y=FALSE,all.x=TRUE)
  return(uu)
}