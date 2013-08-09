LoadDataforPlotting <- function () {
  sdatal <- read.csv('./data-output/SuperData.csv')
  data <- sdatal
  labels <- read.csv('./data-output/KapitolyNames.txt')
  uu <- merge(data,labels)
  deflators <- read.csv('./data-input/deflators.txt')
  deflators$Year <- as.Date(deflators$Year,format='%Y-%m-%d')
  uu$Year <- as.Date(uu$Year,format='%Y-%m-%d')
  uu <- merge(uu,deflators,by='Year',all.y=FALSE,all.x=TRUE)
  return(uu)
}