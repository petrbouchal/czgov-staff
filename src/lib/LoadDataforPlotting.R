LoadDataforPlotting <- function (whatdata) {
  if(whatdata=='chapters') {
    sdatal <- read.csv('./data-output/SuperData_chapters.csv')
    data <- sdatal
    labels <- read.csv('./data-output/KapitolyNames.txt')
    uu <- merge(data,labels)
    deflators <- read.csv('./data-input/deflators.txt')
    deflators$Year <- as.Date(deflators$Year,format='%Y-%m-%d')
    uu$Year <- as.Date(uu$Year,format='%Y-%m-%d')
    uu <- merge(uu,deflators,by='Year',all.y=FALSE,all.x=TRUE)
    data <- uu
  } else if(whatdata=='orgs') {
    sdatal <- read.csv('./data-output/SuperData_orgs_ALL.csv')
    data <- sdatal
    labels <- read.csv('./data-output/KapitolyNames.txt')
    uu <- merge(data,labels)
    deflators <- read.csv('./data-input/deflators.txt')
    deflators$Year <- as.Date(deflators$Year,format='%Y-%m-%d')
    uu$Year <- as.Date(uu$Year,format='%Y-%m-%d')
    uu <- merge(uu,deflators,by='Year',all.y=FALSE,all.x=TRUE)
    data <- uu
  }
  return(uu)
}