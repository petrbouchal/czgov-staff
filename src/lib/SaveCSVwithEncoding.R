SaveCSVwithEncoding <- function (data, encoding='WINDOWS-1252') {
  dataenc <- data
  iconvlist()
  datacz$KapName <- iconv(dataenc$KapName,to=encoding)
  write.csv(dataenc,file='./data-output/SuperData-encoded.csv')
}
