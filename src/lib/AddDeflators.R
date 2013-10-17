AddDeflators <- function (data) {
  deflators <- read.csv('./data-input/deflators.txt')
  dataout <- merge(data, deflators)
  return(dataout)
}