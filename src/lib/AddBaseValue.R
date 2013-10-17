AddBaseValue <- function (data, basetime,range=2:length(data)) {
  basedata <- data[data$Year==basetime,range]
  basedata$value_base <- basedata$value
  basedata$value <- NULL
  datawithbase <- merge(data,basedata)
  datawithbase$diff_base <- datawithbase$value-datawithbase$value_base
  datawithbase$perc_base <- datawithbase$value/datawithbase$value_base
  return(datawithbase)
}