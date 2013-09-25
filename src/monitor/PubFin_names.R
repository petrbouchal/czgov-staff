source('./src/lib/lib_PubFinCZ.R')

data <- read.csv('./data-input/VydajeDruhove_20130511_2309_cleanedup.csv',
                 colClasses='character')

orgidtable <- unique(data[data$orgtype!='chapter',3:4])
write.csv(orgidtable,'./data-output/ICOnazev.csv',row.names=FALSE)