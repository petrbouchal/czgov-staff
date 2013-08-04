library(stringr)
library(doBy)
library(compare)

# Note this reads in files cleaned up in OpenRefine to avoid mismatches
#inc <- read.csv('./data-input/VydajeVysledovka_20130511_2309_cleanedup.csv')

bud <- read.csv('./data-input/VydajeDruhove_20130511_2309_cleanedup.csv',
                colClasses='character')
bud$rozpocet <- as.numeric(bud$rozpocet)
bud$pozmenach <- as.numeric(bud$pozmenach)
bud$skutecnost <- as.numeric(bud$skutecnost)

orgattribs <- unique(bud[2:6])

budsub <- bud

budsub$category <- NULL
budsub$zrizovatel <- NULL
budsub$orgchapter <- NULL
budsub$orgchaptername <- NULL
budsub$orgtype <- NULL
budsub$orgname <- NULL

bud12 <- subset(budsub, orgyear=='2012')
bud11 <- subset(budsub, orgyear=='2011')
bud10 <- subset(budsub, orgyear=='2010')

bud10$orgyear <- NULL
bud11$orgyear <- NULL
bud12$orgyear <- NULL

bud10 <- renameCol(bud10,c('rozpocet','pozmenach','skutecnost','rowid','rowparent'),
                   c('rozpocet2010','pozmenach2010','skutecnost2010','rowid2010','rowparent2010'))
bud11 <- renameCol(bud11,c('rozpocet','pozmenach','skutecnost','rowid','rowparent'),
                   c('rozpocet2011','pozmenach2011','skutecnost2011','rowid2011','rowparent2011'))
bud12 <- renameCol(bud12,c('rozpocet','pozmenach','skutecnost','rowid','rowparent'),
                   c('rozpocet2012','pozmenach2012','skutecnost2012','rowid2012','rowparent2012'))

bud1112 <- merge(bud11, bud12, by=c('orgid','rowname','rowlevel'),all=TRUE)
budw <- merge(bud1112, bud10, by=c('orgid','rowname','rowlevel'),all=TRUE)

# deal with cases where one ID has several names next to it - this creates duplicates

orgattribs$duplicate <- 'No'
duplicates <- c('00026018','00215732','48135453','49625586','64934276')
for (i in duplicates ) {
  subattrs <- subset(orgattribs,orgattribs$orgid==i)
  orgname <- subattrs$orgname[1] # these will be deleted
  print(orgname)
  orgattribs$duplicate <- ifelse(orgattribs$orgname==orgname, "Yes","No")
  orgattribs <- subset(orgattribs, orgattribs$duplicate=='No')
}
orgattribs$duplicate <- NULL
budw_2 <- merge(budw, orgattribs, by.x='orgid', by.y='orgid', all=FALSE)

budw <- budw_2