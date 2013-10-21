# Load and prep -----------------------------------------------------------

source('./src/lib/lib_PubFinCZ.R')

# Create new layers & sums of layers of pub admin ------------------------

uu$grouping <- as.character(uu$sheetname)
uu$grouping[uu$sheetname=='UO' & uu$Ministerstvo==TRUE]  <- 'UO - Ministerstva'
uu$grouping[uu$sheetname=='UO' & uu$Ministerstvo==FALSE]  <- 'UO - Ostatní'
uu$grouping[uu$sheetname=='StatniSprava']  <- 'St. sprava se SOBCPO'
uu$Year <- as.Date(uu$Year)

# turn into wide - expand groupings
uu <- uu[uu$Udaj=='PlatyOPPP' | uu$Udaj=='Platy' | uu$Udaj=='OPPP' | uu$Udaj=='Zam',]
uuw1 <- cast(uu,Udaj+BudgetStage+Year+KapAbb+KapNum+KapName+Ministerstvo~grouping)

# create new groupings
uuw1$StatniSpravabezSOBCPO <- ifelse(uuw1$SOBCPO==0, uuw1$`St. sprava se SOBCPO`, 
                                     uuw1$`St. sprava se SOBCPO`-uuw1$SOBCPO)
uuw1$UO <- psum(uuw1$`UO - Ministerstva`,uuw1$`UO - Ostatní`,na.rm=T)
uuw1$Exekutiva <- psum(uuw1$UO,uuw1$`OSS-SS`,na.rm=T)

# turn back to long
uuw1 <- as.data.frame(uuw1)
uu <- melt(uuw1,id.vars=c('KapAbb','KapName','KapNum','Year','BudgetStage','Udaj','Ministerstvo'))
uu$grouping <- uu$variable
uu$variable <- NULL

# create marker for which groupings are sums of others
uu$groupingsum <- TRUE
uu$groupingsum[uu$grouping=='UO - Ministerstva' | uu$grouping=='UO - Ostatní' |
                 uu$grouping=='PO' | uu$grouping=='OOSS' | uu$grouping=='OSS-SS' |
                 uu$grouping=='SOBCPO'] <- FALSE