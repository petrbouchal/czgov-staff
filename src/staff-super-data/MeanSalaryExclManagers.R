source('./src/lib/lib_PubFinCZ.R')

# Assumptions on top management salaries & staff numbers, estimating from LN data
toppocet <- 8
topplat <- 981000/toppocet
ministerstvo <- 'MF'

onlyonedept <- TRUE

#ff0 <- LoadDataforPlotting('chapters')
ff <- ff0
ff <- ff[ff$Udaj=='Zam' & ff$Year=='2011-01-01' & ff$BudgetStage=='skutecnost' &
           ff$Ministerstvo==TRUE & ff$sheetname=='UO',]
if(onlyonedept) {
  ff <- ff[ff$Udaj=='Zam' & ff$Year=='2011-01-01' & ff$BudgetStage=='skutecnost' &
             ff$Ministerstvo==TRUE & ff$sheetname=='UO' & ff$KapAbb==ministerstvo,]
  pocetlidi <- sum(ff$value, na.rm=T)
}

ff <- ff0
ff <- ff[ff$Udaj=='Platy' & ff$Year=='2011-01-01' & ff$BudgetStage=='skutecnost' &
           ff$Ministerstvo==TRUE & ff$sheetname=='UO',]
if(onlyonedept) {
  ff <- ff[ff$Udaj=='Platy' & ff$Year=='2011-01-01' & ff$BudgetStage=='skutecnost' &
             ff$Ministerstvo==TRUE & ff$sheetname=='UO' & ff$KapAbb==ministerstvo,]
  platysuma <- sum(ff$value, na.rm=T)
}

# checking the totals from MFCR data
prumernyplat <- platysuma*1000/pocetlidi/12

platysuma
pocetlidi
prumernyplat


# Mean salary excluding top managers
prumplatbeztop <- (platysuma*1000-toppocet*topplat*12)/(pocetlidi-toppocet)/12
prumplatbeztop
prumernyplat
prumplatbeztop-prumernyplat

# top x% of people takes y% of the pot
y <- topplat*toppocet*12/1000/platysuma*100
x <- toppocet/pocetlidi*100

x
y

topplat/prumernyplat
