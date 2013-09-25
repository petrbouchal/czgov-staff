source('./src/lib/lib_PubFinCZ.R')

ff <- LoadDataforPlotting('chapters')
ff <- ff[ff$Udaj=='Zam' & ff$Year=='2011-01-01' & ff$BudgetStage=='skutecnost' &
           ff$Ministerstvo==TRUE & ff$sheetname=='UO',]
pocetlidi <- sum(ff$value, na.rm=T)

ff <- LoadDataforPlotting('chapters')
ff <- ff[ff$Udaj=='Platy' & ff$Year=='2011-01-01' & ff$BudgetStage=='skutecnost' &
           ff$Ministerstvo==TRUE & ff$sheetname=='UO',]
platysuma <- sum(ff$value, na.rm=T)

# checking the totals from MFCR data
prumernyplat <- platysuma*1000/pocetlidi/12
platysuma
pocetlidi
prumernyplat

# Assumptions on top management salaries & staff numbers, estimating from LN data
topplat <- 70000
toppocet <- 420

# Calculating mean salary excluding top managers
prumplatbeztop <- (platysuma*1000-toppocet*topplat*12)/(pocetlidi-toppocet)/12
prumplatbeztop-prumernyplat