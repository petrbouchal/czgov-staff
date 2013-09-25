source('./src/lib/lib_PubFinCZ.R')

#source('./src/SuperData_ReadXLS.R')

uu0 <- LoadDataforPlotting('chapters')

uu <- uu0
#uu <- uu[uu$Udaj=='AvgSalIndexSchv2schv' | uu$Udaj=='AvgSalIndexSkut2skut',]
uu <- uu[uu$Udaj=='Zam',]
uu <- uu[uu$sheetname=='UO' & uu$BudgetStage=='skutecnost',]
uu <- uu[uu$Year=='2012-01-01',]

sum(uu$value, na.rm=T)

uu1 <- LoadDataforPlotting('orgs')

unique(uu1$OrgName)

uu2 <- uu1[uu1$Udaj=='PlatyOPPP' & uu1$BudgetStage=='schvaleny' & uu1$Year=='2012-01-01' &
               uu1$OrgName=='Celkem',]
sum(uu1$value, na.rm=T)
