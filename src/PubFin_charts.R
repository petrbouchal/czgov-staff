# source('./PubFin_process.R')
library(ggplot2)
library(plyr)
library(reshape2)

# define strings for subsetting budget lines
platyrow <- "Platy zaměstnanců v pracovním poměru"
sluzbarow <- "Platy zaměstnanců ozbrojených sborů a složek ve služebním poměru"
odvozenerow <- "Platy zaměstnanců v pracovním poměru odvozené od platů ústavních činitelů"
ustavnirow <- "Platy představitelů státní moci a některých orgánů"
ostosobrow <- "Ostatní osobní výdaje"
# will need to add others - insurance and health contributions, etc.

# subset
platl <- subset(bud, bud$rowname==platyrow & bud$orgtype=="chapter")
# summarise
platsum <- ddply(platl, .(orgyear), summarise,
                 skutecnost = sum(skutecnost),
                 rozpocet = sum(rozpocet),
                 pozmenach = sum(pozmenach))
# reshape summary to long
platsumlong <- melt(platsum)

# plot totals over years, budget, revised budget, and outturn
plot <- ggplot(data=platsumlong, aes(x=orgyear, y=value, group=variable, colour=variable)) +
  geom_line() +
  scale_y_continuous(limits=c(0,3.5e10))
plot