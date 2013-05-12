# source('./PubFin_process.R')
library(ggplot2)
library(plyr)
library(reshape2)

platyrow <- "Platy zaměstnanců v pracovním poměru"
sluzbarow <- "Platy zaměstnanců ozbrojených sborů a složek ve služebním poměru"
odvozenerow <- "Platy zaměstnanců v pracovním poměru odvozené od platů ústavních činitelů"
ustavnirow <- "Platy představitelů státní moci a některých orgánů"
ostosobrow <- "Ostatní osobní výdaje"

platl <- subset(bud, bud$rowname==platyrow & bud$orgtype=="chapter")
platsum <- ddply(platl, .(orgyear), summarise,
                 skutecnost = sum(skutecnost),
                 rozpocet = sum(rozpocet),
                 pozmenach = sum(pozmenach))
platsumlong <- melt(platsum)

plot <- ggplot(data=platsumlong, aes(x=orgyear, y=value, group=variable, colour=variable)) +
  geom_line() +
  scale_y_continuous(limits=c(0,3.5e10))
plot