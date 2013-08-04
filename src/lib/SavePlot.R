SavePlot <- function (plotname='Plot', plotformat='eps', ffamily='Helvetica',
                      splot=last_plot() ,ploth=210/2, plotw=14) {
  try(dev.off(),silent=TRUE)
  plotobjdir <- './charts/ACSES chart objects/'
  plotimagedir <- './charts/ACSES charts/'
  plotimagepath = paste0(plotimagedir,plotname,'.',plotformat)
  plotobjpath = paste0(plotobjdir,plotname,'.','ggp')
  if(plotformat=='pdf') {
    ggsave(plotimagepath, plot=splot, family=ffamily, device=cairo_pdf,
           height=ph, width=pw, units='cm')  
  } else if(plotformat=='eps') {
    ggsave(plotimagepath, plot=splot, family=ffamily, device=cairo_ps,
           height=ph, width=pw, units='cm')
  } else {
    ggsave(plotimagepath, plot=splot, family=ffamily,
           height=ploth, width=plotw, units='cm')
  }
  save(splot,file=plotobjpath)
  write.csv(splot$data,file=paste0(plotimagedir,plotname,'_data.csv'))
  splot
}