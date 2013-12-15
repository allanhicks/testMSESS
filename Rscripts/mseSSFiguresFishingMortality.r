# mseSSFiguresFishingMortality.r
#
# All' F' plots for mseSS
#
# Authors: Chris Grandin

source("mseSSGlobals.r")

plotF <- function(scenario,
                  figPath  = .FIGS_DIR,
                  ylim     = c(0,max(scenario$Fvalue)),
                  xlim     = c(1,ncol(scenario$Fvalue)),
                  res      = .RESOLUTION,
                  width    = .WIDTH,
                  height   = .HEIGHT,
                  units    = .UNITS,
                  png      = .PNG,
                  verbose  = .VERBOSE){
  # Selectivity plots for mseSS
  if(scenario$runAssessments){
    filename <- file.path(figPath,paste(Sys.Date(),"_Fvalue_",scenario$scenarioName,".png",sep=""))
    numSims <- length(scenario$simNums)
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    par(mar=c(5,4,3,1))
    plot(1:ncol(scenario$Fvalue), scenario$Fvalue[1,],
         type="l", xlim=xlim, ylim=ylim, xlab="Year", ylab="F",axes=FALSE)
    # Fix years for x-axis
    axis(1,
         at=1:ncol(scenario$Fvalue),
         labels=scenario$firstTSYear:scenario$lastAssessYear,
         las=2)
    axis(2)
    for(s in 2:numSims){
      lines(1:ncol(scenario$Fvalue), scenario$Fvalue[s,], col=s)
    }
    mainTitle <- paste(scenario$prettyName, " F values",sep="")
    title(mainTitle)
    box()
    if(png){
      dev.off()
    }
  }
}
