# mseSSFiguresSelectivity.r
#
# All selectivity plots for mseSS
#
# Authors: Chris Grandin

source("mseSSGlobals.r")

plotSelex <- function(scenario,
                      figPath  = .FIGS_DIR,
                      ylim     = c(0,1),             # Selectivity always from 0 to 1.
                      xlim     = c(0,scenario$maxA), # Age from zero to the max age.
                      isSurvey = TRUE,               # If true, plot the survey selectivity. If false, plot fishery selectivity.
                      addVert  = FALSE,              # Add vertical lines to the ages
                      res      = .RESOLUTION,
                      width    = .WIDTH,
                      height   = .HEIGHT,
                      units    = .UNITS,
                      png      = .PNG,
                      verbose  = .VERBOSE){
  # Plots of the assessment estimated selectivity for mseSS
  if(scenario$runAssessments){
    if(isSurvey){
      filename <- file.path(figPath,paste(Sys.Date(),"_Survey_Selectivity_",scenario$scenarioName,".png",sep=""))
      selex <- scenario$assessSelexSurvey
      ylab <- "Survey Selectivity"
    }else{
      filename <- file.path(figPath,paste(Sys.Date(),"_Fishery_Selectivity_",scenario$scenarioName,".png",sep=""))
      selex <- scenario$assessSelexFishery
      ylab <- "Fishery Selectivity"
    }
    numSims <- length(scenario$simNums)
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    par(mar=c(5,4,3,1))
    if(isSurvey){
      plot(scenario$minA:scenario$maxA, scenario$assessSelexSurvey[1,scenario$minA:scenario$maxA],
         type="l", xlim=xlim, ylim=ylim, xlab="Age", ylab=ylab)
    }else{
      plot(scenario$minA:scenario$maxA, scenario$assessSelexFishery[1,scenario$minA:scenario$maxA],
         type="l", xlim=xlim, ylim=ylim, xlab="Age", ylab=ylab)
    }
    for(s in 2:numSims){
      if(isSurvey){
        lines(scenario$minA:scenario$maxA, scenario$assessSelexSurvey[s,scenario$minA:scenario$maxA], col=s)
      }else{
        lines(scenario$minA:scenario$maxA, scenario$assessSelexFishery[s,scenario$minA:scenario$maxA], col=s)
      }
    }
    if(addVert){
      for(age in scenario$minA:scenario$maxA){
        abline(v=age, lty=2, lwd=1, col="grey")
      }
    }
    ageTextLabels <- paste("Min age =",scenario$minA,"\nMax age =",scenario$maxA,"\nPlus group =",scenario$plusA)
    text(scenario$maxA-2,0.9,labels=ageTextLabels)
    mainTitle <- paste(scenario$prettyName, " Selectivities",sep="")
    title(mainTitle)
    box()
    if(png){
      dev.off()
    }
  }
}
