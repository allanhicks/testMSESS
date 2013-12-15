# mseSSFiguresRuntime.r
#
# Runtime plots for mseSS
#
# Authors: Chris Grandin

source("mseSSGlobals.r")

plotRuntime <- function(scenario,
                        figPath  = .FIGS_DIR,
                        ylim     = c(0,max(scenario$assessRuntime)),
                        xlim     = c(1,ncol(scenario$assessRuntime)),
                        res      = .RESOLUTION,
                        width    = .WIDTH,
                        height   = .HEIGHT,
                        units    = .UNITS,
                        png      = .PNG,
                        verbose  = .VERBOSE){
  # Selectivity plots for mseSS
  if(scenario$runAssessments){
    filename <- file.path(figPath,paste(Sys.Date(),"_Assessment_Runtime_",scenario$scenarioName,".png",sep=""))
    numSims <- length(scenario$simNums)
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    par(mar=c(5,4,3,1))
    plot(1:ncol(scenario$assessRuntime), scenario$assessRuntime[1,],
         type="l", xlim=xlim, ylim=ylim, xlab="Year", ylab="Runtime (seconds)",axes=FALSE)
    # Fix years for x-axis
    axis(1,
         at=1:ncol(scenario$assessRuntime),
         labels=scenario$firstAssessYear:scenario$lastAssessYear,
         las=2)
    axis(2)
    for(s in 2:numSims){
      lines(1:ncol(scenario$assessRuntime), scenario$assessRuntime[s,], col=s)
    }
    mainTitle <- paste(scenario$prettyName, " Assessment Runtimes",sep="")
    title(mainTitle)
    box()
    if(png){
      dev.off()
    }
  }
}

plotSimtime <- function(scenario,
                        figPath  = .FIGS_DIR,
                        ylim     = c(min(scenario$simRuntime),max(scenario$simRuntime)),
                        xlim     = c(1,length(scenario$simRuntime)),
                        simLab   = 2,     # Show every nth label for sims
                        res      = .RESOLUTION,
                        width    = .WIDTH,
                        height   = .HEIGHT,
                        units    = .UNITS,
                        png      = .PNG,
                        verbose  = .VERBOSE){
  # Selectivity plots for mseSS
  if(scenario$runAssessments){
    filename <- file.path(figPath,paste(Sys.Date(),"_Simulation_Runtime_",scenario$scenarioName,".png",sep=""))
    numSims <- length(scenario$simNums)
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    par(mar=c(5,4,3,1))
    plot(1:numSims, scenario$simRuntime,
         type="l", xlim=xlim, ylim=ylim, xlab="", ylab="Runtime (seconds)", axes=FALSE)

    labels <- makeLabels(names(scenario$simRuntime), simLab)
    axis(1,
         at=1:length(scenario$simRuntime),
         labels=labels,
         las=2)
    axis(2)

    mainTitle <- paste(scenario$prettyName, " Simulation Runtimes",sep="")
    title(mainTitle)
    box()
    if(png){
      dev.off()
    }
  }
}
