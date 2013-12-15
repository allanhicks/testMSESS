# mseSSPlotAll.r
#
# Functions used to make the MSE performance plots.
# Source this file, see the call to plotAll()
#  at the end of this file.
#
# Authors: Chris Grandin


source("mseSSGlobals.r")
source("mseSSUtilities.r")

source("mseSSFiguresTimeSeries.r")
source("mseSSFiguresDensity.r")
source("mseSSFiguresSPR.r")
source("mseSSFiguresActualvsPerceived.r")
source("mseSSFiguresRetrospectives.r")
source("mseSSFiguresSelectivity.r")
source("mseSSFiguresConvergence.r")
source("mseSSFiguresFishingMortality.r")
source("mseSSFiguresBiomass.r")
source("msessFiguresRuntime.r")

plotAll <- function(plotTS     = T,    # Make timeseries plots
                    plotDens   = T,    # Make density plots
                    plotSPR    = T,    # Make SPR plots for perfect information case
                    plotAAV    = T,    # Make Average Annual Variability in catch plots
                    plotAvsP   = T,    # Make Actual vs. Perceived plots
                    plotF      = T,    # Make Fishing mortality plots
                    plotSSB    = T,    # Make Spawning stock biomass plots
                    plotRetros = T,    # Make single-simulation retrospective plots
                    retroSim   = 1,    # Simulation index for plotting retrospectives
                    plotSelex  = T,    # Plot selectivities for survey and fishery
                    plotGrads  = T,    # Plot final gradient values for assessments as a bubble plot
                    plotObjFun = T,    # Plot objective function values for assessments as a bubble plot
                    objFunTol  = 0.05, # Circles on ObjFun plot will be black if less that this, red if equal or greater.
                    simLab     = 2,    # Show every nth simulation label for runtime output
                    plotRuntime= T,    # Plot runtimes for assessments by simulation
                    shortTerm  = 2013:2015,
                    mediumTerm = 2016:2020,
                    longTerm   = 2021:2029,
                    removeScen = NULL, # Vector of scenario numbers to remove from the plotting altogether
                    addVertSel = FALSE,# Add vertical lines to the selectivity plots for ages
                    png        = .PNG, # If TRUE, PNGs will be written to disk. If FALSE, plots will be shown on the screen device.
                    verbose    = .VERBOSE
                    ){

  simResultsPath <- file.path(.SCENARIOS_DIR_NAME,.RESULTS_FILE)
  load(simResultsPath)
  makeFigsDir()
  if(!is.null(removeScen)){
    # remove No fishing and Perfect info case
    s <- s[-removeScen]
  }

  if(exists("s")){
    if(plotTS){
      plotTS(s, type="dep", showNoFishing=TRUE, addMean=FALSE, ylim=c(0,1.4), png=png, verbose=verbose)
      plotTS(s, type="dep", showNoFishing=TRUE, addMean=TRUE, ylim=c(0,1.4), png=png, verbose=verbose)
      plotTS(s, type="spr", showNoFishing=TRUE, addMean=FALSE, ylim=c(0,1.2), png=png, verbose=verbose)
      plotTS(s, type="cat", showNoFishing=FALSE, addMean=FALSE, ylim=c(0,1000), png=png, verbose=verbose)
      plotTS(s, type="tarcat", showNoFishing=TRUE, addMean=FALSE, ylim=c(0,1000), png=png, verbose=verbose)
      plotTS(s, type="ssbcompare", showNoFishing=TRUE, addMean=FALSE, ylim=c(-1000,1000), png=png, verbose=verbose)
      plotTS(s, type="relerrssb", showNoFishing=TRUE, addMean=FALSE, ylim=c(-0.01,0.01), png=png, verbose=verbose)
      plotTS(s, type="depcompare", showNoFishing=TRUE, addMean=FALSE, ylim=c(-0.5,0.5), png=png, verbose=verbose)
      plotTS(s, type="relerrdep", showNoFishing=TRUE, addMean=FALSE, ylim=c(-1.5,1.5), png=png, verbose=verbose)
    }
    if(plotDens){
      plotDensity(s, type="dep", years=shortTerm, png=png, showNoFishing=TRUE, addMean=TRUE, xMax=1, yMax=4, verbose=verbose)
      plotDensity(s, type="dep", years=mediumTerm, png=png, showNoFishing=TRUE, addMean=TRUE ,xMax=1, yMax=4, verbose=verbose)
      plotDensity(s, type="dep", years=longTerm, png=png, showNoFishing=TRUE, addMean=TRUE, xMax=1, yMax=4, verbose=verbose)
      plotDensity(s, type="cat", years=shortTerm, png=png, showNoFishing=FALSE, addMean=TRUE, xMax=2100, yMax=0.0035, verbose=verbose)
      plotDensity(s, type="cat", years=mediumTerm, png=png, showNoFishing=FALSE, addMean=TRUE, xMax=2100, yMax=0.0035, verbose=verbose)
      plotDensity(s, type="cat", years=longTerm, png=png, showNoFishing=FALSE, addMean=TRUE, xMax=2100, yMax=0.0035, verbose=verbose)
      plotDensity(s, type="aav", years=longTerm, png=png, showNoFishing=FALSE, addMean=TRUE, xMax=1, yMax=6, verbose=verbose)
      plotDensity(s, type="aaveven", years=longTerm, png=png, showNoFishing=FALSE, addMean=TRUE, xMax=1, yMax=6, verbose=verbose)
      plotDensity(s, type="aavodd", years=longTerm, png=png, showNoFishing=FALSE, addMean=TRUE, xMax=1, yMax=6, verbose=verbose)
    }
    if(plotSPR){
      plotSPR(s, type="dep", years=longTerm[length(longTerm)], ylab="Depletion", yMax=1.5, scale=1, png=png, verbose=verbose)
      plotSPR(s, type="dep", years=longTerm, ylab="Depletion", yMax=1.5, scale=1, png=png, verbose=verbose)
      plotSPR(s, type="cat", years=longTerm, ylab="Average Catch ('000 mt)",yMax=1200, scale=1000, png=png, verbose=verbose)
      plotSPR(s, type="aav", years=longTerm, ylab="Average Annual Variability In Catch", yMax=0.7, scale=1, png=png, verbose=verbose)
      plotSPR(s, type="fsprtgt", years=longTerm, ylab="Exploitation rate associated with target harvest rate",
              yMax=0.45, scale=1, png=png, verbose=verbose)
    }
    if(plotAAV){
      plotAAVDepvsCatch(s, years=longTerm, depYlim=c(0,1), aavYlim=c(0,0.4), png=png, verbose=verbose)
    }
    if(plotAvsP){
      plotActualvsPerceived(s, years=longTerm,  png=png, verbose=verbose)
    }
    if(plotRetros){
      for(scenario in s){
        plotRetrospectives(scenario, type="dep",  ylim=c(0,3.5), retroSim=retroSim, png=png, verbose=verbose)
        plotRetrospectives(scenario, type="recr", ylim=c(0,7e7), retroSim=retroSim, png=png, verbose=verbose)
      }
    }
    if(plotSelex){
      for(scenario in s){
        plotSelex(scenario, isSurvey=TRUE,  addVert=addVertSel, png=png, verbose=verbose)
        plotSelex(scenario, isSurvey=FALSE, addVert=addVertSel, png=png, verbose=verbose) # Plots fishery selectivity
      }
    }
    if(plotGrads){
      for(scenario in s){
        plotFinalGradients(scenario,  colorTol=objFunTol, simLab=simLab, addLines=TRUE, png=png, verbose=verbose)
      }
    }

    if(plotObjFun){
      for(scenario in s){
        plotObjectiveFunctionVal(scenario, simLab=simLab, addLines=TRUE, png=png, verbose=verbose)
      }
    }
    if(plotRuntime){
      for(scenario in s){
        plotRuntime(scenario, png=png, verbose=verbose)
        plotSimtime(scenario, simLab=simLab, png=png, verbose=verbose)
      }
    }
    if(plotF){
      for(scenario in s){
        plotF(scenario, png=png, verbose=verbose)
      }
    }
    if(plotSSB){
      for(scenario in s){
        plotSSB(scenario, png=png, verbose=verbose)
      }
    }
  }else{
    stop("Error: No 's' object.  Run mergeMSE() to create this.\n\n")
  }
}

T <- TRUE
F <- FALSE
plotAll(plotTS     = T,    # Make timeseries plots
        plotDens   = T,    # Make density plots
        plotF      = T,    # Make plots for fishing mortality
        plotSSB    = T,    # Plot spawning stock biomass
        plotRetros = T,    # Make Retrospective plots by simulation
        retroSim   = 100,  # Simulation index for plotting retrospectives
        plotSelex  = T,    # Make Selectivity plots
        plotGrads  = T,    # Make final gradient plots for all assessments
        plotObjFun = T,    # Make objective function value plots for all assessments
        objFunTol  = 0.05, # color tolerance for Objective function value plot
        plotRuntime= T,    # Make runtime plots
        simLab     = 5,    # Plot every nth simulation label.  Avoids clutter for larger number of simulation runtime plots
        removeScen = 1:2,  # Remove first two scenarios from plots, i.e. No fishing(1) and Perfect info(2)
        addVertSel = T, # Add vertical lines for ages in selectivity plots
        png        = T,

        # Perfect Information and/or No Fishing case(s) must be present for these to work:
        plotSPR    = F,    # Perfect Info scenario only - Make SPR plots for perfect information case
        plotAAV    = F,    # Perfect Info scenario only - Make Average Annual Variability in catch plots
        plotAvsP   = F     # NoFishing and Perfect Info required - Make Actual vs. Perceived plots
        )

