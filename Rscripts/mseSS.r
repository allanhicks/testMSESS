# mseSS.r
#
# Management Strategy Evaluationi framework for Stock Synthesis
# See Readme.md for documentation.
#
# Authors: Allan Hicks, Chris Grandin

#require(roxygen2)
source("mseSSGlobals.r")
source("mseSSUtilities.r")

##' Run the MSE for each scenario in the Scenarios.csv file
##' If a name in the Scenarios.csv file does not exist as a folder in
##' the .SCENARIOS_DIR_NAME directory, an error will be thrown and the runs aborted.
##' This check will happen first so that you may correct the names before
##' starting a long set of runs.
##' @param continue If TRUE, continue simulations numerically. Useful in case system crashes during runs.
##' @param useSystem If TRUE, R will use the system() function instead of the shell() function.
##' @param verbose If TRUE, write extra output to the screen.
##' @param printstats: If TRUE, r4ss will print out stats for each assessment
##' @param sendEmailUpdates If TRUE, email will be sent to the address specified.
##' @param scen The scenario number(s) for uncommented scenarios in Scenarios.csv to run.  This was added to allow parallelization.
##'             If NULL, all uncommented scenarios will be run on one process.
##' @param emailAddress The gmail address you want email sent to.
##' @param emailPassword The password corresponding to the gmail emailAddress.
##' @param updateR4ss Update the r4ss library. If TRUE, the r4SSDir parameter will be used to update.
##' @param r4ssDir: Null updates r4ss from web, a valid path updates from that directory.
##' @return The 'scenarios' object containing the results of the MSE runs.
runMSE <- function(continue         = T,
                   useSystem        = T,
                   verbose          = F,
                   printstats       = F,
                   sendEmailUpdates = F,
                   scen             = NULL,
                   emailAddress     = NULL,
                   emailPassword    = NULL,
                   updateR4ss       = TRUE,
                   r4ssDir          = NULL){

  require(MASS)
  require(r4ss)

  if(updateR4ss){
    if(is.null(r4ssDir)){
      cat("Updating r4ss from the web\n")
      update_r4ss_files()
    }else{
      cat("Updating r4ss from directory ",r4ssDir,"\n")
      update_r4ss_files(r4ssDir)
    }
  }

  if(exists("printDebug")){if(printDebug) {print("Debug statements will be printed because an object in your working directory named printDebug is TRUE")}}

  startTime         <- proc.time()
  startTimeReadable <- format(Sys.time(), "%a %b %d %X %Y")

  scenariosFullPath <- list.dirs(.SCENARIOS_DIR_NAME,recursive=F)
  scenarioDirs      <- basename(scenariosFullPath)
  scenarioInfo      <- loadScenarioInfoFile()
  omDirsFullPath    <- list.dirs(.OM_DIR_NAME,recursive=F)
  omDirs            <- basename(omDirsFullPath)

  if(!checkInfoVSDirs(scenarioInfo, scenarioDirs, omDirs)){
    stop("runMSE: There was a problem with the information file '",.SCENARIOS_INFO,"'. Stopping....\n\n",sep="")
  }
  computerInfo <- Sys.info()
  computerName <- computerInfo["nodename"]
  computerOS   <- paste(computerInfo["sysname"],computerInfo["release"])
  if(sendEmailUpdates){
    if(is.null(emailPassword) | is.null(emailAddress)){
      stop("runMSE: If you want to send email you must supply your gmail address and password.\n\n")
    }
    # Test email first, make sure it works..
    emailMessage <- paste("Starting mseSS runs:\n\n",
                          "Start time: ",startTimeReadable,"\n",
                          "Computer Name: ",computerName,"\n",
                          "Operating system: ",computerOS,"\n",
                          "Scenarios directory: ",.SCENARIOS_DIR_NAME,"\n",
                          "Full Path for R script: ",getwd(),"\n\n",sep="")
    sendEmail(emailAddress  = emailAddress,
              emailPassword = emailPassword,
              emailMessage  = emailMessage,
              subject = "Starting mseSS run")
 }

  # Scenario folders verified to exist, now run them
  if(exists("printDebug")){if(printDebug) {print("Starting runScenarios")}}
  scenarios <- runScenarios(scenarioInfo = scenarioInfo,
                            continue     = continue,
                            useSystem    = useSystem,
                            verbose      = verbose,
                            printstats   = printstats,
                            scen         = scen,
                            sendEmailUpdates = sendEmailUpdates,
                            emailAddress     = emailAddress,
                            emailPassword    = emailPassword)

  if(exists("printDebug")){if(printDebug) {print("runMSE: Finished runScenarios")}}
  if(exists("printDebug")){if(printDebug) {browser()}}

  endTime           <- proc.time()
  endTimeReadable   <- format(Sys.time(), "%a %b %d %X %Y")
  runTime           <- endTime - startTime
  scenarios$runTime <- runTime[3]

  if(verbose){
    cat("runMSE:\n",
        "   Start time: ",startTimeReadable,"\n",
        "     End time: ",endTimeReadable,"\n",
        "      Runtime: ",scenarios$runTime," seconds\n\n",sep="")
  }

  if(sendEmailUpdates){
    emailMessage <- paste("mseSS run completed.\n\n",
                          "Start time: ",startTimeReadable,"\n",
                          "End time: ",endTimeReadable,"\n",
                          "Runtime: ",round(scenarios$runTime)," seconds\n",
                          "Computer Name: ",computerName,"\n",
                          "Operating system: ",computerOS,"\n",
                          "Scenarios directory: ",.SCENARIOS_DIR_NAME,"\n",
                          "Full Path for R script: ",getwd(),"\n\n",
                          "You must run the mergeMSE() function before viewing results.\n\n",sep="")
    sendEmail(emailAddress  = emailAddress,
              emailPassword = emailPassword,
              emailMessage  = emailMessage,
              subject = "Finshed mseSS run")
  }
  return(scenarios)
}

##' Runs the scenarios present in the scenarioInfo object
##' Steps:
##' 1. Copy the information for each scenario from scenarioInfo into the main
##'    list, scenarios.
##' 2. Copy the model files from the operating model folder to the current scenario folder
##' 3. Read in the posterior and mse_posterior files for each scenario.
##'    This is done first so any errors will occur before running the simulations.
##' 4. Run the simulations.
##' @param scenarioInfo The user input data retrieved from the Scenarios.csv file.
##' @param continue If TRUE, continue simulations numerically.  In case system crashes during runs.
##' @param useSystem If TRUE, R will use the system() function instead of the shell() function.
##' @param verbose If TRUE, write extra output to the screen.
##' @param scen The scenario number for uncommented scenarios in Scenarios.csv to run.  This was added to allow parallelization.
##'             If NULL, all uncommented scenarios will be run on one process.
##' @param sendEmailUpdates If TRUE, email will be sent to the address specified.
##' @param emailAddress The gmail address you want email sent to.
##' @param emailPassword The password corresponding to the gmail emailAddress.
##' @return The 'scenarios' object containing the results of the MSE runs.
runScenarios <- function(scenarioInfo,
                         continue         = T,
                         useSystem        = T,
                         verbose          = F,
                         printstats       = F,
                         scen             = NULL,
                         sendEmailUpdates = F,
                         emailAddress     = NULL,
                         emailPassword    = NULL){

  scenarios   <- vector("list",length=length(scenarioInfo$scenarioName))
  prettyNames <- makePrettyNames(scenarioInfo)

  firstScenario <- 1
  lastScenario  <- length(scenarioInfo$scenarioName)
  if(!is.null(scen)){
    firstScenario <- scen
    lastScenario  <- scen
  }
  for(scenario in firstScenario:lastScenario){

    # Start of .SCENARIOS_INFO settings
    scenarios[[scenario]]               <- as.list(scenarioInfo[scenario,])
    scenarios[[scenario]]$omFullPath    <- file.path(.OM_DIR_NAME,scenarios[[scenario]]$operatingModelName)
    scenarios[[scenario]]$fullPath      <- file.path(.SCENARIOS_DIR_NAME,scenarios[[scenario]]$scenarioName)

    # Copy Operating model (SS) files over to the current scenarios folder.
    # create an empty ss3.dat file to eliminate error message when running SS
    copySSInputFiles(sourceDir = scenarios[[scenario]]$omFullPath,
                     destDir   = scenarios[[scenario]]$fullPath,
                     all       = T,
                     ss3.dat   = T)

    scenarios[[scenario]]$assessYears   <- scenarios[[scenario]]$firstAssessYear:scenarios[[scenario]]$lastAssessYear
    # TS = time series, so assessTSYears is from the start of the time series to the last assessed year.
    scenarios[[scenario]]$assessTSYears <- scenarios[[scenario]]$firstTSYear:scenarios[[scenario]]$lastAssessYear
    #scenarios[[scenario]]$sims          <- scenarioInfo$firstSimNum[scenario]:scenarioInfo$lastSimNum[scenario]
    scenarios[[scenario]]$seed          <- scenarioInfo$seed[scenario]

    scenarios[[scenario]]$timeVarySelexOM     <- scenarioInfo$timeVarySelexOM[scenario]
    scenarios[[scenario]]$timeVarySelexAssess <- scenarioInfo$timeVarySelexAssess[scenario]
    # End of .SCENARIOS_INFO settings

    # Setup Ageing error matrix and a pretty name for plotting purposes
    scenarios[[scenario]]$prettyName    <- prettyNames[scenario]
    scenarios[[scenario]]$ageingError   <- getAgeErrorMatrix(scenarioDat=scenarios[[scenario]])

    # The posterior files for this scenario
    postFile    <- file.path(.SCENARIOS_DIR_NAME,scenarios[[scenario]]$scenarioName,.POST_FILE)
    if(exists("printDebug")){if(printDebug) {print("runScenarios: posterior files for this scenario")}}
    if(exists("printDebug")){if(printDebug) {browser()}}
    msePostFile <- file.path(.SCENARIOS_DIR_NAME,scenarios[[scenario]]$scenarioName,.MSE_POST_FILE)
    tryCatch({
      if(scenarios[[scenario]]$runAssessments){
        scenarios[[scenario]]$post      <- read.table(postFile,header=T)
        msePost                         <- read.table(msePostFile,header=T)
        scenarios[[scenario]]$msePost   <- msePost
        scenarios[[scenario]]$selex     <- split(msePost,msePost$Fleet)
      }
    },error=function(err){
      stop("runScenarios: Error reading file ",postFile," for scenario '",scenarios[[scenario]]$scenarioName,
           "'  Check that it exists.\n",sep="")
    })

    startTime                         <- proc.time()
    sims                              <- runSimulations(scenarioDat      = scenarios[[scenario]],
                                                        continue         = continue,
                                                        useSystem        = useSystem,
                                                        verbose          = verbose,
                                                        printstats       = printstats,
                                                        sendEmailUpdates = sendEmailUpdates,
                                                        emailAddress     = emailAddress,
                                                        emailPassword    = emailPassword)

    if(!is.null(sims)){
      # If continue=T and all sim files existed, NULL was returned and we want to keep the structure as-is.
      scenarios[[scenario]]           <- sims
    }
    runTime                           <- proc.time() - startTime
    scenarios[[scenario]]$runTimeSecs <- runTime[3]
    cleanScenarioDir(dirName=scenarios[[scenario]]$fullPath)
  }
  names(scenarios) <- scenarioInfo$scenarioName
  return(scenarios)
}

##' Run all the simulations for the given scenario.
##' This includes copying all SS files from the scenario directory to its
##' 'simulations' subdirectory, running the assessment models with feedback loop
##' for those scenarios in Scenarios.csv which have runAssessments = TRUE, and cleaning
##' up the temporary files afterwards.
##' For those scenarios with runAssessments = FALSE, an RDATA file
##' (see .RESULTS_FILE in mseSSGlobals.r) will be saved, containing the
##' simulation data for that scenario, and the runAssessments() function will not
##' be called.
##' @param scenarioDat A single scenario list object as defined in Scenarios.csv.
##' @param continue If TRUE, continue simulations numerically.  In case system crashes during runs.
##' @param useSystem If TRUE, R will use the system() function instead of the shell() function.
##' @param verbose If TRUE, write extra output to the screen.
##' @param sendEmailUpdates If TRUE, email will be sent to the address specified.
##' @param emailAddress The gmail address you want email sent to.
##' @param emailPassword The password corresponding to the gmail emailAddress.
##' @return The modified scenarioDat list object containing the results of the MSE run,
##'         or NULL of continue=T was used but no simulations qualified none wer run.
runSimulations <- function(scenarioDat,
                           continue  = T,
                           useSystem = T,
                           verbose   = F,
                           printstats = F,
                           sendEmailUpdates = F,
                           emailAddress     = NULL,
                           emailPassword    = NULL){
  scenarioDir <- scenarioDat$scenarioName

  # If this is just a simulation run, i.e. no assessments, then this just
  #   saves an RDATA file in the scenario directory containing the information
  #   about this scenario for use by mergeMSE() later.
  #   We'll call this file whatever .RESULTS_FILE is in mseSSGlobals.r
  if(!scenarioDat$runAssessments){
    simDat         <- scenarioDat
    simulationsDir <- file.path(simDat$fullPath,.SIMULATIONS_DIR_NAME)
    simDatFullPath <- file.path(simulationsDir,.RESULTS_FILE)
    simDat <- getSimResults(simDat)
    # Create simulations directory if it doesn't exist
    dir.create(simulationsDir,showWarnings=F)
    # Create the RDATA file
    save(simDat,file=simDatFullPath)
    if(verbose){
      cat("runSimulations: Saved information for scenario: '",simDatFullPath,"'.\n\n",sep="")
    }
  }

  # Set the random seed for the scenario
  set.seed(scenarioDat$seed)

  # Change the sims vector in this scenario to be random numbers in the range selected in Scenarios.csv file
  scenarioDat$sims <- sort(sample(scenarioDat$minSimNum:scenarioDat$maxSimNum,scenarioDat$numSims))

  simDir <- file.path(scenarioDat$fullPath,.SIMULATIONS_DIR_NAME)
  # Create simulations directory if it doesn't exist
  dir.create(simDir,showWarnings=F)

  # Set all simulations to run
  simsNotYetRun <- scenarioDat$sims

  if(continue & scenarioDat$runAssessments){
    # Get file listing for the simulations directory
    files            <- list.files(simDir)
    # find all the sim RDATA files and extract the sim numbers from them
    pattern          <- paste(.SIM_FILE_PREFIX,"[0-9]+",.SIM_RDATA_FILE_EXT,sep="")
    simFiles         <- files[grep(pattern,files)]
    subPattern       <- paste(.SIM_FILE_PREFIX,"([0-9]+)\\",.SIM_RDATA_FILE_EXT,sep="")
    simNumsCompleted <- sort(as.numeric(gsub(subPattern,"\\1",simFiles)))

    # The random draw simulations should continue, the ones not yet run are the difference
    # between scenarioDat$sims (all of them) and simNums (the ones in the directory already).
    simsNotYetRun <- setdiff(scenarioDat$sims,simNumsCompleted)
  }

  if(length(simsNotYetRun) >0){
    # create a vector of simulations for this scenario
    simulations <- vector("list",length=length(simsNotYetRun))

    for(sim in 1:length(simsNotYetRun)){
      if(scenarioDat$runAssessments){  # if it's an assessment scenario...

        # Delete everything but the RDATA files from the sim directory,
        # including the assessments folder and its contents.
        # This is done here at the beginning as well as at the end,
        # for the case where a simulation is stopped halfway through, then
        # upon continuing, the simulation that was terminated will deleted
        # and be started over.  This only takes 0.07 seconds so it doesn't
        # really add too much overhead.

        cleanSimDir(simDir)

        #copy over psv executables if needed
        file.copy(file.path(.OM_DIR_NAME,.NEW_PSV_EXE),file.path(simDir,.NEW_PSV_EXE))
        if(scenarioDat$timeVarySelexOM) {
          file.copy(file.path(.OM_DIR_NAME,.ADD_PSV_EXE),file.path(simDir,.ADD_PSV_EXE))
        }

        # Store start time for setup of simulations
        startSetupSimTime <- proc.time()

        simulations[[sim]]                      <- scenarioDat
        simulations[[sim]]$simNum               <- simsNotYetRun[sim]
        simulations[[sim]]$sims                 <- NULL
        simulations[[sim]]$fullPath             <- file.path(scenarioDat$fullPath,.SIMULATIONS_DIR_NAME)
        # Create empty assessment vectors for admb outputs and catch
        simulations[[sim]]$numParameters        <- createAssessVector(length(scenarioDat$assessYears))
        simulations[[sim]]$finalGradient        <- createAssessVector(length(scenarioDat$assessYears))
        simulations[[sim]]$objectiveFunctionVal <- createAssessVector(length(scenarioDat$assessYears))
        # The catch vector must have 1 more element (for the last forecast year) than the others
        simulations[[sim]]$catch                <- createAssessVector(length(scenarioDat$assessYears)+1)
        simulations[[sim]]$assessYears          <- scenarioDat$firstAssessYear:scenarioDat$lastAssessYear
        simulations[[sim]]$catchYears           <- scenarioDat$firstAssessYear:(scenarioDat$lastAssessYear+1)

        # setup initial catch for assessment scenarios
        if(!scenarioDat$zeroCatch){
          # The set catch for first assessment year
          simulations[[sim]]$catch[1] <- scenarioDat$initialCatch
        }
        # setup the survey years for this simulation
        simulations[[sim]]$survYrs    <- c(scenarioDat$firstAssessYear,
                                           seq((scenarioDat$firstAssessYear+1),
                                               scenarioDat$lastAssessYear,
                                               scenarioDat$survFreq))
        if(verbose){
          cat("runSimulations: Scenario '",scenarioDat$prettyName,
              "', simulation #",simsNotYetRun[sim],"\n",sep="")
        }

        # Copy the SS files from scenario directory to simulation directory
        copySSInputFiles(sourceDir = scenarioDat$fullPath,
                         destDir   = simulations[[sim]]$fullPath,
                         ss3.dat=T)

        #create a new psv file with only 1 line for the particular sim # (saves a lot of time in mceval)
        #first copy over executable into simulations directory
        newPSV(simulations[[sim]]$simNum,simulations[[sim]]$fullPath)

        # Copy the data files and control files in the same directory (sim directory)
        # to files with the previous year
        copySSInputFiles(srcFileList = c(.DATA_FILE,.SIM_CONTROL_FILE,.ASSESS_CONTROL_FILE),
                         desFileList = c(.ORIG_DATA_FILE,.ORIG_SIM_CONTROL_FILE,.ORIG_ASS_CONTROL_FILE),
                         sourceDir   = simulations[[sim]]$fullPath,
                         destDir     = simulations[[sim]]$fullPath)

        setupStarterFile(dir     = simulations[[sim]]$fullPath,
                         #simNum  = scenarioDat$sims[sim],
                         ctlFile = .SIM_CONTROL_FILE)

        setupForecastFile(dir = simulations[[sim]]$fullPath)

        # Run the simulation (Operating Model) for this sim (mceval only)
        runMCEval(dir=simulations[[sim]]$fullPath)
        
        #update the ss3.par file with the current MCMC parameters for future additions
        updateParFile(dir=simulations[[sim]]$fullPath)

        # Store variables for this particular MCMC sample
        simulations[[sim]]$M            <- getM(scenarioDat,scenarioDat$sims[sim])
        simulations[[sim]]$qSurv        <- getQ(scenarioDat,scenarioDat$sims[sim])
        selex                           <- getSelexByFleet(scenarioDat,scenarioDat$sims[sim])
        simulations[[sim]]$selexFishery <- selex[[1]]
        simulations[[sim]]$selexSurvey  <- selex[[2]]

        if(exists("printDebug")){if(printDebug) {print("runSimulations: Store the runTime for the setup of simulations")}}
        if(exists("printDebug")){if(printDebug) {browser()}}
        # Store the runTime for the setup of simulations
        endSetupSimTime <- proc.time() - startSetupSimTime
        simulations[[sim]]$setupSimTimeSecs <- endSetupSimTime[3]

        # Setup structure for recording output from simulation and assessments
        simulations[[sim]]$assessNatage   <- NULL
        simulations[[sim]]$assessWtatage  <- NULL
        simulations[[sim]]$assessBatage   <- NULL
        # Pass the timeVarying flag along to single assessment runs
        simulations[[sim]]$timeVarySelexAssess <- scenarioDat$timeVarySelexAssess
        simulations[[sim]]$timeVarySelexOM     <- scenarioDat$timeVarySelexOM

        simulations[[sim]]$assessRecr     <- matrix(ncol=length(simulations[[sim]]$assessTSYears),
                                                    nrow=length(simulations[[sim]]$assessYears))
        simulations[[sim]]$assessSSB      <- matrix(ncol=length(simulations[[sim]]$assessTSYears),
                                                    nrow=length(simulations[[sim]]$assessYears))
        simulations[[sim]]$assessDepl     <- matrix(ncol=length(simulations[[sim]]$assessTSYears),
                                                    nrow=length(simulations[[sim]]$assessYears))

        simulations[[sim]]$assessSelexFishery <-
                                             matrix(nrow=length(simulations[[sim]]$assessYears),
                                                    ncol=scenarioDat$maxA + 1)
        simulations[[sim]]$assessSelexSurvey <-
                                             matrix(nrow=length(simulations[[sim]]$assessYears),
                                                    ncol=scenarioDat$maxA + 1)
        simulations[[sim]]$omSelexFishery     <-
                                             matrix(nrow=length(simulations[[sim]]$assessYears),
                                                    ncol=scenarioDat$maxA + 1)
        simulations[[sim]]$omSelexSurvey     <-
                                             matrix(nrow=length(simulations[[sim]]$assessYears),
                                                    ncol=scenarioDat$maxA + 1)

        simulations[[sim]]$assessRuntime  <- vector(mode="numeric",length=length(simulations[[sim]]$assessYears))
        simulations[[sim]]$assessVirgRecr <- vector(mode="numeric",length=length(simulations[[sim]]$assessYears))
        simulations[[sim]]$assessInitRecr <- vector(mode="numeric",length=length(simulations[[sim]]$assessYears))


        # **********************************************************************************
        # Run the simulation/assessments for this particular simulation run
        startTime                      <- proc.time()
        simulations[[sim]]             <- runAssessments(simulations[[sim]],
                                                         useSystem = useSystem,
                                                         verbose = verbose,
                                                         printstats = printstats,
                                                         sendEmailUpdates = sendEmailUpdates,
                                                         emailAddress     = emailAddress,
                                                         emailPassword    = emailPassword)
        runTime                        <- proc.time() - startTime
        simulations[[sim]]$runTimeSecs <- runTime[3]
        # **********************************************************************************

        # Get Simulation results.
        startCleanSimTime              <- proc.time()
        simulations[[sim]]             <- getSimResults(simulations[[sim]],
                                                        sendEmailUpdates=sendEmailUpdates)
        endCleanSimTime                       <- proc.time() - startCleanSimTime
        simulations[[sim]]$cleanupSimTimeSecs <- endCleanSimTime[3]

        # Set the column and row names for the assess at-age outputs
        rownames(simulations[[sim]]$assessNatage)  <- simulations[[sim]]$assessYears
        rownames(simulations[[sim]]$assessBatage)  <- simulations[[sim]]$assessYears
        rownames(simulations[[sim]]$assessWtatage) <- simulations[[sim]]$assessYears
        rownames(simulations[[sim]]$assessSelexFishery) <- simulations[[sim]]$assessYears
        rownames(simulations[[sim]]$assessSelexSurvey) <- simulations[[sim]]$assessYears
        rownames(simulations[[sim]]$assessRecr)    <- simulations[[sim]]$assessYears
        rownames(simulations[[sim]]$assessSSB)     <- simulations[[sim]]$assessYears
        rownames(simulations[[sim]]$assessDepl)    <- simulations[[sim]]$assessYears
        ageLabels <- paste(.AGE_ERROR_COL_PREFIX,0:simulations[[sim]]$maxA,sep="")
        colnames(simulations[[sim]]$assessNatage)  <- ageLabels
        colnames(simulations[[sim]]$assessBatage)  <- ageLabels
        colnames(simulations[[sim]]$assessWtatage) <- ageLabels
        colnames(simulations[[sim]]$assessSelexFishery) <- ageLabels
        colnames(simulations[[sim]]$assessSelexSurvey) <- ageLabels
        colnames(simulations[[sim]]$assessRecr)    <- simulations[[sim]]$assessTSYears
        colnames(simulations[[sim]]$assessSSB)     <- simulations[[sim]]$assessTSYears
        colnames(simulations[[sim]]$assessDepl)    <- simulations[[sim]]$assessTSYears

        # Remove redundant information from the sim object.  These are tracking variables for
        # assessment results and have already been saved in another place in the list
        simulations[[sim]]$currAssessYear <- NULL
        simulations[[sim]]$batage         <- NULL
        simulations[[sim]]$natage         <- NULL
        simulations[[sim]]$wtatage        <- NULL
        simulations[[sim]]$recr           <- NULL
        simulations[[sim]]$catches        <- NULL
        simulations[[sim]]$msePost        <- NULL
        simulations[[sim]]$post           <- NULL

        # Save the results for this simulation as a Rdata binary file
        simDat <- saveSimFile(simulations[[sim]])

        # Delete everything except Rdat files from the sim directory,
        # including the assessments folder and its contents.
        cleanSimDir(simulations[[sim]]$fullPath)
      }
      if(sendEmailUpdates){
        computerInfo <- Sys.info()
        computerName <- computerInfo["nodename"]
        computerOS   <- paste(computerInfo["sysname"],computerInfo["release"])
        emailMessage <- paste("Simulation # ",simulations[[sim]]$simNum," completed for scenario '",simDat$scenarioName,"'.\n",
                              "Years assessed: ",paste(simDat$assessYears,collapse=" "),"\n\n",
                              "Computer Name: ",computerName,"\n",
                              "Operating system: ",computerOS,"\n",
                              "Directory: ",simDat$fullPath,"\n\n",sep="")
        sendEmail(emailAddress = emailAddress,
                  emailPassword = emailPassword,
                  emailMessage = emailMessage,
                  subject = paste("Finished '",simDat$scenarioName,"'simulation #",simulations[[sim]]$simNum,sep=""))
      }
    }
    # Attach the simulations to the scenarioDat object
    scenarioDat$simulations <- simulations
    return(scenarioDat)
  }else{
    # All the sims were already run and continue=T was used, so nothing to do
    cat("runSimulations: Scenario '",scenarioDat$scenarioName,
        "' - no simulations were run because '",simDir,"' contains ",
        .SIM_FILE_PREFIX,"XX",.SIM_RDATA_FILE_EXT,"\n files for the 'lastSimNum' or higher (defined in '",.SCENARIOS_INFO,"').\n",sep="")
    return(NULL)
  }
}

##' Run all assessments for a single instance of a simulation.
##' This includes modifying SS files with simulated inputs such as survey indices and catch,
##' copying all SS files from the 'simulations' directory to its 'assessments' subdirectory,
##' running the assessment models, storing the results, and cleaning up the 'assessments' directory.
##' This is the function where the feedback loop is implemented.
##' @param simDat A single simulation list object.
##' @param useSystem If TRUE, R will use the system() function instead of the shell() function.
##' @param verbose If TRUE, write extra output to the screen.
##' @param sendEmailUpdates If TRUE, email will be sent to the address specified.
##' @param emailAddress The gmail address you want email sent to.
##' @param emailPassword The password corresponding to the gmail emailAddress.
##' @return The modified scenarioDat list object containing the results of the MSE run.
runAssessments <- function(simDat,
                           useSystem = T,
                           verbose = F,
                           printstats = F,
                           sendEmailUpdates = F,
                           emailAddress  = NULL,
                           emailPassword = NULL){
  # simDat is a single instance of a simulation, i.e. sim2 or sim98
  # simDat$dat will contain the datafile information which is read in,
  # appended to for the next simulated year, and written back out
  # for the assessment model.

  if(exists("printDebug")){if(printDebug) {print("Starting runAssessments function")}}

  simNum                <- simDat$simNum
  simDat$assessFullPath <- file.path(simDat$fullPath,.ASSESS_DIR_NAME)

  # Create the assessment directory in which the assessment runs will be performed
  dir.create(simDat$assessFullPath,showWarnings=F)

  # Copy the SS executable into the assessment dir once, never removing it for this simulation
  fullPathSimSS    <- file.path(simDat$fullPath,.SS_EXE_FILE)
  fullPathAssessSS <- file.path(simDat$assessFullPath,.SS_EXE_FILE)
  file.copy(from=fullPathSimSS,to=fullPathAssessSS)

  # Read in whole natage file once, then subset from within the assessment loop
  natageFile       <- file.path(simDat$fullPath,.POST_NATAGE_FILE)
  natage           <- read.table(natageFile,header=T)

  if(exists("printDebug")){if(printDebug) {print("Starting assessYr loop")}}

  SSfiles <- list()
  SSfiles$simNum <- simNum

  for(assessYr in simDat$assessYears){
    # set up paths and filenames for this assessment year
    fullPathSimData       <- file.path(simDat$fullPath,.DATA_FILE)
    fullPathSimControl    <- file.path(simDat$fullPath,.ASSESS_CONTROL_FILE)
    fullPathSimForecast   <- file.path(simDat$fullPath,.FORECAST_FILE)
    simDat$dat            <- SS_readdat(fullPathSimData,verbose=verbose)

    ######################
    # TEMPORARY - change natage so that it has 2 sexes to test the split sex simulations
    #cat("WARNING - hacked single-sex natage so that there are 2 sexes.. for testing..\n")
    #natage2 <- natage
    #natage2$Sex <- 2
    #natage <- rbind(natage,natage2)
    ######################

    # Save numbers-at-age for this assessment
    simDat$natage           <- natage[natage$mceval==simNum & natage$Yr==assessYr,.NATAGE_MIN_AGE_IND:.NATAGE_MAX_AGE_IND]
    # Save the sex structure (Males/Females/Both)of the numbers-at-age

    sexes    <- unique(natage$Sex)
    sexNames <- NULL
    for(sex in 1:length(sexes)){
      if(sexes[sex] == .MALE){
        sexNames[sex] <- "Male"
      }
      if(sexes[sex] == .FEMALE){
        sexNames[sex] <- "Female"
      }
      if(sexes[sex] == .COMBINEDSEX){
        sexNames[sex] <- "CombinedSexes"
      }
    }
    # sexes and sexNames correspond to each other element-for-element
    simDat$sexes    <- sexes
    simDat$sexNames <- sexNames
    # Set column names to the sex of the natage row
    rownames(simDat$natage) <- sexNames
    # store the natage for writing to assessment data file - the index will always be 1 here because it is a single assessYr during the loop
    simDat$assessNatage     <- rbind(simDat$assessNatage,simDat$natage)

    if(nrow(simDat$natage)!=length(simDat$sexes)){
      cat("runAssessment: Warning: problem with numbers at age (",natageFile,") for simulation: ",simDat$simNum,
          ", assessment year: ",assessYr,"\n",sep="")
    }

    # Save the constant weight-at-age vector for now.  Future work may include time varying weight at age
    simDat$assessWtatage  <- rbind(simDat$assessWtatage,.WT_AT_AGE)

    # Save biomass-at-age for this assessment
    simDat$batage         <- t(t(simDat$natage) * .WT_AT_AGE)
    simDat$assessBatage   <- rbind(simDat$assessBatage,simDat$batage)

    # Simulate new commercial data for year, this changes the data to be written to the assessment datafile
    # *** FLEET = 1 ***
    simDat <- simulateAgeComps(simDat,assessYr,fleet=1)

    # Simulate new survey data for year, this changes the data to be written to the assessment datafile
    # *** FLEET = 2 ***
    if(assessYr %in% simDat$survYrs){
      simDat <- simulateSurveyIndex(simDat,assessYr)
      simDat <- simulateAgeComps(simDat,assessYr,fleet=2)
    }

    if(exists("printDebug")){if(printDebug) {print("runAssessments: Change the end year in the datafile data for the assessment model")}}
    if(exists("printDebug")){if(printDebug) {browser()}}

    # Change the end year in the datafile data for the assessment model
    simDat$dat$endyr   <- assessYr

    # Update the catch data for the assessment model, and write the data file out
    simDat$dat$N_catch <- simDat$dat$N_catch + 1
    catchThisYear      <- simDat$catch[simDat$catchYears==assessYr] # Scalar value
    simDat$dat$catch   <- rbind(simDat$dat$catch,c(catchThisYear,assessYr,.SEASON))
    # The following two lines were added to try and get the correct catches for TV selectivity.
    # The problem was that these catches were being recorded from the derived_posterior.sso
    # file before, from the ForeCatch_ fields, but these had to be changed every assessment year
    # for TV selectivity so only the last 17 years of the time series was being stored at any given
    # iteration.
    simDat$catchesReal <- simDat$dat$catch[,1]
    names(simDat$catchesReal) <- simDat$dat$catch[,2]

    SS_writedat(simDat$dat,outfile=fullPathSimData,overwrite=T,verbose=verbose)

    # In simulation directory, add recruitment deviation year and bias corrections years to control files, and assessment time-varying selex if doing it
    modifyControlFile(simDat$fullPath,ctlFile=.SIM_CONTROL_FILE,timeVarySelex=simDat$timeVarySelexOM) #OM will be updated every year
    modifyControlFile(simDat$fullPath,ctlFile=.ASSESS_CONTROL_FILE,timeVarySelex=simDat$timeVarySelexAssess,assessYr=assessYr)  #put in assessYr for generalarity in case go with no annual assessment

    # Copy the data and control files for this year as backup, to debug with.
    srcDataFile <- file.path(simDat$fullPath,.DATA_FILE)
    desDataFile <- file.path(simDat$fullPath,paste(.DATA_FILE_BASENAME,"_",assessYr,.SS_FILE_EXT,sep=""))
    srcCtlFile  <- file.path(simDat$fullPath,.ASSESS_CONTROL_FILE)
    desCtlFile  <- file.path(simDat$fullPath,paste(.ASSESS_CTL_FILE_BASE,"_",assessYr,.SS_FILE_EXT,sep=""))

    file.copy(srcDataFile,desDataFile,overwrite=T)
    file.copy(srcCtlFile,desCtlFile,overwrite=T)

    # Copy the SS input files from the Simulation directory to the assessment directory
    #create an empty ss3.dat file to eliminate error message
    copySSInputFiles(srcFileList=c(.DATA_FILE,.ASSESS_CONTROL_FILE,.FORECAST_ASSESS_FILE,.STARTER_FILE,.WT_AT_AGE_FILE),
                     desFileList=c(.DATA_FILE,.ASSESS_CONTROL_FILE,.FORECAST_FILE,.STARTER_FILE,.WT_AT_AGE_FILE),
                     sourceDir=simDat$fullPath,
                     destDir=simDat$assessFullPath,
                     ss3.dat=T)

    setupStarterFile(dir = simDat$assessFullPath,
                     ctlFile = .ASSESS_CONTROL_FILE)

    # Create a parfile for the new year in the fullPathAssess directory based on the last year's Parfile
    # also modifies the Starter file so SS reads in par file.
    if(assessYr != simDat$assessYears[1]){
      createParfile(simDat$assessFullPath,timeVarySelex=simDat$timeVarySelexAssess)
    }

    # ********************************************
    # Run the assessment model and get the results
    if(exists("printDebug")){if(printDebug) {print("Running single assessment")}}
    startTime <- proc.time()
    simDat    <- runSingleAssessment(simDat,
                                     assessYr  = assessYr,
                                     useSystem = useSystem,
                                     verbose   = verbose,
                                     printstats= printstats,
                                     sendEmailUpdates = sendEmailUpdates,
                                     emailAddress     = emailAddress,
                                     emailPassword    = emailPassword)
    runTime   <- proc.time() - startTime
    yearTF    <- simDat$assessYears==assessYr
    simDat$assessRuntime[yearTF] <- runTime[3]
    if(exists("printDebug")){if(printDebug) {print("Finished running single assessment")}}
    # ********************************************

    #Read in SS files and Report file so that the assessment could be repeated if necessary
    SSfiles[[as.character(assessYr)]]$dat <- readLines(paste(simDat$assessFullPath,.DATA_FILE,sep="/"))
    SSfiles[[as.character(assessYr)]]$ctl <- readLines(paste(simDat$assessFullPath,.ASSESS_CONTROL_FILE,sep="/"))
    SSfiles[[as.character(assessYr)]]$frc <- readLines(paste(simDat$assessFullPath,.FORECAST_FILE,sep="/"))
    SSfiles[[as.character(assessYr)]]$sta <- readLines(paste(simDat$assessFullPath,.STARTER_FILE,sep="/"))
    SSfiles[[as.character(assessYr)]]$wt  <- readLines(paste(simDat$assessFullPath,.WT_AT_AGE_FILE,sep="/"))
    SSfiles[[as.character(assessYr)]]$rep <- readLines(paste(simDat$assessFullPath,.REPORT_FILE,sep="/"))
    # Clean up the assessment directory
    cleanAssessDir(path=simDat$assessFullPath)

    # Add catch from the assessment run to the sim forecast file to generate true population at start of next year
    forecastSim      <- readLines(fullPathSimForecast)
    ind              <- grep(.FORECAST_CATCH_TEXT,forecastSim)
    tmp              <- forecastSim[ind]
    tmp              <- strsplit(tmp,"#")[[1]]
    tmp[1]           <- assessYr+1-simDat$firstAssessYear
    forecastSim[ind] <- paste(tmp,collapse=" #")
    forecastSim      <- forecastSim[1:(length(forecastSim)-1)]
    # Fishery catch only, i.e. catch for only one fleet.

    # Write the catch to the forecast sim file
    catchForYear     <- simDat$catch[simDat$catchYears==assessYr] # Scalar value
    catchLine        <- paste(assessYr,"1 1",catchForYear)
    forecastSim      <- c(forecastSim,catchLine,.END_OF_INPUT)
    writeLines(forecastSim,fullPathSimForecast)

    #if time-varying selectivity in OM
    ### increment year and catch in dat file of OM
    ### add recruitment dev and selex devs to psv file

    # Make sure to use proper data and control files, so that it reads in forecast catches
    if(!simDat$timeVarySelexOM) {
        origDataFile <- file.path(simDat$fullPath,.ORIG_DATA_FILE)
        origCtlFile  <- file.path(simDat$fullPath,.ORIG_ASS_CONTROL_FILE)
        file.copy(origDataFile,srcDataFile,overwrite=T)
        file.copy(origCtlFile,srcCtlFile,overwrite=T)
    }

    if(simDat$timeVarySelexOM) {
      #add 1 year to psv file (and dim of admodel.cov)
      addToPSV(simNum,simDat$fullPath)
    }

    # runMCEval to get next years beginning biomass
    # This call will break if the original data and control files were not restored above
    #    when time varying selectivity in the OM is NOT used.
    #runMCEval(simDat$fullPath)

    #add new parameters to par file, update SS files for 1 more year, and run without estimation (phase=0)
    #this will provide all of the necessary quantities
    
    

    # Copy the current year's data and control files back again.
    file.copy(desDataFile,srcDataFile,overwrite=T)
    file.copy(desCtlFile,srcCtlFile,overwrite=T)
    if(exists("printDebug")){if(printDebug) {print(paste("Finished running single simulation for assessYr",assessYr))}}
  }

  #Save the objects as one Rdata binary file
  saveSSFiles(SSfiles,simDat$fullPath)

  # Strip out the agecomps from the $dat list before removing the whole $dat list
  # Store the entire datafile ageComps and Catch for this simulation's last assessment.
  #  - The previous dat files contain the same info minus later assessed years.
  simDat$ageComps   <- simDat$dat$agecomp
  simDat$simTSCatch <- simDat$dat$catch

  # remove the datafile (simDat$dat) part of the simDat list.  It is the last assessment and the data are
  # already captured elsewhere in the simDat list.
  simDat$dat <- NULL

  return(simDat)
}

##' Run a single assessment. This is the workhorse function and will be called every
##' time an assessment is run.
##' This includes a convergence check, see .CONV_CRITERIA and .MAX_ITERATIONS in mseSSGlobals.r.
##' @param simDat A single simulation list object.
##' @param assessYr The current assessment year, used to update the R matrices.
##' @param useSystem If TRUE, R will use the system() function instead of the shell() function.
##' @param verbose If TRUE, write extra output to the screen.
##' @param sendEmailUpdates If TRUE, email will be sent to the address specified.
##' @param emailAddress The gmail address you want email sent to.
##' @param emailPassword The password corresponding to the gmail emailAddress.
##' @return The modified simDat object containing the results of the assessment run.
runSingleAssessment <- function(simDat,
                                assessYr,
                                useSystem = T,
                                verbose   = F,
                                printstats= F,
                                sendEmailUpdates = F,
                                emailAddress     = NULL,
                                emailPassword    = NULL){

  assessCmd         <- paste(.SS_EXE_FILE,.NOHESS)
  fullPathAssessCmd <- file.path(simDat$assessFullPath,assessCmd)
  fullPathParFile   <- file.path(simDat$assessFullPath,.PAR_FILE)
  fullPathForFile   <- file.path(simDat$assessFullPath,.FORECAST_REPORT_FILE)
  fullPathSelexFile <- file.path(simDat$assessFullPath,.SELEX_POST_FILE)
  # Iterations of this assessment, used to see whether or not to redo assessment
  # if there is not convergence.
  iteration         <- 1

  # Because of problems with system calling using full paths, we must change directories here.
  currDir <- getwd()
  setwd(simDat$assessFullPath)
  if(exists("printDebug")){if(printDebug) {cat("Starting single assessment in directory",getwd(),"\n")}}

  tryCatch({
    if(useSystem){
      cmdOut <- system(command=assessCmd,intern=T)
    }else{
      cmdOut <- shell(cmd=assessCmd,intern=T)
    }
  },error=function(err){
    setwd(currDir)
    cat("runSingleAssessment: Scenario ",simDat$scenarioName,", Simulation ",simDat$simNum,
        ": Error running assessment in ",simDat$assessFullPath,".\n",sep="")
    return(NULL)
  })

  if(!is.null(attributes(cmdOut))) {  #an error in SS occurred
    stop("\nAn error in SS occurred from the assessment directory.\nTrying running SS from that directory to determine error.\n")
  }

  if(exists("printDebug")){if(printDebug) {cat("Finished single assessment in directory",getwd(),"\n")}}

  setwd(currDir) # Go back to the RSCRIPTS directory

  # Do convergence checks
  convCheck <- readLines(fullPathParFile,n=1)    # read first line from parfile
  #convCheck <- gsub("[[:alpha:]]+","",convCheck) # remove all letters
  #convCheck <- gsub(" ","",convCheck)            # remove all spaces
  #convCheck <- gsub("#","",convCheck)            # remove hash marks
  #convCheck <- gsub("-","e-",convCheck)          # replace "e" in scientific notation that may have been removed in the alpha gsub above
  #convCheck <- strsplit(convCheck,"=")           # remove equals signs
  #convCheck <- convCheck[[1]][-1]                # remove first element which is a null string ""
  #The above commented out lines fail when the gradient is greater than 0 (+ in scientific notation)
  convCheck <- strsplit(convCheck," ")[[1]] #split by space
  ind <- which(convCheck=="=") + 1  #select the indices 1 after the equals sign
  convCheck <- as.numeric(convCheck[ind])
  # convCheck is now a vector of three values:
  # 1. Number of parameters
  # 2. Objective function value
  # 3. Maximum gradient component

  if(exists("printDebug")){if(printDebug) {
    cat("runSingleAssessment\n")
    cat("Finished convergence checks for assessment in directory",getwd(),"\n")
    cat("Pausing before doing convergence checks\n")
    cat("Investigate the variables or press Enter to continue\n")
    browser()
  }}

  jit <- .JITTER

  while(convCheck[3] > .CONV_CRITERIA & iteration < .MAX_ITERATIONS){
    if(exists("printDebug")){if(printDebug) {cat("Redo assessment in directory",getwd(),"due to lack of convergence \n")}}

    # Redo the assessment with jitter if gradient is less than the convergence criterion
    iteration <- iteration + 1
    if(verbose){
      cat("redoing assessment due to non-convergence. Sim",simDat$simNum,"Yr",assessYr,"\n")
    }

    fullPathAssessStarter <- file.path(simDat$assessFullPath,.STARTER_FILE)
    starter <- SS_readstarter(file=fullPathAssessStarter,verbose=verbose)
    # Put in some jitter
    starter$jitter_fraction <- jit
    SS_writestarter(starter,dir=simDat$assessFullPath,file=.STARTER_FILE,overwrite=T,verbose=verbose,warn=F)

    # Because of problems with system calling using full paths, we must change directories here.
    currDir <- getwd()
    setwd(simDat$assessFullPath)
    tryCatch({
      if(useSystem){
        system(command=assessCmd,intern=T)
      }else{
        shell(cmd=assessCmd,intern=T)
      }
    },error=function(err){
      setwd(currDir)
      cat("runSingleAssessment: Scenario ",simDat$scenarioName,", Simulation ",simDat$simNum,
          ": Error running assessment in ",simDat$assessFullPath,".\n",sep="")
      return(NULL)
    })
    setwd(currDir) # Go back to the RSCRIPTS directory

    # Do convergence checks
    convCheck <- readLines(fullPathParFile,n=1)    # read first line from parfile
    #convCheck <- gsub("[[:alpha:]]+","",convCheck) # remove all letters
    #convCheck <- gsub(" ","",convCheck)            # remove all spaces
    #convCheck <- gsub("#","",convCheck)            # remove hash marks
    #convCheck <- gsub("-","e-",convCheck)          # replace "e" in scientific notation that may have been removed in the alpha gsub above
    #convCheck <- strsplit(convCheck,"=")           # remove equals signs
    #convCheck <- convCheck[[1]][-1]                # remove first element which is a null string ""
    #The above commented out lines fail when the gradient is greater than 0 (+ in scientific notation)
    convCheck <- strsplit(convCheck," ")[[1]] #split by space
    ind <- which(convCheck=="=") + 1  #select the indices 1 after the equals sign
    convCheck <- as.numeric(convCheck[ind])
    #convCheck is now a vector of three values (see above)

    jit <- jit * .JITTER_MULTIPLIER
  }

  if(exists("printDebug")){if(printDebug) {cat("Finished convergence checks for assessment in directory",simDat$assessFullPath,"\n")}}

  # The following line sets up the following lines so that the 'simNum' labelled row (and not the actual numerical row) are written to.
  simDat$numParameters[simDat$assessYears==assessYr]          <- as.numeric(convCheck[1])
  simDat$objectiveFunctionVal[simDat$assessYears==assessYr]   <- as.numeric(convCheck[2])
  simDat$finalGradient[simDat$assessYears==assessYr]          <- as.numeric(convCheck[3])

  # since we are in the assessment directory, just readLines the filename without path
  forecastOut <- readLines(fullPathForFile)
  ind         <- grep(.FCAST_WITH_FMATCH,forecastOut)
  forecastOut <- read.table(fullPathForFile,skip=(ind[1]),nrows=1,header=T)
  forecastOut <- floor(forecastOut$Total_Catch)
  if(forecastOut < .MINIMUM_CATCH){
    # SS cannot handle zero catch so we set some small amount
    forecastOut <- .MINIMUM_CATCH
  }

  if(exists("printDebug")){if(printDebug) {cat("Finished convergence checks for assessment in directory",getwd(),"\n")}}

  # Store forecast catch
  simDat$catch[simDat$catchYears==(assessYr+1)] <- forecastOut

  # Store the SSB and depletion for the assessment run
  simDat$currAssessYear <- assessYr # So getAssessmentResults knows what year it is
  simDat                <- getAssessmentResults(simDat,sendEmailUpdates=sendEmailUpdates,verbose=verbose,printstats=printstats)

  # Clean up the assessment directory
  #cleanAssessDir(path=simDat$assessFullPath) moved this out so that I can save SS files
  return(simDat)
}

##' Save a simulation file, which is a binary RDATA file containing the output of a single
##' simulation, for example 'sim002.rdata'. If the simulation ran assessments,
##' there will be assessment output for each year in the file. If the simulation
##' did not run assessments, the file will be called mseSS.rdata (.RESULTS_FILE in mseSSGlobals.r).
##' These file will be stored in the scenarios' 'simulations' directory.
##' @param simDat A single simulation list object.
##' @return The modified simDat object which will have the simFile location added.
saveSimFile <- function(simDat){
  simFile <- paste(.SIM_FILE_PREFIX,simDat$simNum,.SIM_RDATA_FILE_EXT,sep="")
  simFile <- file.path(simDat$fullPath,simFile)
  save(simDat,file=simFile)
  simDat$simFile <- simFile
  return(simDat)
}

##' Save SS files, as a binary RDATA file containing the data, control, forecast, starter
##' wtatage, and Report files, if the simulation ran assessments.
##' The name would be, for example 'ssFiles002.rdata'.
##' This binary file will be stored in the scenarios' 'simulations' directory.
##' @param ssFiles: A single simulation list object.
##' @return: NULL
saveSSFiles <- function(ssFiles,fullPath){
  simFile <- paste(.SS_FILES_PREFIX,ssFiles$simNum,.SIM_RDATA_FILE_EXT,sep="")
  simFile <- file.path(fullPath,simFile)
  save(ssFiles,file=simFile)
}

##' Get the simulation results from the derived_posteriors.sso file.
##' Depletion is calculated by dividing the SSB by the virgin SSB from the file.
##' @param simDat A single simulation list object.
##' @param sendEmailUpdates If TRUE, email will be sent to the address specified.
##' @param emailAddress The gmail address you want email sent to.
##' @param emailPassword The password corresponding to the gmail emailAddress.
##' @return The modified simDat object.
getSimResults <- function(simDat,
                          sendEmailUpdates = F,
                          emailAddress     = NULL,
                          emailPassword    = NULL){
  # Get the simulation results from the derived posterior file
  # for this particular simulation.
  # Returns the modified simDat object.
  
  derivedPostFile <- file.path(simDat$fullPath,.DER_POST_FILE)
  derPost <- read.table(derivedPostFile,header=T)
  # Filter for this simulation's number if it's an assessment run simulation
  if(!("runAssessments" %in% names(simDat))){
    simDat$runAssessments <- FALSE
  }
  if(simDat$runAssessments){
    derPost <- derPost[derPost$Iter==simDat$simNum,]
  }

  isSPRCase <- F
  if(!("firstTSYear" %in% names(simDat))){
    # Special case for the spr cases..
    # TODO: make spr cases individual scenarios instead of special case
    # For now, just get all years in the derived posteriors file.  It won't hurt anything.
    isSPRCase <- T
  }

  # Get Spawning stock biomass and calculate depletion
  simDat$SSB         <- derPost[,grep(.SSB_REP,names(derPost))]
  simDat$virgSSB     <- simDat$SSB[1]
  simDat$initSSB     <- simDat$SSB[2]
  simDat$SSB         <- simDat$SSB[-c(1,2)]

  # Get Recruitment
  simDat$virgRecr    <- derPost[,grep(.VIRGIN_RECR,names(derPost))]
  simDat$initRecr    <- derPost[,grep(.INITIAL_RECR,names(derPost))]
  simDat$recruitment <- derPost[,grep(.RECR_PREFIX,names(derPost))]
  # Above command also matches Virgin (1), Initial (2), and Unfished (last value) recruitment, so remove them
  simDat$recruitment <- simDat$recruitment[-c(1,2,length(simDat$recruitment))]
  simDat$SPRratio    <- derPost[,grep(.SPR_RATIO_REP,names(derPost))]
  simDat$Fvalue      <- derPost[,grep(.F_PREFIX,names(derPost))]

  # Note that forecasted catch (simDat$catch) is stored in the function runSingleAssessment.
  #simDat$catchesReal <- derPost[,grep(.FCAST_CATCH,names(derPost))]
  # CG: Because of time-varying selectivity, I moved catchesReal to the function runSingleAssessment,
  #     where the datafile is written out.
  simDat$F_Btgt      <- derPost[,.FB_TARGET]
  simDat$F_SPRtgt    <- derPost[,.FSPR_TARGET]

  if(!isSPRCase){
    currTSYears        <- simDat$firstTSYear:simDat$lastAssessYear
    simDat$SSB         <- simDat$SSB[1:length(currTSYears)]
    simDat$recruitment <- simDat$recruitment[1:length(currTSYears)]
    simDat$SPRratio    <- simDat$SPRratio[1:length(currTSYears)]
    simDat$Fvalue      <- simDat$Fvalue[1:length(currTSYears)]
    # See above - catchesReal problem
    #simDat$catchesReal <- simDat$catchesReal[1:length(simDat$assessYears)]
  }

  simDat$depl        <- simDat$SSB/simDat$virgSSB[[1]]
  return(simDat)
}

##' Get the assessment results for the given simulation/assessment year
##' Done using SS_output function from the r4ss package.
##' @param simDat A single simulation list object.
##' @param nLines The number of lines to retrieve from the Report.sso file. Be careful, because if you have a lot of parameters, you may not read in enough lines.
##' @param sendEmailUpdates If TRUE, email will be sent to the address specified.
##' @param emailAddress The gmail address you want email sent to.
##' @param emailPassword The password corresponding to the gmail emailAddress.
##' @return The modified simDat object.
getAssessmentResults <- function(simDat,
                                 nLines=5000,
                                 sendEmailUpdates = F,
                                 emailAddress     = NULL,
                                 emailPassword    = NULL,
                                 verbose=T,
                                 printstats=T){
  # predicted biomass for start of year after assessment is done (what catch is based on)
  assessYr   <- simDat$currAssessYear

  reportFile <- file.path(simDat$assessFullPath,.REP_FILE)
  out        <- readLines(reportFile,n=nLines)

  # Get next year bratio since that is what is used to determine catch
  tmp        <- out[grep(paste(.BRATIO_REP,assessYr+1,sep="_"),out)]
  brat       <- as.numeric(strsplit(tmp," ")[[1]][3])

  # Get next year SSB since that is what is used to determine catch
  tmp        <- out[grep(paste(.SSB_REP,assessYr+1,sep="_"),out)]
  ssb        <- as.numeric(strsplit(tmp," ")[[1]][3])

  yearTF     <- simDat$assessYears==assessYr

  # Read in the output without using the derived_posteriors.sso file
  output <- SS_output(dir=simDat$assessFullPath,covar=F,verbose=verbose,printstats=printstats)
  out    <- output$timeseries

  # Now read in output from the derived_posteriors file for this assessment
  #derPostFile <- file.path(simDat$assessFullPath,.DER_POST_FILE)
  #out         <- readLines(derPostFile)
  #headers     <- unlist(strsplit(out[1],split=" "))
  #values      <- unlist(strsplit(out[2],split=" "))

  # Save the estimated parameters from the derived_posteriors file
  #simDat$assessVirgRecr[yearTF]   <- as.numeric(values[grep(.VIRGIN_RECR,headers)])
  #simDat$assessInitRecr[yearTF]   <- as.numeric(values[grep(.INITIAL_RECR,headers)])
  #simDat$assessVirgSSB[yearTF]    <- as.numeric(values[grep(.VIRGIN_SSB,headers)])
  #simDat$assessInitSSB[yearTF]    <- as.numeric(values[grep(.INITIAL_SSB,headers)])
  # Save the estimated parameters from the SS_output output. Replaced the above lines with these
  simDat$assessVirgRecr[yearTF]   <- out$Recruit_0[1]
  simDat$assessInitRecr[yearTF]   <- out$Recruit_0[2]
  simDat$assessVirgSSB[yearTF]    <- out$SpawnBio[1]
  simDat$assessInitSSB[yearTF]    <- out$SpawnBio[2]

  # The assessment SSB, Depletion, and Recruitment vectors are
  #   stored in the same way as the assessment at-age vectors
  #recrValues <- values[grep(.RECR_PREFIX,headers)] # This was used for the dereived_posteriors.sso file output
  recrValues <- out$Recruit_0
  # Recr has 2 proceeding columns (Virgin and Initial) and one following (Recr_Unfished)
  # which need to be removed
  recrValues <- recrValues[-c(1,2,length(recrValues))]

  #ssbValues <- values[grep(.SSB_PREFIX,headers)] # This was used for the dereived_posteriors.sso file output
  ssbValues <- out$SpawnBio
  # SSB has 2 proceeding columns (Virgin and Initial) which need to be removed
  ssbValues <- ssbValues[-c(1,2)]
  # Depletion is the ssb / virgin ssb
  deplValues <- ssbValues/simDat$assessVirgSSB[yearTF]

  # Save assessment-estimated recruitment, SSB, and depletion
  currTSYears    <- simDat$firstTSYear:assessYr
  currAssessRecr <- recrValues[1:length(currTSYears)]
  currAssessSSB  <- ssbValues[1:length(currTSYears)]
  currAssessDepl <- deplValues[1:length(currTSYears)]

  # Store estimated selectivity information
  fullPathSelexFile <- file.path(simDat$assessFullPath,.SELEX_POST_FILE)
  selexOut <- readLines(fullPathSelexFile)
  selexDat <- selexOut[2:3]  # Remove excess output (it is repeated three times in the file)
  # selex[1] is the header and looks like this:
  # "run mceval Yr Fleet Sex  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20"
  # first row of selexDat is fleet 1, fishery
  # second row is fleet 2, survey
  selexComma <- gsub(" +",",",selexDat)
  # change from comma-delimited strings to vectors
  selex1 <- unlist(strsplit(selexComma[1],split=","))
  selex2 <- unlist(strsplit(selexComma[2],split=","))

  yearTF <- simDat$assessYears==assessYr

  # Strip out the first 5 columns, which are "run mceval Yr Fleet Sex" and make numeric
  simDat$assessSelexFishery[yearTF] <- as.numeric(selex1[-(1:5)]) # 1 = Fishery
  simDat$assessSelexSurvey[yearTF]  <- as.numeric(selex2[-(1:5)]) # 2 = Survey

  # Because this is a ragged matrix, this is simplist to fill it in
  for(col in 1:length(currAssessRecr)){
    simDat$assessRecr[yearTF,col] <- currAssessRecr[col]
    simDat$assessSSB[yearTF,col]  <- currAssessSSB[col]
    simDat$assessDepl[yearTF,col] <- currAssessDepl[col]
  }
  return(simDat)
}

##' This is a wrapper function to get the special 'Perfect Information'
##' SPR-varying scenarios. A call to the getSimResults() function is
##' made to get the results.
##' @param dirName The directory containing the SPR subdirectories.
##' @param defaultSPR This SPR is a special case and is loaded directly from
##'                   the 'dirName' directory.
##' @return a list of outputs from getSimResults(), one for each SPR case.
getSPRResults <- function(dirName,defaultSPR="0.4"){
  # load all SPR folders' data found in the given dirName.
  # defaultSPR is a special case, and is assumed as the base case and loaded from the
  # dirName directory and not its subdirectories
  dirList <- list.dirs(dirName,recursive=F)
  if(length(dirList)==0){
    cat("getSPRResults: No spr directories found in '",dirName,
        "'. Check that it exists and has a subdirectory for each spr case!.\n",sep="")
    return(NULL)
  }
  sprList <- basename(dirList)
  defaultSPRName <- paste(.SPR_DIR_PREFIX,defaultSPR,sep="")
  if(!any(sprList==defaultSPRName)){
    sprList <- c(sprList,defaultSPRName)
  }
  if(any(sprList==.SIMULATIONS_DIR_NAME)){
    # remove the simulations directory as it is not the spr case.
    pattern <- .SIMULATIONS_DIR_NAME
    sprList <- sprList[-grep(pattern,sprList)]
  }
  sprList        <- sort(sprList)
  sprs           <- sort(as.numeric(gsub("spr","",sprList)))
  results        <- vector(mode="list",length=length(sprs))
  names(results) <- sprList
  for(i in 1:length(sprs)) {
    simDat <- NULL
    if(sprs[i] == as.numeric(defaultSPR)) {
      simDat$fullPath <- dirName
    }else{
      simDat$fullPath <- file.path(dirName,paste(.SPR_DIR_PREFIX,sprs[i],sep=""))
    }
    results[[i]] <- getSimResults(simDat)
    cat("getSPRResults: Perfect information scenario - loaded ",sprList[i],"\n",sep="")
  }
  cat("\n")
  return(results)
}

updateParFile <- function(dir,iter,postsParNames,parFileNames,parIn="ss3.par",parOut=parIn,postsF="posteriors.sso") {
    # updates the parameters in the par file with values from the mcmc for iteration iter
    # This function does not add parameters, but simply updates all exisiting parameters
    # @dir: is the directory with the posteriors.sso and ss3.par file
    # @iter: is the iteration number matching the posteriors.sso file
    # @postsParNames: general names of the parameters in the posteriors file. This does not include year and links to parFileNames.
    # @parFileNames:  names in the par file which link to postsParNames
    # @parIn: name of the par file to read in
    # @parOut: name of the updated par file to output
    # @postsF: name of the posteriors file to read in
    
    if(length(parFileNames) != length(postsParNames)) {stop("length of par file names does not match length of posteriors file names in updateParFile function\n")} 

    #print(parOut) if I don't use parOut before parIn is called, it changes parOut since parOut=parIn in the args. 
    parIn <- file.path(dir,parIn)
    #parOut <- file.path(dir,parOut)
    postsF <- file.path(dir,postsF)
    
    #first read in parameters from posteriors.sso file in current directory
    posts <- read.table(postsF,header=T)
    posts <- posts[posts$Iter==iter,]
    pars  <- par2Rlist(parF)

    #grep interprets a "[" as a special character, which occurs in par names, therefore use fixed=T

    #Loop over each parFileNames to add in appropriate postsParNames
    #pars is a list with named elements
    #posts is a vector with named elements, thus need to extract appropriate elements
    #check that the length of each parameter type matches between the two files
    for(i in 1:length(parFileNames)) {
        tmpParsInd <- grep(parFileNames[i],names(pars),fixed=T)
        tmpPosts <- posts[,grep(postsParNames[i],names(posts))]
        if(substring(parFileNames[i],1,8)=="selparm[") {
            #remove Devs if they exist
            tmpPosts <- tmpPosts[!((1:length(tmpPosts)) %in% grep("DEV",names(tmpPosts)))]
            for(j in 1:length(tmpPosts)) {
                pars[[tmpParsInd+j-1]] <- tmpPosts[j]
            }
        } else {
            if(parFileNames[i] == "Fcast_recruitments") {
                tmpPosts <- c(posts[,grep("Late_",names(posts))],posts[,grep("ForeRecr",names(posts))])
            }
            if(length(pars[[tmpParsInd]]) != length(tmpPosts)) {
                stop("The length of ",parFileNames[i]," in the pars file does not equal the length of ",postsParNames[i]," in the posts file\n")
            }
            if(is.matrix(pars[[tmpParsInd]])) {  #specific for selparm_dev right now
                pars[[tmpParsInd]] <- matrix(tmpPosts,ncol=ncol(pars[[tmpParsInd]]),nrow=nrow(pars[[tmpParsInd]]),byrow=T)
            } else {
                pars[[tmpParsInd]] <- tmpPosts
            }
        }
    }

print("writing ss3.par")
print(parOut)
    #write out new par file
    cat(names(pars)[1],"\n",file=parOut,append=F)
    cat(pars[[1]],"\n",file=parOut,append=T)
    for(i in 2:length(pars)) {
        cat(names(pars)[i],"\n",file=parOut,append=T)
        if(is.matrix(pars[[i]])) {
            write.table(pars[[i]],file=parOut,append=T,row.names=F,col.names=F)
        } else {
            cat(unlist(pars[[i]]),"\n",file=parOut,append=T)
        }
    }

}

modifyBiasCorr <- function(fullPathSim,ctlFile) {
  # changes bias correction so that 1 in all years
  fullPathControl <- file.path(fullPathSim,ctlFile)
  ctl <- readLines(fullPathControl)

  #ind      <- grep(.END_YEAR_RECDEV,ctl)
  #x        <- as.numeric(strsplit(ctl[ind],"#")[[1]][1])
  #comment  <- paste("#",.END_YEAR_RECDEV)
  #ctl[ind] <- paste(x+1,comment)

  ind      <- grep(.LAST_YEAR_NOBIAS,ctl)
  x        <- as.numeric(strsplit(ctl[ind],"#")[[1]][1])
  comment  <- paste("#",.LAST_YEAR_NOBIAS,sep="\t")
  ctl[ind] <- paste(1,comment)

  ind      <- grep(.FIRST_YEAR_BIAS_CORR,ctl)
  x        <- as.numeric(strsplit(ctl[ind],"#")[[1]][1])
  comment  <- paste("#",.FIRST_YEAR_BIAS_CORR,sep="\t")
  ctl[ind] <- paste(2,comment)

  ind      <- grep(.LAST_YEAR_BIAS_CORR,ctl)
  x        <- as.numeric(strsplit(ctl[ind],"#")[[1]][1])
  comment  <- paste("#",.LAST_YEAR_BIAS_CORR,sep="\t")
  ctl[ind] <- paste(3000,comment)

  ind      <- grep(.FIRST_YEAR_NOBIAS,ctl)
  x        <- as.numeric(strsplit(ctl[ind],"#")[[1]][1])
  comment  <- paste("#",.FIRST_YEAR_NOBIAS,sep="\t")
  ctl[ind] <- paste(3001,comment)

  ind      <- grep(.MAX_BIAS_ADJ,ctl)
  x        <- as.numeric(strsplit(ctl[ind],"#")[[1]][1])
  comment  <- paste("#",.MAX_BIAS_ADJ,sep="\t")
  ctl[ind] <- paste(1,comment)

  writeLines(ctl,fullPathControl)
}

postsParNames <- c("NatM_p_1_Fem_GP_1","SR_LN.R0.","SR_BH_steep","Early_",      "Main_",  "ForeRecr",          "Q_extraSD_2_Acoustic_Survey","AgeSel_1P_","AgeSel_2P_","_Fishery_DEVadd_")
parFileNames <- c("MGparm[1]",         "SR_parm[1]","SR_parm[2]","recdev_early","recdev1","Fcast_recruitments","Q_parm[1]",                  "selparm[3]","selparm[25]","selparm_dev")
setwd("C:\\NOAA2014\\Hake\\MSE\\testMSESS\\OperatingModels\\test\\mcmcDev2")
updateParFile(getwd(),iter=4,postsParNames=postsParNames,parFileNames=parFileNames)
#change starter to read from par and last phase = 0
starter <- SS_readstarter("starter.ss")
starter$init_values_src <- 1         #read from par
starter$last_estimation_phase <- 0   #no estimation
SS_writestarter(starter,dir=getwd(),file="starter.ss",overwrite=T)
#Change control file to have bias correction at 1 for all years
modifyBiasCorr(getwd(),"simulation_control.ss")
#There is a bug in SS where it appends a couple of lines to posteriors files (posteriors.sso, derived_posteriors.sso, and maybe more)
#  either copy and replace psoteriors, or run ss3 from par in a separate directory. UpdateParFile won't work with the appended lines.
system("ss3.exe -nohess")

par2Rlist <- function(fn) {
    #reads in ss3.par and converts it to a list with names
    #inspired by Steve Martell's code on the admb-project site
    ifile <- readLines(fn)
    idx <- grep("#",ifile)
    vnam <- ifile[idx] #list names
    nv <- length(vnam) #number of objects
    A <- vector(mode="list",length=nv)
    names(A) <- vnam
    ir <- 0
    for(i in 1:nv) {
        ir <- match(vnam[i],ifile)
        if(i!=nv) irr <- match(vnam[i+1],ifile) else irr=length(ifile)+1 #next row
        dum <- NA
        if(irr-ir==2) dum <- as.double(scan(fn,skip=ir,nlines=1,quiet=T,what=""))
        if(irr-ir>2) dum <- as.matrix(read.table(fn,skip=ir,nrow=irr-ir-1,fill=T))
        if(is.numeric(dum)) { #Logical test to ensure dealing with numbers
            A[[vnam[i]]]=dum
        }
    }
    return(A)
}



##' Merge the simulation binary RDATA files for all scenarios and simulations
##' into a unified object, and store it in another binary RDATA file
##' defined as .RESULTS_FILE in mseSSGlobals.r.
##' Some matrix calculations are done to save time later, such as medians of various values.
##' The output object of this function is recognizable to the plotting code included in
##' mseSSPlotAll.r and mseSSFigures*.r files.
##' The output list will be in order of the $plotOrder variable in each scenario
##' @param verbose If TRUE, write extra output to the screen.
##' @return Nothing, but the scenarios object called 's' which contains all the output
##' from the MSE runs for all scenarios will exists in the global environment.
mergeMSE <- function(verbose = TRUE){
  scenariosFullPath <- list.dirs(.SCENARIOS_DIR_NAME,recursive=F)
  scenarioNames     <- NULL

  scenarios <- NULL

  # Go through each directory and glue the output together
  simDirs          <- file.path(scenariosFullPath,.SIMULATIONS_DIR_NAME)
  foundPerfectInfo <- FALSE
  nextScenario     <- 1

  for(scenario in 1:length(simDirs)){
    # Clean the Simulation directory to cover the case where the user stopped the
    # code prior to the cleanSimDir function being run, which leaves it littered with
    # remnants of the assessment process.
    cleanSimDir(simDirs[[scenario]])

    # Get only files matching the simXX.rdata name format or mseSSResults.rdata
    files <- list.files(simDirs[[scenario]])
    if(length(files) == 1 & files[1] == .RESULTS_FILE){
      # It must be the single file mseSSResults.rdata only.
      simDatFile <- file.path(simDirs[[scenario]],.RESULTS_FILE)

      if(file.exists(simDatFile)){
        # This must be a non-assessment case, so load the mseSSResults.rdata file
        # This loads in the object simDat which can then be parsed using getSimResults()
        load(file=simDatFile)
        scenarios[[nextScenario]] <- getSimResults(simDat)
        scenarios[[nextScenario]] <- mergeScenario(simDirs[[scenario]],assessmentSim=F)
        scenarioNames <- c(scenarioNames,simDat$scenarioName)
        if(simDat$perfectInfoCase){
          foundPerfectInfo <- T
          scenarios[[nextScenario]]$spr <- getSPRResults(scenariosFullPath[[scenario]])
        }
        nextScenario <- nextScenario + 1
      }
    }else if(length(files) > 0){
      # There are simXX.rdata files
      scenarios[[nextScenario]] <- mergeScenario(simDirs[[scenario]])
      scenarioNames <- c(scenarioNames,scenarios[[scenario]]$scenarioName)
      if(verbose){
        cat("mergeMSE: Loaded ",simDirs[[scenario]],".\n\n",sep="")
      }
      nextScenario <- nextScenario + 1
    }else{
      if(verbose){
        cat("mergeMSE: Directory ",simDirs[[scenario]]," does not contain the non-assessment simulation file '",
            .RESULTS_FILE,"' \nor simulation files of the format simXX.rdata.\n Nothing loaded for this scenario.\n\n",sep="")
      }
    }
  }
  if(!foundPerfectInfo){
    if(verbose){
      cat("mergeMSE: No perfect information scenario found! The directory for it must be ",
          .PERFECT_INFO_NAME,".\n\n",sep="")
    }
  }
  names(scenarios) <- scenarioNames
  # sort the scenarios list based on the $plotOrder
  sortedNames      <- vector("character",length=length(scenarios))
  sortedScenarios  <- vector("list",length=length(scenarios))
  if(verbose){
    cat("mergeMSE: Plot Orders are:\n")
    for(scenario in 1:length(scenarios)){
      cat(" plotOrder = ",scenarios[[scenario]]$plotOrder," for ",scenarios[[scenario]]$fullPath,"\n",sep="")
    }
    cat("\n")
    cat("mergeMSE: Plot Colors are:\n")
    for(scenario in 1:length(scenarios)){
      cat(" plotColor = ",scenarios[[scenario]]$plotColor," for ",scenarios[[scenario]]$fullPath,"\n",sep="")
    }
    cat("\n")
  }

  # Plot orders are numerical, with the lowest number being plotted first, then
  # incrementing numerically.
  plotOrder <- data.frame()
  for(scenario in 1:length(scenarios)){
    plotOrder[scenario,1] <- scenario
    plotOrder[scenario,2] <- scenarios[[scenario]]$plotOrder
  }
  # The following commands reorders the table by plotOrder, so now we can just
  # read the scenario number by row incrementally
  sortedPlotOrders <- sort(plotOrder[,2])
  plotOrder <- order(plotOrder[,2],sortedPlotOrders)
  # plotOrder is now a vector of the correct order for scenarios to be in the list
  # i.e. 2,1 means that the scenarios should be ordered with the second one first
  # and the first one second.
  for(scenario in 1:length(scenarios)){
    sortedScenarios[[scenario]] <- scenarios[[plotOrder[scenario]]]
    sortedNames[[scenario]]     <- scenarios[[plotOrder[scenario]]]$scenarioName
  }
  names(sortedScenarios) <- sortedNames
  resultsFile            <- file.path(.SCENARIOS_DIR_NAME,.RESULTS_FILE)
  s                      <- sortedScenarios
  save(s,file=resultsFile)
  if(verbose){
    cat("mergeMSE: The results have been saved in ",resultsFile,"\n\n",sep="")
  }
  # Create the global scenarios object 's'
  s <<- s
}

mergeScenario <- function(simDir,
                          assessmentSim = TRUE){
  # If assessmentSim=T, then this simulation has assessment runs associated with it
  files      <- list.files(simDir)

  if(assessmentSim){
    pattern    <- paste(.SIM_FILE_PREFIX,"[0-9]+",.SIM_RDATA_FILE_EXT,sep="")
    simFiles   <- files[grep(pattern,files)]
    # Get the sim numbers from the filenames
    subPattern <- paste(.SIM_FILE_PREFIX,"([0-9]+)\\",.SIM_RDATA_FILE_EXT,sep="")
    # simNums are stripped as integer X from filenames of the format 'simX.rdata'. simNums can break if
    # user puts something bad in the directory
    simNums    <- as.numeric(gsub(subPattern,"\\1",simFiles))
  }else{
    # It is a non-assessment scenario, so load the single file "mseSSResults.rdata" (.RESULTS_FILE)
    pattern  <- .RESULTS_FILE
    simFiles <- files[grep(pattern,files)]
  }

  sims <- NULL
  # Create a list of matrices of length = number of simulations: for assessment at-age, assessment recruitment,
  #   selectivity for fishery and survey, simulated age comps, and simulated catch (last two from datafiles)
  if(assessmentSim){
    sims$assessNatage  <- vector("list",length=length(simNums))
    sims$assessWtatage <- vector("list",length=length(simNums))
    sims$assessBatage  <- vector("list",length=length(simNums))
    sims$assessRecr    <- vector("list",length=length(simNums))
    sims$assessSelexFishery <- vector("list",length=length(simNums))
    sims$assessSelexSurvey  <- vector("list",length=length(simNums))
    sims$ageComps      <- vector("list",length=length(simNums))
    sims$simTSCatch    <- vector("list",length=length(simNums))
  }

  for(sim in 1:length(simFiles)){  # sim is an index of the simulations, not actual sim numbers!

    if(assessmentSim){
      simFile <- file.path(simDir,files[sim])
    }else{
      simFile <- file.path(simDir,files)
    }
    load(simFile)
    # Now object simDat contains the output of simulation number 'sim' or of mseSSResults.rdata

    # Get all of the simulation related output.
    if(assessmentSim){
      sims$virgSSB            <- c(sims$virgSSB,simDat$virgSSB[[1]])
      sims$initSSB            <- c(sims$initSSB,simDat$initSSB[[1]])
      sims$virgRecr           <- c(sims$virgRecr,simDat$virgRecr)
      sims$initRecr           <- c(sims$initRecr,simDat$initRecr)
      sims$F_Btgt             <- c(sims$F_Btgt,simDat$F_Btgt)
      sims$F_SPRtgt           <- c(sims$F_SPRtgt,simDat$F_SPRtgt)
      sims$M                  <- c(sims$M,simDat$M)
      sims$qSurv              <- c(sims$qSurv,simDat$qSurv)
      sims$simRuntime         <- c(sims$simRuntime,simDat$runTimeSecs)
      sims$simSetuptime       <- c(sims$simSetuptime,simDat$setupSimTimeSecs)
      sims$simCleanuptime     <- c(sims$simCleanuptime,simDat$cleanupSimTimeSecs)
      # Matrices
      sims$SSB                  <- rbind(sims$SSB,simDat$SSB)
      sims$depl                 <- rbind(sims$depl,simDat$depl)
      # Note that assessment catches will have 1 more row than non-assessed catch
      sims$catches              <- rbind(as.data.frame(sims$catches), simDat$catch)
      sims$catchesReal          <- rbind(sims$catchesReal,simDat$catchesReal)

      sims$Fvalue               <- rbind(sims$Fvalue,simDat$Fvalue)
      sims$SPRratio             <- rbind(sims$SPRratio,simDat$SPRratio)
      sims$recruitment          <- rbind(sims$recruitment,simDat$recruitment)

      #### Merge all of the assessment model data
      # At-age matrices, which are added to a list
      sims$assessNatage[[sim]]  <- simDat$assessNatage
      sims$assessWtatage[[sim]] <- simDat$assessWtatage
      sims$assessBatage[[sim]]  <- simDat$assessBatage

      sims$finalGradient        <- rbind(sims$finalGradient,simDat$finalGradient)
      sims$objectiveFunctionVal <- rbind(sims$objectiveFunctionVal,simDat$objectiveFunctionVal)
      sims$numParameters        <- rbind(sims$numParameters,simDat$numParameters)

      # Assessment (estimated) selectivities
      sims$assessSelexFishery[[sim]] <- simDat$assessSelexFishery
      sims$assessSelexSurvey[[sim]]  <- simDat$assessSelexSurvey
      #sims$assessSelexFishery   <- rbind(sims$assessSelexFishery,simDat$assessSelexFishery)
      #sims$assessSelexSurvey    <- rbind(sims$assessSelexSurvey,simDat$assessSelexSurvey)

      # Assessment recruitment, SSB, and Depletion  matrices, which are added to a list
      sims$assessRecr[[sim]]    <- simDat$assessRecr
      sims$assessSSB[[sim]]     <- simDat$assessSSB
      sims$assessDepl[[sim]]    <- simDat$assessDepl

      sims$assessRuntime        <- rbind(sims$assessRuntime,simDat$assessRuntime)
      sims$assessVirgSSB        <- rbind(sims$assessVirgSSB,simDat$assessVirgSSB)
      sims$assessVirgRecr       <- rbind(sims$assessVirgRecr,simDat$assessVirgRecr)
      sims$assessInitSSB        <- rbind(sims$assessInitSSB,simDat$assessInitSSB)
      sims$assessInitRecr       <- rbind(sims$assessInitRecr,simDat$assessInitRecr)
      sims$ageComps[[sim]]      <- simDat$ageComps
      sims$simTSCatch[[sim]]    <- simDat$simTSCatch

      # Selectivity
      sims$selexFishery         <- rbind(sims$selexFishery,simDat$selexFishery)
      sims$selexSurvey          <- rbind(sims$selexSurvey,simDat$selexSurvey)

    }else{
      sims$virgSSB            <- as.vector(simDat$virgSSB[,1])
      sims$initSSB            <- as.vector(simDat$initSSB[,1])
      sims$virgRecr           <- simDat$virgRecr
      sims$virgRecr           <- simDat$initRecr
      sims$F_Btgt             <- simDat$F_Btgt
      sims$SSB                <- simDat$SSB
      sims$depl               <- simDat$depl
      sims$catchesReal        <- simDat$catchesReal
      sims$Fvalue             <- simDat$Fvalue
      sims$SPRratio           <- simDat$SPRratio
      sims$recruitment        <- simDat$recruitment
    }

    # Merge other stuff that does not change from sim to sim in a scenario
    sims$scenarioName    <- simDat$scenarioName
    sims$prettyName      <- simDat$prettyName
    sims$plotColor       <- simDat$plotColor
    sims$plotOrder       <- simDat$plotOrder
    sims$runAssessments  <- simDat$runAssessments
    sims$fullPath        <- simDat$fullPath
    sims$assessFullPath  <- simDat$assessFullPath
    sims$minA            <- simDat$minA
    sims$maxA            <- simDat$maxA
    sims$plusA           <- simDat$plusA
    sims$zeroCatch       <- simDat$zeroCatch
    sims$perfectInfoCase <- simDat$perfectInfoCase
    sims$firstAssessYear <- simDat$firstAssessYear
    sims$lastAssessYear  <- simDat$lastAssessYear
    sims$assessYears     <- simDat$assessYears
    sims$firstTSYear     <- simDat$firstTSYear
    sims$assessTSYears   <- simDat$assessTSYears
    sims$catchYears      <- simDat$catchYears
    sims$survFreq        <- simDat$survFreq
    sims$survSELog       <- simDat$survSELog
    sims$survSETot       <- simDat$survSETot
    sims$survNAge        <- simDat$survNAge
    sims$survNAgeAdj     <- simDat$survNAgeAdj
    sims$commNAge        <- simDat$commNAge
    sims$commNAgeAdj     <- simDat$commNAgeAdj
    sims$ageingError     <- simDat$ageingError
    sims$survYears       <- simDat$survYrs
  }
  if(assessmentSim){
    # Calculate medians of assessed values
    sims$assessSSBMedianByYear  <- medianOfAssessList(sims$assessSSB)
    sims$assessDeplMedianByYear <- medianOfAssessList(sims$assessDepl)
    sims$assessRecrMedianByYear <- medianOfAssessList(sims$assessRecr)

    sims$assessSSBMedianBySim    <- matrix(nrow=length(sims$assessSSB),ncol=ncol(sims$assessSSB[[1]]))
    sims$assessDeplMedianBySim   <- matrix(nrow=length(sims$assessDepl),ncol=ncol(sims$assessDepl[[1]]))
    sims$assessRecrMedianBySim   <- matrix(nrow=length(sims$assessRecr),ncol=ncol(sims$assessRecr[[1]]))
    for(sim in 1:length(sims$assessSSB)){
      sims$assessSSBMedianBySim[sim,]  <- apply(sims$assessSSB[[sim]],2,median,na.rm=T)
      sims$assessDeplMedianBySim[sim,] <- apply(sims$assessDepl[[sim]],2,median,na.rm=T)
      sims$assessRecrMedianBySim[sim,] <- apply(sims$assessRecr[[sim]],2,median,na.rm=T)
    }
    colnames(sims$assessSSBMedianBySim)  <- colnames(sims$assessSSB[[1]])
    colnames(sims$assessDeplMedianBySim) <- colnames(sims$assessDepl[[1]])
    colnames(sims$assessRecrMedianBySim) <- colnames(sims$assessRecr[[1]])

    sims$assessSSBMedian  <- apply(sims$assessSSBMedianByYear,2,median,na.rm=T)
    sims$assessDeplMedian <- apply(sims$assessDeplMedianByYear,2,median,na.rm=T)
    sims$assessRecrMedian <- apply(sims$assessRecrMedianByYear,2,median,na.rm=T)

    # Make all the output nice and pretty...
    for(sim in 1:length(simNums)){
      # Prefix single digits with two 0's and double digits with 3 0's
      if(nchar(as.character(simNums[sim])) == 3){
        simNums[sim] <- as.character(simNums[sim])
      }
      if(nchar(as.character(simNums[sim])) == 2){
        simNums[sim] <- paste("0",as.character(simNums[sim]),sep="")
      }
      if(nchar(as.character(simNums[sim])) == 1){
        simNums[sim] <- paste("00",as.character(simNums[sim]),sep="")
      }
    }
    # Calculate medians of simulated values
    sims$SSBMedian         <- apply(sims$SSB,2,median)
    sims$deplMedian        <- apply(sims$depl,2,median)
    sims$recruitmentMedian <- apply(sims$recruitment,2,median)

    simLabels <- paste(.SIM_FILE_PREFIX,simNums,sep="")
    sims$simNums               <- as.numeric(simNums)

    names(sims$virgSSB)        <- simLabels
    sims$virgSSB               <- sortVectorByName(sims$virgSSB)
    names(sims$initSSB)        <- simLabels
    sims$initSSB               <- sortVectorByName(sims$initSSB)

    names(sims$virgRecr)       <- simLabels
    sims$virgRecr              <- sortVectorByName(sims$virgRecr)
    names(sims$initRecr)       <- simLabels
    sims$initRecr              <- sortVectorByName(sims$initRecr)

    names(sims$M)              <- simLabels
    sims$M                     <- sortVectorByName(sims$M)
    names(sims$qSurv)          <- simLabels
    sims$qSurv                 <- sortVectorByName(sims$qSurv)
    names(sims$F_SPRtgt)       <- simLabels
    sims$F_SPRtgt              <- sortVectorByName(sims$F_SPRtgt)
    names(sims$F_Btgt)         <- simLabels
    sims$F_Btgt                <- sortVectorByName(sims$F_Btgt)

    names(sims$simRuntime)     <- simLabels
    sims$simRuntime            <- sortVectorByName(sims$simRuntime)
    names(sims$simSetuptime)   <- simLabels
    sims$simSetuptime          <- sortVectorByName(sims$simSetuptime)
    names(sims$simCleanuptime) <- simLabels
    sims$simCleanuptime        <- sortVectorByName(sims$simCleanuptime)

    rownames(sims$SSB)         <- simLabels
    sims$SSB                   <- sortDataFrameByRowName(sims$SSB)

    rownames(sims$recruitment) <- simLabels
    sims$recruitment           <- sortDataFrameByRowName(sims$recruitment)

    rownames(sims$depl)        <- simLabels
    sims$depl                  <- sortDataFrameByRowName(sims$depl)
    rownames(sims$catchesReal) <- simLabels
    sims$catchesReal           <- sortDataFrameByRowName(sims$catchesReal)
    rownames(sims$Fvalue)      <- simLabels
    sims$Fvalue                <- sortDataFrameByRowName(sims$Fvalue)
    rownames(sims$SPRratio)    <- simLabels
    sims$SPRratio              <- sortDataFrameByRowName(sims$SPRratio)

    colnames(sims$catches)     <- simDat$catchYears
    rownames(sims$catches)     <- simLabels
    sims$catches               <- sortDataFrameByRowName(sims$catches)

    colnames(sims$finalGradient)  <- simDat$assessYears
    rownames(sims$finalGradient)  <- simLabels
    sims$finalGradient            <- sortDataFrameByRowName(sims$finalGradient)

    colnames(sims$objectiveFunctionVal)  <- simDat$assessYears
    rownames(sims$objectiveFunctionVal)  <- simLabels
    sims$objectiveFunctionVal            <- sortDataFrameByRowName(sims$objectiveFunctionVal)

    colnames(sims$assessVirgRecr) <- simDat$assessYears
    rownames(sims$assessVirgRecr) <- simLabels
    sims$assessVirgRecr           <- sortDataFrameByRowName(sims$assessVirgRecr)

    colnames(sims$assessVirgSSB)  <- simDat$assessYears
    rownames(sims$assessVirgSSB)  <- simLabels
    sims$assessVirgSSB            <- sortDataFrameByRowName(sims$assessVirgSSB)

    colnames(sims$assessInitRecr) <- simDat$assessYears
    rownames(sims$assessInitRecr) <- simLabels
    sims$assessInitRecr           <- sortDataFrameByRowName(sims$assessInitRecr)

    colnames(sims$assessInitSSB)  <- simDat$assessYears
    rownames(sims$assessInitSSB)  <- simLabels
    sims$assessInitSSB            <- sortDataFrameByRowName(sims$assessInitSSB)

    colnames(sims$numParameters)  <- simDat$assessYears
    rownames(sims$numParameters)  <- simLabels
    sims$numParameters            <- sortDataFrameByRowName(sims$numParameters)

    colnames(sims$assessRuntime) <- simDat$assessYears
    rownames(sims$assessRuntime) <- simLabels
    sims$assessRuntime           <- sortDataFrameByRowName(sims$assessRuntime)

    rownames(sims$selexFishery) <- simLabels
    sims$selexFishery           <- sortDataFrameByRowName(sims$selexFishery)
    rownames(sims$selexSurvey)  <- simLabels
    sims$selexSurvey            <- sortDataFrameByRowName(sims$selexSurvey)

    rownames(sims$assessSSBMedianBySim)  <- simLabels
    rownames(sims$assessDeplMedianBySim) <- simLabels
    rownames(sims$assessRecrMedianBySim) <- simLabels

    sims$assessSSBMedianBySim  <- sortDataFrameByRowName(sims$assessSSBMedianBySim)
    sims$assessDeplMedianBySim <- sortDataFrameByRowName(sims$assessDeplMedianBySim)
    sims$assessRecrMedianBySim <- sortDataFrameByRowName(sims$assessRecrMedianBySim)
  }
  return(sims)
}

cleanSimDir <- function(path, showWarnings=F){
  # delete everything from the sim dir given by 'path'
  # keep the simX.rdata files created to store the results
  # remove the assessments directory recursively,
  # but not any other directories or their contents
  files <- file.path(path,dir(path))
  dirs  <- list.dirs(path) # gets full path

  # Get the assessment directory if it exists
  assessDir <- dirs[grep(.ASSESS_DIR_NAME,dirs)]
  # Must use unlink here as file.remove(assessDir) does not work !?
  unlink(assessDir,recursive=T,force=T)
  # Re-list the directory now that the assessment directory is gone.
  files <- file.path(path,dir(path))

  # Keep all the sim RDATA files, pattern matches simXX.rdata where XX is a series of digits of any length.
  #pattern <- paste(.SIM_FILE_PREFIX,"[0-9]+",.SIM_RDATA_FILE_EXT,sep="")
  pattern <- .SIM_RDATA_FILE_EXT
  #pattern <- file.path(path,pattern)

  simFiles     <- files[grep(pattern,files)]
  nonSimFiles  <- files[-grep(pattern,files)]

  if(length(simFiles) == 0){
    nonAssessSimFile <- file.path(path,.RESULTS_FILE)
    if(!file.exists(nonAssessSimFile)){
      # Delete all files because no sim files exist.
      suppressWarnings(
        file.remove(files,showWarnings=showWarnings)
        )
    }else{
      # This must be a non-assessment simulation, so don't delete the results file
      files <- files[!(nonAssessSimFile == files)]
      suppressWarnings(
        file.remove(files,showWarnings=showWarnings)
        )
    }
  }else{
    # Remove all files in the sim directory except the sim rdata files.
    suppressWarnings(
      file.remove(nonSimFiles,showWarnings=showWarnings)
      )
  }
}

cleanAssessDir <- function(path, showWarnings=F){
  # Delete everything from the assessment directory except
  # for the parfile from the previous run if it exists and the
  # model executable.

  files <- file.path(path,dir(path))
  # keep parfile from previous run if it exists
  files <- files[-grep(.PAR_FILE,files)]
  # keep SS executable
  files <- files[-grep(.SS_EXE_FILE,files)]
  # remove all files in the assessment directory except the two filtered above
  # supress warnings because we don't care if a file wasn't found
  suppressWarnings(
    file.remove(files,showWarnings=showWarnings)
  )
}

modifyControlFile <- function(fullPathSim,
                              ctlFile,
                              timeVarySelex = F,
                              assessYr      = NULL) {
  # In simulation directory, adds a year to the main recruit dev section, and bias correction upper ramp
  fullPathControl <- file.path(fullPathSim,ctlFile)
  ctl <- readLines(fullPathControl)

  ind      <- grep(.END_YEAR_RECDEV,ctl)
  x        <- as.numeric(strsplit(ctl[ind],"#")[[1]][1])
  comment  <- paste("#",.END_YEAR_RECDEV)
  ctl[ind] <- paste(x+1,comment)

  ind      <- grep(.LAST_YEAR_BIAS_CORR,ctl)
  x        <- as.numeric(strsplit(ctl[ind],"#")[[1]][1])
  comment  <- paste("#",.LAST_YEAR_BIAS_CORR,sep="\t")
  ctl[ind] <- paste(x+1,comment)

  ind      <- grep(.FIRST_YEAR_NOBIAS,ctl)
  x        <- as.numeric(strsplit(ctl[ind],"#")[[1]][1])
  comment  <- paste("#",.FIRST_YEAR_NOBIAS,sep="\t")
  ctl[ind] <- paste(x+1,comment)

  if(timeVarySelex) {
    #modify control file so that selex devs carry through to all years modeled
    pos <- 11
    ind    <- grep(.TOP_OF_FISHERY_SELEX,ctl)
    indPlus <- 3:7  #the 3rd to 7th row after ind are parameter lines to be changed
    #specifically increment final year of selex devs for ages 2-6
    #assumes that year to be incremented is in 11th position
    x      <- strsplit(ctl[ind+indPlus],"[ \t]")  #split on space or tabs
    x <- unlist(lapply(x,function(x,i=pos,aYr=assessYr) {
                                 x[x!=""]##strip out blanks
                                 x <- x[x!=""]
                                 if(!is.numeric(aYr)) { yr <- as.numeric(x[i])+1 }
                                 if(is.numeric(aYr)) { yr <- aYr }
                                 x[i] <- as.character(yr)
                                 paste(x,collapse=" ")
                               }))
    ctl[ind+indPlus] <- x
  }

  writeLines(ctl,fullPathControl)
}

createParfile <- function(fullPathAssess,timeVarySelex=F,verbose=F){
  # Modify par file from previous year to work for current year
  # Assume the parfile found in the assessment directory is the previous year's parfile
  # Add first fore dev into main recr dev
  # Shift fore devs and add a zero at end
  # Modify the starter file to read the par file

  fullPathAssessPar     <- file.path(fullPathAssess,.PAR_FILE)
  fullPathAssessStarter <- file.path(fullPathAssess,.STARTER_FILE)

  pars                  <- readLines(fullPathAssessPar)
  ind1                  <- grep(.RECDEV_MARKER,pars)
  ind2                  <- grep(.FCAST_RECS_MARKER,pars)

  mainDevs              <- as.numeric(strsplit(pars[ind1+1]," ")[[1]])[-1]
  foreDevs              <- as.numeric(strsplit(pars[ind2+1]," ")[[1]])[-1]
  mainDevs              <- c(mainDevs,foreDevs[1])
  foreDevs              <- c(foreDevs[-1],0)
  pars[ind1+1]          <- paste(mainDevs,collapse=" ")
  pars[ind2+1]          <- paste(foreDevs,collapse=" ")

  if(timeVarySelex) {
    #assumes that there are 5 time-varying parameters for selectivity
    #and that the time varying selex params are at the end of the file
    #could change the argument to a number to indicate how many there are
    for(i in length(pars):(length(pars)-4)) {
      pars[i] <- paste(pars[i],"0")
    }
  }

  writeLines(pars,fullPathAssessPar)

  starter                 <- SS_readstarter(file=fullPathAssessStarter,verbose=verbose)
  starter$init_values_src <- 1  # Tells SS to read in the par file
  SS_writestarter(starter,dir=fullPathAssess,file=.STARTER_FILE,overwrite=T,verbose=verbose,warn=F)
}

simulateAgeComps <- function(simDat,assessYr,fleet){
  # Simulate age comp data from a multinomial for two fleets.
  # It is assumed that fleet=1 is commercial and fleet=2 is survey.

  # simDat$surveyNAge is the assumed sample size for the data
  # simDat$surveyNAgeAdj is the tuning factor that SS3 uses in the Variance adjustment factors
  # - The effective sample size is NAgeAdj*NAge
  # - The numbers at age to sample from are NAge*NAgeAdj
  # - The probabilities at age are scenarioDat$natage*scenarioDat$selexSurvey%*%scenarioDat$AgeingError
  #
  # TODO: Generalize this function to work with N fleets, of which K of them could be surveys.

  natage <- as.matrix(simDat$natage)

  cat("fleet:",fleet,"\n")
  if(sum(natage>0)){
    # Only simulate commercial age comps in years where the catch is greater than ZERO
    #midyr doesn't matter since scaled to sum to 1 (unless take out some catch)
    ageComps <- NULL
    for(sex in 1:length(simDat$sexes)){  #To actually do this by sex, it will have to be carefully thought out
      if(fleet==1){
        selex               <- as.numeric(simDat$selexFishery)
        probs               <- t(t(natage)*selex)
        ageComps            <- rmultinom(1,simDat$commNAge*simDat$commNAgeAdj,probs[sex,])[,1] %*% simDat$ageingError
        simDat$dat$agecomp  <- rbind(simDat$dat$agecomp,c(assessYr,1,fleet,0,0,.NUM_AGE_ERR_DEFS,-1,-1,simDat$commNAge,ageComps))
      }
      if(fleet==2){
        selex               <- as.numeric(simDat$selexSurvey)
        probs               <- t(t(natage)*selex)
        ageComps            <- rmultinom(1,simDat$survNAge*simDat$survNAgeAdj,probs[sex,])[,1] %*% simDat$ageingError
        simDat$dat$agecomp  <- rbind(simDat$dat$agecomp,c(assessYr,1,fleet,0,0,.NUM_AGE_ERR_DEFS,-1,-1,simDat$survNAge,ageComps))
      }
    }

    simDat$dat$N_agecomp      <- nrow(simDat$dat$agecomp)
    if(simDat$dat$N_agecomp == 1){
      # Only one row so far, so set up the column names
      if(fleet==1){
        columnLabels   <- c(.AGE_COMP_LABELS,"commNumAgeSamp")
      }
      if(fleet==2){
        columnLabels   <- c(.AGE_COMP_LABELS,"survNumAgeSamp")
      }
      for(age in 1:length(AgeComp)){
        columnLabels <- c(columnLabels,paste(.AGE_ERROR_COL_PREFIX,age,sep=""))
      }
      colnames(simDat$dat$agecomp) <- columnLabels
    }
  }
  return(simDat)
}

simulateSurveyIndex <- function(simDat,assessYear,verbose=TRUE){
  # Calculate the simulated survey index.
  # The new simulated year of data is appended by row to scenarioDat$CPUE
  # scenarioDat$Ncpue is the number of rows in scenarioDat$CPUE.

  survB <- sum(simDat$batage*simDat$selexSurvey)
  survI <- rlnorm(1,log(survB*simDat$qSurv*exp(-0.5*simDat$M)),simDat$survSETot)   #midyr (but I didn't take out any catch)

  if(verbose){
    cat("Simulation: "              ,simDat$simNum,"\n",sep="")
    cat("yr                      = ",assessYear,"\n",
        "survB                   = ",survB,"\n",
        "qSurv                   = ",simDat$qSurv,"\n",
        "M                       = ",simDat$M,"\n",
        "survB*qSurv*exp(-0.5*M) = ",survB*simDat$qSurv*exp(-0.5*simDat$M),"\n",sep="")
  }
  simDat$dat$CPUE   <- rbind(simDat$dat$CPUE,c(assessYear,1,2,survI,simDat$survSELog))
  simDat$dat$N_cpue <- nrow(simDat$dat$CPUE)
  if(simDat$dat$N_cpue == 1){
    # Only one row so far, so set up the column names.  This is not necessary but makes
    # it easier to read the object lists later.
    colnames(simDat$dat$CPUE) <- c("assessYear","1","2","surveyBiomass","surveySELog")
  }
  return(simDat)
}

getM <- function(scenarioDat,simNum){
  # Get M for this particular MCMC sample (simNum)
  M <- scenarioDat$post$NatM_p_1_Fem_GP_1[scenarioDat$post$Iter==simNum]
  return(M)
}
getQ <- function(scenarioDat,simNum){
  # Get survey Q for this particular MCMC sample (simNum)
  Q <- scenarioDat$post$Q_2[scenarioDat$post$Iter==simNum]
  return(Q)
}
getSelexByFleet <- function(scenarioDat,simNum){
  # Get selectivity for each fleet for this particular MCMC sample (simNum)
  # Returns a list of the fleet selexes.  Modify column names to be the same as the other
  # age-based data
  selexFishery           <- scenarioDat$selex[[1]][scenarioDat$selex[[1]]$mceval==simNum,.SELEX_MIN_AGE_IND:.SELEX_MAX_AGE_IND]  #fishery selex
  selexSurvey            <- scenarioDat$selex[[2]][scenarioDat$selex[[2]]$mceval==simNum,.SELEX_MIN_AGE_IND:.SELEX_MAX_AGE_IND]  #survey selex
  ageLabels              <- paste(.AGE_ERROR_COL_PREFIX,0:scenarioDat$maxA,sep="")
# TODO: fix row names of these to be sim number
  colnames(selexFishery) <- ageLabels
  colnames(selexSurvey)  <- ageLabels
  return(list(selexFishery,selexSurvey))
}


newPSV <- function(simNum,dir) {
  #This function reads in the psv file and only lines up to the current sim
  #It does this to retain the iter number which is often matched to sim number
  #this saves a lot of time when running the mceval
  #it also makes sure that the mcmc burnin in the starter file is set that sim number so that it evaluates only that single line
  #for this function to work, you need the following in the directory (dir)
  #--newPsv executable
  #--ss3.psv file with at least as many lines as simNum
  #--starter.ss that will be used for the mceval later

  currDir <- getwd()
  
  tryCatch({
    setwd(dir)
    shell(paste(.NEW_PSV_EXE,simNum),intern=T)
  },error=function(err){
    cat("newPsv: Error, the system call ",paste(.NEW_PSV,simNum)," failed!\n")
  })
  
  file.copy(.NEW_PSV_FILE,.SS_PSV_FILE,overwrite=T)

  starter <- SS_readstarter(file=.STARTER_FILE,verbose=F)
  starter$MCMCburn <- simNum-1
  SS_writestarter(starter,file=.STARTER_FILE,overwrite=T,verbose=F,warn=F)

  setwd(currDir)
}

addToPSV <- function(simNum,dir) {
  #This is hake specific for incorporating time-varying selectivity
  #Adds one recruitmnet dev (simply zero)
  #Adds 5 selectivity devs for the next year
  #Calls an external function called addYrMCMC which uses a file called addYrMCMC.in
  #Need to modify the addYrMCMC.in file then run addYrMCMC
  #assumes that the last line in the psv file is the one to be worked with
  #Note that you will have to increase the year by 1 in the dat file and add a year to each of the devs in selex in the control file.
  
  origWD <- getwd()
  setwd(dir)

  #simulate selectivity deviates for one additional year
  #first get devs from posteriors file (and the line number that the iteration is on)
  posts <- read.table("posteriors.sso",header=T)
  ind <- grep("Fishery_DEV",names(posts))
  allDevs <- posts[nrow(posts),ind]  #posts should be 1 line (newPsv function: setting of burnin to bypass all but current sim), but just in case it is not 
  paramAge <- unlist(lapply(strsplit(names(allDevs),"_"),function(x){x[3]}))
  devsMat <- as.matrix(as.data.frame(split(unlist(allDevs),paramAge)))
  
  #create addYrMCMC.in
  newRecrIndex <- max(grep("ForeRecr",names(posts))) - 1 #subtract 1 because there are two additional columns at start of posteriors.sso (Iter and Objective)
  newRecr <- 0 #This is just a placeholder because an additional year was added to data.ss and it still has same forecast years
  nSelDevs <- ncol(devsMat)
  newSelDevsIndex <- ind[1]+(1:nSelDevs)*sum(nrow(devsMat))+((1:nSelDevs)-2)
  newDevs <- simulateSelexDevs(devsMat)
  cat(newRecrIndex,newRecr,nSelDevs,newSelDevsIndex,newDevs,file="addYrMCMC.in",sep="\n")

  #run addYrMCMC to add the recruitment and selex devs to the psv
  #it adds to every line, but the last line is the line of interest
  tryCatch({
    shell(.ADD_PSV_EXE,intern=T)
  },error=function(err){
    cat(.ADD_PSV_EXE,"Error, the system call ",.ADD_PSV_EXE," failed!\n")
  })
  
  #Copy newly created files to ss3.psv and admodel.cov
  file.copy(.ADD_PSV_FILE,.SS_PSV_FILE,overwrite=T)
  file.copy(.ADD_ADMODEL_FILE,paste(.ADMODEL,".cov",sep=""),overwrite=T)

  setwd(origWD)  
}

simulateSelexDevs <- function(devs,nForeYrs=1) {
  #devs is a matrix with years down rows and columns as age
  covDevs <- cov(devs)
  newDevs <- mvrnorm(nForeYrs, rep(0,nrow(covDevs)), covDevs)
  return(newDevs)
}


runMCEval <- function(dir){
  # Run SS mceval in the directory 'dir'
  currDir <- getwd()
  tryCatch({
    setwd(dir)
    shell(paste(.SS_EXE_FILE,.MCEVAL),intern=T)
  },error=function(err){
    cat("runMCEval: Error, the system call ",paste(.SS_EXE_FILE,.MCEVAL)," failed!\n")
  })
  setwd(currDir)
}

setupForecastFile <- function(dir,
                              file=file.path(dir,.FORECAST_FILE)
                              ){
  # Setup forecast.ss to have no catch to get beginning of first assessment year
  forecastSS      <- readLines(file)
  ind             <- grep(.FORECAST_CATCH_TEXT,forecastSS)
  tmp             <- forecastSS[ind]
  tmp             <- strsplit(tmp,"#")[[1]]
  tmp[1]          <- 0
  forecastSS[ind] <- paste(tmp,collapse=" #")
  ind             <- grep(.FORECAST_BASIS_TEXT,forecastSS)
  forecastSS      <- forecastSS[1:ind]
  forecastSS      <- c(forecastSS,.END_OF_INPUT)
  writeLines(forecastSS,file)
}

setupStarterFile <- function(dir,
                             file=.STARTER_FILE,
                             ctlFile,
                             verbose=F){
  # Change the names of the data and control files
  fullPathFile     <- file.path(dir,file)
  starter          <- SS_readstarter(file=fullPathFile, verbose=verbose)
  starter$ctlfile  <- ctlFile
  #starter$MCMCthin <- simNum   # Create starter file with the thinning starting at the current sim (to save a little time)
  SS_writestarter(starter,dir=dir,file=file,overwrite=T,verbose=verbose,warn=F)
}

cleanScenarioDir <- function(saveFileList=c(.FORECAST_ASSESS_FILE,
                                            .ASSESS_CONTROL_FILE),
                             dirName
                             ){

  # delete the files from scenario directory given by dirName
  # keep the .ASSESS_CONTROL_FILE, .FORECAST_ASSESS_FILE, and the .SIMULATIONS_DIR_NAME
  # directory.
  allFiles <- dir(dirName)
  filesToRemove <- file.path(dirName,allFiles[!(allFiles%in%saveFileList)])
  unlink(filesToRemove,force=T)
}

copySSInputFiles <- function(srcFileList=c(.DATA_FILE,
                                           .SIM_CONTROL_FILE,
                                           .ASSESS_CONTROL_FILE,
                                           .FORECAST_FILE,
                                           .FORECAST_ASSESS_FILE,
                                           .STARTER_FILE,
                                           .SS_EXE_FILE,
                                           .SS_PSV_FILE,
                                           .POST_NATAGE_FILE,
                                           .WT_AT_AGE_FILE),
                             desFileList=c(.DATA_FILE,
                                           .SIM_CONTROL_FILE,
                                           .ASSESS_CONTROL_FILE,
                                           .FORECAST_FILE,
                                           .FORECAST_ASSESS_FILE,
                                           .STARTER_FILE,
                                           .SS_EXE_FILE,
                                           .SS_PSV_FILE,
                                           .POST_NATAGE_FILE,
                                           .WT_AT_AGE_FILE),
                             sourceDir,
                             destDir,
                             all = F,
                             ss3.dat=F
                             ){

  # Copies the files in the fileList from the sourceDir to the destDir
  # If all=T then all files in the directory will be copied,
  # regardless of what files are passed into srcFileList and desFileList
  if(all){
    allFilesList <- list.files(sourceDir)
    srcFiles <- file.path(sourceDir,allFilesList)
    dstFiles <- file.path(destDir,allFilesList)
  }else{
    srcFiles <- file.path(sourceDir,srcFileList)
    dstFiles <- file.path(destDir,desFileList)
  }
  file.copy(from=srcFiles,to=dstFiles)
  if(ss3.dat) {
    file.create(file.path(destDir,"ss3.dat"))
  }
}

getAgeErrorMatrix <- function(scenarioDat){
  # Load the ageing error matrix from the file .AGE_ERROR_FILE
  # and set it up.  If no age error matrix is to be used, we
  # make it an identity matrix so that calculations later are simpler.

  errorFile <- file.path(.SCENARIOS_DIR_NAME,scenarioDat$scenarioName,.AGE_ERROR_FILE)

  if(!scenarioDat$useAgeingErrorMat){
    # Not using ageing error, so make an identitiy matrix instead.
    ageingError <- matrix(0,
                          ncol=scenarioDat$plusA,nrow=scenarioDat$maxA+1,
                          dimnames=list(paste(.AGE_ERROR_COL_PREFIX,0:scenarioDat$maxA,sep=""),
                            paste(.AGE_ERROR_COL_PREFIX,1:scenarioDat$plusA,sep="")))
    diag(ageingError[scenarioDat$minA:scenarioDat$plusA,]) <- 1
    ageingError[1,1] <- 1   # Age zeros assigned to age 1 (make sure that selectivity is working appropriately
    ageingError[(scenarioDat$plusA+1):(scenarioDat$maxA+1),scenarioDat$plusA] <- 1  # Plus group ages assigned to data plus group
  }else{
    tryCatch({
        ageingError <- t(as.matrix(read.csv(errorFile,row.names=1)))
    },error=function(err){
      stop("getAgeErrorMatrix: Error loading ",errorFile,". Check that file exists and is the correct format.\n",sep="")
    })
  }
  return(ageingError)
}

loadScenarioInfoFile <- function(){
  # Load the information found in the Scenarios csv file,
  # typically found at ../Scenarios/Scenarios.csv
  # Hashes are comments in the file

  tryCatch({
    info <- read.table(.SCENARIOS_INFO,comment.char="#",sep=",",header=T)
    # Sort the info based on Number
    info <- info[order(info$plotOrder),]
  },error=function(err){
    stop("loadScenarioInfoFile: Error loading ",.SCENARIOS_INFO,".  Error message: ",err,"\n",sep="")
  })
  return(info)
}

checkInfoVSDirs <- function(info,
                            scenarioDirs,
                            omDirs){
  # Checks the info table and compares this with the directory names
  # - checks to make sure there is only one perfect information case.

  # If the number of entries in info (scenarios.csv file) is different than the number of directories, return false.
  if(nrow(info) != length(scenarioDirs)){
    cat("checkInfoVSDirs: Warning - The number of uncommented rows in the '",.SCENARIOS_INFO,
        "' file does not correspond with the number\n of directories in '",.SCENARIOS_DIR_NAME,"'\n\n",sep="")
  }

  # If the scenario names in info (scenarios.csv file) and the directory names do not match exactly, return false.
  match <- info$scenarioName %in% scenarioDirs
  if(sum(match) != nrow(info)){
    cat("checkInfoVSDirs: One or more of the scenario names in the '",.SCENARIOS_INFO,
        "' file does not match exactly with the directory\n listing in the '",.SCENARIOS_DIR_NAME,"' directory.\n\n",sep="")
    return(FALSE)
  }

  # If the operating model names in info (scenarios.csv file) and the directory names do not match exactly, return false.
  match <- info$operatingModelName %in% omDirs
  if(sum(match) != nrow(info)){
    cat("checkInfoVSDirs: One or more of the operating model names in the '",.SCENARIOS_INFO,
        "' file does not match exactly with the directory\n listing in the '",.OM_DIR_NAME,"' directory.\n\n",sep="")
    return(FALSE)
  }

  # If there is more than one Perfect information case, return false
  if(sum(info$perfectInfoCase) > 1){
    cat("checkInfoVSDirs: You have more than one perfect information case. Check 'perfectInfoCase' column in ",.SCENARIOS_INFO,
        ".\n There should only be one 'T'.\n\n",sep="")
    return(FALSE)
  }

  # If there is more than one zero catch case, give the user a warning but return true anyway.
  if(sum(info$zeroCatch) > 1){
    cat("checkInfoVSDirs: Warning - you have more than one 'no fishing' case. Check 'zeroCatch' column in ",..SCENARIOS_INFO,".\n",sep="")
  }
  return(TRUE)
}




randWalkSelex.fn <- function(pars) {
    #calculates the selectivity from the random walk parameters in SS (option 17)
    #-1000 means to set equal to 0
    #assumes that this is all pars from age 0 to max age
    
    logS <- rep(NA,length(pars))
    logS[1] <- 0 #first value is never estimated (age 0)
    for(a in 2:length(pars)) {
        ifelse(pars[a] == -1000, logS[a] <- 0, logS[a] <- logS[a-1]+pars[a])
    }
    selex <- exp(logS-max(logS))
    selex[pars== -1000] <- 0
    return(selex)
}

#randWalkSelex.fn(c(-1000,-1000,0,0.295698,0.170968,-0.441426,0.564627,0,0,0,0,0,0,0))
#randWalkSelex.fn(c(-1000,-1000,0,-0.0605365,0.141217,0.474321,-0.00714344,0,0,0,0,0,0,0))



cat("mseSS is ready to use.  To start, read the Readme.md file.\n",
    "  Check the '",.SCENARIOS_INFO,"' file first, then call runMSE() to run Scenarios.\n",
    "  Once the scenarios are completed, run mergeMSE() to merge the outputs.\n",
    "  To plot, source plotALL.r once the merge step has been completed.\n\n",
    sep="")

if(exists("scen")) {  #do this only if sourcing the file from a cmd prompt with scen set
    runMSE(continue=T,
           useSystem=F,
           verbose=F,
           sendEmailUpdates=F,
           scen=scen,
           emailAddress="yourname@gmail.com",
           emailPassword="",
           updateR4ss = FALSE)
    #mergeMSE()
}

#This is how Allan does his testing
if(F) {
  direc <- "C:\\NOAA2014\\Hake\\MSE"

  setwd(paste(direc,"testMSESS\\Rscripts",sep="/"))
  if(exists("scen")) rm(scen)  
  source("mseSS.r")  #if scen exists it will run the scenario
  #printDebug <- T

  out <- runMSE(continue=T, 
                useSystem=F, 
                verbose=F, 
                printstats=F, 
                sendEmailUpdates=F, 
                scen=NULL,
                emailAddress="yourname@gmail.com", 
                emailPassword="",
                r4ssDir=paste(direc,"r4ss",sep="/"))
}
