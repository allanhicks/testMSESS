#Run simulations use msess in R directly

#direc <- "C:\\Users\\Hicks\\Dropbox\\Hake2014\\MSE"
direc <- "C:\\NOAA2014\\Hake\\MSE"

setwd(paste(direc,"testMSESS\\Rscripts",sep="/"))
if(exists("scen")) rm(scen)  
source("mseSS.r")  #if scen exists it will run the scenario
printDebug <- F

out <- runMSE(continue=T, useSystem=F, verbose=F, printstats=F, sendEmailUpdates=F, scen=1,
                emailAddress="yourname@gmail.com", emailPassword="",
                r4ssDir=paste(direc,"r4ss",sep="/"))

direc <- "C:\\NOAA2014\\Hake\\MSE"
setwd(paste(direc,"testMSESS\\Rscripts",sep="/"))
if(exists("scen")) rm(scen)  
source("mseSS.r")  #if scen exists it will run the scenario
out <- runMSE(continue=T, useSystem=F, verbose=F, printstats=F, sendEmailUpdates=F, scen=2,
                emailAddress="yourname@gmail.com", emailPassword="",
                r4ssDir=paste(direc,"r4ss",sep="/"))

direc <- "C:\\NOAA2014\\Hake\\MSE"
setwd(paste(direc,"testMSESS\\Rscripts",sep="/"))
if(exists("scen")) rm(scen)  
source("mseSS.r")  #if scen exists it will run the scenario
out <- runMSE(continue=T, useSystem=F, verbose=F, printstats=F, sendEmailUpdates=F, scen=3,
                emailAddress="yourname@gmail.com", emailPassword="",
                r4ssDir=paste(direc,"r4ss",sep="/"))

direc <- "C:\\NOAA2014\\Hake\\MSE"
setwd(paste(direc,"testMSESS\\Rscripts",sep="/"))
if(exists("scen")) rm(scen)  
source("mseSS.r")  #if scen exists it will run the scenario
out <- runMSE(continue=T, useSystem=F, verbose=F, printstats=F, sendEmailUpdates=F, scen=4,
                emailAddress="yourname@gmail.com", emailPassword="",
                r4ssDir=paste(direc,"r4ss",sep="/"))





#mergeMSE()








##### To recreate assessment files
setwd("C:/NOAA2014/Hake/MSE/testMSESS/Scenarios/annual/simulations")
load("ssFiles2.rdat")
dir.create("sim2")
writeLines(ssFiles$"2013"$sta,"sim2/starter.ss")
writeLines(ssFiles$"2013"$dat,"sim2/data.ss")
writeLines(ssFiles$"2013"$ctl,"sim2/assessment_control.ss")
writeLines(ssFiles$"2013"$frc,"sim2/forecast.ss")
writeLines(ssFiles$"2013"$wt,"sim2/wtatage.ss")
writeLines(ssFiles$"2013"$rep,"sim2/Report.sso")


#Look at MCMC
x <- read.table("C:/NOAA2014/Hake/MSE/2013hake_33_SS3.24s_mcmc_mse/derived_posteriors.sso",header=T)
y <- read.table("C:/NOAA2014/Hake/MSE/2013hake_33_SS3.24s_mcmc_mse/posteriors.sso",header=T)
ind <- seq(3,2999,15)
plot(x$SPB_Virgin,type="l")
acf(x$SPB_Virgin)
acf(x$SPB_Virgin[ind])
plot(y$NatM_p_1_Fem_GP_1,type="l")
acf(y$NatM_p_1_Fem_GP_1[ind])
