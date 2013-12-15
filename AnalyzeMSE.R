setwd("C:\\NOAA2014\\Hake\\MSE\\testMSESS\\Scenarios\\annual\\simulations")
setwd("C:\\NOAA2014\\Hake\\MSE\\testMSESS\\Scenarios\\annualTVom\\simulations")
setwd("C:\\NOAA2014\\Hake\\MSE\\testMSESS\\Scenarios\\annualTVboth\\simulations")
setwd("C:\\NOAA2014\\Hake\\MSE\\testMSESS\\Scenarios\\test\\simulations")

files <- dir()
files <- files[grep("sim[0-9]*.rdata",files)]
simNums <- as.numeric(gsub("[A-Za-z.]","",files))
files <- files[order(simNums)]
simNums <- sort(simNums)

load(files[1])
catchYears <- simDat$catchYears
catch <- matrix(NA,nrow=length(files),ncol=length(simDat$catch),dimnames=list(as.character(simNums),as.character(catchYears)))
catch[1,] <- simDat$catch
deplYears <- as.numeric(substring(names(simDat$depl),5))
depl <- matrix(NA,nrow=length(files),ncol=length(simDat$depl),dimnames=list(as.character(simNums),as.character(deplYears)))
depl[1,] <- unlist(simDat$depl)
recrYears <- as.numeric(substring(names(simDat$recr),6))
recr <- matrix(NA,nrow=length(files),ncol=length(simDat$recruitment),dimnames=list(as.character(simNums),as.character(recrYears)))
recr[1,] <- unlist(simDat$recr)
SsbYears <- as.numeric(substring(names(simDat$SSB),5))
SSB <- matrix(NA,nrow=length(files),ncol=length(simDat$SSB),dimnames=list(as.character(simNums),as.character(SsbYears)))
SSB[1,] <- unlist(simDat$SSB)
SB0 <- simDat$virgSSB
M <- simDat$M
qSurv <- simDat$qSurv
selexSurv <- matrix(NA,nrow=length(files),ncol=length(simDat$selexSurv),dimnames=list(as.character(simNums),names(simDat$selexSurv)))
selexSurv[1,] <- unlist(simDat$selexSurv)
selexFish <- matrix(NA,nrow=length(files),ncol=length(simDat$selexFish),dimnames=list(as.character(simNums),names(simDat$selexFish)))
selexFish[1,] <- unlist(simDat$selexFish)
natage <- batage <- list()
natage[[1]] <- simDat$assessNatage
batage[[1]] <- simDat$assessBatage
for(i in 2:length(files)) {
    load(files[i])
    catch[i,] <- simDat$catch
    depl[i,] <- unlist(simDat$depl)
    recr[i,] <- unlist(simDat$recr)
    SSB[i,] <- unlist(simDat$SSB)
    SB0 <- c(SB0,simDat$virgSSB)
    M <- c(M,simDat$M)
    qSurv <- c(qSurv,simDat$qSurv)
    selexSurv[i,] <- unlist(simDat$selexSurv)
    selexFish[i,] <- unlist(simDat$selexFish)
    natage[[i]] <- simDat$assessNatage
    batage[[i]] <- simDat$assessBatage
}
names(M) <- names(SB0) <- names(qSurv) <- names(natage) <- names(batage) <- simNums

windows(height=5,width=10)
plot(deplYears,apply(depl,2,median),type="l",lwd=3,ylim=c(0,2),yaxs="i")
lines(deplYears,apply(depl,2,quantile,prob=0.1),lty=2)
lines(deplYears,apply(depl,2,quantile,prob=0.9),lty=2)
abline(h=0.4,col="darkgreen")
#title(main="Time-varying OM, time-invariant assessment")

windows(height=5,width=10)
plot(catchYears,apply(catch,2,median),type="l",lwd=3,ylim=c(0,1e6),yaxs="i")
lines(catchYears,apply(catch,2,quantile,prob=0.1),lty=2)
lines(catchYears,apply(catch,2,quantile,prob=0.9),lty=2)
mean(apply(catch,2,median)[-(1:30)])
#title(main="Time-varying OM, time-invariant assessment")




#Check out specific number
theNum <- "2366"
catch[theNum,]
depl[theNum,]
SSB[theNum,]
SB0[theNum]
qSurv[theNum]
selexSurv[theNum,]
natage[[theNum]]
batage[[theNum]]

theYr <- "2050"
batage[[theNum]][theYr,]*selexSurv[theNum,]*qSurv[theNum]*exp(-0.5*M[theNum])
barplot(recr[theNum,])


years <- 2013:2052
load(paste("ssFiles",theNum,".rdata",sep=""))
theDir <- paste("sim",theNum,sep="")
dir.create(theDir)
for(yr in as.character(years)) {
    newDir <- paste(theDir,yr,sep="/")
    dir.create(newDir)

    writeLines(ssFiles[[yr]]$sta,paste(newDir,"starter.ss",sep="/"))
    writeLines(ssFiles[[yr]]$dat,paste(newDir,"data.ss",sep="/"))
    writeLines(ssFiles[[yr]]$ctl,paste(newDir,"assessment_control.ss",sep="/"))
    writeLines(ssFiles[[yr]]$frc,paste(newDir,"forecast.ss",sep="/"))
    writeLines(ssFiles[[yr]]$wt,paste(newDir,"wtatage.ss",sep="/"))
    writeLines(ssFiles[[yr]]$rep,paste(newDir,"Report.sso",sep="/"))
}
