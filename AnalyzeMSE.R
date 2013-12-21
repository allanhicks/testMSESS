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
deplYears <- as.numeric(names(simDat$depl))
depl <- matrix(NA,nrow=length(files),ncol=length(simDat$depl),dimnames=list(as.character(simNums),as.character(deplYears)))
depl[1,] <- unlist(simDat$depl)
recrYears <- as.numeric(names(simDat$recr))
recr <- matrix(NA,nrow=length(files),ncol=length(simDat$recruitment),dimnames=list(as.character(simNums),as.character(recrYears)))
recr[1,] <- unlist(simDat$recr)
SsbYears <- as.numeric(names(simDat$SSB))
SSB <- matrix(NA,nrow=length(files),ncol=length(simDat$SSB),dimnames=list(as.character(simNums),as.character(SsbYears)))
SSB[1,] <- unlist(simDat$SSB)
SB0 <- simDat$virgSSB
M <- simDat$M
qSurv <- simDat$qSurv
assessRunTime <- matrix(NA,nrow=length(files),ncol=length(simDat$assessRuntime),dimnames=list(as.character(simNums),names(simDat$assessRuntime)))
assessRunTime[1,] <- unlist(simDat$assessRuntime)
selexSurv <- matrix(NA,nrow=length(files),ncol=length(simDat$selexSurv),dimnames=list(as.character(simNums),names(simDat$selexSurv)))
selexSurv[1,] <- unlist(simDat$selexSurv)
selexFish <- matrix(NA,nrow=length(files),ncol=length(simDat$selexFish),dimnames=list(as.character(simNums),names(simDat$selexFish)))
selexFish[1,] <- unlist(simDat$selexFish)
natage <- batage <- list()
natage[[1]] <- simDat$omNatage
#batage[[1]] <- simDat$assessBatage
for(i in 2:length(files)) {
    print(i)
    load(files[i])
    catch[i,] <- simDat$catch
    depl[i,] <- unlist(simDat$depl)
    recr[i,] <- unlist(simDat$recr)
    SSB[i,] <- unlist(simDat$SSB)
    SB0 <- c(SB0,simDat$virgSSB)
    M <- c(M,simDat$M)
    qSurv <- c(qSurv,simDat$qSurv)
    assessRunTime[i,] <- unlist(simDat$assessRuntime)
    selexSurv[i,] <- unlist(simDat$selexSurv)
    selexFish[i,] <- unlist(simDat$selexFish)
    natage[[i]] <- simDat$assessNatage
    #batage[[i]] <- simDat$assessBatage
}
names(M) <- names(SB0) <- names(qSurv) <- names(natage) <- simNums  #names(batage) <- simNums

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

plot(2013:2042,assessRunTime[1,]/60,type="l",ylab="Minutes",ylim=c(0,5))
for(i in 2:nrow(assessRunTime)) {
    lines(2013:2042,assessRunTime[i,]/60,col=i)
}


simDat$omWtatage

#Check out specific number
theNum <- "2"
catch[theNum,]
depl[theNum,]
SSB[theNum,]
SB0[theNum]
qSurv[theNum]
selexSurv[theNum,]
natage[[theNum]]

theYr <- "2018"
batage <- natage[[theNum]][theYr,] * simDat$omWtatage[theYr,]

wtatage <- c(0.03,0.0885,0.2562,0.3799,0.4913,0.5434,0.5906,0.662,0.7215,0.791,0.8629,0.9315,0.9681,1.0751,1.0016,1.0202,1.0202,1.0202,1.0202,1.0202,1.0202)
survEst <- NULL
for(yr in as.character(2013:2017)) {
    batage <- natage[[theNum]][yr,] * simDat$omWtatage[yr,]
    survEst <- c(survEst,sum(batage*selexSurv[theNum,])*qSurv[theNum]*exp(-0.5*M[theNum]))
}
names(survEst) <- as.character(2013:2017)

x <- SS_readdat("C:/NOAA2014/Hake/MSE/testMSESS/Scenarios/test/simulations/sim2/2017/data.ss")
x <- x$CPUE[x$CPUE$year%in%(2013:2042),]
plot(x$year,x$obs)
points(2013:2042,survEst,pch=16,col="red")


barplot(recr[theNum,])


years <- 2013:2017
load(paste("SSfilesAssess",theNum,".rdata",sep=""))
theDir <- paste("sim",theNum,sep="")
dir.create(theDir)
for(yr in as.character(years)) {
    newDir <- paste(theDir,yr,sep="/")
    dir.create(newDir)

    writeLines(SSfilesAssess[[yr]]$sta,paste(newDir,"starter.ss",sep="/"))
    writeLines(SSfilesAssess[[yr]]$dat,paste(newDir,"data.ss",sep="/"))
    writeLines(SSfilesAssess[[yr]]$ctl,paste(newDir,"assessment_control.ss",sep="/"))
    writeLines(SSfilesAssess[[yr]]$frc,paste(newDir,"forecast.ss",sep="/"))
    writeLines(SSfilesAssess[[yr]]$wt,paste(newDir,"wtatage.ss",sep="/"))
    writeLines(SSfilesAssess[[yr]]$rep,paste(newDir,"Report.sso",sep="/"))
    writeLines(SSfilesAssess[[yr]]$par,paste(newDir,"ss3.par",sep="/"))
}
