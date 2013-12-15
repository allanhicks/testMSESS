# mseSSFiguresConvergence.r
#
# All convergence and gradient plots mseSS
#
# Authors: Chris Grandin

source("mseSSGlobals.r")

require("PBSmodelling") # for plotBubbles() function and evalCall()

plotFinalGradients <- function(scenario,
                               figPath  = .FIGS_DIR,
                               ylim     = c(1, nrow(scenario$finalGradient)),
                               xlim     = c(1, ncol(scenario$finalGradient)),
                               colorTol = 0.5,   # Color tolerance - if values are less than this then circles black else red
                               simLab   = 2,     # Show every nth label for sims
                               addLines = TRUE,  # Add horizontal lines to aid the eye in a sinlge simulation's progress
                               hide0    = TRUE,  # Hide zero-value bubbles
                               res      = .RESOLUTION,
                               width    = .WIDTH,
                               height   = .HEIGHT,
                               units    = .UNITS,
                               png      = .PNG,
                               verbose  = .VERBOSE){

  if(scenario$runAssessments){
    filename <- file.path(figPath,paste(Sys.Date(),"_Final_Gradients_",scenario$scenarioName,".png",sep=""))
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    par(mar=c(5,4,3,1))
    par(mar=c(5,4,3,1))
    plotBubbles(z=scenario$finalGradient,
                xlab="Assessment year",
                ylab="",
                xlim=xlim,
                ylim=ylim,
                colorTol=colorTol,
                hide0=hide0,
                axes=FALSE)
    for(line in 1:nrow(scenario$finalGradient)){
      # add horizontal lines to aid the eye
      lines(1:nrow(scenario$finalGradient), rep(line, nrow(scenario$finalGradient)), col=sample(rainbow(line)), lwd=0.5, lty=2)
    }
    # Nice x-axis labels
    axis(1,
         at=1:ncol(scenario$finalGradient),
         labels=dimnames(scenario$finalGradient)[[2]],
         las=2)
    # Nice y-axis labels
    labels <- makeLabels(row.names(scenario$finalGradient), simLab)
    axis(2,
         at=1:nrow(scenario$finalGradient),
         labels=labels,
         las=2, cex=0.5)

    mainTitle <- paste(scenario$prettyName, " final gradients from assessment runs",sep="")
    subTitle  <- paste("Red circles are >= ",colorTol,sep="") 
    title(mainTitle, sub=subTitle)

    if(png){
      dev.off()
    }
  }
}

plotObjectiveFunctionVal <- function(scenario,
                                     figPath  = .FIGS_DIR,
                                     ylim     = c(1, nrow(scenario$objectiveFunctionVal)),
                                     xlim     = c(1, ncol(scenario$objectiveFunctionVal)),
                                     simLab   = 2,     # Show every nth label for sims
                                     addLines = TRUE,  # Add horizontal lines to aid the eye in a sinlge simulation's progress
                                     hide0    = TRUE,  # Hide zero-value bubbles
                                     powr     = 3,     # power function to increase difference in size between bubbles
                                     res      = .RESOLUTION,
                                     width    = .WIDTH,
                                     height   = .HEIGHT,
                                     units    = .UNITS,
                                     png      = .PNG,
                                     verbose  = .VERBOSE){

  if(scenario$runAssessments){
    filename <- file.path(figPath,paste(Sys.Date(),"_Objective_Function_",scenario$scenarioName,".png",sep=""))
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    par(mar=c(5,4,3,1))
    par(mar=c(5,4,3,1))
    plotBubbles(z=scenario$objectiveFunctionVal,
                xlab="Assessment year",
                ylab="",
                xlim=xlim,
                ylim=ylim,
                hide0=hide0,
                powr=powr,
                axes=FALSE)
    for(line in 1:nrow(scenario$objectiveFunctionVal)){
      # add horizontal lines to aid the eye
      lines(1:nrow(scenario$objectiveFunctionVal), rep(line, nrow(scenario$objectiveFunctionVal)), col=sample(rainbow(line)), lwd=0.5, lty=2)
    }

    # Nice x-axis labels
    axis(1,
         at=1:ncol(scenario$objectiveFunctionVal),
         labels=dimnames(scenario$objectiveFunctionVal)[[2]],
         las=2)
    # Nice y-axis labels
    labels <- makeLabels(row.names(scenario$objectiveFunctionVal), simLab)
    axis(2,
         at=1:nrow(scenario$objectiveFunctionVal),
         labels=labels,
         las=2, cex=0.5)

    mainTitle <- paste(scenario$prettyName, " objective function values",sep="")
    title(mainTitle)

    if(png){
      dev.off()
    }
  }
}

# plotBubbles was taken from the package PBSmodelling and modified here so that it would not draw axes,
#  and to draw circles larger than some tolerance in red.
# New arguments:
# axes     - TRUE or FALSE for whether or not to draw the axes 
# colorTol - Tolerance for which to switch from black to red. Less values will be drawn in black,
#            Greater or equal values will be drawn in red.
plotBubbles <- function (z, xval = FALSE, yval = FALSE, dnam = FALSE, rpro = FALSE, 
    cpro = FALSE, rres = FALSE, cres = FALSE, powr = 0.5, size = 0.2, 
    lwd = 1, clrs = c("black", "red", "blue"), hide0 = FALSE, 
    frange = 0.1, prettyaxis = FALSE, axes = TRUE, colorTol = 0.01,  ...) 
{
    if (is.data.frame(z)) {
        use = !sapply(z, is.factor) & sapply(z, is.numeric)
        z = z[, use, drop = FALSE]
        if (ncol(z) == 0) {
            showAlert("data frame not useable")
            return()
        }
        z = as.matrix(z)
    }
    dz <- dim(z)
    ny = ny1 = dz[1]
    nx = nx1 = dz[2]
    if (length(dz) > 2) {
        showAlert("Input matrix must have only 2 dimensions")
        return()
    }
    xval1 <- 1:nx
    yval1 <- 1:ny
    if (mode(xval) == "logical") {
        if (xval[1]) {
            xval1 <- z[1, ]
            ny1 <- ny - 1
        }
    }
    if (mode(yval) == "logical") {
        if (yval[1]) {
            yval1 <- z[, 1]
            nx1 <- nx - 1
        }
    }
    xind <- (nx - nx1 + 1):nx
    x2 = xlabel = xval1[xind]
    yind <- (ny - ny1 + 1):ny
    y2 = ylabel = yval1[yind]
    if ((mode(xval) != "logical") & (length(xval) == nx1)) {
        if (mode(xval) == "numeric") 
            x2 = xval
        xlabel = xval
    }
    if ((mode(yval) != "logical") & (length(yval) == ny1)) {
        if (mode(yval) == "numeric") 
            y2 = yval
        ylabel = yval
    }
    zz <- array(z[yind, xind], dim = c(length(yind), length(xind)), 
        dimnames = dimnames(z[yind, xind, drop = FALSE]))
    dots = list(...)
    xlab = dots$xlab
    if (is.null(xlab)) 
        xlab = ""
    ylab = dots$ylab
    if (is.null(ylab)) 
        ylab = ""
    if (dnam & !is.null(dimnames(zz))) {
        warn = options()$warn
        options(warn = -1)
        if (!is.null(dimnames(zz)[[2]])) {
            xpos = try(as.numeric(dimnames(zz)[[2]]), silent = TRUE)
            if (all(is.na(xpos))) 
                xlabel = dimnames(zz)[[2]]
            else if (!any(is.na(xpos)) && all(diff(xpos) > 0 | 
                all(diff(xpos) < 0))) {
                xlabel = as.character(xpos)
                x2 = xpos
            }
        }
        if (!is.null(dimnames(zz)[[1]])) {
            ypos = try(as.numeric(dimnames(zz)[[1]]), silent = TRUE)
            if (all(is.na(ypos))) 
                ylabel = dimnames(zz)[[2]]
            else if (!any(is.na(ypos)) && all(diff(ypos) > 0 | 
                all(diff(ypos) < 0))) {
                ylabel = as.character(ypos)
                y2 = ypos
            }
        }
        options(warn = warn)
    }
    xx <- rep(x2, each = length(y2))
    yy <- rep(y2, length(x2))
    minz <- min(zz, na.rm = TRUE)
    maxz <- max(zz, na.rm = TRUE)
    if (rpro | cpro) {
        if (minz < 0) {
            zz <- zz - minz
            minz <- 0
            maxz <- max(zz, na.rm = TRUE)
        }
    }
    if (rpro) {
        zs <- apply(zz, 1, sum, na.rm = TRUE)
        zz <- sweep(zz, 1, zs, "/")
    }
    if (cpro) {
        zs <- apply(zz, 2, sum, na.rm = TRUE)
        zz <- sweep(zz, 2, zs, "/")
    }
    if (rres) {
        zm <- apply(zz, 1, mean, na.rm = TRUE)
        zz <- sweep(zz, 1, zm, "-")
    }
    if (cres) {
        zm <- apply(zz, 2, mean, na.rm = TRUE)
        zz <- sweep(zz, 2, zm, "-")
    }
    zNA <- is.na(zz) | is.nan(zz) | is.infinite(zz)
    zz[zNA] <- 0
    z0 <- sign(zz) * abs(zz)^abs(powr)
    z1 <- z3 <- z0
    z1[z0 <= 0] <- NA
    z3[z0 < 0 | z0 > 0] <- NA
    z2 <- -z0
    z2[z0 >= 0] <- NA
    za <- max(z0, na.rm = TRUE)
    zb <- min(z0, na.rm = TRUE)
    zM <- max(abs(z0))
    sz1 <- max(za * size/zM, 0.001)
    sz2 <- max(-zb * size/zM, 0.001)
    evalCall(plot, argu = list(x = 0, y = 0, xlim = extendrange(x2, 
        f = frange), ylim = extendrange(y2, f = frange), type = "n", 
        axes = FALSE, xlab = xlab, ylab = ylab), ..., checkdef = TRUE, 
        checkpar = TRUE)
    if (prettyaxis) {
        if (length(min(x2):max(x2)) <= 5) 
            xshow = is.element(x2, x2)
        else xshow = is.element(x2, pretty(x2, n = 10))
        yshow = is.element(y2, pretty(y2, n = 10))
    }
    else {
        xshow = rep(TRUE, length(x2))
        yshow = rep(TRUE, length(y2))
    }
    if (!all(xshow) && axes) 
        axis(1, at = x2[!xshow], labels = FALSE, tcl = ifelse(is.null(dots$tcl), 
            par()$tcl, dots$tcl)/3)
    if (!all(yshow) & axes) 
        axis(2, at = y2[!yshow], labels = FALSE, tcl = ifelse(is.null(dots$tcl), 
            par()$tcl, dots$tcl)/3)
    if(axes){
      evalCall(axis, argu = list(side = 1, at = x2[xshow], labels = xlabel[xshow]), 
               ..., checkpar = TRUE)
      evalCall(axis, argu = list(side = 2, at = y2[yshow], labels = ylabel[yshow]), 
               ..., checkpar = TRUE)
    }
    if (!hide0 && !all(is.na(z3))) {
        evalCall(symbols, argu = list(x = xx, y = yy, circles = as.vector(z3), 
            inches = 0.001, fg = clrs[3], lwd = lwd, add = TRUE), 
            ..., checkpar = TRUE)
    }
    if (!all(is.na(z2))) {
        evalCall(symbols, argu = list(x = xx, y = yy, circles = as.vector(z2), 
            inches = sz2, fg = clrs[2], lwd = lwd, add = TRUE), 
            ..., checkpar = TRUE)
    }
    if (!all(is.na(z1))) {

      # Make a color matrix for values exceeding the colorTol value
      testZ1  <- z1 >= colorTol  # creates a TRUE/FALSE matrix
      colors <- matrix(nrow=nrow(z1),ncol=ncol(z1))
      colors[testZ1]  <- "red"
      colors[!testZ1] <- "black"
      argList <- list(x = xx, y = yy, circles = as.vector(z1), inches = sz1, fg = as.vector(colors), lwd = lwd, add = TRUE)
      evalCall(symbols, argu = argList, ..., checkpar = TRUE)
    }
    box()
    invisible(z0)
}
