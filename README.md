# R_InteractiveGraphicsDevice
This R script was developed by Marc Laurencelle, alias marQIsoft. It is largely based on the R example codes provided for the getGraphicsEvent function, in the R Documentation (Help).

The key advantage of this module compared to other existing solutions such as Plotly or 'zoom' package, is that it fully redraws the plot after each change in the view range. It can optionally call plotFUN(..., xlim = ylim, ylim = ylim, xaxs = xaxs, yaxs = yaxs), a custom function that let the user keep full control on what is to be drawn or not, while also allowing cool things such as prior optimization as to what is likely to be visible and thus needs to be drawn. This way, even large datasets can be explored interactively with relatively fast response to user actions.

To use this module, simply source the R script "Interactive Graphics Device v1e1.R" in the current R session. You may then use the main function of the module, "dragplot", by writing R codes similar to those provided in the examples below. And do not forget to include the lines of active R code around the one calling dragplot(...), as they also are equally important. In particular, "getGraphicsEvent()" is the line that starts the interaction loop inside which R awaits for user interaction events.

## Example test code no 1: Broken lines

    savepar <- par(ask = FALSE)
    dragplot(rnorm(100), sort(rnorm(100)), type="l")
    # This currently only works on the Windows
    # and X11(type = "Xlib") screen devices...
    getGraphicsEvent()
    par(savepar)

## Example test code no 2: Histogram

    v = rnorm(1000)
    H = hist(v,breaks=16,plot=F)
    savepar <- par(ask = FALSE)
    dragplot(H,xlim=range(H$breaks),safe.axes.pars=T)
    getGraphicsEvent()
    par(savepar)

## Example test code no 3: Histogram same as no 2 but "silent" in R console
_(Note "talks = FALSE" added to dragplot arguments)_

    v = rnorm(1000)
    H = hist(v,breaks=16,plot=F)
    savepar <- par(ask = FALSE)
    dragplot(H,xlim=range(H$breaks),safe.axes.pars=T, talks=F)
    getGraphicsEvent()
    par(savepar)
    
## Example test code no 4: Markov random walk + Custom plotting function (plotFUN) + Dynamic selection of visible data only when plotting + Optional interaction
    
    #!Global parameters
    optInteractivePlotMode <- T #to let the user decide if he wants interaction or not
    nsteps <- 23321L #number of random-walk steps (~iterations)

    #Hint: If running an interaction loop where a lot of data needs to be drawn,
    # prefer the use of keyboard shortcuts rather than mouse click & drag, as
    # the latter will likely be hard to control. Once you have zoomed in over a
    # smaller portion of the data (using the keyboard), you will be able to use
    # the mouse much more easily. That is why dynamic selection of visible data
    # is an important strategy that you should include in your R script aiming
    # at interactive plotting of large datasets or sophisticated drawings.

    #!Data generation
    XV <- 0L:nsteps
    YposV <- cumsum(c(0.0, sign(0.5 - runif(n=nsteps)))) #simulated Y positions (V means: of vector class)
    xyDF <- data.frame(x=XV, y=YposV) #DF means: of data.frame class

    #!Custom plotting function
    funplot.markrandwalk <- function(...) {
      #Drawing an empty plot and its labels (axes and title)
      plot(c(0,0), type="n", xlab="Step", ylab="Position", main=sprintf("Simulated elementary Markov random walk (n=%d)", nsteps), ...) #empty plot with labels

      #Determining which data are within the visible area (or "view")
      getxlim = list(...)$xlim #getting the currently imposed X limits of the view
      filtX = if(optInteractivePlotMode) filterXorYdata(xyDF$x, getxlim, extend=T) else T #boolean vector to use to select (or "filter") visible-only data
      if(optInteractivePlotMode) print(sprintf("dynamic selection of visible data only: %d out of %d data points", length(filtX[filtX]), nrow(xyDF)))

      #Drawing data determined as falling in visible area (i.e. in current view)
      lines(xyDF[filtX, ])
      text(head(xyDF, 1L), "D", pos=2L, cex=2)
      text(tail(xyDF, 1L), "A", pos=4L, cex=2)
      #Drawing a legend at a XY-independent position in the plot area
      legend(x="top", inset=0.015, lty=1L, legend="random walk line", bg="white")
    }

    #!Plotting in either interactive or static mode
    if(optInteractivePlotMode) {
      #!.Interactive mode
      writeLines("INTERACTIVE PLOTTING starting...")
      flush.console()
      savepar <- par(ask = FALSE)
      dragplot(mintimelag=0.1, xlim=extendrange(xyDF$x, f=0.05), ylim=range(xyDF$y), plotFUN=funplot.markrandwalk, safe.axes.pars=T, talks=F)
      getGraphicsEvent()
      par(savepar)
      writeLines("INTERACTIVE PLOTTING ended.")
    } else {
      #!.Static mode
      writeLines("PLOTTING once, in STATIC mode.")
      funplot.markrandwalk(xlim=extendrange(xyDF$x, f=0.075), ylim=range(xyDF$y))
      #Note: xlim differs here, due to the different x-axis style (xaxs) setting.
    }


**Please read the accompanying text file to learn the keyboard shortcuts and mouse actions you can use to interact with graphical devices plotted using this module.**
- Interactive Graphics Device HELP and How-To Shortcuts.txt

_marQIsoft, July 2019_
