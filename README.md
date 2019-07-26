# R_InteractiveGraphicsDevice
This R script was developed by Marc Laurencelle, alias marQIsoft. It is largely based on the R example codes provided for the getGraphicsEvent function, in the R Documentation (Help).

The key advantage of this module compared to other existing solutions such as Plotly or 'zoom' package, is that it fully redraws the plot after each change in the view range. It can optionally call plotFUN(..., xlim = ylim, ylim = ylim, xaxs = xaxs, yaxs = yaxs), a custom function that let the user keep full control on what is to be drawn or not, while also allowing cool things such as prior optimization as to what is likely to be visible and thus needs to be drawn. This way, even large datasets can be explored interactively with relatively fast response to user actions.

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

**Please read the accompanying text file to learn the keyboard shortcuts and mouse actions you can use to interact with graphical devices plotted using this module.**
- Interactive Graphics Device HELP and How-To Shortcuts.txt

_marQIsoft, July 2019_
