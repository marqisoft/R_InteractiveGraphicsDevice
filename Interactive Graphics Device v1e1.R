# Copyright (c) 2019 Marc Laurencelle
# Licensed under the terms of the MIT license
# (see '__LICENSE__.TXT' for more details)

#TO DO someday:
#a. Distinguish mouse moves from left vs. right button: left button
#   will be for H|V translation, right button from axial zoom (x|y|x&y).
#b. Do not authorize certain keyboard shortcuts while mouse-moving?
#c. Understand where the center is positioned/located (e.g., with abline) when
#   several actions are combined (e.g., drag & keypress).
#d. Clean up the code and make it more flexible; think notably to inject
#   a function other than plot, which would still use xlim and ylim, but
#   not necessarily xaxs...

#!Watch out: There is a package which uses the same approach: 'zoom', and
#! has the advantage of being able to start interaction on any already drawn
#! plot. I could improve my functions by integrating this replayPlot thing,
#! or else abandon my code but contribute to theirs at:
#! https://github.com/cbarbu/R-package-zoom
#! However, my approach also has advantages compared to theirs, So I'll keep on.

#IDEA: Improve CTRL-L keyboard shortcut action by skipping repeated actions...
# (When zooming+ several times, the 'usr' prior to the first zoom+ is memorized
#  into 'usrlast' internal variable, but not the next ones, since it should be 
#  reversible anyway).
#IDEA: Change variable 'usrlast' type to list:
# (It will then be easier to access, using indeces. When asking for a new view,
#  it will add an usrlast[[i+1]] entry to that list; and when asking to go back
#  to the previous (last) view with the CTRL-L shortcut, it will read [[i-1]].)

#Version notes (partial):
#v1d5: Added 'stillmoving' to protect against drags+MouseUp that miss MouseUp
#      events (The user can do MouseDown+MouseUp at the same location(screen)
#      so as to stop the undesirable dragging (xy final=start).
#v1d6: Added a keyboard shortcut (V) for Vertical Exaggeration.
#v1d7: Cumulative & selective storage (in memory) of the previous 'usr' settings
#      (does not keep 'usr' from repeated instructions, for instance, several
#       consecutive zoom+).

dragplot <- function(..., xlim = NULL, ylim = NULL, xaxs = "r", yaxs = "r", minmove = 0.00, mintimelag = 0.1, plotFUN=NULL, safe.axes.pars = F, talks = T) {
    if(missing(plotFUN) || is.null(plotFUN)) plotFUN <- function(...,xlim,ylim,xaxs,yaxs) plot(..., xlim = xlim, ylim = ylim, xaxs = xaxs, yaxs = yaxs) #defines plotFUN with default behavior if not user-defined
    plotFUN(..., xlim = xlim, ylim = ylim, xaxs = xaxs, yaxs = yaxs)
    if(safe.axes.pars) {par(xaxs = "i"); par(yaxs = "i")} #Makes sure that the axes are not shrunk due to xaxs|yaxs == "r"; important for certain plot methods that do not fully manage these graphical parameters (e.g., plot.histogram, hist)
    rectzoomin <- NULL
    startx <- NULL
    starty <- NULL
    prevx <- NULL
    prevy <- NULL
    timelastmove <- Sys.time()
    usr <- NULL
    usrlastL <- list()
    usrlasti <- 0
    usr0 <- par("usr")
    treatingmove <- FALSE
    stillmoving <- FALSE
    lasteffectkey <- ""

    devset <- function()
        if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)

    dragmousedown <- function(buttons, x, y) {
        #DOES NOT WORK WELL YET: if(buttons != 0) return(NULL) #continues only if left button
        if(stillmoving) return(NULL)
        #writeLines("mousedown begins")
        lasteffectkey <<- "mousedown"
        prevx <<- 0
        prevy <<- 0
        devset()
        usr <<- par("usr")
        usrlastL[[usrlasti+1]] <<- usr; usrlasti <<- usrlasti + 1
        startx <<- x
        starty <<- y
        timelastmove <- Sys.time()+0.25
        #rectzoomin <<- NULL #to disable this feature at mouseup
        if(is.null(rectzoomin)) {
          eventEnv$onMouseMove <- dragmousemove
        } else {
          endx <<- NA #init.
          endy <<- NA #init.
          eventEnv$onMouseMove <- dragmousemoveZR
        }
        #writeLines("mousedown ends")
        #print(ls(eventEnv))
        #print(get("onMouseUp",envir=eventEnv))
        NULL
    }

    rectzoomaction <- function(zoomin) {
        rectzoomin <<- zoomin
        NULL
    }

    dragmousemove <- function(buttons, x, y) {
        #DOES NOT WORK WELL YET: if(buttons != 0) return(NULL) #continues only if left button
        if(treatingmove) return(NULL)
        treatingmove <<- T
        stillmoving <<- T
        devset()
        convx <- grconvertX(c(startx,x), "ndc", "user")
        convy <- grconvertY(c(starty,y), "ndc", "user")
        deltax <- diff(convx)
        deltay <- diff(convy)
        #DEBUG print(c(deltax,deltay))
        #DEBUG print(c(par("xaxs"),par("yaxs")))
        timenow = Sys.time()
        #print(difftime(timenow,timelastmove))
        #if (abs(deltax-prevx) + abs(deltay-prevy) > 0) {
      	if ( (difftime(timenow,timelastmove)>mintimelag) & ( (abs(deltax-prevx)>(minmove*diff(usr[1:2]))) | (abs(deltay-prevy)>(minmove*diff(usr[3:4]))) ) ) {
            #TROP if(safe.axes.pars) {par(xaxs = "i"); par(yaxs = "i")}
            dev.hold()
            plotFUN(..., xlim = usr[1:2]-deltax, xaxs = "i",
      		      ylim = usr[3:4]-deltay, yaxs = "i")
            dev.flush()
      	    prevx <<- deltax
      	    prevy <<- deltay
            timelastmove <<- Sys.time()
      	}
        treatingmove <<- F
        NULL
    }

    dragmousemoveZR <- function(buttons, x, y) {
        #DOES NOT WORK WELL YET: if(buttons != 0) return(NULL) #continues only if left button
        if(treatingmove) return(NULL)
        treatingmove <<- T
        devset()
        convx <- grconvertX(c(startx,x), "ndc", "user")
        convy <- grconvertY(c(starty,y), "ndc", "user")
        deltax <- diff(convx)
        deltay <- diff(convy)
        #DEBUG print(c(deltax,deltay))
        #DEBUG print(c(par("xaxs"),par("yaxs")))
        timenow = Sys.time()
        #if (abs(deltax-prevx) + abs(deltay-prevy) > 0) {
      	if ( (difftime(timenow,timelastmove)>mintimelag) & ( (abs(deltax-prevx)>(minmove*diff(usr[1:2]))) | (abs(deltay-prevy)>(minmove*diff(usr[3:4]))) ) ) {
            #NOT NECESSARY if(safe.axes.pars) {par(xaxs = "i"); par(yaxs = "i")}
            dev.hold()
            plotFUN(..., xlim = usr[1:2], xaxs = "i",
      		      ylim = usr[3:4], yaxs = "i")
            rect(convx[1],convy[1],convx[2],convy[2])
            dev.flush()
      	    prevx <<- deltax
      	    prevy <<- deltay
            timelastmove <<- Sys.time()
      	}
        treatingmove <<- F
        NULL
    }

    mouseup <- function(buttons, x, y) {
      #DOES NOT WORK YET: print(c("mouseup",buttons)); if(buttons != 0) return(NULL) #continues only if left button
      #writeLines("mouseup begins")
      eventEnv$onMouseMove <- NULL
      if(!is.null(rectzoomin)) {
        finalx <- grconvertX(c(startx,x), "ndc", "user")
        finaly <- grconvertY(c(starty,y), "ndc", "user")
        #print(c(finalx[1],finaly[1],finalx[2],finaly[2]))
        rzoomxlim <- sort(finalx)
        rzoomylim <- sort(finaly)
        rectzoomin <<- NULL
        dev.hold()
        plotFUN(..., xlim = rzoomxlim, xaxs = "i", ylim = rzoomylim, yaxs = "i")
        dev.flush()
        usr <<- par("usr")
        if(talks) print("zoomed in a rectangle")
      } else if((x=startx)&(y=starty)&(prevx==0)) {
        if(stillmoving) {stillmoving <<- F; return(NULL)}
        newcenter = list(x=grconvertX(x, "ndc", "user"), y=grconvertY(y, "ndc", "user"))
        newcenter$xlim = usr[1:2]+(newcenter$x-mean(usr[1:2]))
        newcenter$ylim = usr[3:4]+(newcenter$y-mean(usr[3:4]))
        dev.hold()
        plotFUN(..., xlim = newcenter$xlim, xaxs = "i", ylim = newcenter$ylim, yaxs = "i")
        text(newcenter$x, newcenter$y, sprintf("(x=%g, y=%g)", newcenter$x, newcenter$y))
        dev.flush()
        usr <<- par("usr")
        if(talks) print("moved the plot window to a new central point")
      } else {
        stillmoving <<- F
      }
      #writeLines("mouseup ends")
      NULL
    }

    keydown <- function(key) {
        #print(key)
        if (key %in% c("Esc","q","ctrl-[")) {
          eventEnv$onMouseMove <- NULL
          if(talks) print("ended interaction")
          return(invisible(1))
        }
        #other keys are processed only if...
        if(stillmoving) return(NULL)
        if (key %in% c("a","z","+","-","=","6","4","8","2")) {
          if(key == "=") key <- "+"
          zoomin = (key %in% c("a","+","6","8"))
          coeff = rep(if(zoomin) 3/4 else 4/3, 2)
          applyon = c(!key %in% c("8","2"), !key %in% c("6","4"))
          coeff[!applyon] = 1
          usr <<- par("usr")
          #print(usr)
          #NO MORE: abline(h=mean(usr[1:2]),v=mean(usr[3:4]),lty=2)
          zoomedxlim = mean(usr[1:2])+diff(usr[1:2])/2*c(-coeff[1],coeff[1])
          zoomedylim = mean(usr[3:4])+diff(usr[3:4])/2*c(-coeff[2],coeff[2])
          #TROP if(safe.axes.pars) {par(xaxs = "i"); par(yaxs = "i")}
          dev.hold()
          plotFUN(..., xlim = zoomedxlim, xaxs = "i", ylim = zoomedylim, yaxs = "i")
          dev.flush()
          if(key != lasteffectkey) {usrlastL[[usrlasti+1]] <<- usr; usrlasti <<- usrlasti + 1}
          lasteffectkey <<- key
          usr <<- par("usr")
          supptext = c("in X only","in Y only","globally")[1*applyon[1]+2*applyon[2]]
          if(talks) print(paste("zoomed",c("out","in")[1+zoomin],supptext))
        }
        if (key %in% c("ctrl-A","ctrl-Z")) {
          rectzoomaction(zoomin=(key=="ctrl-A"))
        }
        if (key %in% c("ctrl-L")) {
          if(talks) print("restoring last usr")
          if(usrlasti<1) {alarm(); return(NULL)}
          #print(usrlasti)
          #print(usrlastL[sequence(usrlasti)])
          tmp_usrlast <- usrlastL[[usrlasti]]
          dev.hold()
          plotFUN(..., xlim = tmp_usrlast[1:2], xaxs = "i", ylim = tmp_usrlast[3:4], yaxs = "i")
          dev.flush()
          usr <<- par("usr")
          usrlasti <<- usrlasti - 1
        }
        if (key %in% c("v")) {
          if(talks) print("changing vertical exaggeration")
          usr <<- par("usr")
          oldd <- list(x=diff(usr[1:2]), y=diff(usr[3:4]))
          oldvexag <- oldd$x/oldd$y
          exprtext <- winDialogString("enter a valid vertical exag. ratio as numeric or expression:",format(oldvexag))
          if(is.null(exprtext) || exprtext=="") return(NULL)
          newvexag <- eval(parse(text=exprtext))
          newd <- list(x=oldd$x) #max(oldd$y*newasp, oldd$x))
          newd$y = newd$x/newvexag
          newlim <- list(x=mean(usr[1:2])+c(-0.5,0.5)*newd$x,
                         y=mean(usr[3:4])+c(-0.5,0.5)*newd$y)
          dev.hold()
          plotFUN(..., xlim = newlim$x, xaxs = "i", ylim = newlim$y, yaxs = "i")
          dev.flush()
          usr <<- par("usr")
        }
        if (key %in% c("x","y")) {
          if(talks) print("changing the central coordinates of the plot window")
          usr <<- par("usr")
          oldcenter <- list(x=mean(usr[1:2]), y=mean(usr[3:4]))
          oldranges <- list(x=diff(usr[1:2]), y=diff(usr[3:4]))
          exprtext <- winDialogString(sprintf("enter a valid %s coordinate as numeric or expression:",key), format(oldcenter[key]))
          if(is.null(exprtext) || exprtext=="") return(NULL)
          newcoord <- eval(parse(text=exprtext))
          if(!is.finite(newcoord)) return(NULL)
          newcenter <- oldcenter
          newcenter[key] <- newcoord
          newlim <- list(x=newcenter$x + c(-0.5,0.5)*oldranges$x,
                         y=newcenter$y + c(-0.5,0.5)*oldranges$y)
          dev.hold()
          plotFUN(..., xlim = newlim$x, xaxs = "i", ylim = newlim$y, yaxs = "i")
          dev.flush()
          usr <<- par("usr")
        }
        if (key %in% c("Left", "Up", "Right", "Down")) {
          relmove = switch(key,
            Left = c(-0.1, 0.0),
            Right= c( 0.1, 0.0),
            Down = c( 0.0,-0.1),
            Up   = c( 0.0, 0.1)
          )
          usr <<- par("usr")
          #print(usr)
          movexlim = usr[1:2] + relmove[1]*diff(usr[1:2])
          moveylim = usr[3:4] + relmove[2]*diff(usr[3:4])
          #NOT NECESSARY if(safe.axes.pars) {par(xaxs = "i"); par(yaxs = "i")}
          dev.hold()
          plotFUN(..., xlim = movexlim, xaxs = "i", ylim = moveylim, yaxs = "i")
          dev.flush()
          if(key != lasteffectkey) {usrlastL[[usrlasti+1]] <<- usr; usrlasti <<- usrlasti + 1}
          lasteffectkey <<- key
          usr <<- par("usr")
          if(talks) print(paste("moved",key))
        }
        if (key %in% c("r","0")) {
          #NOT NECESSARY if(safe.axes.pars) {par(xaxs = "i"); par(yaxs = "i")}
          dev.hold()
          plotFUN(..., xlim = usr0[1:2], xaxs = "i", ylim = usr0[3:4], yaxs = "i")
          dev.flush()
          if(key != lasteffectkey) {usrlastL[[usrlasti+1]] <<- usr; usrlasti <<- usrlasti + 1}
          lasteffectkey <<- key
          usr <<- par("usr")
          if(talks) print("zoom was reset")
        }
        NULL
    }

    setGraphicsEventHandlers(prompt = "INTERACTIVE PLOT - click & drag, press keys [q or Esc to cease]",
                     onMouseDown = dragmousedown,
                     onMouseUp = mouseup,
                     onKeybd = keydown)
    eventEnv <- getGraphicsEventEnv()
}

filterXorYdata <- function(x, xlim, extend=T) {
  if(is.null(xlim)) return(!logical(length(x)))
  res = x>=xlim[1] & x<=xlim[2]
  arein = which(res)
  if(length(arein)<1) return(logical(length(x)))
  bounds = c(min(arein),max(arein))
  if(extend) {
    if(bounds[1]>1) res[bounds[1]-1] = T
    if(bounds[2]<length(x)) res[bounds[2]+1] = T
  }
  return(res)
}
#TEST: filterXdata(c(1:5,7,2:4), c(2,4), extend=T)
#TEST(x,y): x=rnorm(100); y=rnorm(100); sel=filterXorYdata(x,c(-1,1)) & filterXorYdata(y,c(-1,1)); plot(x,y,col=1+sel)

if(F) {
  savepar <- par(ask = FALSE)
  dragplot(rnorm(100), sort(rnorm(100)), type="l")
  # This currently only works on the Windows
  # and X11(type = "Xlib") screen devices...
  getGraphicsEvent()
  par(savepar)
}

if(F) {
  v = rnorm(1000)
  H = hist(v,breaks=16,plot=F)

  savepar <- par(ask = FALSE)
  dragplot(H,xlim=range(H$breaks),safe.axes.pars=T)
  getGraphicsEvent()
  par(savepar)
}
