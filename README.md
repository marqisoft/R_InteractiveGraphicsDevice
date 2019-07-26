# R_InteractiveGraphicsDevice
This R script was developed by Marc Laurencelle, alias marQIsoft. It is largely based on the R example codes provided for the getGraphicsEvent function, in the R Documentation (Help).

The key advantage of this module compared to other existing solutions such as Plotly or 'zoom' package, is that it fully redraws the plot after each change in the view range. It can optionally call plotFUN(..., xlim = ylim, ylim = ylim, xaxs = xaxs, yaxs = yaxs), a custom function that let the user keep full control on what is to be drawn or not, while also allowing cool things such as prior optimization as to what is likely to be visible and thus needs to be drawn. This way, even large datasets can be explored interactively with relatively fast response to user actions.

marQIsoft, July 2019
