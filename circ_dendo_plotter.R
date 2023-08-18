source("DendogramPlot.R")

circ_dendo_plotter <- function(se, assay, batch_v, category) {
  
  dendoPlot <- dendrogram_plotter(se, assay, batch_v, category)
  
  circular_dendrogram <- dendoPlot + coord_polar(theta="x")
  
  return(circular_dendrogram)
}

# Using the function
circ_dendo_plotter(se, assay, batch_v = "batch", category = "condition")
# print(plot1)