# Load necessary packages
library(tidyverse)

source("DendogramPlot.R")

circ_dendo_plotter <- function(se, assay, batch, category) {
  
  dendoPlot <- dendrogram_plotter(se, assay, batch, category)
  
  circular_dendrogram <- dendoPlot + coord_polar(theta="x")
  
  return(circular_dendrogram)
}

# Using the function
circ_dendo_plotter(se, assay, batch = "batch", category = "condition")
# print(plot1)