# Load necessary packages
library(tidyverse)

source("DendogramPlot.R")

circ_dendo_plotter <- function(se, assay, annotation_column) {
  
  dendoPlot <- dendrogram_plotter(se, assay, annotation_column)
  
  circular_dendrogram <- dendoPlot + coord_polar(theta="x")
  
  return(circular_dendrogram)
}

# Using the function
circ_dendo_plotter(se, assay, annotation_column = col_data_nam[1])