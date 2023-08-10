# Load necessary packages
library(tidyverse)
library(cowplot)

source("Combined_Dendo_plotter.R")

circ_dendo_plotter <- function(se, assay, annotation_columns) {
  
  Combined_dendoPlot <- combined_dendrogram_plot(se, assay, annotation_columns)
  
  circular_dendrogram <- Combined_dendoPlot + coord_polar(theta="x")
  
  return(circular_dendrogram)
}

# Using the function
plot <- circ_dendo_plotter(se, assay, annotation_columns = col_data_nam)
print(plot)