# Load necessary packages
library(tidyverse)
library(patchwork)

source("ProcessDendo.R")
source("DendogramPlot.R")

circ_dendo_plotter <- function(se, assay, annotation_column) {
  
  dendoPlot <- dendrogram_plotter(se, assay, annotation_column)
  
  circular_dendrogram <- dendoPlot + coord_polar(theta="x")
  
  return(circular_dendrogram)
}

# Using the function
plot1 <- circ_dendo_plotter(se, assay, annotation_column = col_data_nam[1])
plot2 <- circ_dendo_plotter(se, assay, annotation_column = col_data_nam[2])
combined_plot <- plot1 + plot2 + plot_layout(ncol = 2)
print(combined_plot)