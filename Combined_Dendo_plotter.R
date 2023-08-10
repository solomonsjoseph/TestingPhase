library(ggpubr)

source("DendogramPlot.R")

combined_dendrogram_plot <- function(se, assay, annotation_columns) {
  
  # Initialize an empty list to store individual dendrogram plots
  plots_list <- list()
  
  # Loop through each annotation_column
  for (annotation_column in annotation_columns) {
    
    # Get dendrogram plot for this annotation_column using the dendrogram_plotter
    dendrogram <- dendrogram_plotter(se, assay, annotation_column)
    
    # Append this dendrogram to the plots_list
    plots_list[[annotation_column]] <- dendrogram
    
  }
  
  # Combine all plots vertically
  combined_plot <- ggarrange(plotlist = plots_list, ncol = 1, nrow = length(plots_list))
  
  return(combined_plot)
}

# Usage example:
combined_plot <- combined_dendrogram_plot(se, assay, annotation_columns=col_data_nam)
 print(combined_plot)
