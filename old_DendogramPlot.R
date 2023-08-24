library(ggdendro)
library(ggplot2)
library(RColorBrewer)
library(dplyr)

source("ProcessDendo.R")
source("custom_color_palette.R")

dendrogram_plotter <- function(se, assay, annotation_column) {
  
  dends <- process_dendrogram(se, assay)
  
  dendrogram_ends <- dends$dendrogram_ends
  
  dendrogram_segments <- dends$dendrogram_segments
  
  # Convert dendrogram_ends[,annotation_column] to numeric, if it's expected to be numeric
  #dendrogram_ends[,annotation_column] <- as.numeric(as.character(dendrogram_ends[,annotation_column]))
  
  ## Attempting to better order legends to work with character and Numeric (Minor issue)
  # # Separate the numeric and non-numeric values
  # numeric_levels <- as.numeric(as.character(dendrogram_ends[,annotation_column]))
  # numeric_levels <- numeric_levels[!is.na(numeric_levels)]
  # sorted_numeric_levels <- sort(unique(numeric_levels))
  # 
  # non_numeric_levels <- dendrogram_ends[,annotation_column][is.na(numeric_levels)]
  # sorted_non_numeric_levels <- sort(unique(as.character(non_numeric_levels)))
  # 
  # # Combine the two sorted lists
  # sorted_levels <- c(as.character(sorted_numeric_levels), sorted_non_numeric_levels)
  
  # Ensure dendrogram_ends[,annotation_column] is treated as a factor with sorted levels
  #dendrogram_ends[,annotation_column] <- factor(dendrogram_ends[,annotation_column]) #, levels = sorted_levels)
  
  #Custom Color palette
  annotation_color <- custom_color_palette(annotation_column)
  
  # Create dendrogram plot
  dendrogram <- ggplot() +
    geom_segment(data = dendrogram_segments, 
                 aes(x=x, y=y, xend=xend, yend=yend)) +
    geom_segment(data = dendrogram_ends,
                 aes(x=x, y=y.x, xend=xend, yend=yend, 
                     color = dendrogram_ends[,annotation_column])) +
    geom_text(data = dendrogram_ends,
                 aes(x=x, y=y.y, label=dendrogram_ends[,annotation_column],
                     color = dendrogram_ends[,annotation_column]), 
              check_overlap = TRUE, size = 2) +
    scale_color_manual(values = annotation_color, limits = names(annotation_color), name = as.character(annotation_column)) +
    scale_y_reverse() +
    coord_flip() + theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    theme_bw() + ylab("Distance")
  
  return(dendrogram=dendrogram)
}
# dendrogram_plotter(se, assay, annotation_column = col_data_nam[1])
