library(ggdendro)
library(ggplot2)
library(dplyr)
library(tibble)

source("ProcessDendo.R")
source("custom_color_palette.R")

dendrogram_plotter <- function(se, assay, batch_v, category) {
  
  dends <- process_dendrogram(se, assay)
  
  dendrogram_ends <- dends$dendrogram_ends
  
  dendrogram_segments <- dends$dendrogram_segments
  
  # Convert dendrogram_ends[,batch_v] to numeric, if it's expected to be numeric
  #dendrogram_ends[,batch_v] <- as.numeric(as.character(dendrogram_ends[,batch_v]))
  
  ## Attempting to better order legends to work with character and Numeric (Minor issue)
  # # Separate the numeric and non-numeric values
  # numeric_levels <- as.numeric(as.character(dendrogram_ends[,batch_v]))
  # numeric_levels <- numeric_levels[!is.na(numeric_levels)]
  # sorted_numeric_levels <- sort(unique(numeric_levels))
  # 
  # non_numeric_levels <- dendrogram_ends[,batch_v][is.na(numeric_levels)]
  # sorted_non_numeric_levels <- sort(unique(as.character(non_numeric_levels)))
  # 
  # # Combine the two sorted lists
  # sorted_levels <- c(as.character(sorted_numeric_levels), sorted_non_numeric_levels)
  
  # Ensure dendrogram_ends[,batch_v] is treated as a factor with sorted levels
  #dendrogram_ends[,batch_v] <- factor(dendrogram_ends[,batch_v]) #, levels = sorted_levels)
  
  #Custom color palette used
  batch_color <- custom_color_palette(col = batch_v)
  category_color <- custom_color_palette(col = category)
  
  buffer <- max(dendrogram_ends$x) * -0.12
  
  # Create dendrogram plot
  dendrogram <- ggplot() +
    geom_segment(data = dendrogram_segments, 
                 aes(x=x, y=y, xend=xend, yend=yend)) +
    geom_segment(data = dendrogram_ends,
                 aes(x=x, y=y.x, xend=xend, yend=yend, 
                     color = dendrogram_ends[,batch_v]
                     )) +
    geom_text(data = dendrogram_ends,
              aes(x=x, y=y.x, label=dendrogram_ends[,category], 
                  # color = dendrogram_ends[,category]
                  ), check_overlap = TRUE, size = 2, show.legend = TRUE) +
    # scale_color_manual("Batch", values = batch_color, limits = names(batch_color), name = as.character(batch_v), guide = guide_legend(order = 1)) +
    scale_color_manual("Category", values = category_color, limits = names(category_color), name = as.character(category), guide = guide_legend(order = 2)) +
    scale_y_reverse() +
    coord_flip() + theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    theme_bw() + ylab("Distance")
  
  return(dendrogram)
}

dendrogram_plotter(se, assay, batch_v = "batch", category = "condition")