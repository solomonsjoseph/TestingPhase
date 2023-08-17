library(ggdendro)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tibble)

source("ProcessDendo.R")

dendrogram_plotter <- function(se, assay, batch, category) {
  
  dends <- process_dendrogram(se, assay)
  
  dendrogram_ends <- dends$dendrogram_ends
  
  dendrogram_segments <- dends$dendrogram_segments
  
  # Convert dendrogram_ends[,batch] to numeric, if it's expected to be numeric
  #dendrogram_ends[,batch] <- as.numeric(as.character(dendrogram_ends[,batch]))
  
  ## Attempting to better order legends to work with character and Numeric (Minor issue)
  # # Separate the numeric and non-numeric values
  # numeric_levels <- as.numeric(as.character(dendrogram_ends[,batch]))
  # numeric_levels <- numeric_levels[!is.na(numeric_levels)]
  # sorted_numeric_levels <- sort(unique(numeric_levels))
  # 
  # non_numeric_levels <- dendrogram_ends[,batch][is.na(numeric_levels)]
  # sorted_non_numeric_levels <- sort(unique(as.character(non_numeric_levels)))
  # 
  # # Combine the two sorted lists
  # sorted_levels <- c(as.character(sorted_numeric_levels), sorted_non_numeric_levels)
  
  # Ensure dendrogram_ends[,batch] is treated as a factor with sorted levels
  #dendrogram_ends[,batch] <- factor(dendrogram_ends[,batch]) #, levels = sorted_levels)
  
  # # Create unique_vars dataframe
  # unique_vars <- levels(factor(dendrogram_ends[,batch])) %>% 
  #   as.data.frame() %>% rownames_to_column("row_id") 
  # 
  # # Determine color count and palette
  # color_count <- length(unique(unique_vars$.))
  # n <- length(unique(dendrogram_ends[, batch]))
  # palette_name <- ifelse(n < 5, "Spectral", "Paired")
  # 
  # get_palette <- grDevices::colorRampPalette(brewer.pal(n = n, name = palette_name))
  # 
  # palette <- get_palette(color_count) %>%
  #   as.data.frame() %>%
  #   dplyr::rename("color" = ".") %>%
  #   rownames_to_column(var = "row_id")
  # 
  # # Join the palette and unique_vars
  # color_list <- left_join(unique_vars, palette, by = "row_id") %>%
  #   select(-row_id)
  # 
  # # Create a named vector for annotation_color
  # annotation_color <- setNames(color_list$color, color_list$.)
  
  buffer <- max(dendrogram_ends$x) * -0.18
  
  # Create dendrogram plot
  dendrogram <- ggplot() +
    geom_segment(data = dendrogram_segments, 
                 aes(x=x, y=y, xend=xend, yend=yend)) +
    geom_segment(data = dendrogram_ends,
                 aes(x=x, y=y.x, xend=xend, yend=yend, 
                     color = batch)) +
    geom_text(data = dendrogram_ends,
              aes(x=x, y=y.x + buffer, label=category, 
                  color = category), check_overlap = TRUE) +
    # scale_color_manual(values = annotation_color, limits = names(annotation_color), name = as.character(batch)) +
    scale_y_reverse(expand = c(0.3, 0)) +
    coord_flip() + theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    theme_bw() + ylab("Distance")
  
  return(dendrogram)
}

dendrogram_plotter(se, assay, batch = "batch", category = "condition")