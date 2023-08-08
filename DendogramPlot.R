library(ggdendro)
library(RColorBrewer)
library(dplyr)

source("ProcessDendo.R")

dendrogram_plotter <- function(se, assay, annotation_column) {
  
  dends <- process_dendrogram(se, assay, annotation_column)
  
  dendrogram_ends <- dends$dendrogram_ends
  
  dendrogram_segments <- dends$dendrogram_segments
  
  unique_vars <- levels(factor(dendrogram_ends[,annotation_column])) %>% 
    as.data.frame() %>% rownames_to_column("row_id") 
  
  color_count <- length(unique(unique_vars$.))
  get_palette <- grDevices::colorRampPalette(brewer.pal(
    n = length(unique(dendrogram_ends[,annotation_column])),
    name = "Paired"))
  palette <- get_palette(color_count) %>% as.data.frame() %>%
    rename("color" = ".") %>%
    rownames_to_column(var = "row_id")
  color_list <- left_join(unique_vars, palette, by = "row_id") %>%
    select(-row_id)
  annotation_color <- as.character(color_list$color)
  names(annotation_color) <- color_list$.
  
  dendrogram <- ggplot() +
    geom_segment(data = dendrogram_segments, 
                 aes(x=x, y=y, xend=xend, yend=yend)) +
    geom_segment(data = dendrogram_ends,
                 aes(x=x, y=y.x, xend=xend, yend=yend, 
                     color = dendrogram_ends[,annotation_column])) +
    scale_color_manual(values = annotation_color, name = as.character(annotation_column)) +
    scale_y_reverse() +
    coord_flip() + theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    theme_bw() + ylab("Distance")
  
  return(dendrogram=dendrogram)
}
dendrogram_plotter(se, assay, annotation_column = col_data_nam[2])