#' Heatmap color palette
#'
#' This function creates the color palette used in the heatmap plot
#' @param ann_col ann_col object representing column data of heatmap
#' @import dplyr
#' @import RColorBrewer
#' @return col_color vector of colors corresponding to col_name variable
#' @examples
#' library(scran)
#' se <- mockSCE()
#' heatmaps <- BatchQC::heatmap_plotter(se,
#'                                 assay = "counts",
#'                                 nfeature = 15,
#'                                 annotation_column = c("Mutation_Status",
#'                                 "Treatment"))
#' ann_col <- heatmaps$ann_col
#' ann_color <- BatchQC::heatmap_color_palette(ann_col = ann_col)
#' ann_color
#'
#' @export

heatmap_color_palette <- function(ann_col) {
  
  # Define a palette function
  col_pal <- color_palette(n = 300)
  col_pal <- as.list(col_pal)
  
  # Initialize an empty named list to hold column color ranges
  col_color <- list()
  
  # Loop through each column in ann_col
  for (col_name in names(ann_col)) {
    
    # Extract unique values from the specified column
    unique_values <- sort(unique(ann_col[[col_name]]))
    
    # Sample colors without replacement for each unique value
    sampled_colors <- sample(col_pal, length(unique_values),
                             replace = FALSE)
    
    # Create a named color vector
    color_vector <- setNames(sampled_colors, unique_values)
    
    # Add this color vector to the list, with the column name as list name
    col_color[[col_name]] <- color_vector
  }
  
  return(col_color)
}
