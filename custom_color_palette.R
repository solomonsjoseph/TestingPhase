library(tibble)
dendrogram_color_palette <- function(col, dendro) {
  
  # Create unique_vars dataframe
  unique_vars <- levels(factor(dendro[,col])) %>%
    as.data.frame() %>% rownames_to_column("row_id")
  
  # Determine color count and palette
  color_count <- length(unique(unique_vars$.))
  n <- length(unique(dendro[,col]))
  # palette_name <- ifelse(n < 5, "RdGnBu", "Paired") #Spectral was used first
  # get_palette <- grDevices::colorRampPalette(brewer.pal(n = n, name = palette_name))
  
  # Determine color count and palette
  color_count <- length(unique(unique_vars$.))
  n <- length(unique(dendro[,col]))
  
  if (n<5) {
    get_palette <- function(n) {
      hues <- seq(25, 375, length = n + 1)
      grDevices::hcl(h = hues, l = c(40, 65), c = 100)[seq_len(n)]
    }
    } else {
    get_palette <- function(n) {
      hues <- seq(376, 1500, length = n + 1)
      grDevices::hcl(h = hues, l = c(35, 65), c = 100)[seq_len(n)]
    }
    }
  palette <- get_palette(color_count) %>%
    as.data.frame() %>%
    dplyr::rename("color" = ".") %>%
    rownames_to_column(var = "row_id")
  
  # Join the palette and unique_vars
  color_list <- left_join(unique_vars, palette, by = "row_id") %>%
    select(-row_id)
  
  # Create a named vector for annotation_color
  annotation_color <- stats::setNames(color_list$color, color_list$.)
  
  return(annotation_color)
}

# dendrogram_color_palette(col = "condition")