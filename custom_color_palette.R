source("ProcessDendo.R")

custom_color_palette <- function(col) {
  
  dend <- process_dendrogram(se, assay)
  dendrogram_ends <- dend$dendrogram_ends
  
  # Create unique_vars dataframe
  unique_vars <- levels(factor(dendrogram_ends[,col])) %>%
    as.data.frame() %>% rownames_to_column("row_id")
  
  # Determine color count and palette
  color_count <- length(unique(unique_vars$.))
  n <- length(unique(dendrogram_ends[,col]))
  # palette_name <- ifelse(n < 5, "RdGnBu", "Paired") #Spectral was used first
  # get_palette <- grDevices::colorRampPalette(brewer.pal(n = n, name = palette_name))
  
  # Determine color count and palette
  color_count <- length(unique(unique_vars$.))
  n <- length(unique(dendrogram_ends[,col]))
  # palette_name <- ifelse(n < 5, "RdGnBu", "Paired") #Spectral was used first
  # get_palette <- grDevices::colorRampPalette(brewer.pal(n = n, name = palette_name))
  
  if (n<5) {
    get_palette <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
  } else {
    get_palette <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 35, c = 100)[1:n]
    }
  }
  # get_palette <- function(n) {
  #   hues = seq(15, 375, length = n + 1)
  #   hcl(h = hues, l = 65, c = 100)[1:n]
  # }
  
  palette <- get_palette(color_count) %>%
    as.data.frame() %>%
    dplyr::rename("color" = ".") %>%
    rownames_to_column(var = "row_id")
  
  # Join the palette and unique_vars
  color_list <- left_join(unique_vars, palette, by = "row_id") %>%
    select(-row_id)
  
  # Create a named vector for annotation_color
  annotation_color <- setNames(color_list$color, color_list$.)
  
  return(annotation_color)
}

custom_color_palette(col = "batch")