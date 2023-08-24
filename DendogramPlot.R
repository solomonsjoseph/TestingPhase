source("ProcessDendo.R")
source("custom_color_palette.R")

dendrogram_plotter <- function(se, assay, batch_v, category) {
  
  dends <- process_dendrogram(se, assay)
  
  dendrogram_ends <- dends$dendrogram_ends
  
  dendrogram_segments <- dends$dendrogram_segments
  
  ##### Minor issue related to legend alignment when displayed fix #####
  # Convert dendrogram_ends[,batch_v] to numeric, if it's expected to be numeric
  #dendrogram_ends[,batch_v] <- as.numeric(as.character(dendrogram_ends[,batch_v]))
  
  ## Attempting to better order **legends** to work with character and Numeric (Minor issue)
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
  ##### ends here #####
  
  #Dendrogram color palette used | This code is moved to a different file
  batch_color <- custom_color_palette(col = batch_v)
  category_color <- custom_color_palette(col = category)
  
  if (category == "condition" && is.numeric((category)))
  {
    # Get unique 'category' values, convert to dataframe with 'unique_string_index'.
    unique_strings <- levels(factor(dendrogram_ends[,category])) %>%
      as.data.frame() %>% rownames_to_column("unique_string_index")
    
    # Concatenate values from 'unique_strings' with "_" and store as a dataframe column 'category_val'.
    label_category_strings <- paste(unlist(unique_strings[2]), sep = "") %>%
      as.data.frame() %>% dplyr::rename("category_val" = ".")
    
    geom_label <- label_category_strings[,"category_val"]
    
  } else {
    # Get unique 'category' values, convert to dataframe with 'unique_string_index'.
    unique_strings <- levels(factor(dendrogram_ends[,category])) %>%
      as.data.frame() %>% rownames_to_column("unique_string_index")
    
    # Concatenate values from 'unique_strings' with "_" and store as a dataframe column 'category_val'.
    label_category_strings <- paste(unlist(unique_strings[1]), unlist(unique_strings[2]), sep = "_") %>%
      as.data.frame() %>% dplyr::rename("category_val" = ".")
    
    geom_label <- label_category_strings[,"category_val"]
  }
  
  # #This line of code needs to be modified to fix the y=y.y issue [This line is introduced]
  # buffer <- -1
  # for (dendro in levels(factor(dendrogram_ends[,category]))) {
  #   if (length(dendro) > buffer) {
  #     buffer <- length(dendro)
  #     buffer_string <- dendro
  #   }
  # }
  # 
  # buffer <- length(buffer_string)
  # 
  # # buffer <- buffer * -1.15
  # # buffer <- buffer + (length(dendrogram_ends[,category]) * -0.04)
  
  # Create dendrogram plot
  dendrogram <- ggplot() +
    geom_segment(data = dendrogram_segments, 
                 aes(x=x, y=y, xend=xend, yend=yend)) +
    geom_segment(data = dendrogram_ends,
                 aes(x=x, y=y.x, xend=xend, yend=yend, 
                     color = dendrogram_ends[,batch_v]
                 )) +
    scale_color_manual(values = batch_color, name = batch_v, guide_legend(override.aes = batch_color, order = 1)) +
    new_scale_color() + # To separate the color palette
    geom_text(data = dendrogram_ends,
              #Minor bug with adding y=y.y-1.5
              # aes(x=x, y=y.y-1.5, label=dendrogram_ends[,category],
              # aes(x=x, y=y.y+buffer, label=dendrogram_ends[,category],
              aes(x=x, y=y.y-2.5, label=as.character(
                as.numeric(factor(dendrogram_ends[,category]))),
                color = dendrogram_ends[,category]), 
              check_overlap = TRUE, size = 2) +
    # scale_color_manual(values = category_color, name = category, guide_legend(override.aes = category_color, order = 2))  +
    # scale_color_manual(values = category_color, name = category, guides(color = guide_legend(override.aes = aes(label = category_color))))  +
    # guides(color = guide_legend(override.aes = aes(label = as.character(category_color), alpha = 1))) +
    # guides(color = guide_legend(override.aes = aes(label = "♦", alpha = 1))) +
    # guides(color = guide_legend(override.aes = aes(label = "⸻", alpha = 1))) +
    # guides(color = guide_legend(override.aes = aes(label = "➤", alpha = 1))) +
    guides(color = guide_legend(override.aes = list(label = "━", alpha = 1))) + # This line directs geom_text with the right color palette | Fixes "a" in legend issue
    # guides(color = guide_legend(override.aes = aes(label = label_category_string, alpha = 1))) + # This line directs geom_text with the right color palette | Fixes "a" in legend issue
    scale_color_manual(labels = geom_label, values = category_color, name = category)  +
    scale_y_reverse(expand = c(0.2,0)) +
    coord_flip() + theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    theme_bw() + ylab("Distance")
  
  return(dendrogram)
}

dendrogram_plotter(se, assay, batch_v = "batch", category = "condition")

