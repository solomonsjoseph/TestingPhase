dendrogram_alpha_numeric_check <- function(dendro_category) {
  numeric_or_alpha <- suppressWarnings(!is.na(as.numeric((levels(
    factor(dendro_category))))))
  all_numeric <- TRUE
  
  for (n in numeric_or_alpha){
    if (n==FALSE){
      all_numeric <- FALSE
      break
    }
  }
  
  if (all_numeric)
  {
    geom_label <- as.character(sort(as.numeric(levels(factor(
      dendro_category)))))
    
  } else {
    # Get unique 'category_var' values, convert to dataframe
    unique_strings <- levels(factor(dendro_category)) %>%
      as.data.frame() %>% rownames_to_column("unique_string_index")
    
    # Concatenate values from 'unique_strings' with "_"
    label_category_strings <- paste(unlist(unique_strings[1]),
                                    unlist(unique_strings[2]),
                                    sep = " - ") %>%
      as.data.frame() %>% dplyr::rename("category_val" = ".")
    
    geom_label <- label_category_strings[,"category_val"]
  }
  
  return(geom_label=geom_label)
}
