library(tibble)
library(ggdendro)
library(dplyr)

#Dataset
se <- readRDS("~/tmp/Projects/TestingPhase/signatureDataSE.RDS")

#To display Batch and condition separately
col_data_nam <- colnames(colData(se))

#Display assay name
assay <- assayNames(se)
batch_dat_list <- list()
col_data_list <- list()
input_dat <- assay(se, assay)
batch_dat <- colData(se)[, col_data_nam %in% "batch"]
col_data <- colData(se)[, col_data_nam %in% "condition"]

# Loop over unique elements
for(i in unique(batch_dat)) {
  # Create a new list element for each unique element in col_data
  batch_dat_list[[paste0("batch_dat", i)]] <- batch_dat[batch_dat == i]
}

# Loop over unique elements
for(i in unique(col_data)) {
  # Create a new list element for each unique element in col_data
  col_data_list[[paste0("col_data", i)]] <- col_data[col_data == i]
}

process_dendrogram <- function(se, assay, annotation_column) {
  
  data <- t(se@assays@data[[assay]])
  dat <- as.data.frame(data) %>%
    mutate(sample_name = paste("sample", seq_len(nrow(data)), sep = "_"))
  rownames(dat) <- dat$sample_name
  sample_name <- dat$sample_name
  metadata <- cbind(as.data.frame(colData(se)),sample_name)
  metadata[] <- lapply(metadata, as.character)
  dist_matrix <- stats::dist(dat, method = "euclidean")
  
  dendrogram <- stats::as.dendrogram(
    stats::hclust(
      dist_matrix, 
      method = "complete")
  )
  
  dendrogram_data <- dendro_data(dendrogram)
  dendrogram_segments <- dendrogram_data$segments
  dendrogram_ends <- dendrogram_segments %>%
    filter(yend == 0) %>% 
    left_join(dendrogram_data$labels, by = "x") %>% 
    rename(sample_name = label) %>%
    left_join(metadata, by = "sample_name")
  
  return(list(dendrogram_ends=dendrogram_ends,
              dendrogram_segments=dendrogram_segments))
  
}

process_dendrogram(se, assay, annotation_column = col_data_nam[1])