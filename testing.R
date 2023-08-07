se <- readRDS("~/tmp/Projects/TestingPhase/signatureDataSE.RDS")
col_data_nam <- colnames(colData(se))
assay_choice <- assayNames(se)
input_dat <- assay(se, assay_choice)
batch_dat <- colData(se)[, col_data_nam %in% "batch"]
col_data <- colData(se)[, col_data_nam %in% "condition"]
batch_dat_list <- list()
col_data_list <- list()
for(i in unique(batch_dat)) {
  # Create a new list element for each unique element in col_data
  batch_dat_list[[paste0("batch_dat", i)]] <- batch_dat[batch_dat == i]
}
for(i in unique(col_data)) {
  # Create a new list element for each unique element in col_data
  col_data_list[[paste0("col_data", i)]] <- col_data[col_data == i]
}
data <- t(se@assays@data[[assay_choice]])
#required lib: tidyverse
dat <- as.data.frame(data) %>% mutate(sample_name = paste("sample", seq_len(nrow(data)), sep = "_"))
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

#uses library(ggdendro)
dendrogram_data <- dendro_data(dendrogram)
dendrogram_segments <- dendrogram_data$segments
dendrogram_ends <- dendrogram_segments %>%
  filter(yend == 0) %>% 
  left_join(dendrogram_data$labels, by = "x") %>% 
  rename(sample_name = label) %>%
  left_join(metadata, by = "sample_name")

annotation_column <- col_data_nam[1]

unique_vars <- levels(factor(dendrogram_ends[,annotation_column])) %>% 
  as.data.frame() %>% rownames_to_column("row_id") 