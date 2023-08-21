#All neccessary libraries loaded here
library(ggplot2)
library(ggnewscale)

#Dataset
# se <- readRDS("~/tmp/Projects/TestingPhase/bladderbatchSE.RDS")

##### New Data added #####
se <- readRDS("~/tmp/Projects/TestingPhase/signatureDataSE.RDS")
sex <- c('Male', 'Female', 'Male', 'Female', 'Female', 'Male', 'Female', 'Male', 'Female', 'Male', 'Male', 'Female', 'Female', 'Male', 'Female', 'Male', 'Female', 'Female', 'Male', 'Female', 'Female', 'Female', 'Female', 'Male', 'Female', 'Female', 'Male', 'Female', 'Male', 'Female', 'Female', 'Female', 'Female', 'Male', 'Male', 'Male', 'Female', 'Male', 'Male', 'Female', 'Male', 'Male', 'Female', 'Female', 'Male', 'Male', 'Male', 'Female', 'Female', 'Male', 'Female', 'Female', 'Female', 'Male', 'Male', 'Female', 'Female', 'Male', 'Female', 'Male', 'Male', 'Male', 'Male', 'Male', 'Female', 'Female', 'Male', 'Female', 'Male', 'Male', 'Female', 'Female', 'Female', 'Female', 'Female', 'Male', 'Male', 'Male', 'Male', 'Female', 'Male', 'Female', 'Male', 'Male', 'Male', 'Female', 'Male', 'Male', 'Male'
)

# Using accessor functions
colData(se)$sex <- sex

# Define 10 specific diseases
unique_diseases <- c("HIV", "Cholera", "Tuberculosis", "Malaria", "Influenza",
                     "Ebola", "Zika", "Dengue", "Typhoid", "Measles")

# Repeating the diseases until we have a list of 89
diseases <- rep(unique_diseases, ceiling(89 / length(unique_diseases)))[1:89]

# Shuffle the diseases list to introduce randomness
random_diseases <- sample(diseases, length(diseases))

colData(se)$diseases <- random_diseases

##### End of Data #####

#Removes NAs if present in SE object
se <- se[which(rownames(se) !="NA")]

#To display Batch and condition separately
col_data_nam <<- colnames(colData(se))

#Display assay name
assay <- assayNames(se)

process_dendrogram <- function(se, assay) {
  
  data <- t(se@assays@data[[assay]])
  dat <- as.data.frame(data) %>%
    mutate(sample_name = paste("sample", seq_len(nrow(data)), sep = "_"))
  rownames(dat) <- dat$sample_name
  sample_name <- dat$sample_name
  metadata <- cbind(as.data.frame(colData(se)),sample_name)
  metadata[] <- lapply(metadata, as.character)
  #This line of code is the reason for the NAs being introduced
  suppressWarnings(dist_matrix <- stats::dist(dat, method = "euclidean"))
  # dist_matrix <- stats::dist(dat, method = "euclidean")
  
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
    # mutate(sample_name = label) %>%  # Use mutate instead of rename
    # select(-label) %>%               # Drop the original 'label' column
    dplyr::rename(sample_name = label) %>%
    filter(!is.na(sample_name)) %>%
    left_join(metadata, by = "sample_name")
  
  return(list(dendrogram_ends=dendrogram_ends,
              dendrogram_segments=dendrogram_segments))
  
}

#process_dendrogram(se, assay)