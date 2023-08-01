# Load necessary packages
library(devtools)
library(SummarizedExperiment)
library(ggraph)
library(tidygraph)
library(ape)

circ_dendo_plotter <- function(se, assay_choice, batch, conditions) {
  
  # Get data out of object
  input_dat <- assay(se, assay_choice)
  batch_dat <- colData(se)[, col_data_nam %in% "batch"]
  col_data <- colData(se)[, col_data_nam %in% "condition"]
 
  
  #To display Batch and condition separately
  col_data_nam <- colnames(colData(se))
  
  #Display assay name
  assay_choice <- assayNames(se)
  
  #Displays assay length (or) list of assay
  assays(se)
  
    # Initialize an empty list
  batch_dat_list <- list()
  
  # Loop over unique elements
  for(i in unique(batch_dat)) {
    # Create a new list element for each unique element in col_data
    batch_dat_list[[paste0("batch_dat", i)]] <- batch_dat[batch_dat == i]
  }
  
  # Initialize an empty list
  col_data_list <- list()
  
  # Loop over unique elements
  for(i in unique(col_data)) {
    # Create a new list element for each unique element in col_data
    col_data_list[[paste0("col_data", i)]] <- col_data[col_data == i]
  }
  
  # Calculate distance matrix
  dist_mat <- dist(input_dat)
  
  # Compute hierarchical clustering
  hc <- hclust(dist_mat)
  
  # Convert to dendrogram
  dend <- as.dendrogram(hc)
  
  # Convert dendrogram to phylo object
  phylo <- as.phylo(dend)
  
  # Convert phylo object to tbl_graph
  graph <- as_tbl_graph(phylo, directed = TRUE)
  
  # Plot circular dendrogram
  ggraph(graph, layout = "dendrogram", circular = TRUE) +
    geom_edge_link() +
    geom_node_point() +
    theme_graph()
  
}

#Se object
se <- readRDS("~/tmp/Projects/TestingPhase/signatureDataSE.RDS")

# Define your assay_choice, batch, and conditions
assay_choice <- assayNames(se)  # Replace with your assay choice
batch <- batch_dat_list # Replace with your batch name
conditions <- col_data_list # Replace with your conditions

# Using the function
circ_dendo_plotter(se, assay_choice, batch, conditions)