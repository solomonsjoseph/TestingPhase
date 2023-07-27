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

#####################################

#To display Batch and condition separately
col_data_nam <- colnames(colData(se))

#Display assay name
assay_nam <- assayNames(se)

#Displays assay lenth (or) list of assay
assays(se)

#####################################

# Define your assay_choice, batch, and conditions
assay_choice <- assayNames(se)  # Replace with your assay choice
batch <- "batch1"  # Replace with your batch name
conditions <- c("condition1", "condition2")  # Replace with your conditions

# Using the function
#circ_dendo_plotter(se, assay_choice, batch, conditions)
