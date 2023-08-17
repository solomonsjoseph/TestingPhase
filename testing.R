# Install and load the required packages
# install.packages("dendextend")
library(dendextend)

# # Load data
# data(USArrests)
# # Compute distances and hierarchical clustering
# dd <- dist(scale(USArrests), method = "euclidean")
# hc <- hclust(dd, method = "ward.D2")
# # # Put the labels at the same height: hang = -1
# # plot(hc, hang = -1, cex = 0.6)
# # # Convert hclust into a dendrogram and plot
# hcd <- as.dendrogram(hc)
# # # Zoom in to the first dendrogram
# # plot(hcd, xlim = c(1, 20), ylim = c(1,8))
# #Define nodePar
# # nodePar <- list(lab.cex = 0.6, pch = c(NA, 19),
# #                 cex = 0.7, col = "blue")
# # # # Customized plot; remove labels
# # # plot(hcd, ylab = "Height", nodePar = nodePar, leaflab = "none")
# # # Horizontal plot
# # # plot(hcd,  xlab = "Height",
# # #      nodePar = nodePar, horiz = TRUE)
# # plot(hcd,  xlab = "Height", ylab = "Test", nodePar = nodePar,
# #      horiz = TRUE)
# # Rotate the plot and remove default theme
# # ggdendrogram(hc, rotate = FALSE, theme_dendro = TRUE)

# Load necessary libraries
library(dendextend)

# Assuming 'se' is your SummarizedExperiment object
se <- readRDS("~/tmp/Projects/TestingPhase/signatureDataSE.RDS")



# Plot the dendrogram
plot(dend)

