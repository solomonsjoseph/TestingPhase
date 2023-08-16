library(ggplot2)
library(ggdendro)

# Load data and generate dendrogram data
data(mtcars)
hclust_result <- hclust(dist(mtcars))
dendro_data <- ggdendro::dendro_data(hclust_result)

# Extract the labels from the dendrogram data
dendro_labels <- as.character(dendro_data$labels$label)

# Get the correct indices from mtcars for the labels
correct_indices <- match(dendro_labels, rownames(mtcars))

# Factorize the 'gear', 'car_name', and 'mpg' columns for coloring
mtcars$gear <- as.factor(mtcars$gear)

# Extract 'gear', 'car_name', and 'mpg' values using the correct indices
gear_labels <- mtcars$gear[correct_indices]

# Build the plot
p <- ggplot() + 
  geom_segment(data=dendro_data$segments, 
               aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_text(data=dendro_data$labels, 
            aes(x, y, label=gear_labels, color=gear_labels), 
            hjust=-0.2, check_overlap = TRUE) +
  coord_flip() + 
  scale_y_reverse() +
  theme_minimal() + 
  theme(legend.position = "right")

print(p)
