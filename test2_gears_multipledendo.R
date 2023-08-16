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

# Factorize the 'gear' column for coloring
mtcars$gear <- as.factor(mtcars$gear)

# Extract 'gear', 'car_name', 'vs', 'carb', and 'mpg' values using the correct indices
gear_labels <- mtcars$gear[correct_indices]
car_name_labels <- rownames(mtcars)[correct_indices]
mpg_labels <- round(mtcars$mpg[correct_indices], 2)
vs_labels <- mtcars$vs[correct_indices]
carb_labels <- mtcars$carb[correct_indices]

# Define spacing and text size parameters
spacing <- 3
text_size <- 2

# Adjust the x-coordinates of the labels for each category with increased spacing
base_x <- max(dendro_data$labels$x)
dendro_data$labels$x_gear <- base_x + spacing
dendro_data$labels$x_car_name <- dendro_data$labels$x_gear + spacing
dendro_data$labels$x_vs <- dendro_data$labels$x_car_name + spacing
dendro_data$labels$x_carb <- dendro_data$labels$x_vs + spacing

# Build the plot with 'gear', 'vs', 'car_name', and 'carb' labels
p <- ggplot() + 
  geom_segment(data=dendro_data$segments, 
               aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_text(data=dendro_data$labels, 
            aes(x=x, y=y, label=gear_labels, color=gear_labels), 
            hjust=-0.5, size=text_size, check_overlap = TRUE) +
  geom_text(data=dendro_data$labels,
            aes(x=x, y=y, label=car_name_labels),
            hjust=-0.5, col="blue", size=text_size, check_overlap = TRUE) +
  geom_text(data=dendro_data$labels,
            aes(x=x, y=y, label=as.character(vs_labels)),
            hjust=-0.5, col="red", size=text_size, check_overlap = TRUE) +
  geom_text(data=dendro_data$labels,
            aes(x=x, y=y, label=as.character(carb_labels)),
            hjust=-0.5, col="green", size=text_size, check_overlap = TRUE) +
  coord_flip() +
  scale_y_reverse() +
  theme_minimal() + 
  theme(legend.position = "right")

print(p)
