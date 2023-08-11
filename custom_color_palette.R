#Testing colors

library(scales)

create_color_palette <- function(n) {
  # Define the base colors for interpolation 
  base_colors <- c("#F15A59", "#3D1766", "#00DFA2", "#FBFFB1", "#9A1663", "#ABC9FF")
  
  # Use colorRampPalette to interpolate
  palette <- colorRampPalette(base_colors)(n)
  
  return(palette)
}

# Test the function
#n <- 100  # Change this value as you need
#colors <- create_color_palette(n)
#show_col(colors)
<<<<<<< HEAD
=======

#test_palette <- create_color_palette(5)
#print(test_palette)

>>>>>>> TestingPhase
