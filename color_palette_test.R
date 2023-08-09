#Testing colors

library(scales)

create_dark_palette <- function(n) {
  # Define the base colors for interpolation (dark blue, dark purple, and dark gray)
  base_colors <- c("#F15A59", "#3D1766", "#00DFA2", "#FBFFB1", "#9A1663", "#ABC9FF")
  
  # Use colorRampPalette to interpolate
  palette <- colorRampPalette(base_colors)(n)
  
  return(palette)
}

# Test the function
n <- 100  # Change this value as you need
dark_colors <- create_dark_palette(n)
show_col(dark_colors)
