# Assuming unique_values is a vector
unique_values <- c("a", 1, "b", 2)

# Loop through each value in unique_values
for (i in seq_along(unique_values)) {
  val <- unique_values[i]
  
  # Check if the value is numeric
  if(is.numeric(as.numeric(val)) && !is.na(as.numeric(val))) {
    # Convert the numeric value to character and add a space
    unique_values[i] <- paste(as.character(val), " ", sep = "")
  }
}

# Print the modified unique_values
print(unique_values)
