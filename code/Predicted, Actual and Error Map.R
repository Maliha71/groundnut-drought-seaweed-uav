########## Error Map (Fixed) ##############
########## Error Map (Red-White Gradient) ##############
library(sf)
library(dplyr)
library(ggplot2)

# Load the shapefile
shapefile_path <- "E:/MY RESEARCH/Groundnut- THESIS/SUBMITTED PAPER +ALL DOCUMENTS/All Files for Journal Submission/J Data and Codes/Data/Shapefile/plot2.shp"
plots <- st_read(shapefile_path)

# Check structure
print(names(plots))  # Should include 'id'

# Set working directory
setwd("E:/MY RESEARCH/Groundnut- THESIS/SUBMITTED PAPER +ALL DOCUMENTS/All Files for Journal Submission/J Data and Codes/Data")

# Load the CSV with error values
yield_data <- read.csv("Predicted Yield.csv")
print(names(yield_data))  # Should show: "id", "Error"

# Merge shapefile with yield error data
merged_data <- plots %>%
  left_join(yield_data, by = "id")

# Plot the absolute error map with red-white gradient
library(grid)
library(grid)
library(grid)

p <- ggplot(data = merged_data) +
  geom_sf(aes(fill = Absolute.Error)) +
  scale_fill_gradient(
    low = "white", 
    high = "red", 
    name = "Absolute Error (t/ha)",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(1, "cm"),
      barheight = unit(5, "cm")
    )
  ) +
  labs(title = "Absolute Error Map ") +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    legend.title = element_text(size = 24, face = "bold", vjust = 3),  # 👈 KEY LINE
    legend.text = element_text(size = 24)
  )


# Display and save the plot
print(p)
ggsave("Abs_error_map_red_white.png", plot = p, width = 10, height = 8, dpi = 300)








###############Actual Yield RRR############
# Load necessary libraries
library(sf)
library(dplyr)
library(ggplot2)

# Load the shapefile
shapefile_path <- "E:/MY RESEARCH/Groundnut/Groundnut/Groundnut Drone Data/Shapefiles/Plot/plot2.shp"
plots <- st_read(shapefile_path)

# Inspect the structure of the shapefile to get column names
print(names(plots))  # This will display the column names in the shapefile

# Set working directory for saving the plot
setwd("E:/MY RESEARCH/Groundnut/Predicted Yield Map")

# Load the CSV file with predicted yield values
yield_data <- read.csv("Absolute Error in dry yield prediciton.csv")

# Print the structure of the yield data to check column names
print(names(yield_data))

# Assuming the plot identifier in the shapefile is called 'plot_id' and in the yield data it's 'plot_id'
# Adjust these names based on what you find from the print statements
merged_data <- plots %>%
  left_join(yield_data, by = "id")  # Replace 'plot_id' with the actual column name

# Plotting the yield map
ggplot(data = merged_data) +
  geom_sf(aes(fill = Actual_Yield)) +  # Use the DYield column for filling
  scale_fill_viridis_c(option = "plasma") +  # Optional: choose a color scale
  labs(title = "Actual Yield Map",
       fill = "Actual Yield") +
  theme_minimal()

# Save the plot as an image
ggsave("Actual_Yield_Map_R.png", width = 10, height = 8)

################# Actual Yield Map__  mmmmmmmm  mmmm  MMM ###############################

library(sf)
library(dplyr)
library(ggplot2)
library(grid)

# Load the shapefile
shapefile_path <- "E:/MY RESEARCH/Groundnut- THESIS/SUBMITTED PAPER +ALL DOCUMENTS/All Files for Journal Submission/J Data and Codes/Data/Shapefile/plot2.shp"
plots <- st_read(shapefile_path)

# Set working directory
setwd("E:/MY RESEARCH/Groundnut- THESIS/SUBMITTED PAPER +ALL DOCUMENTS/All Files for Journal Submission/J Data and Codes/Data")

# Load the CSV with actual yield values
yield_data <- read.csv("Predicted Yield.csv")
print(names(yield_data))  # Should show: "id", "Actual.Dry.Yield"

# Merge the shapefile with yield data
merged_data <- plots %>%
  left_join(yield_data, by = "id")

# Plot Actual Dry Yield
p <- ggplot(data = merged_data) +
  geom_sf(aes(fill = Actual.Dry.Yield)) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Actual Yield (t/ha)",
    breaks = seq(1, 4.5, by = 0.5),   # 👈 Add 0.5 step breaks
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(1, "cm"),
      barheight = unit(5, "cm")
    )
  ) +
  labs(title = "Actual Dry Yield Map") +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 24, face = "bold", vjust = 3),
    legend.text = element_text(size = 24)
  )
p
# Save the plot
ggsave("Actual_Dry_Yield_Map.png", plot = p, width = 10, height = 8, dpi = 300)





#################### Predicted Yield Map ################
library(sf)
library(dplyr)
library(ggplot2)
library(grid)

# Load the shapefile
shapefile_path <- "E:/MY RESEARCH/Groundnut- THESIS/SUBMITTED PAPER +ALL DOCUMENTS/All Files for Journal Submission/J Data and Codes/Data/Shapefile/plot2.shp"
plots <- st_read(shapefile_path)

# Set working directory
setwd("E:/MY RESEARCH/Groundnut- THESIS/SUBMITTED PAPER +ALL DOCUMENTS/All Files for Journal Submission/J Data and Codes/Data")

# Load the CSV with predicted yield values
yield_data <- read.csv("Predicted Yield.csv")
print(names(yield_data))  # Should show: "id", "Error", "Predicted.Dry.Yield"

# Merge the shapefile with prediction data
merged_data <- plots %>%
  left_join(yield_data, by = "id")

# Plot Predicted Dry Yield
p <- ggplot(data = merged_data) +
  geom_sf(aes(fill = Predicted.Dry.Yield)) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Predicted Yield (t/ha)",
    breaks = seq(1, 4.5, by = 0.5),  # 👈 This is the only change (adds 0.5-step legend)
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(1, "cm"),
      barheight = unit(5, "cm")
    )
  ) +
  labs(title = "Predicted Dry Yield Map") +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 24, face = "bold", vjust = 3),
    legend.text = element_text(size = 24)
  )

# Display and save the plot
print(p)
ggsave("Predicted_Dry_Yield_Map.png", plot = p, width = 10, height = 8, dpi = 300)




