# Load necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)

# Set working directory
setwd("E:/soil mositure 2")

# Read the data from CSV
data <- read_csv("Adjusted_Soil_Moisture.csv")

# Reshape the data to long format
data_long <- data %>%
  pivot_longer(
    cols = -c(plot_no, trt),
    names_to = "Date",
    values_to = "Soil_Moisture"
  )

# Define the custom order for the dates
date_order <- c("4_March", "15_March", "26_March", "7_April", "8_APR", "16_April", "28_April")
data_long$Date <- factor(data_long$Date, levels = date_order)

# Calculate summary statistics
data_summary <- data_long %>%
  group_by(trt, Date) %>%
  summarise(
    mean_soil_moisture = mean(Soil_Moisture, na.rm = TRUE),
    sd_soil_moisture = sd(Soil_Moisture, na.rm = TRUE),
    n = n(),
    se_soil_moisture = sd_soil_moisture / sqrt(n)
  )

# Define custom labels for x-axis
date_labels <- c(
  "4_March" = "4 March",
  "15_March" = "15 March",
  "26_March" = "26 March",
  "7_April" = "7 April",
  "8_APR" = "8 April",
  "16_April" = "16 April",
  "28_April" = "28 April"
)

# Plot with larger font sizes
p <- ggplot(data_summary, aes(x = Date, y = mean_soil_moisture, color = trt, group = trt)) +
  geom_line(size = 0.9) +  # Slightly bolder lines
  geom_point(size = 3) +   # Larger points
  geom_errorbar(aes(ymin = mean_soil_moisture - se_soil_moisture, ymax = mean_soil_moisture + se_soil_moisture), width = 0.2) +
  scale_color_manual(values = c("D0" = "#fb8500", "D4" = "#1dd3b0", "D10" = "#086375", "I" = "#3c1642")) +
  scale_x_discrete(labels = date_labels) +
  theme_minimal(base_size = 16) +  # Set base font size
  labs(
    title = "Soil Moisture Across Dates by Treatment Group",
    x = "Date",
    y = "Soil Moisture (%)",
    color = "Treatment"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right"
  )

# Show plot
print(p)

# Save to PNG
ggsave("soil_moisture_from_csv_larger_fonts.png", plot = p, 
       width = 10, height = 8, dpi = 300,
       units = "in", limitsize = FALSE)