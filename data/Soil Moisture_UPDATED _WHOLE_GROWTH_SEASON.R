# Load necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)

# Set working directory
setwd("E:/")

# Read the data from CSV
data <- read_csv("Soil_Moisture_till_30_may.csv")

# Clean column names to remove accidental spaces
names(data) <- trimws(names(data))

# Reshape the data to long format
data_long <- data %>%
  pivot_longer(
    cols = -c(plot_no, trt),
    names_to = "Date",
    values_to = "Soil_Moisture"
  )

# Define the custom order for the dates
date_order <- c(
  "20_Jan",
  "05_Feb",
  "4_March",
  "15_March",
  "26_March",
  "7_April",
  "8_APR",
  "16_April",
  "28_April",
  "06_May",
  "11_May"
)

data_long$Date <- factor(data_long$Date, levels = date_order)

# Calculate summary statistics
data_summary <- data_long %>%
  group_by(trt, Date) %>%
  summarise(
    mean_soil_moisture = mean(Soil_Moisture, na.rm = TRUE),
    sd_soil_moisture = sd(Soil_Moisture, na.rm = TRUE),
    n = sum(!is.na(Soil_Moisture)),
    se_soil_moisture = sd_soil_moisture / sqrt(n),
    .groups = "drop"
  )

# Define custom labels for x-axis
date_labels <- c(
  "20_Jan" = "20 January",
  "05_Feb" = "5 February",
  "4_March" = "4 March",
  "15_March" = "15 March",
  "26_March" = "26 March",
  "7_April" = "7 April",
  "8_APR" = "8 April",
  "16_April" = "16 April",
  "28_April" = "28 April",
  "06_May" = "6 May",
  "11_May" = "11 May"
)

# Plot with larger font sizes and adjusted Y-axis
p <- ggplot(data_summary, aes(x = Date, y = mean_soil_moisture, color = trt, group = trt)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = mean_soil_moisture - se_soil_moisture,
      ymax = mean_soil_moisture + se_soil_moisture
    ),
    width = 0.2
  ) +
  scale_color_manual(
    values = c(
      "D0" = "#fb8500",
      "D4" = "#1dd3b0",
      "D10" = "#086375",
      "I" = "#3c1642"
    )
  ) +
  scale_x_discrete(labels = date_labels) +
  # Set Y-axis limits from 10 to 40 with breaks every 5 units
  scale_y_continuous(limits = c(10, 40), breaks = seq(10, 40, by = 5)) + 
  theme_minimal(base_size = 16) +
  labs(
    x = "Date",
    y = "Soil moisture (%)",
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
ggsave(
  "soil_moisture_till_30_may.png",
  plot = p,
  width = 10,
  height = 8,
  dpi = 300,
  units = "in",
  limitsize = FALSE
)