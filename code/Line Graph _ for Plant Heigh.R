# Load required libraries
library(agricolae)
library(dplyr)
library(ggplot2)
setwd("E:/MY RESEARCH/Groundnut- THESIS/SUBMITTED PAPER +ALL DOCUMENTS/All Files for Journal Submission/J Data and Codes/Data")
# Example data
data <- read.csv("Plant Height.csv") # Replace with your CSV file path
str(data)

# Ensure correct data types
data$Height <- as.numeric(data$Height)
data$Treatment <- as.factor(data$Treatment)

# Define the correct date sequence
date_order <- c("08-Feb", "10-Mar", "30-Mar", "08-Apr", "30-May")

# Convert Date column to a factor with the correct order
data$Date <- factor(data$Date, levels = date_order)

# Map dates to Date values
date_to_Date <- c("08-Feb" = 25, "10-Mar" = 55, "30-Mar" = 75,  "08-Apr" = 85, "30-May" = 135)

# Add a new Date column to the data
data$Date <- as.factor(sapply(data$Date, function(x) date_to_Date[x]))

# Perform Tukey's HSD Test and generate lettering
letters_data <- data %>%
  group_by(Date) %>%
  do({
    # Perform ANOVA
    model <- aov(Height ~ Treatment, data = .)
    
    # Tukey's HSD Test
    tukey_results <- HSD.test(model, "Treatment")
    
    # Extract letters
    groups <- tukey_results$groups
    groups <- as.data.frame(groups)
    groups$Treatment <- rownames(groups)
    groups$Date <- unique(.$Date)
    
    # Rename columns for clarity
    groups <- groups %>%
      select(Date, Treatment, letter = groups)
    
    groups
  })

# Merge letters data with the original data
data <- data %>%
  left_join(letters_data, by = c("Date", "Treatment"))

# Calculate means and standard deviations for error bars
summary_data <- data %>%
  group_by(Date, Treatment) %>%
  summarise(
    mean_height = mean(Height, na.rm = TRUE),
    sd_height = sd(Height, na.rm = TRUE),
    n = n(),
    se_height = sd_height / sqrt(n)  # Standard error
  )

# Merge lettering with summary data
summary_data <- summary_data %>%
  left_join(letters_data, by = c("Date", "Treatment"))

# Custom color palette for each Treatment (Treatment)
custom_colors <- c("I" = "#3c1642", 
                   "D10" = "#086375", 
                   "D4" = "#1dd3b0", 
                   "D0" = "#fb8500")

# Plot the data using ggplot2 with custom colors
p<- ggplot(summary_data, aes(x = Date, y = mean_height, color = Treatment, group = Treatment)) +
  geom_line(size = 1.2) +  # Line graph
  geom_point(size = 2) +  # Points
  geom_errorbar(aes(ymin = mean_height - se_height, ymax = mean_height + se_height),
                width = 0.05) +  # Error bars
  geom_text(aes(label = letter, y = mean_height + 3), 
            vjust = 1.5, hjust = -1, size = 6) +  # Lettering
  labs(x = "Days After Sowing (Date)", y = "Plant Height (cm)") +
  theme_minimal() +
  scale_color_manual(values = custom_colors) +
  theme(
    text = element_text(size = 22),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 22, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
p
#ggsave("Height.png", plot = p, width = 10, height = 8, dpi = 300)
ggsave("ht.png", p, width = 8, height = 6, dpi = 300, bg = "transparent")
