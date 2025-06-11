
################# Boxplot with Significance Letters ######################
#####################gsw (stomatal conductnace)#########################################

# Load necessary libraries
library(agricolae)
library(ggplot2)
library(ggpubr)
library(dplyr)

# Load the data
setwd("E:/MY RESEARCH/Groundnut- THESIS/SUBMITTED PAPER +ALL DOCUMENTS/All Files for Journal Submission/J Data and Codes/Data")
data1 <- read.csv("Physiolocal Data_22.csv")
names(data1)

# Reorder the levels of the Trt factor
data1$Trt <- factor(data1$Trt, levels = c("D0", "D4", "D10", "I"))

# Perform ANOVA
anova_result <- aov(gsw ~ Trt, data = data1)

# Perform post-hoc test (Tukey's HSD) to get significance letters
tukey_result <- HSD.test(anova_result, "Trt", group = TRUE)

# Extract significance letters and merge with the original data
letters_df <- data.frame(Trt = rownames(tukey_result$groups), 
                         Letters = tukey_result$groups$groups)
data1 <- merge(data1, letters_df, by = "Trt")

# Define custom colors for each treatment (darker shades)
custom_colors <- c("D0" = "#FF8C00",   # Dark Orange
                   "D4" = "#008B8B",   # Deep Teal
                   "D10" = "#005F6B",  # Darker Teal
                   "I" = "#4B0082")    # Deep Purple

# Create the boxplot with significance letters
p <- ggboxplot(data1, x = "Trt", y = "gsw",
               color = "Trt", palette = custom_colors,  # Use custom colors
               add = "jitter", 
               lwd = .6) +  # Increase line thickness
  
  # Add significance letters manually
  geom_text(data = letters_df, aes(x = Trt, y = max(data1$gsw) + 0.02, label = Letters), 
            size = 12, color = "black", vjust = .5) +  # Adjust position of significance letters
  
  # Add axis labels
  labs(
    x = "Treatment",
    y = "Stomatal Conductance\n (mol H₂O m⁻² s⁻¹)"
 
) +  
  
  # Customize the theme to make the background transparent
  theme_minimal() +
  theme(
    panel.background = element_blank(),       # Remove panel background
    plot.background = element_blank(),        # Remove plot background
    panel.grid = element_blank(),             # Remove grid lines
    axis.line = element_line(color = "black"), # Keep axis lines for clarity
    axis.text.x = element_text(color = "black", size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(vjust = 2, size = 20), 
    strip.text = element_text(size = 14),    
    legend.position = "bottom", 
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)    
  ) +
  # Apply custom colors to the legend and boxplots
  scale_color_manual(values = custom_colors)

# Print the plot
print(p)




# Save the plot as PNG with transparent background
ggsave("gsw_22april_M.png", p, width = 8, height = 6, dpi = 300, bg = "transparent")


####################################Vapor Pressure Deficit#############################



# Load necessary libraries
library(agricolae)
library(ggplot2)
library(ggpubr)
library(dplyr)


setwd("E:/MY RESEARCH/Groundnut- THESIS/SUBMITTED PAPER +ALL DOCUMENTS/All Files for Journal Submission/J Data and Codes/Data")
data1 <- read.csv("Physiolocal Data_22.csv")
# Reorder the levels of the Trt factor
data1$Trt <- factor(data1$Trt, levels = c("D0", "D4", "D10", "I"))

# Perform ANOVA
anova_result <- aov(VPDleaf ~ Trt, data = data1)

# Perform post-hoc test (Tukey's HSD) to get significance letters
tukey_result <- HSD.test(anova_result, "Trt", group = TRUE)

# Extract significance letters and merge with the original data
letters_df <- data.frame(Trt = rownames(tukey_result$groups), 
                         Letters = tukey_result$groups$groups)
data1 <- merge(data1, letters_df, by = "Trt")

# Define custom colors for each treatment (darker shades)
custom_colors <- c("D0" = "#FF8C00",   # Dark Orange
                   "D4" = "#008B8B",   # Deep Teal
                   "D10" = "#005F6B",  # Darker Teal
                   "I" = "#4B0082")    # Deep Purple

# Create the boxplot with significance letters
p <- ggboxplot(data1, x = "Trt", y = "VPDleaf",
               color = "Trt", palette = custom_colors,  # Use custom colors
               add = "jitter", 
               lwd = .6) +  # Increase line thickness
  
  # Add significance letters manually
  geom_text(data = letters_df, aes(x = Trt, y = max(data1$VPDleaf) + 0.02, label = Letters), 
            size = 12, color = "black", vjust = .1) +  # Adjust position of significance letters
  
  # Add axis labels
  labs(
    x = "Treatment",
    y = "Leaf Vapor Pressure Deficit\n (kPa⁻¹)"
    
  ) +  
  
  # Customize the theme to make the background transparent
  theme_minimal() +
  theme(
    panel.background = element_blank(),       # Remove panel background
    plot.background = element_blank(),        # Remove plot background
    panel.grid = element_blank(),             # Remove grid lines
    axis.line = element_line(color = "black"), # Keep axis lines for clarity
    axis.text.x = element_text(color = "black", size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(vjust = 2, size = 20), 
    strip.text = element_text(size = 14),    
    legend.position = "bottom", 
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)    
  ) +
  # Apply custom colors to the legend and boxplots
  scale_color_manual(values = custom_colors)

# Print the plot
print(p)




# Save the plot as PNG with transparent background
ggsave("Leaf Vapor Pressure Deficit_22april_M.png", p, width = 8, height = 6, dpi = 300, bg = "transparent")

getwd()

######################Transpiraion Rate##################################################


################# Boxplot with Significance Letters ######################
#####################gsw (stomatal conductnace)

# Load necessary libraries
library(agricolae)
library(ggplot2)
library(ggpubr)
library(dplyr)


# Reorder the levels of the Trt factor
data1$Trt <- factor(data1$Trt, levels = c("D0", "D4", "D10", "I"))

# Perform ANOVA
anova_result <- aov(E_apparent ~ Trt, data = data1)

# Perform post-hoc test (Tukey's HSD) to get significance letters
tukey_result <- HSD.test(anova_result, "Trt", group = TRUE)

# Extract significance letters and merge with the original data
letters_df <- data.frame(Trt = rownames(tukey_result$groups), 
                         Letters = tukey_result$groups$groups)
data1 <- merge(data1, letters_df, by = "Trt")

# Define custom colors for each treatment (darker shades)
custom_colors <- c("D0" = "#FF8C00",   # Dark Orange
                   "D4" = "#008B8B",   # Deep Teal
                   "D10" = "#005F6B",  # Darker Teal
                   "I" = "#4B0082")    # Deep Purple

# Create the boxplot with significance letters
p <- ggboxplot(data1, x = "Trt", y = "E_apparent",
               color = "Trt", palette = custom_colors,  # Use custom colors
               add = "jitter", 
               lwd = .6) +  # Increase line thickness
  
  # Add significance letters manually
  geom_text(data = letters_df, aes(x = Trt, y = max(data1$E_apparent) + 0.02, label = Letters), 
            size = 12, color = "black", vjust = .5, hjust = .7) +  # Adjust position of significance letters
  
  # Add axis labels
  labs(
    x = "Treatment",
    y = "Transpiraion Rate\n (mmol H₂O m⁻² s⁻¹)"
    
  ) +  
  
  # Customize the theme to make the background transparent
  theme_minimal() +
  theme(
    panel.background = element_blank(),       # Remove panel background
    plot.background = element_blank(),        # Remove plot background
    panel.grid = element_blank(),             # Remove grid lines
    axis.line = element_line(color = "black"), # Keep axis lines for clarity
    axis.text.x = element_text(color = "black", size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(vjust = 2, size = 20), 
    strip.text = element_text(size = 14),    
    legend.position = "bottom", 
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)    
  ) +
  # Apply custom colors to the legend and boxplots
  scale_color_manual(values = custom_colors)

# Print the plot
print(p)




# Save the plot as PNG with transparent background
ggsave("Transpiraion Rate_22april_M.png", p, width = 8, height = 6, dpi = 300, bg = "transparent")





###########Quantum Yield of Photosystem II ###########################################


################# Boxplot with Significance Letters ######################
#####################gsw (stomatal conductnace)

# Load necessary libraries
library(agricolae)
library(ggplot2)
library(ggpubr)
library(dplyr)

# Load the data
setwd("E:/MY RESEARCH/Groundnut- THESIS/SUBMITTED PAPER +ALL DOCUMENTS/All Files for Journal Submission/J Data and Codes/Data")
data1 <- read.csv("Physiolocal Data_22.csv")
names(data1)
names(data1)

# Reorder the levels of the Trt factor
data1$Trt <- factor(data1$Trt, levels = c("D0", "D4", "D10", "I"))

# Perform ANOVA
anova_result <- aov(PhiPS2 ~ Trt, data = data1)

# Perform post-hoc test (Tukey's HSD) to get significance letters
tukey_result <- HSD.test(anova_result, "Trt", group = TRUE)

# Extract significance letters and merge with the original data
letters_df <- data.frame(Trt = rownames(tukey_result$groups), 
                         Letters = tukey_result$groups$groups)
data1 <- merge(data1, letters_df, by = "Trt")

# Define custom colors for each treatment (darker shades)
custom_colors <- c("D0" = "#FF8C00",   # Dark Orange
                   "D4" = "#008B8B",   # Deep Teal
                   "D10" = "#005F6B",  # Darker Teal
                   "I" = "#4B0082")    # Deep Purple

# Create the boxplot with significance letters
p <- ggboxplot(data1, x = "Trt", y = "PhiPS2",
               color = "Trt", palette = custom_colors,  # Use custom colors
               add = "jitter", 
               lwd = .6) +  # Increase line thickness
  
  # Add significance letters manually
  geom_text(data = letters_df, aes(x = Trt, y = max(data1$PhiPS2) + 0.02, label = Letters), 
            size = 12, color = "black", vjust = .5) +  # Adjust position of significance letters
  
  # Add axis labels
  labs(
    x = "Treatment",
    y = "Quantum Yield of Photosystem II"
    
  ) +  
  
  # Customize the theme to make the background transparent
  theme_minimal() +
  theme(
    panel.background = element_blank(),       # Remove panel background
    plot.background = element_blank(),        # Remove plot background
    panel.grid = element_blank(),             # Remove grid lines
    axis.line = element_line(color = "black"), # Keep axis lines for clarity
    axis.text.x = element_text(color = "black", size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 20), 
    axis.title.y = element_text(vjust = 2, size = 20), 
    strip.text = element_text(size = 14),    
    legend.position = "bottom", 
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)    
  ) +
  # Apply custom colors to the legend and boxplots
  scale_color_manual(values = custom_colors)

# Print the plot
print(p)




# Save the plot as PNG with transparent background
ggsave("Quantum Yield of Photosystem II_22april_M.png", p, width = 8, height = 6, dpi = 300, bg = "transparent")






