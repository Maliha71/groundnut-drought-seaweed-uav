#All indivces showing the significnat lettering === 

# Set working directory
setwd("E:/MY RESEARCH/Groundnut- THESIS/SUBMITTED PAPER +ALL DOCUMENTS/All Files for Journal Submission/J Data and Codes/Data")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(agricolae)

# Load dataset
data <- read.csv("Remote Sensing Data.csv")

# Filter out unwanted DAS dates
excluded_dates <- c("05-Feb", "24-Feb", "27-Feb", "03-Apr", "17-Apr", "07-May", "15-May", "21-May", "29-May")
data <- data %>% filter(!DAS %in% excluded_dates)

# ✅ Correctly defined selected index columns
index_cols <- c( "ARI2", "ARVI",  "MSAVI", "NDVI", "OSAVI",  "RDVI", "PSRI", "TVI", "bNDVI", "ENDVI", "MCARI", "MGVRI", "MSR", "RVI", "SR", "TCARI",  "TNDGR",  "VARI"
)

#all index_cols <- c("Treatment", "MGVRI", "SR", "NDVI", "MSAVI", "SAVI", "OSAVI", "ARVI", "EVI", "GCI", "SIPI", 
#               "CI.rededge.", "NDRE", "CVI", "gNDVI", "bNDVI", "PI", "VARI", "ENDVI", "GDVI", "PSRI", 
#              "MSRGR", "TNDGR", "MSR", "MCARI", "TCARI", "MTCI", "TCARI.OSAVI", "MCARI.OSAVI", 
#             "RVI", "DVI", "ARI", "ARI2", "SRPI", "NPCI", "VDVI", "WDRVI", "TVI", "RDVI", "NDWI")

#All indivces showing the significnat lettering === "ARI2", "ARVI", "NDVI", "MSAVI", "NDVI", "OSAVI", "SAVI", "RDVI", "PSRI", "TVI", "bNDVI", "WDRVI", "ENDVI", "MCARI", "MCARI.OSAVI", "MGVRI", "MSR", "RVI", "SR", "TCARI", "TCARI.OSAVI", "TNDGR", "VDVI", "VARI"

# Select and reshape data
data <- data %>% select(Treatment, all_of(index_cols)) %>% na.omit()
data$Treatment <- factor(data$Treatment, levels = c("D0", "D4", "D10", "I"))

# Convert to long format
# Convert to long format
long_data <- pivot_longer(data, cols = -Treatment, names_to = "Index", values_to = "Value")



# ---- Tukey Lettering Per Index ---- #
tukey_letters <- lapply(unique(long_data$Index), function(index_name) {
  subset_data <- long_data %>% filter(Index == index_name)
  model <- aov(Value ~ Treatment, data = subset_data)
  tukey <- HSD.test(model, "Treatment", group = TRUE)
  groups <- tukey$groups
  data.frame(
    Index = index_name,
    Treatment = rownames(groups),
    Letter = groups$groups
  )
})

# Combine all Tukey results
tukey_df <- bind_rows(tukey_letters)

# Determine max y values for label placement
label_positions <- long_data %>%
  group_by(Index, Treatment) %>%
  summarise(y = max(Value, na.rm = TRUE) + 0.05, .groups = "drop")

# Merge for positioning text labels
label_df <- left_join(tukey_df, label_positions, by = c("Index", "Treatment"))

# ---- Final Plot ---- #
final_plot <- ggplot(long_data, aes(x = Treatment, y = Value, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = label_df,
            aes(x = Treatment, y = y, label = Letter),
            inherit.aes = FALSE,
            size = 12) +
  facet_wrap(~ Index, scales = "free_y", ncol = 5) +
  scale_fill_manual(values = c("#fb8500", "#1dd3b0", "#086375", "#3c1642")) +
  labs(x = "Treatment", y = "Index Value", fill = "Treatment") +
  theme_minimal(base_size = 15) +
  theme(
    strip.background = element_blank(),  # Removes facet header box
    strip.text = element_text(size = 35, face = "bold", color = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 35),
    axis.title = element_text(size = 35),
    legend.title = element_text(size = 40),
    legend.text = element_text(size = 40),
    legend.position = "bottom",
    panel.spacing = unit(2, "cm"),
    plot.margin = margin(20, 20, 20, 20)
    
  )


# Display and save the plot
print(final_plot)
ggsave("(all-4) indivces for supplementary.png", plot = final_plot, width = 22, height = 16, dpi = 300)






################################4 Main Boxplots for Main Paper#################################
# Set working directory
setwd("E:/MY RESEARCH/Groundnut- THESIS/SUBMITTED PAPER +ALL DOCUMENTS/All Files for Journal Submission/J Data and Codes/Data")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(agricolae)

# Load dataset
data <- read.csv("Remote Sensing Data.csv")

# Filter out unwanted DAS dates
excluded_dates <- c("05-Feb", "24-Feb", "27-Feb", "03-Apr", "17-Apr", "07-May", "15-May", "21-May", "29-May")
data <- data %>% filter(!DAS %in% excluded_dates)

# ✅ Correctly defined selected index columns
index_cols <- c( "NDVI", "SAVI",  "WDRVI",  "VDVI"
                 )

#all index_cols <- c("Treatment", "MGVRI", "SR", "NDVI", "MSAVI", "SAVI", "OSAVI", "ARVI", "EVI", "GCI", "SIPI", 
#               "CI.rededge.", "NDRE", "CVI", "gNDVI", "bNDVI", "PI", "VARI", "ENDVI", "GDVI", "PSRI", 
#              "MSRGR", "TNDGR", "MSR", "MCARI", "TCARI", "MTCI", "TCARI.OSAVI", "MCARI.OSAVI", 
#             "RVI", "DVI", "ARI", "ARI2", "SRPI", "NPCI", "VDVI", "WDRVI", "TVI", "RDVI", "NDWI")

#All indivces showing the significnat lettering === "ARI2", "ARVI", "NDVI", "MSAVI", "NDVI", "OSAVI", "SAVI", "RDVI", "PSRI", "TVI", "bNDVI", "WDRVI", "ENDVI", "MCARI", "MCARI.OSAVI", "MGVRI", "MSR", "RVI", "SR", "TCARI", "TCARI.OSAVI", "TNDGR", "VDVI", "VARI"

# Select and reshape data
data <- data %>% select(Treatment, all_of(index_cols)) %>% na.omit()
data$Treatment <- factor(data$Treatment, levels = c("D0", "D4", "D10", "I"))

# Convert to long format
# Convert to long format
long_data <- pivot_longer(data, cols = -Treatment, names_to = "Index", values_to = "Value")

# Set custom order for vegetation indices
long_data$Index <- factor(long_data$Index, levels = c("NDVI", "SAVI", "WDRVI", "VDVI"))

# ---- Tukey Lettering Per Index ---- #
tukey_letters <- lapply(unique(long_data$Index), function(index_name) {
  subset_data <- long_data %>% filter(Index == index_name)
  model <- aov(Value ~ Treatment, data = subset_data)
  tukey <- HSD.test(model, "Treatment", group = TRUE)
  groups <- tukey$groups
  data.frame(
    Index = index_name,
    Treatment = rownames(groups),
    Letter = groups$groups
  )
})

# Combine all Tukey results
tukey_df <- bind_rows(tukey_letters)

# Determine max y values for label placement
label_positions <- long_data %>%
  group_by(Index, Treatment) %>%
  summarise(y = max(Value, na.rm = TRUE) + 0.05, .groups = "drop")

# Merge for positioning text labels
label_df <- left_join(tukey_df, label_positions, by = c("Index", "Treatment"))

# ---- Final Plot ---- #
final_plot <- ggplot(long_data, aes(x = Treatment, y = Value, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = label_df,
            aes(x = Treatment, y = y, label = Letter),
            inherit.aes = FALSE,
            size = 12) +
  facet_wrap(~ Index, scales = "free_y", ncol = 5) +
  scale_fill_manual(values = c("#fb8500", "#1dd3b0", "#086375", "#3c1642")) +
  labs(x = "Treatment", y = "Index Value", fill = "Treatment") +
  theme_minimal(base_size = 15) +
  theme(
    strip.background = element_rect(fill = "#d9e3f0", color = "black", linewidth = 1),
    strip.text = element_text(size = 35, face = "bold", color = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 35),
    axis.title = element_text(size = 35),
    legend.title = element_text(size = 35),
    legend.text = element_text(size = 35),
    legend.position = "bottom",
    panel.spacing = unit(2, "cm"),
    plot.margin = margin(20, 20, 20, 20),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
  )




# Display and save the plot
print(final_plot)
ggsave("4 indivces for paper.png", plot = final_plot, width = 22, height = 16, dpi = 300)

