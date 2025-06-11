
#################################Fresh Yiled#############################################

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(agricolae)

# Set working directory and load data
setwd("C:/Users/user/Desktop/Bar with Lettering")
data <- read.csv("Agronomic Data.csv")
names(data)
# Factorize variables
data$Treatment <- factor(data$Treatment, levels = c("D0", "D4", "D10", "I"))
data$Replication <- as.factor(data$Replication)

# Calculate mean and standard error
summary_stats <- data %>%
  group_by(Treatment) %>%
  summarise(
    mean_Fresh_Yield = mean(Fresh.Yield, na.rm = TRUE),
    se_Fresh_Yield = sd(Fresh.Yield, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA for RCBD
model <- aov(Fresh.Yield ~ Replication + Treatment, data = data)
summary(model)

# Perform Duncan's Multiple Range Test
duncan <- duncan.test(
  y = model,
  trt = "Treatment",
  group = TRUE,
  console = TRUE
)

# Merge significance letters from Duncan test
summary_stats <- summary_stats %>%
  left_join(
    data.frame(Treatment = rownames(duncan$groups), Letter = duncan$groups$groups),
    by = "Treatment"
  )

# Reorder for consistent plotting
summary_stats$Treatment <- factor(summary_stats$Treatment, levels = c("D0", "D4", "D10", "I"))

# Create bar graph
Bar <- ggplot(summary_stats, aes(x = Treatment, y = mean_Fresh_Yield, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_Fresh_Yield - se_Fresh_Yield, ymax = mean_Fresh_Yield + se_Fresh_Yield),
                width = 0.2, size = 0.8) +
  geom_text(aes(label = Letter, y = mean_Fresh_Yield + se_Fresh_Yield + 0.1),
            size = 8, fontface = "bold", vjust = 0) +
  scale_fill_manual(values = c("D0" = "#fb8500", "D4" = "#1dd3b0", "D10" = "#086375", "I" = "#3c1642")) +
  labs(
    x = "Treatment",
    y = "Fresh Yield (t/ha)",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 22),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 22, angle = 45, hjust = .5),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.position = "bottom"
  )

# Show and save the plot
print(Bar)
ggsave("Fresh_Yield_by_Treatment_Duncan.png", plot = Bar, width = 8, height = 8, dpi = 300)




#########################Dry yiield#################################################


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(agricolae)

# Set working directory and load data
setwd("C:/Users/user/Desktop/Bar with Lettering")
data <- read.csv("Groundnut data.csv")

# Factorize variables
data$Treatment <- factor(data$Treatment, levels = c("D0", "D4", "D10", "I"))
data$Replication <- as.factor(data$Replication)

# Calculate mean and standard error
summary_stats <- data %>%
  group_by(Treatment) %>%
  summarise(
    mean_Fresh_Yield = mean(Yield, na.rm = TRUE),
    se_Fresh_Yield = sd(Yield, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA for RCBD
model <- aov(Yield ~ Replication + Treatment, data = data)
summary(model)

# Perform Duncan's Multiple Range Test
duncan <- duncan.test(
  y = model,
  trt = "Treatment",
  group = TRUE,
  console = TRUE
)

# Merge significance letters from Duncan test
summary_stats <- summary_stats %>%
  left_join(
    data.frame(Treatment = rownames(duncan$groups), Letter = duncan$groups$groups),
    by = "Treatment"
  )

# Reorder for consistent plotting
summary_stats$Treatment <- factor(summary_stats$Treatment, levels = c("D0", "D4", "D10", "I"))

# Create bar graph
Bar <- ggplot(summary_stats, aes(x = Treatment, y = mean_Fresh_Yield, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_Fresh_Yield - se_Fresh_Yield, ymax = mean_Fresh_Yield + se_Fresh_Yield),
                width = 0.2, size = 0.8) +
  geom_text(aes(label = Letter, y = mean_Fresh_Yield + se_Fresh_Yield + 0.1),
            size = 8, fontface = "bold", vjust = 0) +
  scale_fill_manual(values = c("D0" = "#fb8500", "D4" = "#1dd3b0", "D10" = "#086375", "I" = "#3c1642")) +
  labs(
    x = "Treatment",
    y = "Dry Yield (t/ha)",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 22),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 22, angle = 45, hjust = .5),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.position = "bottom"
  )

# Show and save the plot
print(Bar)
ggsave("Dry_Yield_by_Treatment_Duncan.png", plot = Bar, width = 8, height = 8, dpi = 300)


#################################frshbiomass#################################


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(agricolae)

# Set working directory and load data
setwd("C:/Users/user/Desktop/Bar with Lettering")
data <- read.csv("Groundnut data.csv")

# Factorize variables
data$Treatment <- factor(data$Treatment, levels = c("D0", "D4", "D10", "I"))
data$Replication <- as.factor(data$Replication)

# Calculate mean and standard error
summary_stats <- data %>%
  group_by(Treatment) %>%
  summarise(
    mean_Fresh_Fbiomass = mean(Fbiomass, na.rm = TRUE),
    se_Fresh_Fbiomass = sd(Fbiomass, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA for RCBD
model <- aov(Fbiomass ~ Replication + Treatment, data = data)
summary(model)

# Perform Duncan's Multiple Range Test
duncan <- duncan.test(
  y = model,
  trt = "Treatment",
  group = TRUE,
  console = TRUE
)

# Merge significance letters from Duncan test
summary_stats <- summary_stats %>%
  left_join(
    data.frame(Treatment = rownames(duncan$groups), Letter = duncan$groups$groups),
    by = "Treatment"
  )

# Reorder for consistent plotting
summary_stats$Treatment <- factor(summary_stats$Treatment, levels = c("D0", "D4", "D10", "I"))

# Create bar graph
Bar <- ggplot(summary_stats, aes(x = Treatment, y = mean_Fresh_Fbiomass, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_Fresh_Fbiomass - se_Fresh_Fbiomass, ymax = mean_Fresh_Fbiomass + se_Fresh_Fbiomass),
                width = 0.2, size = 0.8) +
  geom_text(aes(label = Letter, y = mean_Fresh_Fbiomass + se_Fresh_Fbiomass + 0.1),
            size = 8, fontface = "bold", vjust = -.5) +
  scale_fill_manual(values = c("D0" = "#fb8500", "D4" = "#1dd3b0", "D10" = "#086375", "I" = "#3c1642")) +
  labs(
    x = "Treatment",
    y = "Fresh Fbiomass (t/ha)",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 22),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 22, angle = 45, hjust = .5),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.position = "bottom"
  )

# Show and save the plot
print(Bar)
ggsave("Fbiomass_by_Treatment_Duncan.png", plot = Bar, width = 8, height = 8, dpi = 300)


#########################Thousand seed wt############################################

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(agricolae)

# Set working directory and load data
setwd("C:/Users/user/Desktop/Bar with Lettering")
data <- read.csv("Groundnut data.csv")

# Factorize variables
data$Treatment <- factor(data$Treatment, levels = c("D0", "D4", "D10", "I"))
data$Replication <- as.factor(data$Replication)

# Calculate mean and standard error
summary_stats <- data %>%
  group_by(Treatment) %>%
  summarise(
    mean_Fresh_Seedwt = mean(Seedwt, na.rm = TRUE),
    se_Fresh_Seedwt = sd(Seedwt, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA for RCBD
model <- aov(Seedwt ~ Replication + Treatment, data = data)
summary(model)

# Perform Duncan's Multiple Range Test
duncan <- duncan.test(
  y = model,
  trt = "Treatment",
  group = TRUE,
  console = TRUE
)

# Merge significance letters from Duncan test
summary_stats <- summary_stats %>%
  left_join(
    data.frame(Treatment = rownames(duncan$groups), Letter = duncan$groups$groups),
    by = "Treatment"
  )

# Reorder for consistent plotting
summary_stats$Treatment <- factor(summary_stats$Treatment, levels = c("D0", "D4", "D10", "I"))

# Create bar graph
Bar <- ggplot(summary_stats, aes(x = Treatment, y = mean_Fresh_Seedwt, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_Fresh_Seedwt - se_Fresh_Seedwt, ymax = mean_Fresh_Seedwt + se_Fresh_Seedwt),
                width = 0.2, size = 0.8) +
  geom_text(aes(label = Letter, y = mean_Fresh_Seedwt + se_Fresh_Seedwt + 0.1),
            size = 8, fontface = "bold", vjust = -.5) +
  scale_fill_manual(values = c("D0" = "#fb8500", "D4" = "#1dd3b0", "D10" = "#086375", "I" = "#3c1642")) +
  labs(
    x = "Treatment",
    y = "Thousand Seed Weight (g)",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 22),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 22, angle = 45, hjust = .5),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.position = "bottom"
  )

# Show and save the plot
print(Bar)
ggsave("tseedwt_by_Treatment_Duncan.png", plot = Bar, width = 8, height = 8, dpi = 300)


#########################Pods Per Plant#################################################
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(agricolae)

# Set working directory and load data
setwd("C:/Users/user/Desktop/Bar with Lettering")
data <- read.csv("Groundnut data.csv")
names(data)
# Factorize variables
data$Treatment <- factor(data$Treatment, levels = c("D0", "D4", "D10", "I"))
data$Replication <- as.factor(data$Replication)

# Calculate mean and standard error
summary_stats <- data %>%
  group_by(Treatment) %>%
  summarise(
    mean_Fresh_Pod.plant = mean(Pod.plant, na.rm = TRUE),
    se_Fresh_Pod.plant = sd(Pod.plant, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA for RCBD
model <- aov(Pod.plant ~ Replication + Treatment, data = data)
summary(model)

# Perform Duncan's Multiple Range Test
duncan <- duncan.test(
  y = model,
  trt = "Treatment",
  group = TRUE,
  console = TRUE
)

# Merge significance letters from Duncan test
summary_stats <- summary_stats %>%
  left_join(
    data.frame(Treatment = rownames(duncan$groups), Letter = duncan$groups$groups),
    by = "Treatment"
  )

# Reorder for consistent plotting
summary_stats$Treatment <- factor(summary_stats$Treatment, levels = c("D0", "D4", "D10", "I"))

# Create bar graph
Bar <- ggplot(summary_stats, aes(x = Treatment, y = mean_Fresh_Pod.plant, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_Fresh_Pod.plant - se_Fresh_Pod.plant, ymax = mean_Fresh_Pod.plant + se_Fresh_Pod.plant),
                width = 0.2, size = 0.8) +
  geom_text(aes(label = Letter, y = mean_Fresh_Pod.plant + se_Fresh_Pod.plant + 0.1),
            size = 8, fontface = "bold", vjust = -.5) +
  scale_fill_manual(values = c("D0" = "#fb8500", "D4" = "#1dd3b0", "D10" = "#086375", "I" = "#3c1642")) +
  labs(
    x = "Treatment",
    y = "Pods per Plant",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 22),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 22, angle = 45, hjust = .5),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.position = "bottom"
  )

# Show and save the plot
print(Bar)
ggsave("Pod.plant_by_Treatment_Duncan.png", plot = Bar, width = 8, height = 8, dpi = 300)

