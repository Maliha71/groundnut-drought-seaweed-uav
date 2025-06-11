### Corr Sorted Highly sduip #############
# Set working directory (change as needed)
setwd("E:/MY RESEARCH/Groundnut- THESIS/SUBMITTED PAPER +ALL DOCUMENTS/All Files for Journal Submission/J Data and Codes/Data")

# Read the data
data <- read.csv("Correlation & Regression Model.csv")
colnames(data)
str(data)

# Load required libraries
library(Hmisc)
library(dplyr)
library(caret)
library(corrplot)

# Create a data frame for correlation analysis (remove non-relevant columns)
data <- data %>% dplyr::select(-Replication,-Treatment, -plot,  -FyielD_T, - VPDleaf )  # Remove non-relevant columns

# Correlation coefficient with Yield only and sort the parameters in descending order
vi_columns <- names(data)[1:(ncol(data) - 1)]  # Last column should be Dyield
yield_column <- "Dyield"  # Correlation with Dyield

# Calculate correlations
correlations <- sapply(data[vi_columns], function(col) cor(col, data[[yield_column]], use = "complete.obs"))
correlations_sorted <- sort(abs(correlations), decreasing = TRUE)
correlations_sorted  # Display sorted correlations

# Filter parameters with an absolute correlation greater than 0.6
high_correlation_parameters <- names(correlations_sorted[correlations_sorted > 0.6])

# Display the selected parameters
high_correlation_parameters
filtered_data <- data %>% dplyr::select(all_of(high_correlation_parameters), Fbiomass, Dyield)
head(filtered_data)

########## Multicollinearity check ################
# Calculate correlation matrix
corr_matrix <- cor(filtered_data, use = "pairwise.complete.obs")
cor_with_yield <- corr_matrix["Dyield", -which(colnames(corr_matrix) == "Dyield")]
high_corr <- findCorrelation(corr_matrix, cutoff = 0.9, names = TRUE)

# Group highly correlated predictors
high_corr_pairs <- which(abs(corr_matrix) > 0.9, arr.ind = TRUE)
high_corr_pairs <- high_corr_pairs[high_corr_pairs[, 1] != high_corr_pairs[, 2], ]

# Keep only the predictor most correlated with yield in each group
to_keep <- setdiff(names(cor_with_yield), high_corr)
cor_with_yield[order(-abs(cor_with_yield))]  # Sorted by correlation with yield
to_keep

# Final reduced dataset
df_reduced <- data[, c(to_keep, "Dyield")]
str(df_reduced)

# Initial model (linear regression)
initial_model <- lm(Dyield ~ ., data = df_reduced)

# Stepwise regression
stepwise_model <- step(initial_model, direction = "both")
summary(stepwise_model)

############################################# Check VIF for Multicollinearity ##################
library(car)
vif_values <- vif(stepwise_model)  # VIF values to check for multicollinearity
vif_values
library(performance)
performance::model_performance(stepwise_model)

#######################################################################################################
######################## Correlation Plot ############################
filtered_data <- data %>% dplyr::select(all_of(high_correlation_parameters), Fbiomass, Mean_Soil_Moisture, Dyield)
cor_matrix <- cor(filtered_data)
library(corrplot)

# Create and save correlation plot
png("correlation_plot.png", width = 800, height = 800)  # Open PNG device
corrplot(cor_matrix, 
         method = "square", 
         order = "FPC", 
         type = "upper", 
         diag = FALSE)
dev.off()  # Close the device

########################################################################################################
############### Model visualization ##################
# Check if the model is trained and predictions are valid
if (exists("stepwise_model") && length(coef(stepwise_model)) > 0) {
  # Make predictions
  predictions <- predict(stepwise_model, df_reduced)
  
  # Debugging: Check predictions
  print(length(predictions))  # Print the length of predictions
  print(head(predictions))    # Print the first few predictions
  
  # Check if predictions are made and match the number of rows in df_reduced
  if (length(predictions) != nrow(df_reduced)) {
    stop("Mismatch in the number of rows between actual data and predictions.")
  }
  
  residuals <- df_reduced$Dyield - predictions
  rmse <- sqrt(mean(residuals^2))
  
  r_squared <- summary(stepwise_model)$r.squared
  adjusted_r_squared <- summary(stepwise_model)$adj.r.squared
  
  # Create a plot dataframe
  plot_data <- data.frame(
    Actual = df_reduced$Dyield,
    Predicted = predictions,
    Treatment = data$Treatment  # Include treatment information
  )
  
  # Check if the plot data is correctly created
  print(head(plot_data))  # Debug: Check the first few rows of plot_data
  
  # Scatter plot with points colored by treatment
  p <- ggplot(plot_data, aes(x = Actual, y = Predicted, color = Treatment)) +
    geom_point(size = 3) +                              # Points
    geom_smooth(method = "lm", se = FALSE, color = "black") +            # Regression line
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") + # 1:1 line
    labs(
      x = "Actual Yield (Ton/ha)",
      y = "Predicted Yield (Ton/ha)",
      color = "Treatment"
    ) +
    annotate(
      "text", x = 0.5, y = max(plot_data$Predicted) + 0.15,   # Position for R²
      label = paste0("R² = ", round(r_squared, 2)),
      hjust = 0, size = 5, color = "black"
    ) +
    annotate(
      "text", x = 0.5, y = max(plot_data$Predicted), # Position for Adjusted R²
      label = paste0("Adjusted R² = ", round(adjusted_r_squared, 2)),
      hjust = 0, size = 5, color = "black"
    ) +
    annotate(
      "text", x = 0.5, y = max(plot_data$Predicted) - 0.15, # Position for RMSE
      label = paste0("RMSE = ", round(rmse, 2)),
      hjust = 0, size = 5, color = "black"
    ) +
    xlim(0, 3.5) +
    ylim(0, 3.5) +
    theme_bw() +
    theme(
      text = element_text(size = 16),          # Increase overall font size
      axis.title = element_text(size = 16),    # Axis titles
      axis.text = element_text(size = 14)      # Axis text
    )
  
  # Print and save the plot
  print(p)
  ggsave("agMLRmodel_treatment.png", p, width = 8, height = 8, dpi = 300)
  


#######################################################################
################## Model Validation ###################
################ Leave one out cross validation (LOOCV) ###############
# Modify formula to exclude missing variable (MCARI_by_OSAVI)

model_formula <- #####Dyield ~ VPDleaf + Fbiomass + gsw + Podperplant  # Use available variables
train.control <- trainControl(method = "LOOCV")
model.loocv <- train(model_formula, data = df_reduced, method = "lm", trControl = train.control)
print(model.loocv)


#original_data <- read.csv("Correlation & Regression Model.csv")
original_data <- read.csv("Correlation & Regression Model.csv")
plot_data <- data.frame(
  Actual = df_reduced$Dyield,
  Predicted = predictions,
  Treatment = original_data$Treatment
)

library(ggplot2)

p <- ggplot(plot_data, aes(x = Actual, y = Predicted, color = Treatment)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Actual Yield (Ton/ha)",
    y = "Predicted Yield (Ton/ha)",
    color = "Treatment"
  ) +
  annotate("text", x = 0.5, y = max(plot_data$Predicted) + 0.15,
           label = paste0("R² = ", round(summary(stepwise_model)$r.squared, 2)),
           hjust = 0, size = 5, color = "black") +
  annotate("text", x = 0.5, y = max(plot_data$Predicted),
           label = paste0("Adjusted R² = ", round(summary(stepwise_model)$adj.r.squared, 2)),
           hjust = 0, size = 5, color = "black") +
  annotate("text", x = 0.5, y = max(plot_data$Predicted) - 0.15,
           label = paste0("RMSE = ", round(sqrt(mean((plot_data$Actual - plot_data$Predicted)^2)), 2)),
           hjust = 0, size = 5, color = "black") +
  xlim(0, 3.5) + ylim(0, 3.5) +
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

# Show plot
print(p)


