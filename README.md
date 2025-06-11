# 🌿 Dataset for Integrating Seaweed Biostimulant and UAV-Based Sensing for Precision Drought Management in Groundnut

This repository contains all datasets and R scripts used in the study:  
**"Integrating Seaweed Biostimulant and UAV-Based Sensing for Precision Drought Management in Groundnut"**

The project combines field-collected agronomic and physiological data with UAV-derived remote sensing indices to assess and model drought stress in groundnut cultivation under bio-stimulant treatment.

---

## 📁 Repository Structure

### 🔸 `/data/` Folder (CSV Files)

| Filename | Content |
|----------|---------|
| **Agronomic Data.csv** | Growth/yield metrics like pod yield, seed weight, and plant height. |
| **Correlation & Regression Model.csv** | Combined dataset for modeling vegetation indices against yield and traits. |
| **PCA Analysis.csv** | Dataset for multivariate analysis of treatment effects and indices. |
| **Physiolocal Data.csv** | Stomatal conductance , transpiration rates, Leaf vapor pressure deficit  and photosynthetic efficiency data. |
| **Predicted Yield , Actual Yield , Absolute Error Data.csv** | Prediction model outputs. |
| **Remote Sensing Data.csv** | Raw and processed UAV spectral data and vegetation indices. |
| **Soil_Moisture.csv** | Soil moisture at various time points. |
| **Temporal Plant Height.csv** | Longitudinal height measurements. |
| **Weather Data.csv** | Daily rainfall and temperature readings. |
| **Soil Test Report.xlsx** | Soil analysis report. 

---

## 🧠 R Scripts (`/code/` Folder)

| Script | Description |
|--------|-------------|
| **Bar Graph with Lettering for Agronomic Data.R** | Bar plot of yield/treatment data with significance lettering. |
| **Boxplot for Vegetation Indices (non-transparent).R** | Boxplots comparing VIs across treatments. |
| **Boxplot with jitter for Physiological Data.R** | Jittered plots for stomatal conductance , transpiration rates, Leaf vapor pressure deficit and photosynthetic efficiency by treatment. |
| **Line Graph for Plant Height.R** | Time-series visualization of plant height. |
| **Line Graph Soil Moisture.R** | Soil moisture trend graphs across dates and treatments. |
| **PCA Analysis.R** | Runs PCA on agronomic + spectral traits and visualizes clusters. |
| **Predicted, Actual and Error Map.R** | Plots actual vs. predicted yield and  prediction errors map visualization. |
| **Regression Model and Correlation Analysis.R** | Stepwise regression modeling and correlation analysis between traits and VIs. |

---

## 📦 Requirements

You need **R ≥ 4.1.0** and the following packages:

```r
install.packages(c("readr", "readxl", "ggplot2", "dplyr", "tidyr", "factoextra"))
