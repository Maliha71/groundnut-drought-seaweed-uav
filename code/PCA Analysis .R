#Package
library(dplyr)
library("FactoMineR")
library("factoextra")
library(tidyverse)
library(RColorBrewer)

#Directory set
setwd("C:/Users/user/Desktop/PAPER FOR JOURNAL WRITE FROM THEISI/All Files for Journal Submission/J Data and Codes/Sudip Sen PCA/")
df <- read.csv("pca1.csv")
str(df)


df.PCA<-df %>% dplyr::select(-Plot, -I, -Treatment)  
str(df.PCA)
#Normalize and plotting
pca_result <- PCA(df.PCA, scale.unit = TRUE, ncp = 10,graph = F)
df <- read.csv("pca1.csv")
col_unique <- df$Treatment  # This is the KEY CHANGE

# Convert to factor if it's not already (recommended)
col_unique <- factor(col_unique)

p <- fviz_pca_biplot(pca_result,
                     geom.ind = "point",
                     pointsize = 2.5,
                     col.var = "black",
                     col.ind = col_unique,
                     pointshape = 19,
                     addEllipses = TRUE,
                     ellipse.level = 0.8,  # Adjust this value (0 to 1)
                     repel = TRUE) # Recommended
p  
ggsave("PCA2.png", p, width = 10, height = 8,dpi=300)




cor_matrix <- cor(df.PCA)
corrplot::corrplot(cor_matrix, method = "circle")

# EIG Value visualization
eig <- get_eigenvalue(pca_result)

eig <- get_eigenvalue(pca_result)

ggplot(data.frame(Dimension = 1:nrow(eig), 
                  CumulativeVariance = cumsum(eig[, "variance.percent"])), 
       aes(x = Dimension, y = CumulativeVariance)) +
  geom_line() +
  geom_point(size = 2) +
  labs(title = "Cumulative Variance Explained",
       x = "Principal Components",
       y = "Cumulative Variance (%)") +
  theme_minimal()



df <- data.frame(Dimension = 1:nrow(eig), 
                 Eigenvalue = eig[, "eigenvalue"],
                 Variance = eig[, "variance.percent"])

p<- ggplot(df, aes(x = Dimension, y = Eigenvalue)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(Variance, 1), "%")), 
            vjust = -0.5, size = 3) +
  labs(title = "Eigenvalues of Principal Components",
       x = "Dimension",
       y = "Eigenvalue") +
  theme_minimal() +
  geom_hline(yintercept = 1, linetype = "longdash",size=0.5, color = "red")
p  
ggsave("Screeplot2.png", p, width = 8, height = 8,dpi=300)

fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))


##### PCA biplot and color
# 1. Define col_unique based on your 'Treatment' column
df <- read.csv("pca1.csv")
col_unique <- df$Treatment  # This is the KEY CHANGE

# Convert to factor if it's not already (recommended)
col_unique <- factor(col_unique)

p <- fviz_pca_biplot(pca_result,
                     geom.ind = "point",
                     pointsize = 2.5,
                     col.var = "black",
                     col.ind = col_unique,
                     pointshape = 19,
                     addEllipses = TRUE,
                     ellipse.level = 0.8,  # Adjust this value (0 to 1)
                     repel = TRUE) # Recommended
p  
ggsave("PCA.png", p, width = 8, height = 8,dpi=300)
