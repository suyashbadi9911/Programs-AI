# Load necessary libraries
library(ggplot2)
library(cluster)
library(factoextra)
library(readxl)

# Load the dataset (Ensure correct file path)
df <- read_excel("C:/Users/DELL/Documents/DSV/datasets/grades_km_input.xlsx")

# Print dataset information to verify it's loaded
print(head(df))  
print(names(df))  
print(str(df))   

# Selecting only numeric grade columns
X <- df[, c("English", "Math", "Science")]
X <- as.matrix(X)  # Convert to numeric matrix

# Ensure X contains only numeric values
if (!is.numeric(X)) {
  stop("Error: Non-numeric values detected in X. Please check dataset.")
}

print(str(X))  # Check if X is numeric

# Apply K-Means clustering
set.seed(42)
kmeans_result <- kmeans(X, centers = 3, nstart = 25)

# Verify cluster assignments
print(kmeans_result$cluster)

# Add cluster labels to the dataset
df$Cluster <- as.factor(kmeans_result$cluster)
print(head(df))  # Verify cluster assignment

# Print cluster means
print("Cluster Centers (Mean Scores per Subject):")
print(kmeans_result$centers)

# Perform PCA for visualization
pca_result <- prcomp(X, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x)
pca_data$Cluster <- df$Cluster

# Project cluster centers into PCA space
centers_pca <- predict(pca_result, newdata = kmeans_result$centers)
centers_pca <- as.data.frame(centers_pca)
colnames(centers_pca)[1:2] <- c("PC1", "PC2")  # Rename for clarity

# Plot the clusters
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3) +
  geom_point(data = centers_pca, aes(x = PC1, y = PC2), color = 'red', size = 6, shape = 8) +
  ggtitle('K-Means Clustering on Student Grades') +
  theme_minimal()


