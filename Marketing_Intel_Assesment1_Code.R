# Load necessary libraries
library(readxl)      # For reading Excel files
library(tidyverse)   # For data manipulation and visualization
library(cluster)     # For clustering methods
library(openxlsx)    # For exporting data to Excel

# Load data
file_path <- "C:/Users/timot/Downloads/SmartWatch Data File.xlsx" # change if neccesarry
data <- read_excel(file_path)

# Data Exploration
summary(data)
anyNA(data)

# Remove ID column if necessary (assuming the first column is an ID)

print(data)
# Standardize the data
scaled_data <- scale(data)

# Compute Euclidean distance
distance <- dist(scaled_data, method = "euclidean")

# Perform hierarchical clustering
hc <- hclust(distance, method = "ward.D")

# Plot dendrogram
plot(hc, main = "Cluster Dendrogram", xlab = "Observations", ylab = "Height")
rect.hclust(hc, k = 4, border = 2:5)  # Choose k=4 for visualization

# Determine the optimal number of clusters using the elbow method
x <- 1:10
sort_height <- sort(hc$height, decreasing = TRUE)
y <- sort_height[1:10]
plot(x, y, type = "b", 
     main = "Elbow Plot for Optimal Clusters",
     xlab = "Number of Clusters",
     ylab = "Height")
lines(x, y, col = "blue")
abline(v = 4, col = "red", lty = 2)


# Assign clusters to data
cluster <- cutree(hc, k = 4)
df_final <- cbind(data, cluster)

# Calculate segment sizes
proportions <- table(df_final$cluster) / nrow(df_final)
percentages <- proportions * 100
print(percentages)

# Compute mean values of variables per cluster
segments <- df_final %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, .names = "{col}_mean"))

# Save results to Excel
write.xlsx(segments, 'segments.xlsx')