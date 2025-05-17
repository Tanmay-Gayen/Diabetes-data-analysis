# Load necessary libraries
library(class)
library(ggplot2)

# Load the dataset
data <- read.csv("C:/Users/tanma/Desktop/ALL project and rescarch/Diabates data analysis/diabetes.dataset.csv")

# Select features and label
features <- data[, c("BMI", "Age")]
labels <- data$Outcome

# Normalize features
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
features_norm <- as.data.frame(lapply(features, normalize))

# Normalize new observation
new_obs <- data.frame(BMI = 43.6, Age = 40)
new_obs_norm <- data.frame(
  BMI = normalize(new_obs$BMI),
  Age = normalize(new_obs$Age)
)

# Choose k
k <- 5

# Predict using KNN
prediction <- knn(train = features_norm, test = new_obs_norm, cl = labels, k = k)
cat("Predicted Outcome for BMI = 43.6 and Age = 40:", prediction, "\n")

# Calculate distances and get top-k neighbors
distances <- sqrt((features_norm$BMI - new_obs_norm$BMI)^2 + (features_norm$Age - new_obs_norm$Age)^2)
ranked_indices <- order(distances)
ranked_neighbors <- data.frame(
  Index = ranked_indices[1:k],
  Distance = distances[ranked_indices[1:k]],
  Outcome = labels[ranked_indices[1:k]],
  BMI = features$BMI[ranked_indices[1:k]],
  Age = features$Age[ranked_indices[1:k]]
)

# Plotting
ggplot(data, aes(x = BMI, y = Age, color = factor(Outcome))) +
  geom_point(size = 2) +
  scale_color_manual(values = c("blue", "red"), name = "Actual Outcome") +
  geom_point(data = ranked_neighbors, aes(x = BMI, y = Age), color = "black", shape = 4, size = 3, stroke = 1.5) +
  geom_point(data = new_obs, aes(x = BMI, y = Age), color = "green", size = 4, shape = 17) +
  labs(title = paste("KNN (k =", k, ") Prediction: Outcome =", prediction),
       x = "BMI", y = "Age") +
  theme_minimal()

