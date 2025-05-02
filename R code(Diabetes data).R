library(readr)
library(caTools)
library(caret)
library(e1071)
data=read.csv("C:/Users/tanma/Desktop/ALL/2nd same isi/Machine learning assigment/diabetes.dataset.csv")
head(data)

################
# Check data summary
summary(data)

# Check for missing values
colSums(is.na(data))
is.na(data)
sum(is.na(data))
is.na(data)

#########################
# import libraries
library(ggplot2)
library(reshape2)

correlation_matrix <- cor(data)

# Convert correlation matrix to long format
correlation_melted <- melt(correlation_matrix)

# Plot heatmap
ggplot(correlation_melted, aes(Var1, Var2, fill=value)) +
  geom_tile(color="white") +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, 
                       limit=c(-1,1), space="Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Correlation Heatmap", x="Features", y="Features")
##################################
outcome_counts <- table(data$Outcome)
outcome_df <- data.frame(Outcome = names(outcome_counts), 
                         Count = as.numeric(outcome_counts))

# Create bar plot
ggplot(outcome_df, aes(x=Outcome, y=Count)) +
  geom_bar(stat="identity", fill="pink") +
  labs(title="Distribution of Diabetes Outcomes", x="Outcome", y="Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=12),
        plot.title = element_text(size=16, face="bold"))
##########################
library(ggplot2)

# Select relevant columns
diabetes_subset <- data[, c("Pregnancies", "Glucose", "BloodPressure", 
                            "BMI", "Age", "Outcome")]

# Histograms
ggplot(diabetes_subset, aes(x = Pregnancies, fill = factor(Outcome))) +
  geom_histogram(position = "identity", bins = 30, alpha = 0.7) +
  labs(title = "Distribution of Pregnancies by Outcome") +
  facet_wrap(~Outcome, scales = "free_y") +
  theme_minimal()
#####################
library(ggplot2)

# Boxplot
ggplot(data, aes(x = factor(Outcome), y = BMI, fill = factor(Outcome))) +
  geom_boxplot() +
  labs(title = "BMI Distribution by Outcome") +
  theme_minimal()

########################################
# Normalize
preProc <- preProcess(data[, -ncol(data)], method = c("center", "scale"))
data_norm <- predict(preProc, data)

# Add target
data_norm$Outcome <- data$Outcome

# Split
set.seed(123)
index <- createDataPartition(data_norm$Outcome, p = 0.8, list = FALSE)
train <- data_norm[index, ]
test <- data_norm[-index, ]
model_log <- glm(Outcome ~ ., data = train, family = "binomial")
pred_log <- predict(model_log, test, type = "response")
pred_class_log <- ifelse(pred_log > 0.5, 1, 0)
confusionMatrix(factor(pred_class_log), factor(test$Outcome))
library(randomForest)
model_rf <- randomForest(factor(Outcome) ~ ., data = train, ntree = 100)
pred_rf <- predict(model_rf, test)
confusionMatrix(pred_rf, factor(test$Outcome))
library(e1071)
model_svm <- svm(factor(Outcome) ~ ., data = train, kernel = "radial")
pred_svm <- predict(model_svm, test)
confusionMatrix(pred_svm, factor(test$Outcome))
library(pROC)

roc_log <- roc(test$Outcome, pred_log)
roc_rf <- roc(test$Outcome, as.numeric(pred_rf))
roc_svm <- roc(test$Outcome, as.numeric(pred_svm))

plot(roc_log, col = "blue", main = "ROC Curves")
lines(roc_rf, col = "green")
lines(roc_svm, col = "red")
legend("bottomright", legend=c("Logistic", "Random Forest", "SVM"), col=c("blue", "green", "red"), lwd=2)
# AUC values
auc_log <- auc(roc_log)
auc_rf <- auc(roc_rf)
auc_svm <- auc(roc_svm)

# Print AUC values
auc_log
auc_rf
auc_svm
cat("AUC Values:\n")
cat("Logistic Regression:", round(auc_log, 4), "\n")
cat("Random Forest:", round(auc_rf, 4), "\n")
cat("SVM:", round(auc_svm, 4), "\n")




##################
# Split data into X and y
X <- data[, 1:8]
y <- data[, 9]

# Scale only the features (X)
scaled_X <- as.data.frame(scale(X))

# Bind scaled X with y
scaled_data <- cbind(scaled_X, y)


# Split scaled data into X and y
X <- scaled_data[, 1:8]
y <- scaled_data[, 9]

# Split X and y into training and testing sets
set.seed(123)
sample <- sample.split(y, SplitRatio = 0.7)
X_train <- X[sample == TRUE, ]
y_train <- y[sample == TRUE]
X_test <- X[sample == FALSE, ]
y_test <- y[sample == FALSE]
###########################################
log_model <- glm(y_train ~ ., data = X_train, family = binomial)
# Make predictions
predictions <- predict(log_model, newdata = X_test, type = "response")

# Convert predicted outcome to factor with levels matching actual outcome
predictions <- factor(ifelse(predictions > 0.5, 1, 0), 
                      levels = levels(as.factor(y_test)))

# Generate confusion matrix
confusionMatrix(predictions, as.factor(y_test))
###############################################

######################################################



predict_diabetes <- function(pregnancies, glucose, bloodpressure, skinthickness, 
                             insulin, bmi, diabetespedigreefunction, age) {
  input_data <- data.frame(
    Pregnancies = pregnancies,
    Glucose = glucose,
    BloodPressure = bloodpressure,
    SkinThickness = skinthickness,
    Insulin = insulin,
    BMI = bmi,
    DiabetesPedigreeFunction = diabetespedigreefunction,
    Age = age
  )
  input <- as.data.frame(input_data)
  prediction <- predict(log_model, newdata = input, type = "response")
  prediction <- factor(ifelse(prediction > 0.5, 1, 0), 
                       levels = levels(as.factor(prediction)))
  
  return(prediction)
}

new_patient <- data.frame(
  pregnancies = 6,
  glucose = 148,
  bloodpressure = 72,
  skinthickness = 35,
  insulin = 0,
  bmi = 33.6,
  diabetespedigreefunction = 0.627,
  age = 50
)

prediction <- predict_diabetes(
  new_patient$pregnancies,
  new_patient$glucose,
  new_patient$bloodpressure,
  new_patient$skinthickness,
  new_patient$insulin,
  new_patient$bmi,
  new_patient$diabetespedigreefunction,
  new_patient$age
)

if (any(prediction == 1)) {
  cat("Based on the model's prediction, there is a higher chance of diabetes.")
} else {
  cat("Based on the model's prediction, the risk of diabetes appears lower.")
}


