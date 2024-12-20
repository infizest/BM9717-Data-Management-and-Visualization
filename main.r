install.packages("caret")
library(readr)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
data_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSyApjLGxCbRCnuTeFhnbN1r3KFprc0o0bl6UuK8Z3fzfuy-VTVq246gXUXMdb0Ihxq3wMETPz63HzB/pub?output=csv"
data <- read_csv(data_url)
str(data)
summary(data)
missing_values <- colSums(is.na(data))
print(missing_values)
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$ca <- as.factor(data$ca)
data$thal <- as.factor(data$thal)
data$target <- as.factor(data$target)
continuous_vars <- c("age", "trestbps", "chol", "thalach", "oldpeak")
data[continuous_vars] <- scale(data[continuous_vars])
cor_matrix <- cor(data[continuous_vars])
corrplot(cor_matrix, method = "circle", type = "upper")
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "Age Distribution", x = "Age", y = "Count")
  ggplot(data, aes(x = cp, fill = target)) +
  geom_bar(position = "dodge") +
  labs(title = "Chest Pain Type vs. Target", x = "Chest Pain Type", y = "Count")
  key_summary <- data %>% 
  group_by(target) %>% 
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
print(key_summary)
colnames(data)