# set working directory

library(readxl)
library(dplyr)
library(tidyr)
library(caTools)
library(nnet)
library(AER)
library(ROCR)
library(ggcorrplot)

# 1. read xlsx files

data <- read_excel("Breast Cancer Wisconsin (Diagnostic).xlsx")

# 2. summary dataset

summary(data)

# 3. mengamati korelasi antar column (minus 'diagnosis')

reduced_data <- subset(data, select = -diagnosis)
corr_matrix = round(cor(reduced_data), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)

# 4. drop column 'id'

data <- data[,-1]

# 5. drop column 'perimeter_mean' dan 'area_mean'

data <- subset(data, select = -c(perimeter_mean, area_mean))

# 6. drop column 'concave points_mean'

to_remove <- c('concave points_mean')
data <- data[ , !(names(data) %in% to_remove)]

# 7. cek missing value tiap column

sum(is.na(data$diagnosis))
sum(is.na(data$radius_mean))
sum(is.na(data$texture_mean))
sum(is.na(data$smoothness_mean))
sum(is.na(data$compactness_mean))
sum(is.na(data$concavity_mean))
sum(is.na(data$symmetry_mean))
sum(is.na(data$fractal_dimension_mean))

# 8. mengganti M menjadi 1 dan B menjadi 0

data$diagnosis[which(data$diagnosis == "M")] <- "1"
data$diagnosis[which(data$diagnosis == "B")] <- "0"
data$diagnosis <- as.numeric(data$diagnosis)

# 9. membagi dataset menjadi 2 (sebagian kecil untuk validasi dan sisanya untuk pembuatan model)

set.seed(5040)

to_take <- floor(0.7 * nrow(data))
to_take

split <- sample(seq_len(nrow(data)), size = to_take)
data_model <- data[split,]
data_test <- data[-split,]

# 10. membuat model logistic regression berdasarkan 'diagnosis'

model_logistic <- glm(diagnosis ~ ., data = data_model, family = binomial)
model_logistic

summary(model_logistic)
exp(coefficients(model_logistic))

p <- 1 - pchisq(72.543, 7)
p

# 11. cek validasi

prediction <- predict(model_logistic , data_test, type="response")
prediction <- unname(ifelse(prediction < 0.5, 0, 1))

accuracy <- sum(data_test$diagnosis == prediction)/length(data_test$diagnosis)
accuracy
precision <- sum(data_test$diagnosis == 1 & prediction == 1)/(sum(prediction == 1))
precision
recall <- sum(data_test$diagnosis == 1 & prediction == 1)/(sum(data_test == 1))
recall

prediction <- ifelse(prediction < 0.5, 0, 1)
ROCPred <- prediction(prediction, data_test$diagnosis)
ROCPer <- performance(ROCPred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]

plot(ROCPer)
abline(a = 0, b = 1)
auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)
