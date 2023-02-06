# set working directory

library(readxl)
library(dplyr)
library(ggplot2)
library(pastecs)
library(ggpubr)

# 1. read xlsx files

df <- read_excel("Insurance.xlsx")

# 2. summary dataset

summary(df)
stat.desc(df)

# 3. mencari nilai modus

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(df$age)
getmode(df$sex)
getmode(df$bmi)
getmode(df$children)
getmode(df$smoker)
getmode(df$region)
getmode(df$expenses)

# 4. cek missing value tiap column

sum(is.na(df$age))
sum(is.na(df$sex))
sum(is.na(df$bmi))
sum(is.na(df$children))
sum(is.na(df$smoker))
sum(is.na(df$region))
sum(is.na(df$expenses))

# 5. membuat model multiple linear regression berdasarkan 'expenses'

model <- lm(expenses ~ ., data = df)
model

summary(model)
exp(coefficients(model))

summary(model$residuals)
stat.desc(model$residuals)
getmode(model$residuals)

sqrt(mean(model$residuals^2))

# 6. melakukan analisis model dengan Histogram dan QQ plot

hist <- hist(model$residuals, main="Histogram plot", col='sky blue')
hist

qq <- ggqqplot(model$residuals, main="QQ plot", col='blue')
qq
