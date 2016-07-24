# This is the first model attempt at predicting who will survive on the Titanic

# Read in the training data set

setwd("C:/Users/Sara/OneDrive/Education/Kaggle/Titanic")

train <- read.csv("train.csv")
str(train)

# Next thing to do - create an age range variable
# if NA, then set to Unknown
# 0 - 17
# 18 - 27
# 28 - 37
# 38 - 47
# 48 - 57
# 58 and up

agena <- is.na(train$Age)
train$AgeRange[agena] <- "Unknown"

age017 <- train$Age <= 17
train$AgeRange[age017] <- "0 -17"

age1827 <- train$Age > 17 & train$Age <= 27
train$AgeRange[age1827] <- "18 - 27"

age2837 <- train$Age > 27 & train$Age <= 37
train$AgeRange[age2837] <- "28 - 37"

age3847 <- train$Age > 37 & train$Age <= 47
train$AgeRange[age3847] <- "38 - 47"

age4857 <- train$Age > 47 & train$Age <= 57
train$AgeRange[age4857] <- "48 - 57"

age58 <- train$Age > 57
train$AgeRange[age58] <- "58 and Up"

train$AgeRange <- as.factor(train$AgeRange)

cor(train$Survived, train$AgeRange)
tapply(train$Survived, train$AgeRange, mean)
tapply(train$Survived, train$Sex, mean)

summary(train)

log <- glm(Survived ~ Pclass+Sex+SibSp+AgeRange, data=train, family=binomial)
summary(log)

perf <- predict(log, type="response")
perftab <- table(train$Survived, perf >= 0.5)
perftab
sum(diag(perftab))/sum(perftab)

table(train$Survived)
549/(549+342)

#baseline - 0.6161616
#training set accuracy:  0.8114478

#now let's read in the test set and make some predictions
test <- read.csv("test.csv")
str(test)

# add age range to test
agena <- is.na(test$Age)
test$AgeRange[agena] <- "Unknown"

age017 <- test$Age <= 17
test$AgeRange[age017] <- "0 -17"

age1827 <- test$Age > 17 & test$Age <= 27
test$AgeRange[age1827] <- "18 - 27"

age2837 <- test$Age > 27 & test$Age <= 37
test$AgeRange[age2837] <- "28 - 37"

age3847 <- test$Age > 37 & test$Age <= 47
test$AgeRange[age3847] <- "38 - 47"

age4857 <- test$Age > 47 & test$Age <= 57
test$AgeRange[age4857] <- "48 - 57"

age58 <- test$Age > 57
test$AgeRange[age58] <- "58 and Up"

test$AgeRange <- as.factor(test$AgeRange)

PredTest = predict(log, newdata=test, type="response")
threshold = 0.5

PredTestLabels = ifelse(PredTest<threshold, 0, 1)
MySubmission = data.frame(PassengerId = test$PassengerId, Survived = PredTestLabels)
write.csv(MySubmission, "TitanicLog1.csv", row.names=FALSE)

#public testing set accuracy: 0.74641