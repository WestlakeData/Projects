# Install the libraries and read in the data
library(dplyr)
library(ggplot2)
library(caret)
library(naniar)

#Load Data and rename columns
data <- read.csv("MasterAttendance.csv", header = TRUE)
nextYear <- read.csv("2023.csv", header = TRUE)
names(data) <- c('weekday', 'month','day', 'opponent', 'GB', 'nightGame', 'attendance')
names(nextYear) <- c('nightGame', 'weekday', 'opponent')
nextYear <- nextYear %>% replace_with_na(replace = list(opponent= c("BAL", "BOS", "TB", "TEX", "KC"))) #Replace missing factors with NAs

#Split data into Test and Train sets
datasplit <- createDataPartition(data$attendance, p=0.8, list = FALSE)
trainData <- data[datasplit,]
testData <- data[-datasplit,]

#Linear Regression Model
lmregression <- train(attendance ~ weekday+nightGame, data = trainData, method = "lm", na.action=na.exclude)
summary(lmregression)

#ANOVA Model
aov <- aov(attendance~weekday+nightGame+opponent, data=trainData)
mean(fit$residuals)
anova(aov)

#Linear Regression Predictions on Test Data
predLM <-predict(lmregression, newdata=testData)

#ANOVA Model Predictions on Test Data
predAOV <-predict(aov, newdata=testData)

testData <- cbind(testData, predLM, predAOV)

#Examine Model Performance on Test Data
testData['diffLM'] <- abs(testData$attendance - testData$predLM)
testData['diffAOV'] <- abs(testData$attendance - testData$predAOV)

summary(testData$diffLM)
summary(testData$diffAOV)


#Apply Models to past Games 
predLM <- predict(lmregression, newdata = data)
predAOV <- predict(aov, newdata = data)

new <- cbind(data,predLM, predAOV)

#Calculate Observed Variance
new['diffLM'] <- abs(new$attendance - new$predLM)
new['diffAOV'] <- abs(new$attendance - new$predAOV)

summary(new$diffLM)
summary(new$diffAOV)

#Graph Observed Variance
ggplot(new, aes(diffLM, group = nightGame))+geom_boxplot()+labs(title = "Observered variation of Linear Regression Model", x = 'Observered Variation') + facet_wrap(vars(nightGame))
ggplot(new, aes(diffAOV, group = nightGame))+geom_boxplot()+labs(title = "Observered variation of ANOVA Model", x = "Observed Variation") + facet_wrap(vars(nightGame))

#Apply ANOVA model to 2023 games
pred2023 <- predict(aov, newdata = nextYear, na.action = na.pass)
nextYear <- cbind(nextYear, pred2023)

#Write results to file
write.csv(nextYear, file = "../../2023predictions.csv")
