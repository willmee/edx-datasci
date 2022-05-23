library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# Q1
set.seed(42, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(titanic_clean$Survived, times=1, p=0.2, list=FALSE)
test_set <- titanic_clean[test_index,]
train_set <- titanic_clean[-test_index,]

# Q2
set.seed(3, sample.kind="Rounding") # if using R 3.6 or later
# doesn't work
#test_set_guess <- test_set %>% 
#  mutate(Guess = sample(c(0, 1), 1, replace=TRUE))
guesses <- sample(c(0,1), nrow(test_set), replace=TRUE)
mean(test_set_guess$Survived == guesses)

# Q3
train_set %>%
  group_by(Sex) %>%
  summarise(survival_rate=mean(Survived == 1))

# Q4
pred_sex <- factor(ifelse(test_set$Sex == "female", "1", "0"))
mean(pred_sex == test_set$Survived)

train_set %>%
  group_by(Pclass) %>%
  summarise(survival_rate=mean(Survived == 1))

pred_class <- factor(ifelse(test_set$Pclass == 1, "1", "0"))
mean(pred_class == test_set$Survived)

train_set %>%
  group_by(Sex, Pclass) %>%
  summarise(survival_rate=mean(Survived == 1))

pred_sex_class <-factor(ifelse(test_set$Pclass <= 2 & test_set$Sex == 'female', "1", "0"))
mean(pred_sex_class == test_set$Survived)
# mean((test_set$Pclass <= 2 & test_set$Sex == 'female') == (test_set$Survived == 1))

# Q5
# Sensitivity : 0.873         
# Specificity : 0.739    
# Balanced Accuracy : 0.806    
confusionMatrix(pred_sex, test_set$Survived)
# Sensitivity : 0.855        
# Specificity : 0.464   
# Balanced Accuracy : 0.659    
confusionMatrix(pred_class, test_set$Survived)
# Sensitivity : 0.991         
# Specificity : 0.551 
# Balanced Accuracy : 0.771   
confusionMatrix(pred_sex_class, test_set$Survived)

# Q6 
F_meas(confusionMatrix(pred_sex, test_set$Survived)$table)
F_meas(confusionMatrix(pred_class, test_set$Survived)$table)
F_meas(confusionMatrix(pred_sex_class, test_set$Survived)$table)

# Q7
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
lda_model <- train(Survived ~ Fare, data = train_set, method="lda")
y_hat_lda <- predict(lda_model, test_set, type = "raw")
confusionMatrix(y_hat_lda, test_set$Survived)$overall[["Accuracy"]]

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
qda_model <- train(Survived ~ Fare, data = train_set, method="qda")
y_hat_qda <- predict(qda_model, test_set, type = "raw")
confusionMatrix(y_hat_qda, test_set$Survived)$overall[["Accuracy"]]

# Q8
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
glm_model <- train(Survived ~ Age, data = train_set, method="glm")
y_hat_glm <- predict(glm_model, test_set, type = "raw")
confusionMatrix(y_hat_glm, test_set$Survived)$overall[["Accuracy"]]

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
glm_model_4 <- train(Survived ~ Sex + Pclass + Fare + Age, data = train_set, method="glm")
y_hat_glm_4 <- predict(glm_model_4, test_set, type = "raw")
confusionMatrix(y_hat_glm_4, test_set$Survived)$overall[["Accuracy"]]

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
glm_model_all <- train(Survived ~ ., data = train_set, method="glm")
y_hat_glm_all <- predict(glm_model_all, test_set, type = "raw")
confusionMatrix(y_hat_glm_all, test_set$Survived)$overall[["Accuracy"]]

# Q9a
set.seed(6, sample.kind="Rounding") # if using R 3.6 or later
knn_model <- train(Survived ~ ., data = train_set, method="knn",
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
knn_model$bestTune

# Q9b
knn_model$results %>%
  ggplot() +
  geom_line(aes(k, Accuracy, color='red'))

# Q9c
y_hat_knn <- predict(knn_model, test_set, type = "raw")
confusionMatrix(y_hat_knn, test_set$Survived)$overall[["Accuracy"]]

# Q10
set.seed(8, sample.kind="Rounding") # if using R 3.6 or later
control <- trainControl(method = "cv", number = 10, p = .9)
knn_model_x10 <- train(Survived ~ ., data = train_set, method="knn",
                   tuneGrid = data.frame(k = seq(3, 51, 2)),
                   trControl = control)
ggplot(knn_model_x10, highlight = TRUE)

y_hat_knn_x10 <- predict(knn_model_x10, test_set, type = "raw")
confusionMatrix(y_hat_knn_x10, test_set$Survived)$overall[["Accuracy"]]

# Q11a
set.seed(10, sample.kind="Rounding") # if using R 3.6 or later
train_rpart <- train(Survived ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set)
train_rpart$bestTune

y_hat_rpart <- predict(knn_model_x10, test_set, type = "raw")
confusionMatrix(y_hat_knn_x10, test_set$Survived)$overall[["Accuracy"]]

y_hat_rpart <- predict(train_rpart, test_set, type = "raw")
confusionMatrix(y_hat_rpart, test_set$Survived)$overall[["Accuracy"]]

# Q11b
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
print(train_rpart$finalModel)

# Q12
set.seed(14, sample.kind="Rounding") # if using R 3.6 or later
train_rf <- train(Survived ~ ., method = "rf", data = train_set,
      tuneGrid = data.frame(mtry = seq(1:7)), ntree=100)

train_rf$bestTune

y_hat_rf <- predict(train_rf, test_set, type = "raw")
confusionMatrix(y_hat_rf, test_set$Survived)$overall[["Accuracy"]]

varImp(train_rf)
