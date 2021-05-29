
# ---Library---

library(dplyr) 
library(rpart) # Decision Tree
install.packages("rpart.plot")
library(rpart.plot) # Decision Tree

# ---Import Data---
data.df <- read.csv(file="../data/train.csv", header = T) 

# ---Data Pre-processing---
data.df <- data.df %>% select("Survived", "Pclass", "Sex",
                                "Age","Fare") %>%
                       mutate(Survived = factor(Survived, levels = c(0,1), labels = c('No','Yes'))) %>%
                       mutate(Pclass = factor(Pclass, levels = c(1,2,3),labels = c('Upper','Middle','Lower'))) %>%
                       mutate(Sex = factor(Sex, levels = c('male','female'),labels = c('Male','Female')))


train.index <- sample(x=1:nrow(data.df), size=ceiling(0.8*nrow(data.df) ))
train.df <- data.df[train.index, ]
test.df <- data.df[-train.index, ]


# ---Build Decision Tree Model---

## Build Decision Tree Model
set.seed(5)
cart.model<- rpart(Survived ~. , 
                   data=train.df)
cart.model

## Plot the Decision Tree and Save the image
png("../results/decision_tree.png")
prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray", extra =4,main="Decision Tree")
dev.off()

## Predict on Test Dataset
set.seed(5)
pred <- predict(cart.model, newdata=test.df, type ="class")
confusion_matrix <- table(real=test.df$Survived, predict=pred)
print(confusion_matrix)
accuracy <- (confusion_matrix[1,1]+confusion_matrix[2,2])/nrow(test.df)
print(accuracy)








