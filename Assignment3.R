#install.packages("rpart")
library("rpart")


heart <- data.frame(read.csv("heart.csv"))

data <- na.omit(heart)
i <- duplicated(data)

clean.heart <- unique(heart[complete.cases(heart),])
heart.refined <- clean.heart[clean.heart$Cholesterol > 40,]

heart_classification_tree <- rpart(HeartDisease ~ FastingBS + Age + Sex,
                                   data = heart.refined,
                                   method = "class")
heart_classification_tree