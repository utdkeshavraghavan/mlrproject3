#KESHAV RAGHAVAN
#KAR190002
#ASSIGNMENT 3, CS 4375.002
#install.packages("rpart")
#install.packages("rpart.plot")
library("rpart")
library("rpart.plot")

heart <- data.frame(read.csv("heart.csv"))

data <- na.omit(heart)
i <- duplicated(data)

clean.heart <- unique(heart[complete.cases(heart),])
heart.refined <- clean.heart[clean.heart$Cholesterol > 40,]

heart_classification_tree <- rpart(HeartDisease ~ FastingBS + Age + Sex,
                                   data = heart.refined,
                                   method = "class")
rpart.plot(heart_classification_tree)
