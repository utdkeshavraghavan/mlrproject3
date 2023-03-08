#Keshav Raghavan
#KAR190002
#Assignment 1 - Introduction to Machine Learning
#CS 4375.002
#Prof. Gity Karami
#2/21/2023

#Partner: Pankti Mevada
#netID : psm190008

#Importing libraries needed for Assignment 1 here:
#install.packages(ggplot2)
#install.packages(sampling)
#install.packages(scatterplot3d)
#install.packages(Matrix)
#install.packages(arules)

#used for select feature
#install.packages("dplyr")

library(ggplot2)
library(sampling)
library(scatterplot3d)
library(Matrix)
library(arules)

library(dplyr)



#Question 1: 

#Reading the data as a dataframe from heart.csv
data <- data.frame(read.csv("heart.csv"))

dim(data)
str(data)

#Checking if any data plots are not unique or do not have a complete row.
#If it doesn't have a clean data set, then remove the row.

data <- na.omit(data)
i <- duplicated(data)

clean.data <- unique(data[complete.cases(data),])
summary(clean.data)
#i - omittted from printing, due to excess in html. All showed false.

data.refined <- clean.data[clean.data$Cholesterol > 40,]

write.csv(data.refined, "refined_data.csv", row.names = FALSE)

#Using a ggplot, or more commonly known as a scatter plot, I plotted the patient's Cholesterol versus their Max Heart Rate, trying to see if there are any coorelations between those
#along with having Heart Disease to see if there are any real differences between those who have Heart Disease and the differences in their Max HR and their Cholesterol. The results were inconclusive, as 
#they were nearly identical with those who didn't have any heart disease.

#Using ggplot (scatter plot) to display the information from the improved data set stored in the data frame : 'data'
ggplot(data.refined, aes(x = Cholesterol, y = MaxHR, col = HeartDisease)) + geom_point()


#I also plotted using ggplot Age versus the resting BP, to see if blood pressure is changed by age or Heart Disease. Overall, less than age, if the patient was older and had a higher
#BP there was a larger chance for the patient to have a Heart disease. Overall, the older the patient looks more so that the Heart disease is more common. However regardless of age, or the 
#BP heart disease was present throughout all ages and BPs.

#Using a ggplot to show the correlation between age and resting BP
ggplot(data.refined, aes(x = Age, y = RestingBP, col = HeartDisease)) + geom_point()

#Using ggplot to graph 4 plots between discrete and continuous features
ggplot(data.refined, aes(x = MaxHR, y = HeartDisease)) + geom_bar(stat = "identity")

ggplot(data.refined, aes(x = Sex, y = MaxHR)) + geom_point()

ggplot(data.refined, aes(x = Age, y = RestingECG)) + geom_point()

ggplot(data.refined, aes(x = Cholesterol, y = FastingBS)) + geom_point()

# In the barplot for MaxHR(Continuous) versus HeartDisease(discrete), the data showed that it is more likely for someone to have heart disease when they have a MaxHR between 130-150 where
# there are statically higher amount of those who have Heart disease (as seen in the plot, it looks like a bell curve.)

# In the plot for Sex and Max HR it looks to be that men have slightly higher and lower max HRs. The max for women was about 180 but with men it exceeded 200.

#In the plot for Age and resting ECG, I couldn't see any real range differences. With the age almost within a span of a few years there are points for each ECG. Normal Resting ECG is a base case
#we can assume, and if we see it like that, LVH and ST can be seen as almost similarly existing in terms of where patient's age are almost the same but can see ST or LVH present in their ECGS results.

#For Cholesterol versus FastingBS, we can see that there is a cluster for patients between 150-375 for not Fasting, and between about 200-350 Cholesterol for those who were fasting.


#Extracted continuous data
cont.data = select(data.refined, Age, RestingBP, Cholesterol, MaxHR, Oldpeak)

#Question 2: Extracting continuous features and sex feature from the dataframe 'data'
aggregate(cont.data, list(data.refined$Sex), FUN = mean)

#According to the information giving by the aggregate function the mean age in the data is 52.2 for Females, 53.1 for Males.
#RestingBP is higher for Men, and Cholesterol is lower. However, unlike the original findings I presumed in the charts in question 1,
#MaxHR is lower for Men, and higher for women by ~9 BPM.

#Question 3: Euclidean distances

#This is the euclidean distance before transformation.
dist(cont.data[1:10,], method = "euclidean")

#Applying standardization here:
cont.data.scaled <- scale(cont.data[2:5])

#This is the euclidean distance after the transformation.
dist(cont.data.scaled[1:10,], method = "euclidean")

#The finding is that when applying the standardization, numbers for standardization changed age, restingBP, Cholestorol, MaxHR, and Oldpeak
#To be closer to 0 in order to achieve the 0 mean. The idea was that if achieved, then Euclidean distance would have changed. Originally Euclidean distance
#from 2 to 1 was 112, and after standardization it changed to 2.45. Of course, all distances changed, however 2 to 1 is just an example I'm presenting in my findings.


#Question 4: Random sampling
rand.data <- data.refined[sample(1:nrow(data.refined), size = 300),]

#Checking for duplicates
duplicates.rand <- duplicated(rand.data)
duplicates.rand

#According to the duplicates feature in R, there are no duplicated rows
#Most likely due to each row being randomized, there shouldn't be any duplicates as for 300 rows to be randomized 
#When using the sample data to create random rows, the objects should vary from each row, so duplicated shouldn't find any that repeat.
#this was from H1 - Part 2.R where I found the duplicated and sample features. 

#Question 5: scatter3dplot
scatter.data <- select(data.refined, RestingBP, Cholesterol, FastingBS)
scatterplot3d(scatter.data[,1:3],)

pc <- prcomp(as.matrix(scatter.data[,1:3]))
pc

#The scatter3dplot is divided using the Boolean Fasting to see the difference vertically between if a patient is fasting or not
#This also helps us with seeing the difference between patients clusters in cholesterol and resting BP when they fast or not.
#My findings using these methods show that the difference between fasting or not isn't arguably too much of a difference compared to
#not fasting in terms of cholesterol or in resting BP


#Question 6:  Discrete the Age attribute
age_eq_int<-discretize(data.refined$Age, method="interval", breaks=4)
summary(age_eq_int)

age_eq_freq<-discretize(data.refined$Age, method="frequency", breaks=4)
summary(age_eq_freq)

#Using discretize function, data refined is Age. The method is interval, and is handled with 4 breaks. The breaks are at 40.2, 52.5, 64.8, 77
#Using the discretize function, data refined is Age, the method is frequency. This time the 4 breaks are handled at 46 54 59 and 77.
#It is evident frequency breaks at different periods in the list, and that the age width is different. In the number of objects,
#given below of the breaks, we can see that the width between 52.5 and 64.8 is where the largest amount is for interval. For frequency
#Between 59 and 77 is where the largest amount of objects are present.

#Question 7: Pearson correlation
continuous_some.data <- select(data.refined, Age, Cholesterol) %>% sample_n(50)

cor(continuous_some.data[,1:2])

cor(continuous_some.data$Age, continuous_some.data$Cholesterol)
plot(continuous_some.data$Age, continuous_some.data$Cholesterol)

#There is no linear trend or relationship between Age and Cholesterol, 
#The Matrix showed that age would be 1.000 and Cholesterol would be .05875824 (and vice versa when rows are swapped)
#The trend doesn't seem to visually show any linear trend when looks at, more so seems like a bell curve with 55 being the curve at the top