library(rlang) 
library(MASS)
library(e1071)
library(caret)
library(cowplot)
library(ggplot2)
library(dplyr)
library(randomForest)

#Load Telco database
Telco.df <- read.csv("Telco-Churn.csv")
head(Telco.df)

#Checking for the Structure and Dimension of the Database
dim(Telco.df)  #7043 * 21 
str(Telco.df) 

#Checking for Missing Values
sum(is.na(Telco.df))  #Shows 11 missing values
sum(is.na(Telco.df$TotalCharges))   #Total Charges column has 11 missing values. No other variable has missing values.
#Replacing Missing Values with mean
mean(Telco.df[["TotalCharges"]],na.rm = TRUE) #2283.3
Telco.df[is.na(Telco.df)] <- #2283.3 
  sum(is.na(Telco.df))
sum(is.na(Telco.df$TotalCharges))  #No more missing values in the Database. 

#Checking Class for predictors
class(Telco.df$SeniorCitizen) #integer class
class(Telco.df$tenure) #integer class
class(Telco.df$TotalCharges) #numeric
class(Telco.df$MonthlyCharges)  #numeric
# Tenure, Monthly Charges and Total Charges are continuous variables. Remaining are categorical Variables.

#Converting Senior citizen into Categorical Variables.
Telco.df$SeniorCitizen <- as.factor(ifelse(Telco.df$SeniorCitizen==1, 'YES', 'NO'))
head(Telco.df)
#_______________________________________________________________________________________________________
#*******************************************************************************************************
#Exploratory Data Analysis:
#Part 1: Understanding the effect of Categorical Variables on Churn-->

#Plotting Gender, Senior Citizen, Partne, Dependents, PhoneService, MultipleLines v/s Churn. 
options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(Telco.df, aes(x=gender,fill= Churn))+ geom_bar()+scale_fill_manual("legend", values = c("dark blue","violetred1")),
          ggplot(Telco.df, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual("legend", values = c("dark blue","violetred1")),
          ggplot(Telco.df, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual("legend", values = c("dark blue","violetred1")),
          ggplot(Telco.df, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual("legend", values = c("dark blue","violetred1")),
          ggplot(Telco.df, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual("legend", values = c("dark blue","violetred1")),
          ggplot(Telco.df, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual("legend", values = c("dark blue","violetred1"))+theme_bw()+
            scale_x_discrete(labels = function(x) strwrap(x, width = 10)),align = "h")


#plotting Internet Service, Online Security, OnlineBAckup, DeviceProtection, TechSupport and Streaming TV v/s Churn
options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(Telco.df, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual("legend", values = c("dark blue","violetred1"))+
            scale_x_discrete(labels = function(x) strwrap(x, width = 10)), 
          ggplot(Telco.df, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual("legend", values = c("dark blue","violetred1"))+
            scale_x_discrete(labels = function(x) strwrap(x, width = 10)),
          ggplot(Telco.df, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual("legend", values = c("dark blue","violetred1"))+
            scale_x_discrete(labels = function(x) strwrap(x, width = 10)),
          ggplot(Telco.df, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual("legend", values = c("dark blue","violetred1"))+
            scale_x_discrete(labels = function(x) strwrap(x, width = 10)),
          ggplot(Telco.df, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual("legend", values = c("dark blue","violetred1"))+
            scale_x_discrete(labels = function(x) strwrap(x, width = 10)),
          ggplot(Telco.df, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual("legend", values = c("dark blue","violetred1"))+theme_bw()+
            scale_x_discrete(labels = function(x) strwrap(x, width = 10)),
          align = "h")

#Plotting Streaming Movies, Contract, Paperless Billing and Payment Method v/s churn
plot_grid(ggplot(Telco.df, aes(x=StreamingMovies,fill=Churn))+ 
            geom_bar(position = 'fill')+scale_fill_manual("legend", values = c("dark blue","violetred1"))+
            scale_x_discrete(labels = function(x) strwrap(x, width = 10)), 
          ggplot(Telco.df, aes(x=Contract,fill=Churn))+ 
            geom_bar(position = 'fill')+scale_fill_manual("legend", values = c("dark blue","violetred1"))+
            scale_x_discrete(labels = function(x) strwrap(x, width = 10)),
          ggplot(Telco.df, aes(x=PaperlessBilling,fill=Churn))+ 
            geom_bar(position = 'fill')+scale_fill_manual("legend", values = c("dark blue","violetred1"))+
            scale_x_discrete(labels = function(x) strwrap(x, width = 10)),
          ggplot(Telco.df, aes(x=PaymentMethod,fill=Churn))+
            geom_bar(position = 'fill')+scale_fill_manual("legend", values = c("dark blue","violetred1"))+theme_bw()+
            scale_x_discrete(labels = function(x) strwrap(x, width = 35)),
          align = "h")
#-----------------------------------------------------------------------------------
#Part 2: Understanding the effect of Continuous Variables on Churn-->

#Plotting Tenure V/s Churn
options(repr.plot.width =6, repr.plot.height = 2)
ggplot(Telco.df, aes(y= tenure, x = "", fill = Churn)) +   geom_boxplot()+ scale_fill_manual("legend", values = c("cyan","hotpink1"))+  theme_bw()+  xlab(" ")

#plotting Monthly Charges v/s Churn
ggplot(Telco.df, aes(y= MonthlyCharges, x = "", fill = Churn)) + geom_boxplot()+ scale_fill_manual("legend", values = c("cyan","hotpink1"))+ theme_bw()+ xlab(" ")

#Plotting Total Charges v/s Churn
ggplot(Telco.df, aes(y= TotalCharges, x = "", fill = Churn)) + geom_boxplot()+ scale_fill_manual("legend", values = c("cyan","hotpink1"))+theme_bw()+ xlab(" ")

#------------------------------------------------------------------------------------------------
#Part 3: Percentage of Churn for Last Month on Telco Platform
options(repr.plot.width = 6, repr.plot.height = 4)
Telco.df %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("violetred1", "gold1"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+  
  xlab("Churn") + 
  ylab("Percent")+
  ggtitle("Churn Percent")

#-----------------------------------------------------------------------------------------------
# Part 4: Checking for Outliers--> 

#Boxplot for Tenure
options(repr.plot.width =4, repr.plot.height = 4)
boxplot(Telco.df$tenure)$out

#Boxplot for Monthly Charges
boxplot(Telco.df$MonthlyCharges)$out

#Boxplot for Total charges
boxplot(Telco.df$TotalCharges)$out

#------------------------------------------------------------------------------------------------
#Part 5: Cleaning Categorical Variables-->
#From the EDA above, we know that there are some categorical features that have 
#'No' and 'No Internet Service' or 'No Phone Service' as a category, we can make them as 
#'No' and clean these features.

Telco.df <- data.frame(lapply(Telco.df, function(x) {
  gsub("No internet service", "No", x)}))

Telco.df <- data.frame(lapply(Telco.df, function(x) {
  gsub("No phone service", "No", x)}))

head(Telco.df)
#------------------------------------------------------------------------------------------------
#Part 6--> Standardizing Continuous Variables-->
num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
Telco.df[num_columns] <- sapply(Telco.df[num_columns], as.numeric)

telco_int <- Telco.df[,c("tenure", "MonthlyCharges", "TotalCharges")]
telco_int <- data.frame(scale(telco_int))
#Telco.df$int <- telco_int

head(telco_int)
#------------------------------------------------------------------------------------------------
#Part 7: Binning the Tenure Column
#Tenure is in months, so binned them as '0-1 year', '2-3 years', '3-4 years' etc.

max(Telco.df$tenure)
min(Telco.df$tenure)
Telco.df <- mutate(Telco.df, tenure_bin = tenure)

Telco.df$tenure_bin[Telco.df$tenure_bin >=0 & Telco.df$tenure_bin <= 12] <- '0-1 year'
Telco.df$tenure_bin[Telco.df$tenure_bin > 12 & Telco.df$tenure_bin <= 24] <- '1-2 years'
Telco.df$tenure_bin[Telco.df$tenure_bin> 24 & Telco.df$tenure_bin <= 36] <- '2-3 years'
Telco.df$tenure_bin[Telco.df$tenure_bin > 36 & Telco.df$tenure_bin <= 48] <- '3-4 years'
Telco.df$tenure_bin[Telco.df$tenure_bin > 48 & Telco.df$tenure_bin <= 60] <- '4-5 years'
Telco.df$tenure_bin[Telco.df$tenure_bin > 60 & Telco.df$tenure_bin <= 72] <- '5-6 years'
Telco.df$tenure_bin[Telco.df$tenure_bin > 72 & Telco.df$tenure_bin <= 84] <- '6-7 years'

Telco.df$tenure_bin <- as.factor(Telco.df$tenure_bin)

#Plotting binned Tenure v/s Count
options(repr.plot.width =6, repr.plot.height = 3)
ggplot(Telco.df, aes(tenure_bin, fill = tenure_bin)) + geom_bar()+scale_fill_manual("legend", values = c("dark blue","hotpink1","gold1","turquoise","dark green","red","black"))

#-------------------------------------------------------------------------------------------------
#Part 8: Creating Dummy Variables
telco_cat <- Telco.df[,-c(1,6,19,20)] #Removing Customer_ID, Tenure,Monthly_Charges, Total Charges and int

#Creating Dummy Variables
dummy<- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))
dummy <- dummy[ ,-27]  #REmoving Binned Tenure of 6-7 years. Outlier values. 
head(dummy)
#-----------------------------------------------------------------------------------------------------
#Part 9--> Preparing the Final DataSet
telco_final <- cbind(telco_int,dummy)
head(telco_final)
names(telco_final)  # Removed the intercept columns.

#-----------------------------------------------------------------------------------------------------
#Part 10--> Partitioning the Final DataSet
## partitioning into training (50%), validation (30%), test (20%) 
set.seed(123)
train.rows <- sample(rownames(telco_final), dim(telco_final)[1]*0.5)
valid.rows <- sample(setdiff(rownames(telco_final), train.rows), dim(telco_final)[1]*0.3)
test.rows <- setdiff(rownames(telco_final), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from the appropriate rows 
train.data <- telco_final[train.rows, ]
valid.data <- telco_final[valid.rows, ]
test.data <- telco_final[test.rows, ]
#Checking dimension of Train,Valid and Test Data
dim(train.data)
dim(valid.data)
dim(test.data)
#-------------------------------------------------------------------------------------------------------
#Running Algorithm 1-->
# Running Logistic Regression Algorithm (With Orignal Features)-->
logit.reg <- glm(Churn~ ., data = train.data, family = "binomial")
options(scipen=999)
summary(logit.reg)
#------------
#Stepwise
glm.step <- step(logit.reg, direction = "both")
summary(glm.step)
#-------------
library(forecast)
# use predict() with type = "response" to compute predicted probabilities.
logit.reg.pred <- predict(glm.step, valid.data, type = "response")
# first 25 actual and predicted records
data.frame(actual = valid.data$Churn[1:25], predicted = logit.reg.pred[1:25])
#Creating confusion Matrix to check the Accuracy of the Logistic Algorithm. ---Accuracy : 0.7827 
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), as.factor(valid.data$Churn))

#*************************************************************************************************

#----------------------------------------------------------------------------------------------------------------------------------
#***********************************************************************************************************************************
#Algorithm 2 --> 
#Random Forest Algorithm
#Training the Random Forest Model
model.rf <- randomForest(as.factor(Churn) ~ ., data=train.data, proximity=FALSE,importance = FALSE,
                         ntree=1000,mtry=4, do.trace=FALSE)
model.rf

#Predicting on the validation set and checking the Confusion Matrix.
testPred <- predict(model.rf, newdata=valid.data)
table(testPred, valid.data$Churn)

confusionMatrix(testPred, as.factor(valid.data$Churn)) #-- Accuracy  0.777 - 77.7% , specificity improves. Sensitivity decreases.

#Checking the variable Importance Plot
varImpPlot(model.rf)
#-------------------------------------------------------------------------------------------------------
#Trying Random Forest with interaction Variables--?
model.rf <- randomForest(as.factor(Churn) ~ .+Partner*PhoneService+PhoneService*Dependents+Contract.xOne.year*gender+MonthlyCharges*gender, data=train.data, proximity=FALSE,importance = FALSE,
                         ntree=500,mtry=4, do.trace=FALSE)
model.rf

#Testing if any change in plot-->   #Little change in variable plot .
varImpPlot(model.rf)

#Predicting on the validation set and checking the Confusion Matrix.
testPred <- predict(model.rf, newdata=valid.data)
table(testPred, valid.data$Churn)

confusionMatrix(testPred, as.factor(valid.data$Churn))  #Accuracy is 77.75%

#------------------------------------------------------------------------------------------------------
#******************************************************************************************************
#Running Algorithm 3-->
library(rpart)
library(rpart.plot)
#---------------------------------------------------------------------------------------------------------
#Clssification tree Algorithm

#######Create a deeper tree, go all the way by mentioning cp=0..
deeper.ct <- rpart(Churn ~ ., data = train.data, method = "class", cp = 0, minsplit = 1)

# length of deeper tree
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])

# plot deeper tree
#prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
#box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))

#check Accuracy on validation data of deeper tree
deeper.ct.point.pred.valid <- predict(deeper.ct,valid.data,type = "class")
confusionMatrix(deeper.ct.point.pred.valid, as.factor(valid.data$Churn))$overall[1]

# argument xval refers to the number of folds to use in rpart's built-in
# cross-validation procedure
# argument cp sets the smallest value for the complexity parameter.
cv.ct <- rpart(Churn ~ ., data = train.data, method = "class", 
               cp = 0.00001, minsplit = 25,xval=5)

# print the complexity parameter table
printcp(cv.ct)

# tree prune by lower cp
pruned.ct <- prune(cv.ct, cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])

#Find the length of the pruned tree
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])

#Plot the pruned tree
prp(pruned.ct, type = 1, extra = 1, split.font = 4, varlen = -10)

# classify records in the validation data.
#check Accuracy on validation data of pruned tree
pruned.ct.point.pred.valid1 <- predict(pruned.ct,valid.data,type = "class")
confusionMatrix(pruned.ct.point.pred.valid1, as.factor(valid.data$Churn))$overall[1]

#Checking which is best cp and std error for cp+1 xstd error and cp+2 xstd error
cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"]
std = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"xstd"]
xerror = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"xerror"]
cp
std
xerror
#Adding Std error and cp
s<-sum(xerror,std)
s
# Tree pruned by lower cp+1 xstd error
pruned.ct <- prune(cv.ct, cp=0.00766 )

# length of pruned tree with lower cp+1 xstd error
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])

# Check accuracy on validation data of pruned tree with lower cp+1 xstd error
pruned.ct.point.pred.valid2 <- predict(pruned.ct,valid.data,type = "class")
confusionMatrix(pruned.ct.point.pred.valid2, as.factor(valid.data$Churn))$overall[1]
prp(pruned.ct, type = 1, extra = 1, split.font = 4, varlen = -10)









