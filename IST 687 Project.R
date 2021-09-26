############### http://www.analytictech.com/mb021/mlk.htm
#Andrew Ku
#IST 687
#08/18/2021
#Titanic Project
###############
library(ggplot2)
library(sqldf)
library(dplyr)
library(mice)
library(kernlab)
library(randomForest)
library(randomForestExplainer)
#Questions:
##Does the family size affect the survival?
##Does ones socio-economic status affect the survival?
##Does age affect the survival?
## Did the embarked location affect the survial?
## which sex survived more?
############Reading Data#################
test <- read.csv("test.csv")
train <- read.csv("train.csv")
############Combining train and test Data######
combineData <- bind_rows(train,test)
str(combineData)
summary(combineData)
View(combineData)
summary(combineData$Cabin)
#weka 
############checking for NAs##################
combineData[combineData ==""] <- NA
md.pattern(combineData)
#take the average 
##remove Cabin attribute 
combineData <- select(combineData,Survived,Pclass,Name,Sex,Age,SibSp,Parch,Ticket,Fare,Embarked)
str(combineData)

####################Numeric Data histogram##############################
##histogram of age
hist(train$Age)
ggplot(train, aes(x = Age, fill = factor(Survived))) +
        geom_histogram(position = 'dodge') +
        ggtitle("Histogram of Age")
#Box plot of age
ggplot(train, aes(x = Age)) +
        geom_boxplot(outlier.colour = "red", 
                     outlier.shape = 16, outlier.size = 2, notch = F ) + 
        coord_flip() +
        ggtitle("Boxplot of Age")
##histogram of sibsp
hist(train$SibSp)
ggplot(train, aes(x = SibSp, fill = factor(Survived))) +
        geom_histogram(position = 'dodge', bins = 10) +
        ggtitle("Histogram of Sibling/Spouse")
#box plot of sibsp
ggplot(train, aes(x = SibSp)) +
        geom_boxplot(outlier.colour = "red", 
                     outlier.shape = 16, outlier.size = 2, notch = F ) + 
        coord_flip() +
        ggtitle("Boxplot of SibSp")
##histogram of Parch
hist(train$Parch)
ggplot(train, aes(x = Parch, fill = factor(Survived))) +
        geom_histogram(position = 'dodge', bins = 8) +
        ggtitle("Histogram of parent/Children")
#box plot of Parch
ggplot(train, aes(x = Parch)) +
        geom_boxplot(outlier.colour = "red", 
                     outlier.shape = 16, outlier.size = 2, notch = F ) + 
        coord_flip() +
        ggtitle("Boxplot of Parch")
##Family size relate to survival?
##histogram of Fare
hist(train$Fare)
ggplot(train, aes(x = Fare, fill = factor(Survived))) +
        geom_histogram(position = 'dodge') +
        ggtitle("Histogram of Fare")
#box plot of Fare
ggplot(train, aes(x = Fare)) +
        geom_boxplot(outlier.colour = "red", 
                     outlier.shape = 16, outlier.size = 2, notch = F ) + 
        coord_flip() +
        ggtitle("Boxplot of Fare") ##too much outliers needs scaling 
#who survived more (father, mother, or child?)
#####################char Data################################

        
#####################char in to factors#######################
factors <- c("Survived", "Pclass", "Sex", "Embarked")
#lapply will apply as.factor() function to these attributes
combineData[factors] <- lapply(combineData[factors], function(x) as.factor(x))
str(combineData) ## change "male" values to -1 from 0 

##################Imputing Embark############################ 
which(is.na(combineData[,"Embarked"])) #row 62 & 830 missing
combineData$Embarked[c(62,830)]
combineData[c(62,830),]
#They are both p-class and paid $80 Fare
embarkData <- combineData[c(-62,-830),]
embarkData <- select(embarkData, Fare, Embarked, Pclass)
median(embarkData$Fare, na.rm = T)
g.embark <- ggplot(embarkData, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
        geom_boxplot() +
        geom_hline(aes(yintercept = 76.5)) +
        ggtitle("Boxplot of Embarked vs Fare for each Pclass")
g.embark
combineData[c(62,830),"Embarked"] <- "C"
#############Impute Fare#####################
which(is.na(combineData[,"Fare"]))
combineData[1044,]
ggplot(combineData[combineData$Embarked == "S" & combineData$Pclass == "3",], aes(x= Fare)) +
        geom_boxplot() +
        geom_vline(aes(xintercept= median(Fare, na.rm = T))) +
        ggtitle("Histogram of Embarked(S) vs PClass(3)")
which(is.na(combineData[,"Fare"]))
combineData$Fare[1044] <- median(combineData$Fare, na.rm =T)

####################Imputing Age#################################
ageData <- select(combineData, Age, Sex, SibSp, Parch, Embarked, Fare)
set.seed(123)
miceModel <- mice(ageData, method = "rf") #rf stands for random forest 
miceOutput <- complete(miceModel)
View(miceOutput)
hist(miceOutput$Age, freq = F, ylim=c(0,0.04)) #Freq = F will visualize density 
hist(combineData$Age, freq = F, ylim=c(0,0.04))
#inserting imputed Age Values 
combineData <- mutate(combineData, Age = miceOutput$Age)
summary(combineData)
#####################Family Size##############################
#adding the column with ones family size including him/herself
combineData <- mutate(combineData, FamilySize = SibSp + Parch +1)
##visualization between family size & survival 
familyData <- select(combineData[1:891,],Survived, FamilySize)
fam <- ggplot(familyData, aes(x=FamilySize, fill = factor(Survived)))
fam <- fam + geom_histogram(position = 'dodge', binwidth = 1)
fam <- fam + scale_x_continuous(breaks = c(1:11))
fam <- fam + ggtitle("Family Size and Survival")
fam #less the family size higher number of not surviving. 

#####################Surname###################################
combineData$Surname <- gsub("(.*,)","", combineData$Name)
combineData$Surname <- gsub("(\\..*)","", combineData$Surname)
combineData$Surname <- gsub(" ","", combineData$Surname)
#substituting some values to more commonly used surname
combineData$Surname[combineData$Surname == 'Mlle'] <- "Miss"
combineData$Surname[combineData$Surname == "Mme"] <- "Mrs"
combineData$Surname[combineData$Surname == "Ms"] <- "Miss"
other <- c("Capt", "Col", "Don", "Dona", "Jonkheer","Lady",
           "Major", "Rev", "Sir", "theCountess", "Dr")
combineData$Surname[combineData$Surname %in% other] <- "Other"
table(combineData$Sex,combineData$Surname)
combineData$Surname <- as.factor(combineData$Surname)

##########################################################################
#plotting multivariant historgram of sex and age
ggplot(train, aes(Age, fill = factor(Survived))) +
        geom_histogram() +
        facet_grid( .~Sex) 
####################Father,Mother,Minor###################################

combineData <- mutate(combineData, Father = 
                  ifelse(Sex == "male" & Parch > 0 & Age >= 18
                         ,1,0))
combineData <- mutate(combineData, Mother = 
                  ifelse(Sex == "female" & Parch > 0 & Age >= 18 & Surname != "Miss",
                         1,0))
combineData <- mutate(combineData, minor = 
                  ifelse(Age < 18, 1,0))
combineData$Father <- as.factor(combineData$Father)
combineData$Mother <- as.factor(combineData$Mother)
combineData$minor <- as.factor(combineData$minor)
#histogram of father
################checking for NAs#######################
train <- combineData[1:891,]
test <- combineData[1:418,]
test <- select(test, Pclass, Sex, Fare, Age, Embarked, FamilySize, Surname, Father, Mother, minor)
train <- select(train, Pclass, Sex, Fare, Age, Embarked, FamilySize, Surname, Father, Mother, minor, Survived)
#######################################################
#Support Vector Machine 
svmOutput <- ksvm(Survived ~., data = train, kernel = "rbfdot", kpar = "automatic", C= 25,
                  cross = 3, prob.model = T)
svmOutput

svmPred <- predict(svmOutput, test, type = "votes")
#########################################################\
#Random Forest 
rf_model <- randomForest(Survived ~ Pclass + Sex + Fare + Age + Embarked + 
                                 FamilySize + Surname + Father + 
                                 Mother + minor,
                         data = train, localImp = TRUE)
rf_model

rfPredict <- predict(rf_model, test)
#Distribution
min_depth_frame <- min_depth_distribution(rf_model)
save(min_depth_frame, file = 'min_depth_frame.rda')
load('min_depth_frame.rda')
plot_min_depth_distribution(min_depth_frame)

#variable importance
importance_frame <- measure_importance(rf_model)
importance_frame
save(importance_frame, file = 'importance_frame.rda')
plot_multi_way_importance(importance_frame)
plot_importance_ggpairs(importance_frame)

#Final Data 
final <- read.csv("test.csv")
rfSolution <- data.frame(PassengerID = final$PassengerId, 
                         rf_Survived= rfPredict,
                         svm_Survived = svmPred[2,])