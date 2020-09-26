
library(dplyr)
library(tidyr)
library(mltools)
library(glmnet)
library(caret)


##### Data Cleaning #####
set.seed(123)
data <- read.csv("Admissions_Combined.csv", stringsAsFactors = T)

data <- data %>%
  select(-c(Sport.3.Rating,35:46,School..2.Organization.Category,School..3.Organization.Category))#get rid of all columns that have no entries

data <- data %>%
  select(-c(Admit.Type,19:21,School..1.Organization.Category,School.1.GPA,School.1.GPA.Scale,35:39,
            SAT.I.Writing,SAT.R.Evidence.Based.Reading.and.Writing.Section,
            SAT.R.Math.Section,ACT.Concordance.Score..of.SAT.,SAT.I.CR...M,
            SAT.I.Critical.Reading,SAT.I.Math,Permanent.Postal,Permanent.Country,
            Academic.Interest.2,SAT.Concordance.Score..of.SAT.R.,
            ACT.Concordance.Score..of.SAT.R.,School.1.Code))#Removed excess sport columns as secondary and third sports had almost no ratings
#Removed Org category as there we're only 17 records that were not high school
#Admit type had only one factor
#Removed School 1 gpa and gpa scale as the recalculated score accounts for the difficulty of the school so it is a more reliable measuring stick
#Removed ACT subscores as they all average to the ACT composite score, ACT writing removed as there were only 3 obs
#Removed SAT R reading,writing,math, because they have no value. Concordance score removed as it had very few obs.
#Remvoed SAT I information as there were not enough obs
#Removed postal and country as that information is effectively the geomarket
#Removed academic interest 2 as the first interest is far more important

data <- data %>% mutate_all(na_if,"")#Change all blanks to NA for ease of use


### Changing data types ###

data$Total.Event.Participation <- as.factor(data$Total.Event.Participation)
data$Count.of.Campus.Visits <- as.factor(data$Count.of.Campus.Visits)
data$ACT.Composite <- as.factor(data$ACT.Composite)
data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section <- as.factor(data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section)
data$Academic.Index <- as.factor(data$Academic.Index)
data$Intend.to.Apply.for.Financial.Aid. <- as.factor(data$Intend.to.Apply.for.Financial.Aid.)

data$School.1.Class.Rank..Numeric. <- as.double(data$School.1.Class.Rank..Numeric.)
data$School.1.Class.Size..Numeric. <- as.double(data$School.1.Class.Size..Numeric.)



### The following block is filling in NA values ###
levels(data$Ethnicity) <- c(levels(data$Ethnicity),"Undisclosed")
data$Ethnicity[is.na(data$Ethnicity)] <- "Undisclosed"

levels(data$Race) <- c(levels(data$Race),"Undisclosed")
data$Race[is.na(data$Race)] <- "Undisclosed"

levels(data$Religion) <- c(levels(data$Religion),"Undisclosed")
data$Religion[is.na(data$Religion)] <- "Undisclosed"

levels(data$Staff.Assigned.Name) <- c(levels(data$Staff.Assigned.Name),"Unassigned")
data$Staff.Assigned.Name[is.na(data$Staff.Assigned.Name)] <- "Unassigned"

levels(data$Sport.1.Sport) <- c(levels(data$Sport.1.Sport),"None")
data$Sport.1.Sport[is.na(data$Sport.1.Sport)] <- "None"

levels(data$Sport.1.Rating) <- c(levels(data$Sport.1.Rating),"None")
data$Sport.1.Rating[is.na(data$Sport.1.Rating)] <- "None"

levels(data$ACT.Composite) <- c(levels(data$ACT.Composite),"Not Taken")
data$ACT.Composite[is.na(data$ACT.Composite)] <- "Not Taken"

levels(data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section) <- c(levels(data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section),"Not Taken")
data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section[is.na(data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section)] <- "Not Taken"

levels(data$Academic.Index) <- c(levels(data$Academic.Index),"3")
data$Academic.Index[is.na(data$Academic.Index)] <- "3"

data$Intend.to.Apply.for.Financial.Aid.[is.na(data$Intend.to.Apply.for.Financial.Aid.)] <- 0 #Filled in the NA's with zero because students who did not reply at all certainly did not want to apply for financial aid

data <- data[-1878,]#Remove this entry as it is the only entry without an academic interest

data$School.1.Class.Rank..Numeric.[is.na(data$School.1.Class.Rank..Numeric.)] <- mean(data$School.1.Class.Rank..Numeric.,na.rm = T)#I decided to impute the mean here because theres no reliable way to determine which missing entries are homeschooled and which ones just werent provided
data$School.1.Class.Size..Numeric.[is.na(data$School.1.Class.Size..Numeric.)] <- mean(data$School.1.Class.Size..Numeric.,na.rm = T)


### Recoding/Modifying Data w/REGEX ####

data$First_Source.Origin.First.Source.Date <- as.character(data$First_Source.Origin.First.Source.Date)
data$First_Source.Origin.First.Source.Date <- gsub("\\s.*","",data$First_Source.Origin.First.Source.Date)#Got rid of the times after the dates, this will allow the model to treat every day as one category instead of having multiple times in the same day
data$First_Source.Origin.First.Source.Date[grep("2013",data$First_Source.Origin.First.Source.Date)] <- 2013
data$First_Source.Origin.First.Source.Date[grep("2014",data$First_Source.Origin.First.Source.Date)] <- 2014
data$First_Source.Origin.First.Source.Date[grep("2015",data$First_Source.Origin.First.Source.Date)] <- 2015
data$First_Source.Origin.First.Source.Date[grep("2016",data$First_Source.Origin.First.Source.Date)] <- 2016
data$First_Source.Origin.First.Source.Date[grep("2017",data$First_Source.Origin.First.Source.Date)] <- 2017
data$First_Source.Origin.First.Source.Date[grep("2018",data$First_Source.Origin.First.Source.Date)] <- 2018
data$First_Source.Origin.First.Source.Date[grep("2019",data$First_Source.Origin.First.Source.Date)] <- 2019
data$First_Source.Origin.First.Source.Date <- as.factor(data$First_Source.Origin.First.Source.Date)

data$Inquiry.Date <- as.character(data$Inquiry.Date)
data$Inquiry.Date[is.na(data$Inquiry.Date)] <- "None"
data$Inquiry.Date <- gsub("\\s.*","",data$Inquiry.Date)#Same as code above
data$Inquiry.Date[grep("2016",data$Inquiry.Date)] <- 2016
data$Inquiry.Date[grep("2017",data$Inquiry.Date)] <- 2017
data$Inquiry.Date[grep("2018",data$Inquiry.Date)] <- 2018
data$Inquiry.Date[grep("2019",data$Inquiry.Date)] <- 2019
data$Inquiry.Date <- as.factor(data$Inquiry.Date)

data$Submitted <- as.character(data$Submitted)
data$Submitted <- gsub("\\s.*","",data$Submitted)#Same as code above
data$Submitted <- gsub("/.*","",data$Submitted)#Im just interested in the month here cause my theory is that those who apply early to trinity are the most likely to accept admission
data$Submitted <- as.factor(data$Submitted)

### Making Legacy/Athlete Columns Usable ###
data <- data %>%
  unite("Info", c(Legacy,Athlete))

data$Info <- gsub("NA_NA", NA,data$Info)
data$Info <- gsub(",\\s","/",data$Info)
data$Info <- gsub("NA_","",data$Info)
data$Info <- gsub("_NA","",data$Info)
data$Info <- gsub("_.*","",data$Info)

data$Athlete <- data$Info
data$Legacy <- data$Info
data$VIP <- data$Info
data$Fine.Arts <- data$Info
data$Opt.Out <- data$Info

Athlete_Idx <- grep("*Athlete*",data$Athlete)
data$Athlete[Athlete_Idx] <- "Athlete"
data$Athlete[-Athlete_Idx] <- "Non-Athlete"

Legacy_Idx <- grep("Legacy",data$Legacy)
data$Legacy[Legacy_Idx] <- "Legacy"
data$Legacy[-Legacy_Idx] <- "Non_Legacy"

VIP_Idx <- grep("VIP",data$VIP)
data$VIP[VIP_Idx] <- "VIP"
data$VIP[-VIP_Idx] <- "Non_VIP"

FineArts_Idx <- grep("Fine Arts",data$Fine.Arts)
data$Fine.Arts[FineArts_Idx] <- "Fine Arts"
data$Fine.Arts[-FineArts_Idx] <- "Non-Fine Arts"

OptOut_Idx <- grep("Opt Out",data$Opt.Out)
data$Opt.Out[OptOut_Idx] <- "Opt Out"
data$Opt.Out[-OptOut_Idx] <- "Non_Opt Out"

data <- data %>%
  select(-c(Info,Sport.1.Sport))#After this block I have 5 new columns that contain the information from the Info column in usable format
#Also removing the sport column as I dont think the sport the student plays matters, just if they're an athlete and their sport rating is needed
### Making a Modified Ivy League Academic Index ###

#(Outside Research from https://blog.collegevine.com/what-is-the-academic-index-how-is-it-calculated/)

data <- data %>%
  mutate(AI_Ivy_Sat = ((as.double(as.character(SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section))/1600)*120)
         +((School.1.GPA.Recalculated/4)*120), AI_Ivy_Act = ((as.double(as.character(ACT.Composite))/36)*120)+((School.1.GPA.Recalculated/4)*120))

data$AI_Ivy_Act[is.na(data$AI_Ivy_Act)] <- 0
data$AI_Ivy_Sat[is.na(data$AI_Ivy_Sat)] <- 0

data$Ivy_Academic_Index <- pmax(data$AI_Ivy_Act,data$AI_Ivy_Sat)

data$Ivy_Academic_Index[data$Ivy_Academic_Index == 0] <- mean(data$Ivy_Academic_Index,na.rm = T)

data <- data %>%
  select(-c(AI_Ivy_Sat,AI_Ivy_Act))

data$Athlete <- as.factor(data$Athlete)
data$Legacy <- as.factor(data$Legacy)
data$VIP <- as.factor(data$VIP)
data$Fine.Arts <- as.factor(data$Fine.Arts)
data$Opt.Out <- as.factor(data$Opt.Out)

##### Logistic Regression #####

##### Sampling #####

data$Decision <- as.character(data$Decision)
data$Decision[grep("26",data$Decision)] <- 0
data$Decision[grep("28",data$Decision)] <- 0
data$Decision[grep("30",data$Decision)] <- 0
data$Decision[grep("ST",data$Decision)] <- 1
data$Decision[grep("10",data$Decision)] <- 1
data$Decision <- as.numeric(data$Decision)#1 is accept offer and 0 is did not accept offer

data <- data %>%
  select(-c(Academic.Interest.1,Religion,Race,Submitted,Merit.Award,Athlete,First_Source.Origin.First.Source.Summary,
            SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section,ï..Entry.Term..Application.,School.1.Class.Size..Numeric.,
            School.1.Class.Rank..Numeric.,Fine.Arts))
data <- data[data$Staff.Assigned.Name != "Sarah Yaccino",]
data <- data[data$First_Source.Origin.First.Source.Date != "2013",]
data <- data[data$Staff.Assigned.Name != "Unassinged",]
data <- data[-c(5058,5658),]
data <- data[-c(3261,5663),]
data <- data[-c(2697,4144),]
data <- data[data$Count.of.Campus.Visits != "6",]

data <- data %>%
  select(-Permanent.Geomarket)


smp_size <- floor(0.50 * nrow(data))#Code from Dr. Jorge Colazo
train_ind <- sample(seq_len(nrow(data)),size = smp_size)
train <- data[train_ind, ]
test <- head(data[-train_ind, ],smp_size)

test <- test %>%
  select(-Decision)


model <- glm(Decision ~. ,family=binomial(link='logit'),data=train)


fitted.results <- predict(model,newdata=test,type='response')

final.results <- ifelse(fitted.results > 0.35,1,0)

test <- head(data[-train_ind, ],smp_size)
accuracy <- mean(as.matrix(final.results) == as.matrix(test$Decision),na.rm=TRUE)#Adapted from Dr. Jorge Colazo   
print(paste('Accuracy is',accuracy))
t<-table(Real=test[,'Decision'],Pred=final.results)
t

library(Metrics)
Kappa<-ScoreQuadraticWeightedKappa(test$Decision, final.results,min.rating =0, max.rating =1)
Kappa

##### Random Forests #####
library(tree)
library(randomForest) 
library(gbm)

#bagging

test <- test %>%
  select(-Decision) 

bagging <- randomForest(Decision~.,mtry=19,train,ntrees=500)#Adapted from Dr. Jorge Colazo        

bagging.results <- predict(bagging,mtry=19,newdata=test)

bagging.final <- ifelse(bagging.results > 0.4,1,0)
test <- head(data[-train_ind, ],smp_size)

accuracy <- mean(as.matrix(bagging.final) == as.matrix(test$Decision),na.rm=TRUE)
print(paste('Accuracy is',accuracy))
t<-table(Real=test[,'Decision'],Pred=bagging.final)
t

Kappa <-ScoreQuadraticWeightedKappa(test$Decision, bagging.final,min.rating =0, max.rating =1)
Kappa

#bagging with random variables

test <- test %>%
  select(-Decision) 

bagging2 <- randomForest(Decision~.,train,mtry=8,ntrees=500)

bagging2.results <- predict(bagging2,mtry = 8,newdata = test)

bagging2.final <- ifelse(bagging2.results > 0.3,1,0)
test <- head(data[-train_ind, ],smp_size)

accuracy <- mean(as.matrix(bagging2.final) == as.matrix(test$Decision),na.rm=TRUE)
print(paste('Accuracy is',accuracy))
t<-table(Real=test[,'Decision'],Pred=bagging2.final)
t

Kappa <-ScoreQuadraticWeightedKappa(test$Decision, bagging2.final,min.rating =0, max.rating =1)
Kappa

#bagging with importance 

test <- test %>%
  select(-Decision) 

bagging3 <- randomForest(Decision~.,mtry=19,train,ntrees = 500, importance = T)

bagging3.results <- predict(bagging3,newdata = test)

bagging3.final <- ifelse(bagging3.results > 0.3,1,0)
test <- head(data[-train_ind, ],smp_size)

accuracy <- mean(as.matrix(bagging3.final) == as.matrix(test$Decision),na.rm=TRUE)
print(paste('Accuracy is',accuracy))
t<-table(Real=test[,'Decision'],Pred=bagging3.final)
t

Kappa <-ScoreQuadraticWeightedKappa(test$Decision, bagging3.final,min.rating =0, max.rating =1)
Kappa

#boosting

test <- test %>%
  select(-Decision) 

boosted_model <- gbm(Decision~.,data=train,distribution="bernoulli",n.trees=500)

boosted_results <- predict.gbm(boosted_model,newdata = test,n.trees = 500)

boosted.final <- ifelse(boosted_results > 0.5,1,0)

test <- head(data[-train_ind, ],smp_size)

accuracy <- mean(as.matrix(boosted.final) == as.matrix(test$Decision),na.rm=TRUE)
print(paste('Accuracy is',accuracy))
t<-table(Real=test[,'Decision'],Pred=boosted.final)
t

Kappa <-ScoreQuadraticWeightedKappa(test$Decision, bagging3.final,min.rating =0, max.rating =1)
Kappa

true_negatives <- t[1,1]/(t[1,1]+t[1,2]) #This model correctly classifies 97% of the students who decline acceptance!! Using this confusion table you could eliminate most of the students who arent coming which would leave you with the students who are.
print(paste("Percent of negatives correctly classified", true_negatives))

#Neural Networks 
library(neuralnet)

train <- data.table::as.data.table(train)
train <- one_hot(train)
test <- data.table::as.data.table(test)
test <- one_hot(test)

xnam <- paste0(colnames(train)[-80])
xnam <- gsub("\\s","_",xnam)
f <- as.formula(paste("Decision ~", paste(xnam,collapse = "+")))
colnames(train) <- gsub("\\s","_",colnames(train))

train$School.1.GPA.Recalculated <- (train$School.1.GPA.Recalculated - min(train$School.1.GPA.Recalculated))/(max(train$School.1.GPA.Recalculated)-min(train$School.1.GPA.Recalculated))
test$School.1.GPA.Recalculated <- (test$School.1.GPA.Recalculated - min(test$School.1.GPA.Recalculated))/(max(test$School.1.GPA.Recalculated)-min(test$School.1.GPA.Recalculated))
train$Ivy_Academic_Index <- (train$Ivy_Academic_Index - min(train$Ivy_Academic_Index))/(max(train$Ivy_Academic_Index)-min(train$Ivy_Academic_Index))
test$Ivy_Academic_Index <- (test$Ivy_Academic_Index - min(test$Ivy_Academic_Index))/(max(test$Ivy_Academic_Index)-min(test$Ivy_Academic_Index))


nn <- neuralnet(formula = Decision ~ Sex_F + Sex_M + Ethnicity_ + Ethnicity_Undisclosed + 
                  First_Source.Origin.First.Source.Date_2013 + First_Source.Origin.First.Source.Date_2014 + 
                  First_Source.Origin.First.Source.Date_2015 + First_Source.Origin.First.Source.Date_2016 + 
                  First_Source.Origin.First.Source.Date_2017 + First_Source.Origin.First.Source.Date_2018 + 
                  First_Source.Origin.First.Source.Date_2019 + Inquiry.Date_2016 + Inquiry.Date_2017 + 
                  Inquiry.Date_2018 + Inquiry.Date_2019 + Inquiry.Date_None + Application.Source_ApplyTexas + 
                  Application.Source_Coalition + Application.Source_CommonApp + Application.Source_Select_Scholar + 
                  Decision.Plan_Early_Action_I + Decision.Plan_Early_Action_II + Decision.Plan_Early_Decision_I + 
                  Decision.Plan_Early_Decision_II + Decision.Plan_Regular_Decision + Staff.Assigned.Name_ + 
                  Staff.Assigned.Name_Bradford_Durchslag + Staff.Assigned.Name_Charles_Clark + 
                  Staff.Assigned.Name_Elena_Wilson + Staff.Assigned.Name_Gail_Roberson + 
                  Staff.Assigned.Name_Hillary_Everts + Staff.Assigned.Name_Jeremy_Boyce + 
                  Staff.Assigned.Name_Jesse_Gamble + Staff.Assigned.Name_Jessica_Reyes + 
                  Staff.Assigned.Name_Juan_Perez + Staff.Assigned.Name_Justin_Doty + 
                  Staff.Assigned.Name_Kindel_Hollis + Staff.Assigned.Name_Michaela_Knipp + 
                  Staff.Assigned.Name_Morgan_Hybert + Staff.Assigned.Name_Nicole_Fratto + 
                  Staff.Assigned.Name_Sarah_Yaccino + Staff.Assigned.Name_Unassigned + 
                  Sport.1.Rating_ + Sport.1.Rating_Blue_Chip + Sport.1.Rating_Franchise + 
                  Sport.1.Rating_Varsity + Sport.1.Rating_None + Total.Event.Participation_0 + 
                  Total.Event.Participation_1 + Total.Event.Participation_2 + Total.Event.Participation_3 + 
                  Total.Event.Participation_4 + Count.of.Campus.Visits_0 + Count.of.Campus.Visits_1 + 
                  Count.of.Campus.Visits_2 + Count.of.Campus.Visits_3 + Count.of.Campus.Visits_4 + 
                  Count.of.Campus.Visits_6 + School.1.GPA.Recalculated + ACT.Composite_20 + ACT.Composite_21 + 
                  ACT.Composite_22 + ACT.Composite_23 + ACT.Composite_24 + ACT.Composite_25 + ACT.Composite_26 + 
                  ACT.Composite_27 + ACT.Composite_28 + ACT.Composite_29 + ACT.Composite_30 + ACT.Composite_31 + 
                  ACT.Composite_32 + ACT.Composite_33 + ACT.Composite_34 + ACT.Composite_35 + ACT.Composite_36 + 
                  ACT.Composite_Not_Taken + Citizenship.Status_Foreign_National + Citizenship.Status_Permanent_Resident + 
                  Citizenship.Status_US_Citizen + Academic.Index_1 + Academic.Index_2 + Academic.Index_3 + 
                  Academic.Index_4 + Academic.Index_5 + Intend.to.Apply.for.Financial.Aid._0 + 
                  Intend.to.Apply.for.Financial.Aid._1 + Legacy_Legacy + Legacy_Non_Legacy + VIP_Non_VIP + VIP_VIP + 
                  Opt.Out_Non_Opt_Out + Opt.Out_Opt_Out + Ivy_Academic_Index,train, hidden=c(3), 
                act.fct = "logistic",linear.output = T)
plot(nn)

test <- test %>%
  select(-Decision,) 

detach("package:dplyr",unload = T)
colnames(test) <- colnames(train)[-80]
result <- compute(nn,test[,-c(4:5)])
result$net.result

result.final <- ifelse(result$net.result >.5,1,0)

test <- head(data[-train_ind, ],smp_size)

accuracy <- mean(as.matrix(result.final) == as.matrix(test$Decision),na.rm=TRUE)
print(paste('Accuracy is',accuracy))
t<-table(Real=test[,'Decision'],Pred=result.final)
t

Kappa <-ScoreQuadraticWeightedKappa(test$Decision, result.final,min.rating =0, max.rating =1)
Kappa

####################                                                                    ####################
#################### This Cleaning Code is repeated to ensure clustering works properly ####################
####################                                                                    ####################
library(dplyr)
library(tidyr)
library(mltools)
library(glmnet)
library(caret)

##### Data Cleaning #####
set.seed(123)
data <- read.csv("Admissions_Combined.csv", stringsAsFactors = T)

data <- data %>%
  select(-c(Sport.3.Rating,35:46,School..2.Organization.Category,School..3.Organization.Category))#get rid of all columns that have no entries

data <- data %>%
  select(-c(Admit.Type,19:21,School..1.Organization.Category,School.1.GPA,School.1.GPA.Scale,35:39,
            SAT.I.Writing,SAT.R.Evidence.Based.Reading.and.Writing.Section,
            SAT.R.Math.Section,ACT.Concordance.Score..of.SAT.,SAT.I.CR...M,
            SAT.I.Critical.Reading,SAT.I.Math,Permanent.Postal,Permanent.Country,
            Academic.Interest.2,SAT.Concordance.Score..of.SAT.R.,
            ACT.Concordance.Score..of.SAT.R.,School.1.Code))#Removed excess sport columns as secondary and third sports had almost no ratings
#Removed Org category as there we're only 17 records that were not high school
#Admit type had only one factor
#Removed School 1 gpa and gpa scale as the recalculated score accounts for the difficulty of the school so it is a more reliable measuring stick
#Removed ACT subscores as they all average to the ACT composite score, ACT writing removed as there were only 3 obs
#Removed SAT R reading,writing,math, because they have no value. Concordance score removed as it had very few obs.
#Remvoed SAT I information as there were not enough obs
#Removed postal and country as that information is effectively the geomarket
#Removed academic interest 2 as the first interest is far more important

data <- data %>% mutate_all(na_if,"")#Change all blanks to NA for ease of use


### Changing data types ###

data$Total.Event.Participation <- as.factor(data$Total.Event.Participation)
data$Count.of.Campus.Visits <- as.factor(data$Count.of.Campus.Visits)
data$ACT.Composite <- as.factor(data$ACT.Composite)
data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section <- as.factor(data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section)
data$Academic.Index <- as.factor(data$Academic.Index)
data$Intend.to.Apply.for.Financial.Aid. <- as.factor(data$Intend.to.Apply.for.Financial.Aid.)

data$School.1.Class.Rank..Numeric. <- as.double(data$School.1.Class.Rank..Numeric.)
data$School.1.Class.Size..Numeric. <- as.double(data$School.1.Class.Size..Numeric.)



### The following block is filling in NA values ###
levels(data$Ethnicity) <- c(levels(data$Ethnicity),"Undisclosed")
data$Ethnicity[is.na(data$Ethnicity)] <- "Undisclosed"

levels(data$Race) <- c(levels(data$Race),"Undisclosed")
data$Race[is.na(data$Race)] <- "Undisclosed"

levels(data$Religion) <- c(levels(data$Religion),"Undisclosed")
data$Religion[is.na(data$Religion)] <- "Undisclosed"

levels(data$Staff.Assigned.Name) <- c(levels(data$Staff.Assigned.Name),"Unassigned")
data$Staff.Assigned.Name[is.na(data$Staff.Assigned.Name)] <- "Unassigned"

levels(data$Sport.1.Sport) <- c(levels(data$Sport.1.Sport),"None")
data$Sport.1.Sport[is.na(data$Sport.1.Sport)] <- "None"

levels(data$Sport.1.Rating) <- c(levels(data$Sport.1.Rating),"None")
data$Sport.1.Rating[is.na(data$Sport.1.Rating)] <- "None"

levels(data$ACT.Composite) <- c(levels(data$ACT.Composite),"Not Taken")
data$ACT.Composite[is.na(data$ACT.Composite)] <- "Not Taken"

levels(data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section) <- c(levels(data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section),"Not Taken")
data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section[is.na(data$SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section)] <- "Not Taken"

levels(data$Academic.Index) <- c(levels(data$Academic.Index),"3")
data$Academic.Index[is.na(data$Academic.Index)] <- "3"

data$Intend.to.Apply.for.Financial.Aid.[is.na(data$Intend.to.Apply.for.Financial.Aid.)] <- 0 #Filled in the NA's with zero because students who did not reply at all certainly did not want to apply for financial aid

data <- data[-1878,]#Remove this entry as it is the only entry without an academic interest

data$School.1.Class.Rank..Numeric.[is.na(data$School.1.Class.Rank..Numeric.)] <- mean(data$School.1.Class.Rank..Numeric.,na.rm = T)#I decided to impute the mean here because theres no reliable way to determine which missing entries are homeschooled and which ones just werent provided
data$School.1.Class.Size..Numeric.[is.na(data$School.1.Class.Size..Numeric.)] <- mean(data$School.1.Class.Size..Numeric.,na.rm = T)


### Recoding/Modifying Data w/REGEX ####

data$First_Source.Origin.First.Source.Date <- as.character(data$First_Source.Origin.First.Source.Date)
data$First_Source.Origin.First.Source.Date <- gsub("\\s.*","",data$First_Source.Origin.First.Source.Date)#Got rid of the times after the dates, this will allow the model to treat every day as one category instead of having multiple times in the same day
data$First_Source.Origin.First.Source.Date[grep("2013",data$First_Source.Origin.First.Source.Date)] <- 2013
data$First_Source.Origin.First.Source.Date[grep("2014",data$First_Source.Origin.First.Source.Date)] <- 2014
data$First_Source.Origin.First.Source.Date[grep("2015",data$First_Source.Origin.First.Source.Date)] <- 2015
data$First_Source.Origin.First.Source.Date[grep("2016",data$First_Source.Origin.First.Source.Date)] <- 2016
data$First_Source.Origin.First.Source.Date[grep("2017",data$First_Source.Origin.First.Source.Date)] <- 2017
data$First_Source.Origin.First.Source.Date[grep("2018",data$First_Source.Origin.First.Source.Date)] <- 2018
data$First_Source.Origin.First.Source.Date[grep("2019",data$First_Source.Origin.First.Source.Date)] <- 2019
data$First_Source.Origin.First.Source.Date <- as.factor(data$First_Source.Origin.First.Source.Date)

data$Inquiry.Date <- as.character(data$Inquiry.Date)
data$Inquiry.Date[is.na(data$Inquiry.Date)] <- "None"
data$Inquiry.Date <- gsub("\\s.*","",data$Inquiry.Date)#Same as code above
data$Inquiry.Date[grep("2016",data$Inquiry.Date)] <- 2016
data$Inquiry.Date[grep("2017",data$Inquiry.Date)] <- 2017
data$Inquiry.Date[grep("2018",data$Inquiry.Date)] <- 2018
data$Inquiry.Date[grep("2019",data$Inquiry.Date)] <- 2019
data$Inquiry.Date <- as.factor(data$Inquiry.Date)

data$Submitted <- as.character(data$Submitted)
data$Submitted <- gsub("\\s.*","",data$Submitted)#Same as code above
data$Submitted <- gsub("/.*","",data$Submitted)#Im just interested in the month here cause my theory is that those who apply early to trinity are the most likely to accept admission
data$Submitted <- as.factor(data$Submitted)

### Making Legacy/Athlete Columns Usable ###
data <- data %>%
  unite("Info", c(Legacy,Athlete))

data$Info <- gsub("NA_NA", NA,data$Info)
data$Info <- gsub(",\\s","/",data$Info)
data$Info <- gsub("NA_","",data$Info)
data$Info <- gsub("_NA","",data$Info)
data$Info <- gsub("_.*","",data$Info)

data$Athlete <- data$Info
data$Legacy <- data$Info
data$VIP <- data$Info
data$Fine.Arts <- data$Info
data$Opt.Out <- data$Info

Athlete_Idx <- grep("*Athlete*",data$Athlete)
data$Athlete[Athlete_Idx] <- "Athlete"
data$Athlete[-Athlete_Idx] <- "Non-Athlete"

Legacy_Idx <- grep("Legacy",data$Legacy)
data$Legacy[Legacy_Idx] <- "Legacy"
data$Legacy[-Legacy_Idx] <- "Non_Legacy"

VIP_Idx <- grep("VIP",data$VIP)
data$VIP[VIP_Idx] <- "VIP"
data$VIP[-VIP_Idx] <- "Non_VIP"

FineArts_Idx <- grep("Fine Arts",data$Fine.Arts)
data$Fine.Arts[FineArts_Idx] <- "Fine Arts"
data$Fine.Arts[-FineArts_Idx] <- "Non-Fine Arts"

OptOut_Idx <- grep("Opt Out",data$Opt.Out)
data$Opt.Out[OptOut_Idx] <- "Opt Out"
data$Opt.Out[-OptOut_Idx] <- "Non_Opt Out"

data <- data %>%
  select(-c(Info,Sport.1.Sport))#After this block I have 5 new columns that contain the information from the Info column in usable format
#Also removing the sport column as I dont think the sport the student plays matters, just if they're an athlete and their sport rating is needed
### Making a Modified Ivy League Academic Index ###

#(Outside Research from https://blog.collegevine.com/what-is-the-academic-index-how-is-it-calculated/)

data <- data %>%
  mutate(AI_Ivy_Sat = ((as.double(as.character(SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section))/1600)*120)
         +((School.1.GPA.Recalculated/4)*120), AI_Ivy_Act = ((as.double(as.character(ACT.Composite))/36)*120)+((School.1.GPA.Recalculated/4)*120))

data$AI_Ivy_Act[is.na(data$AI_Ivy_Act)] <- 0
data$AI_Ivy_Sat[is.na(data$AI_Ivy_Sat)] <- 0

data$Ivy_Academic_Index <- pmax(data$AI_Ivy_Act,data$AI_Ivy_Sat)

data$Ivy_Academic_Index[data$Ivy_Academic_Index == 0] <- mean(data$Ivy_Academic_Index,na.rm = T)

data <- data %>%
  select(-c(AI_Ivy_Sat,AI_Ivy_Act))

data$Athlete <- as.factor(data$Athlete)
data$Legacy <- as.factor(data$Legacy)
data$VIP <- as.factor(data$VIP)
data$Fine.Arts <- as.factor(data$Fine.Arts)
data$Opt.Out <- as.factor(data$Opt.Out)



##### K-Means #####
data1 <- data.table::as.data.table(data)
data1 <- one_hot(data1)

sum_sq <- sapply(1:10, 
                 function(k){
                   kmeans(data1, k, nstart=30,iter.max = 15 )$tot.withinss
                 })#Code from Dr. Jorge Colazo

plot(1:10, sum_sq,
     type="b", pch = 1,  
     xlab="K",
     ylab="Total within-clusters sum of squares")#Code from Dr. Jorge Colazo

model<-kmeans(data1,4,nstart=30)
kmean <-cbind(data1,model$cluster)


library(psych)
G <- describeBy(kmean,model$cluster)

G1_desc <- order(G[["1"]][["mean"]],decreasing = T)[1:100]
G$`1`[G1_desc,c("vars","mean")]

G2_desc <- order(G[["2"]][["mean"]],decreasing = T)[1:100]
G$`2`[G2_desc,c("vars","mean")]

G3_desc <- order(G[["3"]][["mean"]],decreasing = T)[1:100]
G$`3`[G3_desc,c("vars","mean")]

G4_desc <- order(G[["4"]][["mean"]],decreasing = T)[1:100]
G$`4`[G4_desc,c("vars","mean")]

##### Hierarchical Clustering #####
detach("package:tree")
detach("package:randomForest") 
detach("package:gbm")

hc.average=hclust(dist(data1), method="average")


plot(hc.average,main="Average Linkage", xlab="", sub="", cex=.9)
k3_complete<-cutree(hc.average, k=2)
G <- describeBy(data1,k3_complete)

G1_desc <- order(G[["1"]][["mean"]],decreasing = T)[1:100]
G$`1`[G1_desc,c("vars","mean")]

G2_desc <- order(G[["2"]][["mean"]],decreasing = T)[1:100]
G$`2`[G2_desc,c("vars","mean")]
