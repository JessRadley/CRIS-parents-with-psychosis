library(tidyverse) #install.packages('readxl', repos='http://r.akriviahealth.nhs.uk')
library(psych)
library(readxl)
library(sjmisc)
library(lubridate)
library(anchors)
library(rsq)
library(stats)
library(rms)
library(car)
library(mice)

#realised in 'OU18 unknowns' dataset I had duplicated six entries, 093SLON08587W, 316FGUO03205C, 409OHTQ09244K, 412LKBA54020Q, 666NMJA46035P, 929OGAV71561X
#realised that 3 BRC_IDs with unknowns 145RC..., 517GZ..., 581KN...


main <- read_excel("participants_all_details_cleaned_OU18unknownsadded.xlsx", 
                   col_types = c("text", "date", "numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", 
                                 "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", 
                                 "date", "numeric", "date", "numeric", "date", "numeric", "date", "numeric", "date", "numeric", "date", "numeric", "date", "numeric", "date", 
                                 "date", "date", "date", "date", "date", "date", "numeric", "text", "text", "text", "numeric", "numeric"))

file<-main
file%>%count(Has_children)

#Assuming those with NA in 'Has_children' don't have children. 
file1<-replace.value(file, "Has_children", NA, 0)
file1%>%count(Has_children)

#Didn't clean this with the other characteristics
y<-file1
#F20
y<-replace.value(y, "Diagnosis", c("F20", "F20.0", "F20.1", "F20.2", "F20.3", "F20.4", "F20.5", "F20.6", "F20.8", "F20.9"), "F20")
#F21
y<-replace.value(y, "Diagnosis", c("F21", "F21.X"), "F21")
#F22
y<-replace.value(y, "Diagnosis", c("F22", "F22.0", "F22.8", "F22.9", "F24.X"), "F22&F24")
#F23
y<-replace.value(y, "Diagnosis", c("F23", "F23.0", "F23.1", "F23.2", "F23.3", "F23.8", "F23.9"), "F23")
#F25
y<-replace.value(y, "Diagnosis", c("F25.0", "F25.1", "F25.2", "F25.8", "F25.9"), "F25")
#F28
y<-replace.value(y, "Diagnosis", c("F28.0", "F28.X"), "F28")

y<-replace.value(y, "Diagnosis", c("F31.2", "F31.5"), "F31.2&F31.5")

file1<-y



###Changing character variables to factors

file1$Ethnicity_Value<-as.factor(file1$Ethnicity_Value)
file1$Ethnicity_Value<-relevel(file1$Ethnicity_Value, "White - British")

file1$Gender_Value<-as.factor(file1$Gender_Value)
file1$Gender_Value<-relevel(file1$Gender_Value, "Male")

file1$Marital_Status_Value<-as.factor(file1$Marital_Status_Value)
file1$Marital_Status_Value<-relevel(file1$Marital_Status_Value, "Single")

file1$Diagnosis<-as.factor(file1$Diagnosis)

file1$Accommodation<-as.factor(file1$Accommodation)
file1$Accommodation<-relevel(file1$Accommodation, "Supported Living")

file1$Employment<-as.factor(file1$Employment)
file1$Employment<-relevel(file1$Employment, "Unemployed")

file1$Smoking<-as.factor(file1$Smoking)
file1$Smoking<-relevel(file1$Smoking, "Non-smoker")

file1$group<-as.numeric(file1$group)

#file2 = only variables used in regression model
file2<-file1[, c(1, 3:10, 69:70, 74)]


###Linear modelling of parenting status##############
###IMPUTING MISSING DATA USING MICE####################
set.seed(1234)
my_imp = mice(file2, m=5, method=c("", "pmm", "polyreg", "polyreg", "polyreg", "polyreg", "polyreg", "polyreg", "", "", "polyreg", "pmm"), maxit=10)

fit<-with(data=my_imp, exp=glm(Has_children ~ 1 + Marital_Status_Value + CurrentAge + Gender_Value + Ethnicity_Value + Accommodation + Employment + Diagnosis + wardstay + Smoking + group, family = binomial(link = 'logit')))
pool<-pool(fit)
summary(pool)

###odds ratios#########
or<-pool(fit)%>%summary(conf.int = T, exponentiate = T)


###Linear modelling of parenting status (dependants)##############
y<-file1%>%filter(OU18_1 == "Under"|OU18_2 == "Under"|OU18_3 == "Under"|OU18_4 == "Under"|OU18_5 == "Under"|OU18_6 == "Under"|OU18_7 == "Under"|OU18_8 == "Under"|OU18_9 == "Under"|OU18_10 == "Under"|OU18_11 == "Under"|OU18_12 == "Under")
#nochildren
n<-file1%>%filter(Has_children == 0)
file3<-rbind(y, n)
file3%>%count(Has_children)
file3<-file3[, c(1, 3:10, 69:70, 74)]

###Changing character variables to factors


my_imp = mice(file3, m=5, method=c("", "pmm", "polyreg", "polyreg", "polyreg", "polyreg", "polyreg", "polyreg", "", "", "polyreg", "pmm"), maxit=10)


fit<-with(data=my_imp, exp=glm(Has_children ~ 1 + Marital_Status_Value + CurrentAge + Gender_Value + Ethnicity_Value + Accommodation + Employment + Diagnosis + wardstay + Smoking + group, family = binomial(link = 'logit')))
pool<-pool(fit)
summary(pool)

###odds ratios#########

or<-pool(fit)%>%summary(conf.int = T, exponentiate = T)

