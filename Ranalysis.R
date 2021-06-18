library(tidyverse)
library(psych)
library(readxl)
library(sjmisc)
library(lubridate)
library(anchors)
library(psych)
library(sjmisc)
library(rsq)
library(stats)
library(rms)

#realised in 'OU18 unknowns' dataset I had duplicated six entries, 093SLON08587W, 316FGUO03205C, 409OHTQ09244K, 412LKBA54020Q, 666NMJA46035P, 929OGAV71561X
#realised that 3 BRC_IDs with unknowns 145RC..., 517GZ..., 581KN...


main <- read_excel("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/Data for Migration to AWS/UKCRIS Docs/participants_all_details_cleaned_OU18unknownsadded.xlsx", 
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

#######DESCRIPTIVES##########################################

#AGE
age_withchildren<-(file1%>%filter(Has_children == 1)%>%count(CurrentAge))
age_withoutchildren<-(file1%>%filter(Has_children == 0)%>%count(CurrentAge))

colSums(age_withchildren[c(1), ]) #1 13-19 year olds
colSums(age_withchildren[c(2:11), ]) #78 20-29 year olds
colSums(age_withchildren[c(12:21), ]) #295 30-39 year olds
colSums(age_withchildren[c(22:31), ]) #443 40-49 year olds
colSums(age_withchildren[c(32:41), ]) #462 50-59 year olds
colSums(age_withchildren[c(42:51), ]) #310 60-69 year olds
colSums(age_withchildren[c(52:61), ]) #249 70-79 year olds
colSums(age_withchildren[c(62:71), ]) #131 80-89 year olds
colSums(age_withchildren[c(72:79), ]) #32 90-100 year olds
#11 NA

colSums(age_withoutchildren[c(1:7), ]) #122 13-19 year olds
colSums(age_withoutchildren[c(8:17), ]) #739 20-29 year olds
colSums(age_withoutchildren[c(18:27), ]) #728 30-39 year olds
colSums(age_withoutchildren[c(28:37), ]) #627 40-49 year olds
colSums(age_withoutchildren[c(38:47), ]) #493 50-59 year olds
colSums(age_withoutchildren[c(48:57), ]) #245 60-69 year olds
colSums(age_withoutchildren[c(58:67), ]) #125 70-79 year olds
colSums(age_withoutchildren[c(68:75), ]) #41 80-89 year olds
colSums(age_withoutchildren[c(76:82), ]) #10 90-100 year olds
#37 NA

#GENDER
file1%>%filter(Has_children == 1)%>%count(Gender_Value)
file1%>%filter(Has_children == 0)%>%count(Gender_Value)

#ETHNICITY
file1%>%filter(Has_children == 1)%>%count(Ethnicity_Value)
file1%>%filter(Has_children == 0)%>%count(Ethnicity_Value)

#MARITAL STATUS
file1%>%filter(Has_children == 1)%>%count(Marital_Status_Value)
file1%>%filter(Has_children == 0)%>%count(Marital_Status_Value)

#EMPLOYMENT
file1%>%filter(Has_children == 1)%>%count(Employment)
file1%>%filter(Has_children == 0)%>%count(Employment)

#ACCOMMODATION
file1%>%filter(Has_children == 1)%>%count(Accommodation)
file1%>%filter(Has_children == 0)%>%count(Accommodation)

#DIAGNOSIS
file1%>%filter(Has_children == 1)%>%count(Diagnosis)
file1%>%filter(Has_children == 0)%>%count(Diagnosis)

#WARD STAY
file1%>%filter(Has_children == 1)%>%count(wardstay)
file1%>%filter(Has_children == 0)%>%count(wardstay)

#smoking
file1%>%filter(Has_children == 1)%>%count(Smoking)
file1%>%filter(Has_children == 0)%>%count(Smoking)


###Number of children#######
file1%>%filter(Has_children == 1)%>%count(No.ofchild)

file1%>%summarise(sd = sd(No.ofchild, na.rm = TRUE), mean = mean(No.ofchild, na.rm = TRUE), median = median(No.ofchild, na.rm = TRUE))
#sd = 1.1, mean = 1.87, median = 2

###Over/Under 18#######
y<-file1%>%filter(OU18_1 == "Under"|OU18_2 == "Under"|OU18_3 == "Under"|OU18_4 == "Under"|OU18_5 == "Under"|OU18_6 == "Under"|OU18_7 == "Under"|OU18_8 == "Under"|OU18_9 == "Under"|OU18_10 == "Under"|OU18_11 == "Under"|OU18_12 == "Under")
#740 people have any children under the age of 18
x<-file1%>%filter(OU18_1 == "unknown")
anti_join(x, y, by = "BRC_ID")
#148 have all children unknown

z<-file1%>%filter(OU18_1 == "Over")
anti_join(z, y, by = "BRC_ID")
#1118 have all children over 18

###############Difference in recording for dependants and non-dependants##########
y%>%count(Children_reported) #no- 497 (67.2%), yes - 243 (32.8%)
z%>%count(Children_reported) #no - 614 (54.9%), yes - 504 (45.1%)

########GATHERING DATASET SO IT'S BY CHILD#################
##OU18##
zz<-file1%>%rename(one = OU18_1, two = OU18_2, three = OU18_3, four = OU18_4, five = OU18_5, six = OU18_6, seven = OU18_7, eight = OU18_8, nine = OU18_9, ten = OU18_10, eleven = OU18_11, twelve = OU18_12)
zz<-zz%>%gather(c(one, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve), key = "Child", value = "OU18", na.rm = TRUE)
zz<-zz[order(zz$BRC_ID), ]
zz1<-zz[, c(1:14, 63:64)]

##CurrentAge##
zz<-file1%>%rename(one = CurrentAge1, two = CurrentAge2, three = CurrentAge3, four = CurrentAge4, five = CurrentAge5, six = CurrentAge6, seven = CurrentAge7, eight = CurrentAge8)
zz<-zz%>%gather(c(one, two, three, four, five, six, seven, eight), key = "Child", value = "CurrentAgeofchild", na.rm = TRUE)
zz<-zz[order(zz$BRC_ID), ]
zz2<-zz[, c(1, 67:68)]

##Gender##
zz<-file1%>%rename(one = Gender1, two = Gender2, three = Gender3, four = Gender4, five = Gender5, six = Gender6, seven = Gender7, eight = Gender8, nine = Gender9, ten = Gender10, eleven = Gender11, twelve = Gender12)
zz<-zz%>%gather(c(one, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve), key = "Child", value = "Gender", na.rm = TRUE)
zz<-zz[order(zz$BRC_ID), ]
zz3<-zz[, c(1, 63:64)]

##Age reported##
zz<-file1%>%rename(one = Age1, two = Age2, three = Age3, four = Age4, five = Age5, six = Age6, seven = Age7, eight = Age8)
zz<-zz%>%gather(c(one, two, three, four, five, six, seven, eight), key = "Child", value = "Age", na.rm = TRUE)
zz<-zz[order(zz$BRC_ID), ]
zz4<-zz[, c(1, 67:68)]

##Date age reported##
zz<-file1%>%rename(one = DateDoc1, two = DateDoc2, three = DateDoc3, four = DateDoc4, five = DateDoc5, six = DateDoc6, seven = DateDoc7, eight = DateDoc8)
zz<-zz%>%gather(c(one, two, three, four, five, six, seven, eight), key = "Child", value = "DateDoc", na.rm = TRUE)
zz<-zz[order(zz$BRC_ID), ]
zz5<-zz[, c(1, 67:68)]

##DOB##
zz<-file1%>%rename(one = DOB1, two = DOB2, three = DOB3, four = DOB4, five = DOB5, six = DOB6)
zz<-zz%>%gather(c(one, two, three, four, five, six), key = "Child", value = "DOB", na.rm = TRUE)
zz<-zz[order(zz$BRC_ID), ]
zz6<-zz[, c(1, 69:70)]

#JOIN ALL TOGETHER
xx<-left_join(zz1, zz2, by = c("BRC_ID", "Child"))
xx<-left_join(xx, zz3, by = c("BRC_ID", "Child"))
xx<-left_join(xx, zz4, by = c("BRC_ID", "Child"))
xx<-left_join(xx, zz5, by = c("BRC_ID", "Child"))
xx<-left_join(xx, zz6, by = c("BRC_ID", "Child"))
file_bychildren<-xx

write_csv(file_bychildren, "BYCHILDREN_participants_all_details_cleaned_OU18_unknowns_added")


###Gender of children###
file_bychildren%>%count(Gender)

###Over/Under 18###
file_bychildren%>%count(OU18)

###Ages of children###
agesofchildren<-file_bychildren%>%count(CurrentAgeofchild)
colSums(agesofchildren[c(1:3), 2]) #0-2 86
colSums(agesofchildren[c(4:7), 2]) #3-6 209
colSums(agesofchildren[c(8:12), 2]) #7-11 310
colSums(agesofchildren[c(13:16), 2]) #12-15 256
colSums(agesofchildren[c(17:18), 2]) #16-17 123
colSums(agesofchildren[c(19:55), 2]) #over 18 690
colSums(agesofchildren[c(56), 2]) #unknown 2071

#################COMPARING MEANS OF WITH AND WITHOUT CHILDREN#####################
#CurrentAge, Ethnicity_Value, Gender_Value, Marital_Status_Value, Diagnosis, Accommodation, Employment, Ward Stay, Smoking

###CurrentAge####
t.test(CurrentAge ~ Has_children, data = file1) 


###Gender####
#recode gender as 1 or 0
file2<-file1%>%mutate(Gender_Value_no = recode(file$Gender_Value, "Male" = 1, "Female" = 0))
t.test(Gender_Value_no ~ Has_children, data = file2) 

###Ward Stay####
t.test(wardstay ~ Has_children, data = file1) 

wardstaywithoutna<-file1%>%filter(wardstay != 0)
t.test(wardstay ~ Has_children, data = wardstaywithoutna)

file1$group<-as.numeric(file1$group)
t.test(group ~ Has_children, data = file1) 

#N.B. the t-test already excludes the NA values

#need to use chi-squared for the categorical ones

###Gender###
part1<-file1%>%filter(Has_children == 1)%>%count(Gender_Value)%>%rename(with_children = n)
part2<-file1%>%filter(Has_children == 0)%>%count(Gender_Value) %>%rename(without_children = n)
gendertest<-left_join(part1, part2)
gendertest<-as.data.frame(gendertest[, -1])
rownames(gendertest) <-c("Female", "Male", "missing")
chisq<-chisq.test(gendertest)

          #Pearson's Chi-squared test
          
          #data:  gendertest
          #X-squared = 394.05, df = 2, p-value < 2.2e-16

#withoutmissing
gendertest<-gendertest[1:2, ]
chisq<-chisq.test(gendertest)

          #Pearson's Chi-squared test
          
          #data:  gendertest
          #X-squared = 387.24, df = 1, p-value < 2.2e-16

###Ethnicity####
####making a new table with columns as has_children or not and rows as ethnicity
part1<-file1%>%filter(Has_children == 1)%>%count(Ethnicity_Value)%>%rename(with_children = n)
part2<-file1%>%filter(Has_children == 0)%>%count(Ethnicity_Value) %>%rename(without_children = n)
ethnicitytest<-left_join(part1, part2)
ethnicitytest<-as.data.frame(ethnicitytest[, -1])
rownames(ethnicitytest) <-c("Any Other Group", "Asian", "Black", "Mixed", "White - British", "White - Other", "missing")
chisq<-chisq.test(ethnicitytest)

          #Pearson's Chi-squared test
          
          #data:  ethnicitytest
          #X-squared = 17.764, df = 6, p-value = 0.006849

#withoutmissing
ethnicitytest<-ethnicitytest[1:6, ]
chisq<-chisq.test(ethnicitytest)

          #Pearson's Chi-squared test
          
          #data:  ethnicitytest
          #X-squared = 14.78, df = 5, p-value = 0.01135

#looking into Ethnicity_Value t.tests####
#White = 1, Everything else = 0
file2<-file1%>%mutate(Ethnicity_Value_no = recode(file$Ethnicity_Value, "White - British" = 1, "Any Other Group" = 0, "Asian" = 0, "Black" = 0, "Mixed" = 0, "White - Other" = 0))
file2<-replace.value(file2, "Ethnicity_Value_no", NA, 0)
t.test(Ethnicity_Value_no ~ Has_children, data = file2) #t = 2.2074, df = 4219.8, p-value = 0.02734

#Black = 1, Everything else = 0 
file2<-file1%>%mutate(Ethnicity_Value_no = recode(file$Ethnicity_Value, "Black" = 1, "Any Other Group" = 0, "Asian" = 0, "White - British" = 0, "Mixed" = 0, "White - Other" = 0))
t.test(Ethnicity_Value_no ~ Has_children, data = file2) #t = -0.44857, df = 3525.9, p-value = 0.6538

#Asian = 1, Everything else = 0 
file2<-file1%>%mutate(Ethnicity_Value_no = recode(file$Ethnicity_Value, "Asian" = 1, "Any Other Group" = 0, "Black" = 0, "White - British" = 0, "Mixed" = 0, "White - Other" = 0))
t.test(Ethnicity_Value_no ~ Has_children, data = file2) #t = -2.6858, df = 3279.4, p-value = 0.007271

#Mixed = 1, Everything else = 0 
file2<-file1%>%mutate(Ethnicity_Value_no = recode(file$Ethnicity_Value, "Mixed" = 1, "Any Other Group" = 0, "Black" = 0, "White - British" = 0, "Asian" = 0, "White - Other" = 0))
t.test(Ethnicity_Value_no ~ Has_children, data = file2) #t = 1.3413, df = 3890.7, p-value = 0.1799

#Any Other Group = 1, Everything else = 0 
file2<-file1%>%mutate(Ethnicity_Value_no = recode(file$Ethnicity_Value, "Any Other Group" = 1, "Mixed" = 0, "Black" = 0, "White - British" = 0, "Asian" = 0, "White - Other" = 0))
t.test(Ethnicity_Value_no ~ Has_children, data = file2) #t = 2.2113, df = 4222.8, p-value = 0.02706

#Any Other Group = 1, Everything else = 0 
file2<-file1%>%mutate(Ethnicity_Value_no = recode(file$Ethnicity_Value, "White - Other" = 1, "Mixed" = 0, "Black" = 0, "White - British" = 0, "Asian" = 0, "Any Other Group" = 0))
t.test(Ethnicity_Value_no ~ Has_children, data = file2) #t = -1.0713, df = 3461, p-value = 0.2841

###Marital_Status_Value####
part1<-file1%>%filter(Has_children == 1)%>%count(Marital_Status_Value)%>%rename(with_children = n)
part2<-file1%>%filter(Has_children == 0)%>%count(Marital_Status_Value) %>%rename(without_children = n)
maritaltest<-left_join(part1, part2)
maritaltest<-as.data.frame(maritaltest[, -1])
rownames(maritaltest) <-c("Divorced/Separated/Widowed", "Married/Civil Partner", "Single", "missing")
chisq<-chisq.test(maritaltest)

          #Pearson's Chi-squared test
          
          #data:  maritaltest
          #X-squared = 1162.9, df = 3, p-value < 2.2e-16

#withoutmissing
maritaltest<-maritaltest[1:3,]
chisq<-chisq.test(maritaltest)

          #Pearson's Chi-squared test
          
          #data:  maritaltest
          #X-squared = 1163.1, df = 2, p-value < 2.2e-16

###Accommodation####
part1<-file1%>%filter(Has_children == 1)%>%count(Accommodation)%>%rename(with_children = n)
part2<-file1%>%filter(Has_children == 0)%>%count(Accommodation) %>%rename(without_children = n)
accommodationtest<-left_join(part1, part2)
accommodationtest<-as.data.frame(accommodationtest[, -1])
rownames(accommodationtest) <-c("Other", "Owning", "Renting", "Supporting Living","missing")
chisq<-chisq.test(accommodationtest)

          #Pearson's Chi-squared test
          
          #data:  accommodationtest
          #X-squared = 188.41, df = 4, p-value < 2.2e-16
Í
#withoutmissing
accommodationtest<-accommodationtest[1:4, ]
chisq<-chisq.test(accommodationtest)

          #Pearson's Chi-squared test
          
          #data:  accommodationtest
          #X-squared = 170.16, df = 3, p-value < 2.2e-16

###Employment####
part1<-file1%>%filter(Has_children == 1)%>%count(Employment)%>%rename(with_children = n)
part2<-file1%>%filter(Has_children == 0)%>%count(Employment) %>%rename(without_children = n)
employmenttest<-left_join(part1, part2)
employmenttest<-as.data.frame(employmenttest[, -1])
rownames(employmenttest) <-c("Benefits", "Employed", "Retired", "Student","Unemployed","missing")
chisq<-chisq.test(employmenttest)

          #Pearson's Chi-squared test
          
          #data:  employmenttest
          #X-squared = 261.06, df = 5, p-value < 2.2e-16

#withoutmissing
employmenttest<-employmenttest[1:5, ]
chisq<-chisq.test(employmenttest)

          #Pearson's Chi-squared test
          
          #data:  employmenttest
          #X-squared = 194.4, df = 4, p-value < 2.2e-16

###Smoking####
part1<-file1%>%filter(Has_children == 1)%>%count(Smoking)%>%rename(with_children = n)
part2<-file1%>%filter(Has_children == 0)%>%count(Smoking) %>%rename(without_children = n)
smokingtest<-left_join(part1, part2)
smokingtest<-as.data.frame(smokingtest[, -1])
rownames(smokingtest) <-c("Current Smoker", "Ex-smoker", "Non-smoker", "missing")
chisq<-chisq.test(smokingtest)

#Pearson's Chi-squared test

#data:  smokingtest
#X-squared = 10.733, df = 3, p-value = 0.01326

#withoutmissing
smokingtest<-smokingtest[1:3, ]
chisq<-chisq.test(smokingtest)

#Pearson's Chi-squared test

#data:  employmenttest
#X-squared = 3.8798, df = 2, p-value = 0.1437


###Diagnosis####

part1<-file1%>%filter(Has_children == 1)%>%count(Diagnosis)%>%rename(with_children = n)
part2<-file1%>%filter(Has_children == 0)%>%count(Diagnosis) %>%rename(without_children = n)
diagnosistest<-left_join(part1, part2)
diagnosistest<-as.data.frame(diagnosistest[, -1])
rownames(diagnosistest) <-c("F20", "F21", "F22", "F23", "F25","F28","F29", "F31.2&F31.5", "unknown")
chisq<-chisq.test(diagnosistest)

            #Pearson's Chi-squared test
            
            #data:  diagnosistest
            #X-squared = 102.87, df = 8, p-value < 2.2e-16


###COMPARING WARD STAY MEANS IN DIFFERENT GROUPS#######################
#Gender
file1%>%filter(Has_children == 1)%>%group_by(Gender_Value)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE), median = median(wardstay, na.rm = TRUE))
file1%>%filter(Has_children == 0)%>%group_by(Gender_Value)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE), median = median(wardstay, na.rm = TRUE))

#Age
file2<-replace.value(file1, "CurrentAge", c("13", "14", "15", "16", "17", "18", "19"), "1")
file2<-replace.value(file2, "CurrentAge", c("20", "21", "22", "23", "24", "25", "26", "27", "28", "29"), "2")
file2<-replace.value(file2, "CurrentAge", c("30", "31", "32", "33", "34", "35", "36", "37", "38", "39"), "3")
file2<-replace.value(file2, "CurrentAge", c("30", "31", "32", "33", "34", "35", "36", "37", "38", "39"), "3")
file2<-replace.value(file2, "CurrentAge", c("40", "41", "42", "43", "44", "45", "46", "47", "48", "49"), "4")
file2<-replace.value(file2, "CurrentAge", c("50", "51", "52", "53", "54", "55", "56", "57", "58", "59"), "5")
file2<-replace.value(file2, "CurrentAge", c("60", "61", "62", "63", "64", "65", "66", "67", "68", "69"), "6")
file2<-replace.value(file2, "CurrentAge", c("70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "99", "100"), "7")

file2%>%filter(Has_children == 1)%>%group_by(CurrentAge)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE), median = median(wardstay, na.rm = TRUE))
file2%>%filter(Has_children == 0)%>%group_by(CurrentAge)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE))

#Ethnicity
file1%>%filter(Has_children == 1)%>%group_by(Ethnicity_Value)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE), median = median(wardstay, na.rm = TRUE))
file1%>%filter(Has_children == 0)%>%group_by(Ethnicity_Value)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE), median = median(wardstay, na.rm = TRUE))

#Marital Status
file1%>%filter(Has_children == 1)%>%group_by(Marital_Status_Value)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE))
file1%>%filter(Has_children == 0)%>%group_by(Marital_Status_Value)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE))

#Employment
file1%>%filter(Has_children == 1)%>%group_by(Employment)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE))
file1%>%filter(Has_children == 0)%>%group_by(Employment)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE))

#Accommodation
file1%>%filter(Has_children == 1)%>%group_by(Accommodation)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE))
file1%>%filter(Has_children == 0)%>%group_by(Accommodation)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE))

#Smoking
file1%>%filter(Has_children == 1)%>%group_by(Smoking)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE))
file1%>%filter(Has_children == 0)%>%group_by(Smoking)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE))

#IMD
file1%>%filter(Has_children == 1)%>%group_by(group)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE))
file1%>%filter(Has_children == 0)%>%group_by(group)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE))

#Diagnosis
file1%>%filter(Has_children == 1)%>%group_by(Diagnosis)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE))
file1%>%filter(Has_children == 0)%>%group_by(Diagnosis)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE))

#Number of children
file1%>%filter(Has_children == 1)%>%group_by(No.ofchild)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE))

#Any child under 18
y<-file1%>%filter(OU18_1 == "Under"|OU18_2 == "Under"|OU18_3 == "Under"|OU18_4 == "Under"|OU18_5 == "Under"|OU18_6 == "Under"|OU18_7 == "Under"|OU18_8 == "Under"|OU18_9 == "Under"|OU18_10 == "Under"|OU18_11 == "Under"|OU18_12 == "Under")
AnyUnder18<-rep(1, 740)
y<-cbind(y, AnyUnder18)

x<-file1%>%filter(OU18_1 == "unknown")
x<-anti_join(x, y, by = "BRC_ID")
AnyUnder18<-rep(NA, 148)
x<-cbind(x, AnyUnder18)

z<-file1%>%filter(OU18_1 == "Over")
z<-anti_join(z, y, by = "BRC_ID")

AnyUnder18<-rep(0, 1118)
z<-cbind(z, AnyUnder18)

file2<-rbind(y, x)
file2<-rbind(file2, z)

file2<-file2%>%arrange(BRC_ID)

file2%>%filter(Has_children == 1)%>%group_by(AnyUnder18)%>%summarise(mean = mean(wardstay, na.rm = TRUE), sd = sd(wardstay, na.rm = TRUE))


###LINEAR MODELLING - Has_children###################
#realised that in backwards stepwise i might need to remove one by one
#Change NAs to character 'unknown' so doesn't get participants with NAs don't get removed from model. Then change to factor again and relevel
file1<-file1[, c(1, 3:10, 14, 15:26, 69, 70, 73, 74)]
file1noCA<-file1[, c(-2, -26)]
file1CA<-file1[, c(2, 26)]
file1noCA<-file1noCA%>%replace(is.na(.), "unknown")
file1<-cbind(file1CA, file1noCA)%>%arrange(BRC_ID)

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

lm1<-glm(Has_children ~ 1 + Marital_Status_Value + CurrentAge + Gender_Value + Ethnicity_Value + Accommodation + Employment + Diagnosis + wardstay + Smoking + group, data = file1, family = binomial)
summary(lm1)
AIC(lm1) #5012.69

step(lm1, direction = "backward", k = 2)
#this suggests no variables should be removed

finallm1<-glm(Has_children ~ 1 + Marital_Status_Value + CurrentAge + Gender_Value + Ethnicity_Value + Accommodation + Employment + Diagnosis + wardstay + Smoking + group, data = file1, family = binomial)
summary(finallm1)

###odds ratios#########
r<-round(summary(finallm1)[[13]], digits = 4)
r<-as.data.frame(r, row.names = c("Intercept", "Divorced", "Married", "M-unknown", "Age", "Female", "Any Other Group", "Asian", "Black", "Mixed", "E-unknown", "White-Other", "Other", "Owning", 
                                  "Renting", "A-unknown", "Benefits", "Employed", "Retired", "Student", "Ed-unknown", "F21", "F22&24", "F23", "F25", "F28", "F29", "F31.2&31.5", "Cluster", "wardstay", "Current Smoker", "Ex-smoker", "S-unknown", "group"))
r<-r%>%dplyr::mutate(or = exp(Estimate))
ci<-as.data.frame(exp(cbind("Odds ratio" = coef(r), confint.default(finallm1, level = 0.95))))
or<-data.frame(odds_ratio = r$or, ci_l=ci$`2.5 %`, ci_u=ci$`97.5 %`, row.names = c("Intercept", "Divorced", "Married", "M-unknown", "Age", "Female", "Any Other Group", "Asian", "Black", "Mixed", "E-unknown", "White-Other", "Other", "Owning", 
                                                                                   "Renting", "A-unknown", "Benefits", "Employed", "Retired", "Student", "Ed-unknown", "F21", "F22&24", "F23", "F25", "F28", "F29", "F31.2&31.5", "Cluster", "wardstay", "Current Smoker", "Ex-smoker", "S-unknown", "group"))

###check assumptions########

vif(lm5) #Ethnicity Values have high VIFs

###exploratory analysis - diagnosis*ethnicity########
lm4a<-glm(Has_children ~ 1 + Marital_Status_Value + Gender_Value + Diagnosis*Ethnicity_Value + Accommodation + Employment + Smoking, data = file1, family = binomial)
summary(lm4a)
AIC(lm4a) #573
rsq(lm4a) #0.9372

anova(lm4, lm4a, test = "Chisq") #p = 0.7478 so it looks like the less complicated model (without the interaction) is preferred.

###LINEAR MODELLING - Has_children under 18###################
y<-file1%>%filter(OU18_1 == "Under"|OU18_2 == "Under"|OU18_3 == "Under"|OU18_4 == "Under"|OU18_5 == "Under"|OU18_6 == "Under"|OU18_7 == "Under"|OU18_8 == "Under"|OU18_9 == "Under"|OU18_10 == "Under"|OU18_11 == "Under"|OU18_12 == "Under")
#nochildren
n<-file1%>%filter(Has_children == 0)
file3<-rbind(y, n)
file3%>%count(Has_children)

#Empty model with age in it as a control
lm0<-glm(Has_children ~ 1 + CurrentAge, data = file3, family = binomial)
AIC(lm0) #3753.06

lm1<-glm(Has_children ~ 1 + Marital_Status_Value + CurrentAge + Gender_Value + Ethnicity_Value + Accommodation + Employment + Diagnosis + wardstay + Smoking + group, data = file3, family = binomial)
summary(lm1)
AIC(lm1) #3029.12

step(lm1, direction = "backward", k = 2) #removes only Ethnicity 

finallm1<-glm(formula = Has_children ~ Marital_Status_Value + CurrentAge + Gender_Value + Accommodation + Employment + Diagnosis + wardstay + Smoking + group, family = binomial, data = file3)
summary(finallm1)
AIC(finallm1) #3027.66

###odds ratios#########
r<-round(summary(finallm1)[[13]], digits = 4)
r<-as.data.frame(r, row.names = c("Intercept", "Divorced", "Married", "M-unknown", "Age", "Female", "Other", "Owning", 
                                  "Renting", "A-unknown", "Benefits", "Employed", "Retired", "Student", "Ed-unknown", "F21", "F22&24", "F23", "F25", "F28", "F29", "F31.2&31.5", "Cluster", "wardstay", "Current Smoker", "Ex-smoker", "S-unknown", "group"))
r<-r%>%dplyr::mutate(or = exp(Estimate))
ci<-as.data.frame(exp(cbind("Odds ratio" = coef(r), confint.default(finallm1, level = 0.95))))
or<-data.frame(odds_ratio = r$or, ci_l=ci$`2.5 %`, ci_u=ci$`97.5 %`, row.names = c("Intercept", "Divorced", "Married", "M-unknown", "Age", "Female", "Other", "Owning", 
                                                                                   "Renting", "A-unknown", "Benefits", "Employed", "Retired", "Student", "Ed-unknown", "F21", "F22&24", "F23", "F25", "F28", "F29", "F31.2&31.5", "Cluster", "wardstay", "Current Smoker", "Ex-smoker", "S-unknown", "group"))


###exploratory - add diagnosis back in and see if there is interaction with age##############
lm5a<-glm(Has_children ~ 1 + Marital_Status_Value + CurrentAge*Diagnosis + Gender_Value + Accommodation + Employment, data = file3, family = binomial)
summary(lm6a)
AIC(lm5a) #334
rsq(lm5a) #0.9270
rsq(lm5a, adj = T) #0.267

anova(lm5, lm5a, test = "Chisq") #p = 0.118 - favours simpler model (without interaction)

###compare proportions of those with each diagnosis - any child under 18 vs. not
y%>%count(Diagnosis)
p<-anti_join(file1, y)
p%>%filter(Has_children == 1)%>%count(Diagnosis)

#plot age against diagnosis
ggplot(file1, aes(Diagnosis, CurrentAge)) + geom_boxplot() #whole sample diagnosis vs age

###LINEAR MODELLING - Ward stays predicted by child factors within those with children (n= 2006)######################################
file2<-file1%>%filter(Has_children == 1)

###MAKENEWVARIABLE ANYUNDER18###################
y<-file2%>%filter(OU18_1 == "Under"|OU18_2 == "Under"|OU18_3 == "Under"|OU18_4 == "Under"|OU18_5 == "Under"|OU18_6 == "Under"|OU18_7 == "Under"|OU18_8 == "Under"|OU18_9 == "Under"|OU18_10 == "Under"|OU18_11 == "Under"|OU18_12 == "Under")
AnyUnder18<-rep(1, 740)
y<-cbind(y, AnyUnder18)

x<-file2%>%filter(OU18_1 == "unknown")
x<-anti_join(x, y, by = "BRC_ID")
AnyUnder18<-rep(NA, 148)
x<-cbind(x, AnyUnder18)

z<-file2%>%filter(OU18_1 == "Over")
z<-anti_join(z, y, by = "BRC_ID")

AnyUnder18<-rep(0, 1118)
z<-cbind(z, AnyUnder18)

file2<-rbind(y, x)
file2<-rbind(file2, z)

file2<-file2%>%arrange(BRC_ID)

####Now modelling

lm1<-glm(wardstay ~ 1 + Marital_Status_Value + CurrentAge + Gender_Value + Ethnicity_Value + Accommodation + Employment + Diagnosis + Smoking + group + No.ofchild + AnyUnder18, data = file2, family = poisson)
summary(lm1)
AIC(lm1) #1295
rsq(lm1) #0.8352
rsq(lm1, adj = T) #-0.653

#remove currentage

lm2<-glm(wardstay ~ 1 + Marital_Status_Value + Gender_Value + Ethnicity_Value + Accommodation + Employment + Diagnosis + Smoking + group + No.ofchild + AnyUnder18, data = file2, family = poisson)
summary(lm2)
AIC(lm2) #1294
rsq(lm2) #0.8350
rsq(lm2, adj = T) #-0.646

#remove ethnicity
lm3<-glm(wardstay ~ 1 + Marital_Status_Value + Gender_Value + Accommodation + Employment + Diagnosis + Smoking + group + No.ofchild + AnyUnder18, data = file2, family = poisson)
summary(lm3)
AIC(lm3) #1321
rsq(lm3) #0.8230
rsq(lm3, adj = T) #-0.631

#remove no.of child
lm4<-glm(wardstay ~ 1 + Marital_Status_Value + Gender_Value + Accommodation + Employment + Diagnosis + Smoking + group + AnyUnder18, data = file2, family = poisson)
summary(lm4)
AIC(lm4) #1320
rsq(lm4) #0.8301
rsq(lm4, adj = T) #-0.622

#remove IMD
lm5<-glm(wardstay ~ 1 + Marital_Status_Value + AnyUnder18 + Gender_Value + Accommodation + Employment + Diagnosis + Smoking, data = file2, family = poisson)
summary(lm5)
AIC(lm5) #1318
rsq(lm5) #0.8301
rsq(lm5, adj = T) #-0.614

###odds ratios#########
r<-round(summary(lm5)[[13]], digits = 4)
r<-as.data.frame(r, row.names = c("Intercept", "Divorced", "Married", "AnyUnder18", "Female", "Other", "Owning", 
                                  "Renting","Benefits", "Employed", "Retired", "Student", "F21", "F22&24", "F23", "F25", "F28", "F29", "F31.2&31.5", "Current Smoker", "Ex-smoker"))
r<-r%>%dplyr::mutate(or = exp(Estimate))
ci<-as.data.frame(exp(cbind("Odds ratio" = coef(r), confint.default(lm5, level = 0.95))))
or<-data.frame(odds_ratio = r$or, ci_l=ci$`2.5 %`, ci_u=ci$`97.5 %`, row.names = c("Intercept", "Divorced", "Married", "AnyUnder18", "Female", "Other", "Owning", 
                                                                                   "Renting","Benefits", "Employed", "Retired", "Student", "F21", "F22&24", "F23", "F25", "F28", "F29", "F31.2&31.5", "Current Smoker", "Ex-smoker"))


#remove smoking
#lm6<-glm(wardstay ~ 1 + Marital_Status_Value + Gender_Value + Accommodation + Employment + Diagnosis + AnyUnder18, data = file2, family = poisson)
#summary(lm6)
#AIC(lm6) #1432
#rsq(lm6) #0.8255
#rsq(lm6, adj = T) #-0.446

###exploratory analysis - marital status*anyunder18

lm5a<-glm(wardstay ~ 1 + Marital_Status_Value*AnyUnder18 + Gender_Value + Accommodation + Employment + Diagnosis + Smoking, data = file2, family = poisson)
summary(lm5a)
AIC(lm5a) #1303
rsq(lm5a) #0.8343

file2$AnyUnder18<-as.character(file2$AnyUnder18)
ggplot(file2, aes(Marital_Status_Value, wardstay, fill = AnyUnder18)) + geom_bar(stat = "identity", position = "fill")
ggplot(file2, aes(Marital_Status_Value, wardstay, fill = AnyUnder18)) + geom_bar(stat = "identity", position = "dodge")

###exploratory analysis - gender*accommodation#######
lm5b<-glm(wardstay ~ 1 + Marital_Status_Value*AnyUnder18 + Gender_Value*Accommodation + Employment + Diagnosis + Smoking, data = file2, family = poisson)
summary(lm5b)
AIC(lm5b) #1278.87
rsq(lm5b) #0.8395

ggplot(file2, aes(Gender_Value, wardstay)) + geom_jitter() + coord_cartesian(ylim = c(0, 10))
ggplot(file2, aes(Accommodation, wardstay)) + geom_jitter() + coord_cartesiane(ylim = c(0, 10))

ggplot(file2, aes(Accommodation, wardstay, fill = Gender_Value)) + geom_bar(stat = "identity", position = "fill")
ggplot(file2, aes(Accommodation, wardstay, fill = Gender_Value)) + geom_bar(stat = "identity", position = "dodge")

anova(lm5, lm5a, test = "Chisq")
anova(lm5, lm5b, test = "Chisq")
anova(lm5a, lm5b, test = "Chisq")


