#install.packages('flextable', repos='http://r.akriviahealth.nhs.uk')
library(tidyverse)
library(psych)
library(readxl)
library(sjmisc)
library(lubridate)
library(anchors)
library(rsq)
library(stats)
library(rms)
library(survival)
library(survminer)
library(ggforce)
library(gtsummary)

main <- read_excel("F:/68-Parentswithpsychosis/Parentswithpsychosis/Parentswithpsychosis/Data for Migration to AWS/UKCRIS Docs/participants_all_details_cleaned_OU18unknownsadded.xlsx", 
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

#######Survival analysis - cox proportional hazards model
ward_stay_wide<- read_excel("F:/68-Parentswithpsychosis/Parentswithpsychosis/Parentswithpsychosis/Data for Migration to AWS/UKCRIS Docs/patient characteristics/ward_stay_wide_upto20190214.xlsx", 
                            col_types = c("text", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date", "date"))

#calculate time(in days) between end of first ward stay and beginning of second ward stay                   
ws<-ward_stay_wide%>%mutate(WS_days = (ASD_2 - AED_1)/86400) #need to divide seconds by 86400 to get days
#2651 people have at least one ward stay recorded

ws_error<-ws%>%filter(WS_days == 0) #848 - time between end of first WS and beginning of second is 0 days
A<-ws%>%filter(WS_days>0) #849 - have at least 1 day - this is fine
B<-ws%>%filter(is.na(WS_days)) #924 only have one WD - this is fine
D<-ws%>%filter(WS_days<0) #30 - have a negative number - this must be incorrect - can probably just times by -1
D<-D%>%mutate(WS_days= WS_days*-1)

#for the 848 that had no time between end of first WS and beginning of second - using third instead
ws_error_2<-ws_error%>%mutate(WS_days = (ASD_3 - AED_1)/86400) #need to divide seconds by 86400 to get days

G<-ws_error_2%>%filter(WS_days == 0) #15 time between end of first WS and beginning of third is 0 days. Just going to assume only one ward stay so recode as 0
G[G == 0]<-NA
E<-ws_error_2%>%filter(WS_days > 0) #579 - have a least 1 day - can assume third ward stay is effectively their second
ws_error_2%>%filter(WS_days < 0) #0 have negative numbers - this is good
C<-ws_error_2%>%filter(is.na(WS_days)) #254 only have 2 ward stays and no third so include these with the 924 that only have one ward stay

ws_clean<-rbind(A, B, C, D, E, G)

###do same checks again to check if really is clean
ws_clean%>%filter(WS_days == 0) #0
ws_clean%>%filter(WS_days<0) #0
ws_clean_days<-ws_clean%>%filter(WS_days>0) #1458
ws_clean_na<-ws_clean%>%filter(is.na(WS_days)) #1193

#Need to right-censor those that DON'T have a ward stay. I think that means I need to work out the 
#days between the end of their first ward stay and the date the data was extracted and input that into their 'days' column
#then put a '0' to indicate that there was no ward stay and that they have been censored

ws_clean_na<-ws_clean_na%>%mutate(WS_days = ((as.POSIXct("2019-02-14", format = "%Y-%m-%d"))- ASD_1)/86400)
presence_of_WS<-rep(0, 1193)
ws_clean_na<-cbind(ws_clean_na, presence_of_WS)                  

presence_of_WS<-rep(1, 1458)
ws_clean_days<-cbind(ws_clean_days, presence_of_WS) 

ws_clean<-rbind(ws_clean_na, ws_clean_days)
ws_clean<-ws_clean%>%arrange(BRC_ID)

ws_clean$WS_days<-as.numeric(ws_clean$WS_days)

#Join with main dataset - only need BRC_ID, dates of ward stays used in analysis, WS_days (days between ward stays) and presence_of_WS (presence of second ward stay)
ws_clean<-ws_clean[, c(1, 3, 4, 6, 32, 33)]

file1_WS<-full_join(file1, ws_clean, by = "BRC_ID")

#select only to be used in survival analysis (those with at least one ward stay, n = 2651)
file1_WS<-file1_WS%>%filter(!is.na(WS_days))

#Developing a formula to see who had a child inbetween their ward stays
file1_WS%>%filter(DOB1<"2019-04-02"&DOB1>"2014-02-01")%>%dplyr::select(BRC_ID, DOB1, WS_days)
file1_WS%>%filter(DOB1>AED_1&DOB1<ASD_2)%>%dplyr::select(BRC_ID, DOB1, AED_1, ASD_2)

child_born <- function(x, y, z) {
  if(is.na(x)){
    NA
  } else if (is.na(y)) {
    NA
  } else if (is.na(z)) {
    NA
  } else if (x > y & x < z) {
    "Yes"
  } else {
    "No"
  }
}

#On children with DOBs
#Take out those where i used ASD_3 rather than ASD_2 and compute two 'child_born_inb' for them separately then combine
#Look at all DOBs - DOB1, DOB2, DOB3, DOB4, DOB5, DOB6
#Minus Age1 from DateDoc1 etc... to get new estimated DOBs and run the process again. Look at R file where CurrentAge is computed
#to see if I can use any of that code

#Take out those where ASD_3 were used rather than ASD_2
E<-E[, c(1)]
file1_WS_ASD2<-anti_join(file1_WS, E, by = "BRC_ID")
file1_WS_ASD3<-inner_join(file1_WS, E, by = "BRC_ID")

######DOBs ASD2##########
a<-mapply(child_born, file1_WS_ASD2$DOB1, file1_WS_ASD2$AED_1, file1_WS_ASD2$ASD_2) #6 Yes
a<-as_tibble(a)
colnames(a)[1]<-"child_born_inba"

b<-mapply(child_born, file1_WS_ASD2$DOB2, file1_WS_ASD2$AED_1, file1_WS_ASD2$ASD_2) #4 Yes
b<-as_tibble(b)
colnames(b)[1]<-"child_born_inbb"

c<-mapply(child_born, file1_WS_ASD2$DOB3, file1_WS_ASD2$AED_1, file1_WS_ASD2$ASD_2)
c<-as_tibble(c)
colnames(c)[1]<-"child_born_inbc"

d<-mapply(child_born, file1_WS_ASD2$DOB4, file1_WS_ASD2$AED_1, file1_WS_ASD2$ASD_2)
d<-as_tibble(d)
colnames(d)[1]<-"child_born_inbd"

e<-mapply(child_born, file1_WS_ASD2$DOB5, file1_WS_ASD2$AED_1, file1_WS_ASD2$ASD_2)
e<-as_tibble(e)
colnames(e)[1]<-"child_born_inbe"

f<-mapply(child_born, file1_WS_ASD2$DOB6, file1_WS_ASD2$AED_1, file1_WS_ASD2$ASD_2)
f<-as_tibble(f)
colnames(f)[1]<-"child_born_inbf"

######DOBs ASD3##########################################

a2<-mapply(child_born, file1_WS_ASD3$DOB1, file1_WS_ASD3$AED_1, file1_WS_ASD3$ASD_3)
a2<-as_tibble(a2)
colnames(a2)[1]<-"child_born_inba2"

b2<-mapply(child_born, file1_WS_ASD3$DOB2, file1_WS_ASD3$AED_1, file1_WS_ASD3$ASD_3) #1 Yes
b2<-as_tibble(b2)
colnames(b2)[1]<-"child_born_inbb2"

c2<-mapply(child_born, file1_WS_ASD3$DOB3, file1_WS_ASD3$AED_1, file1_WS_ASD3$ASD_3)
c2<-as_tibble(c2)
colnames(c2)[1]<-"child_born_inbc2"

d2<-mapply(child_born, file1_WS_ASD3$DOB4, file1_WS_ASD3$AED_1, file1_WS_ASD3$ASD_3)
d2<-as_tibble(d2)
colnames(d2)[1]<-"child_born_inbd2"

e2<-mapply(child_born, file1_WS_ASD3$DOB5, file1_WS_ASD3$AED_1, file1_WS_ASD3$ASD_3)
e2<-as_tibble(e2)
colnames(e2)[1]<-"child_born_inbe2"

f2<-mapply(child_born, file1_WS_ASD3$DOB6, file1_WS_ASD3$AED_1, file1_WS_ASD3$ASD_3)
f2<-as_tibble(f2)
colnames(f2)[1]<-"child_born_inbf2"

####DateDoc/Age ASD2################################
file1_WS_ASD2<-mutate(file1_WS_ASD2, 
       Est_DOB1 = (ymd(`DateDoc1`) - (years(`Age1`))), 
       Est_DOB2 = (ymd(`DateDoc2`) - (years(`Age2`))),
       Est_DOB3 = (ymd(`DateDoc3`) - (years(`Age3`))),
       Est_DOB4 = (ymd(`DateDoc4`) - (years(`Age4`))),
       Est_DOB5 = (ymd(`DateDoc5`) - (years(`Age5`))),
       Est_DOB6 = (ymd(`DateDoc6`) - (years(`Age6`))),
       Est_DOB7 = (ymd(`DateDoc7`) - (years(`Age7`))),
       Est_DOB8 = (ymd(`DateDoc8`) - (years(`Age8`))))

g<-mapply(child_born, file1_WS_ASD2$Est_DOB1, file1_WS_ASD2$AED_1, file1_WS_ASD2$ASD_2) #2 Yes
g<-as_tibble(g)
colnames(g)[1]<-"child_born_inbg"

h<-mapply(child_born, file1_WS_ASD2$Est_DOB2, file1_WS_ASD2$AED_1, file1_WS_ASD2$ASD_2) #1 Yes
h<-as_tibble(h)
colnames(h)[1]<-"child_born_inbh"

i<-mapply(child_born, file1_WS_ASD2$Est_DOB3, file1_WS_ASD2$AED_1, file1_WS_ASD2$ASD_2) #1 Yes
i<-as_tibble(i)
colnames(i)[1]<-"child_born_inbi"

j<-mapply(child_born, file1_WS_ASD2$Est_DOB4, file1_WS_ASD2$AED_1, file1_WS_ASD2$ASD_2)
j<-as_tibble(j)
colnames(j)[1]<-"child_born_inbj"

k<-mapply(child_born, file1_WS_ASD2$Est_DOB5, file1_WS_ASD2$AED_1, file1_WS_ASD2$ASD_2)
k<-as_tibble(k)
colnames(k)[1]<-"child_born_inbk"

l<-mapply(child_born, file1_WS_ASD2$Est_DOB6, file1_WS_ASD2$AED_1, file1_WS_ASD2$ASD_2)
l<-as_tibble(l)
colnames(l)[1]<-"child_born_inbl"

m<-mapply(child_born, file1_WS_ASD2$Est_DOB7, file1_WS_ASD2$AED_1, file1_WS_ASD2$ASD_2)
m<-as_tibble(m)
colnames(m)[1]<-"child_born_inbm"

n<-mapply(child_born, file1_WS_ASD2$Est_DOB8, file1_WS_ASD2$AED_1, file1_WS_ASD2$ASD_2)
n<-as_tibble(n)
colnames(n)[1]<-"child_born_inbn"

####DateDoc/Age ASD3################################
file1_WS_ASD3<-mutate(file1_WS_ASD3, 
                      Est_DOB1 = (ymd(`DateDoc1`) - (years(`Age1`))), 
                      Est_DOB2 = (ymd(`DateDoc2`) - (years(`Age2`))),
                      Est_DOB3 = (ymd(`DateDoc3`) - (years(`Age3`))),
                      Est_DOB4 = (ymd(`DateDoc4`) - (years(`Age4`))),
                      Est_DOB5 = (ymd(`DateDoc5`) - (years(`Age5`))),
                      Est_DOB6 = (ymd(`DateDoc6`) - (years(`Age6`))),
                      Est_DOB7 = (ymd(`DateDoc7`) - (years(`Age7`))),
                      Est_DOB8 = (ymd(`DateDoc8`) - (years(`Age8`))))

g2<-mapply(child_born, file1_WS_ASD3$Est_DOB1, file1_WS_ASD3$AED_1, file1_WS_ASD3$ASD_3) #1 Yes
g2<-as_tibble(g2)
colnames(g2)[1]<-"child_born_inbg2"

h2<-mapply(child_born, file1_WS_ASD3$Est_DOB2, file1_WS_ASD3$AED_1, file1_WS_ASD3$ASD_3) #1 Yes
h2<-as_tibble(h2)
colnames(h2)[1]<-"child_born_inbh2"

i2<-mapply(child_born, file1_WS_ASD3$Est_DOB3, file1_WS_ASD3$AED_1, file1_WS_ASD3$ASD_3)
i2<-as_tibble(i2)
colnames(i2)[1]<-"child_born_inbi2"

j2<-mapply(child_born, file1_WS_ASD3$Est_DOB4, file1_WS_ASD3$AED_1, file1_WS_ASD3$ASD_3)
j2<-as_tibble(j2)
colnames(j2)[1]<-"child_born_inbj2"

k2<-mapply(child_born, file1_WS_ASD3$Est_DOB5, file1_WS_ASD3$AED_1, file1_WS_ASD3$ASD_3)
k2<-as_tibble(k2)
colnames(k2)[1]<-"child_born_inbk2"

l2<-mapply(child_born, file1_WS_ASD3$Est_DOB6, file1_WS_ASD3$AED_1, file1_WS_ASD3$ASD_3)
l2<-as_tibble(l2)
colnames(l2)[1]<-"child_born_inbl2"

m2<-mapply(child_born, file1_WS_ASD3$Est_DOB7, file1_WS_ASD3$AED_1, file1_WS_ASD3$ASD_3)
m2<-as_tibble(m2)
colnames(m2)[1]<-"child_born_inbm2"

n2<-mapply(child_born, file1_WS_ASD3$Est_DOB8, file1_WS_ASD3$AED_1, file1_WS_ASD3$ASD_3)
n2<-as_tibble(n2)
colnames(n2)[1]<-"child_born_inbn2"

###Now need to combine into one column
temp<-cbind(file1_WS_ASD2, a, b, c, d, e, f, g, h, i, j, k, l, m, n)

#Found all the BRC_IDs who did have a child born in between ward stays, separated out and marked with 'Yes' column
temp2<-temp%>%filter(child_born_inba == "Yes" | child_born_inbb == "Yes" | child_born_inbg == "Yes" | child_born_inbh == "Yes"| child_born_inbi == "Yes")
child_born_inb<-rep("Yes", 14)   
temp2<-temp2[, c(1:87)]
temp2<-cbind(temp2, child_born_inb)

#Took the rest and coalesced those children with DOB1 and Age1 to input as many 'No's as existed
temp3<-anti_join(temp, temp2, by = "BRC_ID")
temp3<-temp3%>%mutate(child_born_inb = coalesce(child_born_inba, child_born_inbg))
temp3<-temp3[, c(1:87, 102)]

#Put back into original file - have managed to create new column saying whether a child was born in between called 'child_born_inb'

file1_WS_ASD2<-rbind(temp2, temp3)%>%arrange(BRC_ID)

#Do the same for file1_WS_ASD3

###Now need to combine into one column
temp<-cbind(file1_WS_ASD3, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)

#Found all the BRC_IDs who did have a child born in between ward stays, separated out and marked with 'Yes' column
temp2<-temp%>%filter(child_born_inbb2 == "Yes" | child_born_inbg2 == "Yes" | child_born_inbh2 == "Yes")
child_born_inb<-rep("Yes", 3)   
temp2<-temp2[, c(1:87)]
temp2<-cbind(temp2, child_born_inb)

#Took the rest and coalesced those children with DOB1 and Age1 to input as many 'No's as existed
temp3<-anti_join(temp, temp2, by = "BRC_ID")
temp3<-temp3%>%mutate(child_born_inb = coalesce(child_born_inba2, child_born_inbg2))
temp3<-temp3[, c(1:87, 102)]

#Put back into original file

file1_WS_ASD3<-rbind(temp2, temp3)%>%arrange(BRC_ID)

file1_WS_child_inb<-rbind(file1_WS_ASD2, file1_WS_ASD3)%>%arrange(BRC_ID)
#Total = 17 Yes, 365 No, 2269 NA

file1_WS_child_inb<-file1_WS_child_inb%>%mutate(WS_year = WS_days/365.25)

###MAKENEWVARIABLE ANYUNDER18###################
file2<-file1_WS_child_inb%>%filter(Has_children == 1)
y<-file2%>%filter(OU18_1 == "Under"|OU18_2 == "Under"|OU18_3 == "Under"|OU18_4 == "Under"|OU18_5 == "Under"|OU18_6 == "Under"|OU18_7 == "Under"|OU18_8 == "Under"|OU18_9 == "Under"|OU18_10 == "Under"|OU18_11 == "Under"|OU18_12 == "Under")
AnyUnder18<-rep(1, 439)
y<-cbind(y, AnyUnder18)

x<-file2%>%filter(OU18_1 == "unknown")
x<-anti_join(x, y, by = "BRC_ID")
AnyUnder18<-rep(NA, 79)
x<-cbind(x, AnyUnder18)

z<-file2%>%filter(OU18_1 == "Over")
z<-anti_join(z, y, by = "BRC_ID")

AnyUnder18<-rep(0, 576)
z<-cbind(z, AnyUnder18)

file2<-rbind(y, x)
file2<-rbind(file2, z)
file3<-file1_WS_child_inb%>%filter(Has_children == 0)
AnyUnder18<-rep(NA, 1557)
file3<-cbind(file3, AnyUnder18)

file2<-rbind(file2, file3)

file1_WS_child_inb<-file2%>%arrange(BRC_ID)

#Change NAs to character 'unknown' so doesn't get participants with NAs don't get removed from model. Then change to factor again and relevel
file1_WS_child_inb<-file1_WS_child_inb[, c(1:10, 14, 69, 70, 73, 74, 78, 79, 88, 89, 90)]
file1_WS_child_inb<-replace.value(file1_WS_child_inb, "No.ofchild")
file1_WS_child_inb$child_born_inb<-as.character(file1_WS_child_inb$child_born_inb)
file1_WS_child_inb<-file1_WS_child_inb%>%replace(is.na(.), "unknown")

file1_WS_child_inb$Ethnicity_Value<-as.factor(file1_WS_child_inb$Ethnicity_Value)
file1_WS_child_inb$Ethnicity_Value<-relevel(file1_WS_child_inb$Ethnicity_Value, "White - British")

file1_WS_child_inb$Gender_Value<-as.factor(file1_WS_child_inb$Gender_Value)
file1_WS_child_inb$Gender_Value<-relevel(file1_WS_child_inb$Gender_Value, "Male")

file1_WS_child_inb$Marital_Status_Value<-as.factor(file1_WS_child_inb$Marital_Status_Value)
file1_WS_child_inb$Marital_Status_Value<-relevel(file1_WS_child_inb$Marital_Status_Value, "Single")

file1_WS_child_inb$Diagnosis<-as.factor(file1_WS_child_inb$Diagnosis)

file1_WS_child_inb$Accommodation<-as.factor(file1_WS_child_inb$Accommodation)
file1_WS_child_inb$Accommodation<-relevel(file1_WS_child_inb$Accommodation, "Supported Living")

file1_WS_child_inb$Employment<-as.factor(file1_WS_child_inb$Employment)
file1_WS_child_inb$Employment<-relevel(file1_WS_child_inb$Employment, "Unemployed")

file1_WS_child_inb$Smoking<-as.factor(file1_WS_child_inb$Smoking)
file1_WS_child_inb$Smoking<-relevel(file1_WS_child_inb$Smoking, "Non-smoker")

file1_WS_child_inb$child_born_inb<-as.factor(file1_WS_child_inb$child_born_inb)
file1_WS_child_inb$child_born_inb<-relevel(file1_WS_child_inb$child_born_inb, "Yes")

#################Survival Analysis###################
#Covariates = Has_children, child_born_inb, No.ofchild, ethnicity, gender, marital, diagnosis, accommodation, employment, smoking, group, AnyChildUnder18 (not here might be in 'analysis')

fit<-survfit(Surv(WS_year, presence_of_WS)~Ethnicity_Value, data = file1_WS_child_inb)
summary(fit)
ggsurvplot(fit)

#without child_born_inb
mod1<-coxph(Surv(WS_year, presence_of_WS)~CurrentAge + Ethnicity_Value + Gender_Value + Marital_Status_Value + Diagnosis + Accommodation + Employment + Has_children + Smoking + group + No.ofchild + AnyUnder18, data = file1_WS_child_inb)
summary(mod1)
AIC(mod1) #21157.92
tbl_regression(mod1)

#Backwards stepwise regression using AIC
step(mod1, direction = "backward", k = 2)
#Removes group then Employment then Marital Status then Has_children then No.ofchild then gender_value

finalmod1<-coxph(Surv(WS_year, presence_of_WS) ~ CurrentAge + Ethnicity_Value + Diagnosis + Accommodation  + Smoking, data = file1_WS_child_inb)
summary(finalmod1)
AIC(finalmod1) #21135.54

#Survival analysis just with those with children (n= 1094)
file2_WS_child_inb<-file1_WS_child_inb%>%filter(Has_children == 1)
#without child_born_inb
mod1<-coxph(Surv(WS_year, presence_of_WS)~CurrentAge + Ethnicity_Value + Gender_Value + Marital_Status_Value + Diagnosis + Accommodation + Employment + Smoking + group + No.ofchild + AnyUnder18, data = file2_WS_child_inb)
summary(mod1)
AIC(mod1) #7641.39
tbl_regression(mod1, exponentiate = T, pvalue_fun = ~style_pvalue(.x, digits = 3)) %>% as_flex_table() %>% flextable::save_as_docx(path = "survanalysis_firstmod.docx")

#Backwards stepwise regression using AIC
step(mod1, direction = "backward", k = 2)
#Removes group then Employment then ethnicity then accommodation then no.of child then anyunder 18 then gender value

finalmod1<-coxph(Surv(WS_year, presence_of_WS) ~ CurrentAge + Marital_Status_Value + Diagnosis + Smoking, data = file2_WS_child_inb)
summary(finalmod1)
AIC(finalmod1) #7615.248

tbl_regression(finalmod1, exponentiate = T, pvalue_fun = ~style_pvalue(.x, digits = 3)) %>% as_flex_table() %>% flextable::save_as_docx(path = "survanalysis_finalmod.docx")

##################Graphs#####################
#Have chosen this model
finalmod1<-coxph(Surv(WS_year, presence_of_WS) ~ CurrentAge + Marital_Status_Value + Diagnosis + Smoking, data = file2_WS_child_inb)

#Now represent each varible with Kaplan-Meier curves
fit1<-survfit(Surv(WS_days, presence_of_WS)~Smoking, data = file2_WS_child_inb)
ggsurvplot(fit1, censor.size = 10, size = 1.5,legend.labs = c("Non-smoker", "Current Smoker", "Ex-smoker", "Unknown"), legend.title="", palette = c("gray34", "gray65", "steelblue1", "navy"), 
           xlim = c(0, 2500), xlab = "Days", break.x.by = 500, legend = c(0.2, 0.16), 
           font.x = 24,
           font.y = 24,
           font.legend = 24,
           font.tickslab = 18)

fit2<-survfit(Surv(WS_days, presence_of_WS)~Marital_Status_Value, data = file2_WS_child_inb)
ggsurvplot(fit2, censor.size = 10, size = 1.5,legend.labs = c("Divorced", "Married", "Single", "Unknown"), legend.title="", palette = c("gray34", "gray65", "steelblue1", "navy"), 
           xlim = c(0, 2500), xlab = "Days", break.x.by = 500, legend = c(0.2, 0.16), 
           font.x = 24,
           font.y = 24,
           font.legend = 24,
           font.tickslab = 18)

fit3<-survfit(Surv(WS_days, presence_of_WS)~Diagnosis, data = file2_WS_child_inb)
ggsurvplot(fit3, censor.size = 10, size = 1.5,legend.labs = c("F20", "F21", "F24", "F23", "F25", "F28", "F29", "F31.2&F31.5", "Cluster level"), legend.title="", palette = c("gray16","gray34", "gray65", "cadetblue1", "lightcyan4","deepskyblue", "steelblue1", "royalblue1", "navy"), 
           xlim = c(0, 2500), xlab = "Days", break.x.by = 500, legend = c(0.1, 0.3), 
           font.x = 24,
           font.y = 24,
           font.legend = 16,
           font.tickslab = 18)


#Remove some variables for neater plot

temp_file<-file2_WS_child_inb%>%filter(!Smoking=="unknown")
fit1<-survfit(Surv(WS_days, presence_of_WS)~Smoking, data = temp_file)
ggsurvplot(fit1, censor.size = 10, size = 1.5, legend.labs = c("Non-smoker", "Current Smoker", "Ex-smoker"), legend.title="", palette = c("Dark2"), 
           xlim = c(0, 2500), xlab = "Days", break.x.by = 500, legend = c(0.8, 0.8), 
           font.x = 20,
           font.y = 20,
           font.legend = 16,
           font.tickslab = 18)


temp_file<-file2_WS_child_inb%>%filter(!Marital_Status_Value=="unknown")
fit2<-survfit(Surv(WS_days, presence_of_WS)~Marital_Status_Value, data = temp_file)
ggsurvplot(fit2, censor.size = 10, size = 1.5, legend.labs = c("Single", "Divorced", "Married"), legend.title="", palette = c("Dark2"), 
           xlim = c(0, 2500), xlab = "Days", break.x.by = 500, legend = c(0.8, 0.8), 
           font.x = 20,
           font.y = 20,
           font.legend = 16,
           font.tickslab = 18)

temp_file<-file2_WS_child_inb%>%filter(Diagnosis == 'F20'|Diagnosis == 'F23'|Diagnosis == 'F25'|Diagnosis == 'F31.2&F31.5')
fit3<-survfit(Surv(WS_days, presence_of_WS)~Diagnosis, data = temp_file)
ggsurvplot(fit3, censor.size = 10, size = 1.5, legend.labs = c("F20", "F23", "F25", "F31.2&F31.5"), legend.title="", palette = c("Dark2"),
           xlim = c(0, 2500), xlab = "Days", break.x.by = 500, 
           font.x = 20,
           font.y = 20,
           font.legend = 16,
           font.tickslab = 18)
  