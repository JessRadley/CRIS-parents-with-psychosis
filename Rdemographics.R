participants<-read_excel("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/demographics.xlsx", col_types = c("text", "date", "text", "text"))

participants%>%count(Gender_Value)

participants<-mutate(participants, CurrentAge = as.integer((ymd(20190214) - ymd(Date_Of_Birth))/365.25))
            
age<-(participants%>%count(CurrentAge))          
colSums(age[c(1:7), ]) #123 13-19 year olds
colSums(age[c(8:17), ]) #817 20-29 year olds
colSums(age[c(18:27), ]) #1020 30-39 year olds
colSums(age[c(28:37), ]) #1067 40-49 year olds
colSums(age[c(38:47), ]) #955 50-59 year olds
colSums(age[c(48:57), ]) #555 60-69 year olds
colSums(age[c(58:67), ]) #374 70-79 year olds
colSums(age[c(68:77), ]) #172 80-89 year olds
colSums(age[c(78:87), ]) #42 90-100 year olds
#48 NA

ethnicity<-participants%>%count(Ethnicity_Value)
justnumbers<-ethnicity[2]
colSums(justnumbers[c(2:12), ]) #403 Asian or Asian British
colSums(justnumbers[c(13:20), ]) #260 black or Black British
colSums(justnumbers[c(21:28), ]) #145 Mixed 
colSums(justnumbers[c(29:33, 47, 73), ]) #739 not stated, not known 
colSums(justnumbers[c(1, 34:46), ]) #78 other ethnic groups
colSums(justnumbers[c(52, 56, 67), ]) #3137 White British 
colSums(justnumbers[c(48:51, 53:55, 57:66, 68:72), ]) #411 White Other
