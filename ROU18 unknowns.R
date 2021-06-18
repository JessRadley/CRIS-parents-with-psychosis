library(tidyverse)
library(psych)
library(readxl)
library(sjmisc)
library(lubridate)
#updated file and then updated code below - this was the original file i did the coding with. Below is updated file - participants_child_details <- read_excel("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/participants_child_details.xlsx")
main <- read_excel("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/participant_all_details_cleaned.xlsx", 
                   col_types = c("text", "date", "numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", 
                                 "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", 
                                 "date", "numeric", "date", "numeric", "date", "numeric", "date", "numeric", "date", "numeric", "date", "numeric", "date", "numeric", "date", 
                                 "date", "date", "date", "date", "date", "date"))

file<-main

file%>%count(Has_children) #1940 No, 2006 Yes, 1227 NA - these 1227 are ones who didn't have children reported nor did they have any CareNotes returned for any of the searches so we just don't know
file%>%count(Children_reported) #4349 No, 824 Yes. Whether they were originally reported when searching the 'Contact' field (2 of them turned out to not actually have children)
file%>%count(Carenotes_returned_NC) #3775 No, 574 Yes, 824 NA. 824 didn't search because already knew they had children. 574 returned with 'no children'
file%>%count(Carenotes_returned_SD) #1426 No, 3173  Yes, 574 NA. 574 didn't search because already knew they had 'no children'. The 1227 who I'm not sure have children or not make up the majority of the 1426 and the rest is made up with participants who had children reported in their Contacts field but had no accompanying CareNotes
file%>%filter(Has_children == 0) %>% filter(No.ofchild == 0) #The two participants who were returned as having children in their Contacts fields but actually don't have children when reading CareNotes

####t-tests to see whether the NA groups are sig. diff from the non NA groups in terms of demographics??###

#changing all the unknowns to '1' and over and under to '0' so can use as a grouping variable to test if there is any difference in terms of gender or age. If not can try to determine these unknown ages
file%>%count(OU18_1)
file1<-file %>% mutate(OU18_1, OU18_1 = if_else(OU18_1 == "unknown", 1L, 0L))
#recode gender as 1 or 0
file1<-file1 %>% mutate(Gender_Value, Gender_Value = if_else(Gender_Value == "Male", 1L, 0L))

t.test(Gender_Value ~ OU18_1, data = file1)
t.test(CurrentAge ~ OU18_1, data = file1)

#I can conclude therefore that neither Gender nor Age is significantly different between those who's child ages are and aren't unknown
#and therefore i can do some kind of computation to estimate whether their children are over or under 18 using the parent's ages

################OU18_1#########################################################################################

file%>%group_by(OU18_1)%>%summarise(sd = sd(CurrentAge, na.rm = TRUE), mean = mean(CurrentAge, na.rm = TRUE), quantile_75 = quantile(CurrentAge, probs = 0.75, na.rm = TRUE))
ggplot(data = file, aes(x = OU18_1, y = CurrentAge)) + geom_violin()
#Can definitely say for everyone 38 and younger, they can get 'Under' and for everyone 62 and older they can get 'Over' but what to do for between 38 and 62? Leave as unknown? Use quartiles?
#Over - mean 62.4, sd 13.2. #Under mean - 38.3, sd - 8.24. Under+sd = 46.54, Over-sd = 49.2 - so they still don't overlap. Could use this as a way to decide and then leave the 47 and 48 year olds as unknowns.
file%>%filter(CurrentAge>38&CurrentAge<62)%>%select(CurrentAge, OU18_1) %>% filter(OU18_1 == "unknown") #171 we can't say anything for but 123 we can.
file%>%filter(OU18_1 == "Over"&CurrentAge<38)%>%select(BRC_ID, CurrentAge, OU18_1, CurrentAge1) #only 11/1168 in Over category are younger than 38
file%>%filter(OU18_1 == "Under"&CurrentAge>62)%>%select(BRC_ID, CurrentAge, Gender_Value, OU18_1, CurrentAge1) #only 1 person is Under category is older than 62 (is a father)

file%>%filter(OU18_1 == "Over")%>%select(OU18_1, CurrentAge)%>%count(CurrentAge)
#doing this makes it looks like someone who's 21 has a child who's 'Over' - this definitely isn't right - sort

file%>%filter(OU18_1 == "Under")%>%select(OU18_1, CurrentAge)%>%count(CurrentAge)

OU18_1_Unknown <- function(x) {
  if(is.na(x)){
    NA
  } else if (x >= 62) {
    "Over"
  } else if (x <= 38) {
    "Under"
  } else {
    "unknown"
  }
}

m <- matrix(c(54, 32, 12, 35, 87, 34, 32, 39, NA, 8, 54, 23, 45, 21, 38), nrow = 15, ncol = 1)
colnames(m) <- c("a", "b", "c")
test <- as_tibble(m)

apply(test, 1, OU18_1_Unknown)

file3<-file%>%filter(OU18_1 == "unknown") #separate out people who have OU18_1 as unknown
file3<-file3%>%select(-OU18_1) #get rid of old 'unknowns'

OU18_1<-apply(file3[, 3, drop=F], 1, OU18_1_Unknown) #generate new 'Over/Under' using function
file3<-cbind(file3, OU18_1) #put back into dataset
file3<-file3[, c(1:14, 68, 15:67)] #put in correct order
file3$OU18_1<-as.character(file3$OU18_1) #change from factor to character

file4<-file%>%filter(OU18_1 != "unknown"|(is.na(OU18_1)))
file<-rbind(file3, file4)%>%arrange(BRC_ID)


################OU18_2#########################################################################################

file%>%filter(OU18_2 == "unknown")%>%count(OU18_2)
file%>%group_by(OU18_2)%>%summarise(sd = sd(CurrentAge, na.rm = TRUE), mean = mean(CurrentAge, na.rm = TRUE), quantile_75 = quantile(CurrentAge, probs = 0.75, na.rm = TRUE))
ggplot(data = file, aes(x = OU18_2, y = CurrentAge)) + geom_violin()
file%>%filter(OU18_2 == "unknown"&CurrentAge>40&CurrentAge<64)%>%select(CurrentAge, OU18_2) #89 will be left as unknown, can update 60
file%>%filter(OU18_2 == "Over"&CurrentAge<40)%>%select(BRC_ID, CurrentAge, OU18_2, CurrentAge2)#only 3/638 in the Over category have age over 40
file%>%filter(OU18_2 == "Under"&CurrentAge>64)%>%select(BRC_ID, CurrentAge, OU18_2, CurrentAge2) #0 in the Under category have age above 64

file%>%filter(OU18_2 == "Over")%>%select(OU18_2, CurrentAge)%>%count(CurrentAge)

file%>%filter(OU18_2 == "Under")%>%select(OU18_2, CurrentAge)%>%count(CurrentAge)

OU18_2_Unknown <- function(x) {
  if(is.na(x)){
    NA
  } else if (x >= 64) {
    "Over"
  } else if (x <= 40) {
    "Under"
  } else {
    "unknown"
  }
}

apply(test, 1, OU18_2_Unknown)

#for those whose OU18_2 is unknown but OU18_1, OU18_2 should be automatically Under
file2<-file%>%filter(OU18_1== "Under"&OU18_2 == "unknown")
file2%>%replace.value(file2, "OU18_2", "unknown", "Under")

file3<-file%>%filter(!(OU18_1== "Under"&OU18_2 == "unknown"))%>%filter(OU18_2 == "unknown") #separate out people who have OU18_2 as unknown and weren't covered above
file3<-file3%>%select(-OU18_2) #get rid of old 'unknowns'

OU18_2<-apply(file3[, 3, drop=F], 1, OU18_2_Unknown) #generate new 'Over/Under' using function
file3<-cbind(file3, OU18_2) #put back into dataset
file3<-file3[, c(1:15, 68, 16:67)] #put in correct order
file3$OU18_2<-as.character(file3$OU18_2) #change from factor to character

file4<-file%>%filter(OU18_2 != "unknown"|(is.na(OU18_2)))
file5<-rbind(file2, file3)
file<-rbind(file4, file5)%>%arrange(BRC_ID)

################OU18_3#########################################################################################

file%>%filter(OU18_3 == "unknown")%>%count(OU18_3)
file%>%group_by(OU18_3)%>%summarise(sd = sd(CurrentAge, na.rm = TRUE), mean = mean(CurrentAge, na.rm = TRUE), quantile_75 = quantile(CurrentAge, probs = 0.75, na.rm = TRUE))
ggplot(data = file, aes(x = OU18_3, y = CurrentAge)) + geom_violin()
file%>%filter(OU18_3 == "unknown"&CurrentAge>42&CurrentAge<65)%>%select(CurrentAge, OU18_3) #30 will be left as unknown, can update 24
file%>%filter(OU18_3 == "Over"&CurrentAge<42)%>%select(BRC_ID, CurrentAge, OU18_3, CurrentAge2)#only 0 in the Over category have age over 42
file%>%filter(OU18_3 == "Under"&CurrentAge>65)%>%select(BRC_ID, CurrentAge, Gender_Value, OU18_3, CurrentAge3) #1 in the Under category have age above 65

file%>%filter(OU18_3 == "Over")%>%select(OU18_3, CurrentAge)%>%count(CurrentAge)

file%>%filter(OU18_3 == "Under")%>%select(OU18_3, CurrentAge)%>%count(CurrentAge)

OU18_3_Unknown <- function(x) {
  if(is.na(x)){
    NA
  } else if (x >= 65) {
    "Over"
  } else if (x <= 42) {
    "Under"
  } else {
    "unknown"
  }
}

apply(test, 1, OU18_3_Unknown)

#for those whose OU18_3 is unknown but OU18_1, OU18_3 should be automatically Under
file2<-file%>%filter(OU18_1== "Under"&OU18_3 == "unknown")
file2<-replace.value(file2, "OU18_3", "unknown", "Under")

file3<-file%>%filter(!(OU18_1== "Under"&OU18_3 == "unknown"))%>%filter(OU18_3 == "unknown") #separate out people who have OU18_3 as unknown and weren't covered above
file3<-file3%>%select(-OU18_3) #get rid of old 'unknowns'

OU18_3<-apply(file3[, 3, drop=F], 1, OU18_3_Unknown) #generate new 'Over/Under' using function
file3<-cbind(file3, OU18_3) #put back into dataset
file3<-file3[, c(1:16, 68, 17:67)] #put in correct order
file3$OU18_3<-as.character(file3$OU18_3) #change from factor to character

file4<-file%>%filter(OU18_3 != "unknown"|(is.na(OU18_3)))
file5<-rbind(file2, file3)
file<-rbind(file4, file5)%>%arrange(BRC_ID)

################OU18_4#########################################################################################

file%>%filter(OU18_4 == "unknown")%>%count(OU18_4)
file%>%group_by(OU18_4)%>%summarise(sd = sd(CurrentAge, na.rm = TRUE), mean = mean(CurrentAge, na.rm = TRUE), quantile_75 = quantile(CurrentAge, probs = 0.75, na.rm = TRUE))
ggplot(data = file, aes(x = OU18_4, y = CurrentAge)) + geom_violin()
file%>%filter(OU18_4 == "unknown"&CurrentAge>43&CurrentAge<65)%>%select(CurrentAge, OU18_4) #17 will be left as unknown, can update 15
file%>%filter(OU18_4 == "Over"&CurrentAge<43)%>%select(BRC_ID, CurrentAge, OU18_4, CurrentAge2)#only 0 in the Over category have age over 42
file%>%filter(OU18_4 == "Under"&CurrentAge>65)%>%select(BRC_ID, CurrentAge, Gender_Value, OU18_4, CurrentAge4) #1 in the Under category have age above 65

file%>%filter(OU18_4 == "Over")%>%select(OU18_4, CurrentAge)%>%count(CurrentAge)

file%>%filter(OU18_4 == "Under")%>%select(OU18_4, CurrentAge)%>%count(CurrentAge)

OU18_4_Unknown <- function(x) {
  if(is.na(x)){
    NA
  } else if (x >= 65) {
    "Over"
  } else if (x <= 43) {
    "Under"
  } else {
    "unknown"
  }
}

apply(test, 1, OU18_4_Unknown)

#for those whose OU18_4 is unknown but OU18_1, OU18_4 should be automatically Under
file2<-file%>%filter(OU18_1== "Under"&OU18_4 == "unknown")
file2<-replace.value(file2, "OU18_4", "unknown", "Under")

file3<-file%>%filter(!(OU18_1== "Under"&OU18_4 == "unknown"))%>%filter(OU18_4 == "unknown") #separate out people who have OU18_4 as unknown and weren't covered above
file3<-file3%>%select(-OU18_4) #get rid of old 'unknowns'

OU18_4<-apply(file3[, 3, drop=F], 1, OU18_4_Unknown) #generate new 'Over/Under' using function
file3<-cbind(file3, OU18_4) #put back into dataset
file3<-file3[, c(1:17, 68, 18:67)] #put in correct order
file3$OU18_4<-as.character(file3$OU18_4) #change from factor to character

file4<-file%>%filter(OU18_4 != "unknown"|(is.na(OU18_4)))
file5<-rbind(file2, file3)
file<-rbind(file4, file5)%>%arrange(BRC_ID)


################OU18_5#########################################################################################

file%>%filter(OU18_5 == "unknown")%>%count(OU18_5)
file%>%group_by(OU18_5)%>%summarise(sd = sd(CurrentAge, na.rm = TRUE), mean = mean(CurrentAge, na.rm = TRUE), quantile_75 = quantile(CurrentAge, probs = 0.75, na.rm = TRUE))
ggplot(data = file, aes(x = OU18_5, y = CurrentAge)) + geom_violin()
file%>%filter(OU18_5 == "unknown"&CurrentAge>45&CurrentAge<69)%>%select(CurrentAge, OU18_5) #11 will be left as unknown, can update 6
file%>%filter(OU18_5 == "Over"&CurrentAge<45)%>%select(BRC_ID, CurrentAge, OU18_5, CurrentAge2)#only 0 in the Over category have age over 42
file%>%filter(OU18_5 == "Under"&CurrentAge>69)%>%select(BRC_ID, CurrentAge, Gender_Value, OU18_5, CurrentAge5) #1 in the Under category have age above 65

file%>%filter(OU18_5 == "Over")%>%select(OU18_5, CurrentAge)%>%count(CurrentAge)

file%>%filter(OU18_5 == "Under")%>%select(OU18_5, CurrentAge)%>%count(CurrentAge)

OU18_5_Unknown <- function(x) {
  if(is.na(x)){
    NA
  } else if (x >= 69) {
    "Over"
  } else if (x <= 45) {
    "Under"
  } else {
    "unknown"
  }
}

apply(test, 1, OU18_5_Unknown)

#for those whose OU18_5 is unknown but OU18_1, OU18_5 should be automatically Under
file2<-file%>%filter(OU18_1== "Under"&OU18_5 == "unknown")
file2<-replace.value(file2, "OU18_5", "unknown", "Under")

file3<-file%>%filter(!(OU18_1== "Under"&OU18_5 == "unknown"))%>%filter(OU18_5 == "unknown") #separate out people who have OU18_5 as unknown and weren't covered above
file3<-file3%>%select(-OU18_5) #get rid of old 'unknowns'

OU18_5<-apply(file3[, 3, drop=F], 1, OU18_5_Unknown) #generate new 'Over/Under' using function
file3<-cbind(file3, OU18_5) #put back into dataset
file3<-file3[, c(1:18, 68, 19:67)] #put in correct order
file3$OU18_5<-as.character(file3$OU18_5) #change from factor to character

file4<-file%>%filter(OU18_5 != "unknown"|(is.na(OU18_5)))
file5<-rbind(file2, file3)
file<-rbind(file4, file5)%>%arrange(BRC_ID)

################OU18_6#########################################################################################

file%>%filter(OU18_6 == "unknown")%>%count(OU18_6)
file%>%group_by(OU18_6)%>%summarise(sd = sd(CurrentAge, na.rm = TRUE), mean = mean(CurrentAge, na.rm = TRUE), quantile_75 = quantile(CurrentAge, probs = 0.75, na.rm = TRUE))
ggplot(data = file, aes(x = OU18_6, y = CurrentAge)) + geom_violin()
file%>%filter(OU18_6 == "unknown"&CurrentAge>49&CurrentAge<69)%>%select(CurrentAge, OU18_6) #3 will be left as unknown, can update 7
file%>%filter(OU18_6 == "Over"&CurrentAge<49)%>%select(BRC_ID, CurrentAge, OU18_6, CurrentAge2)#only 0 in the Over category have age over 49
file%>%filter(OU18_6 == "Under"&CurrentAge>69)%>%select(BRC_ID, CurrentAge, Gender_Value, OU18_6, CurrentAge6) #1 in the Under category have age above 69

file%>%filter(OU18_6 == "Over")%>%select(OU18_6, CurrentAge)%>%count(CurrentAge)

file%>%filter(OU18_6 == "Under")%>%select(OU18_6, CurrentAge)%>%count(CurrentAge)

OU18_6_Unknown <- function(x) {
  if(is.na(x)){
    NA
  } else if (x >= 69) {
    "Over"
  } else if (x <= 49) {
    "Under"
  } else {
    "unknown"
  }
}

apply(test, 1, OU18_6_Unknown)

#for those whose OU18_6 is unknown but OU18_1, OU18_6 should be automatically Under
file2<-file%>%filter(OU18_1== "Under"&OU18_6 == "unknown")
file2<-replace.value(file2, "OU18_6", "unknown", "Under")

file3<-file%>%filter(!(OU18_1== "Under"&OU18_6 == "unknown"))%>%filter(OU18_6 == "unknown") #separate out people who have OU18_6 as unknown and weren't covered above
file3<-file3%>%select(-OU18_6) #get rid of old 'unknowns'

OU18_6<-apply(file3[, 3, drop=F], 1, OU18_6_Unknown) #generate new 'Over/Under' using function
file3<-cbind(file3, OU18_6) #put back into dataset
file3<-file3[, c(1:18, 68, 19:67)] #put in correct order
file3$OU18_6<-as.character(file3$OU18_6) #change from factor to character

file4<-file%>%filter(OU18_6 != "unknown"|(is.na(OU18_6)))
file5<-rbind(file2, file3)
file<-rbind(file4, file5)%>%arrange(BRC_ID)


################OU18_7#########################################################################################

file%>%count(OU18_7)
file%>%group_by(OU18_7)%>%summarise(sd = sd(CurrentAge, na.rm = TRUE), mean = mean(CurrentAge, na.rm = TRUE), quantile_75 = quantile(CurrentAge, probs = 0.75, na.rm = TRUE))
ggplot(data = file, aes(x = OU18_7, y = CurrentAge)) + geom_violin()
file%>%filter(OU18_7 == "unknown"&CurrentAge>53&CurrentAge<69)%>%select(CurrentAge, OU18_7) #1 will be left as unknown, can update 2
file%>%filter(OU18_7 == "Over"&CurrentAge<53)%>%select(BRC_ID, CurrentAge, OU18_7, CurrentAge2)#only 0 in the Over category have age over 53
file%>%filter(OU18_7 == "Under"&CurrentAge>69)%>%select(BRC_ID, CurrentAge, Gender_Value, OU18_7, CurrentAge6) #0 in the Under category have age above 69

file%>%filter(OU18_7 == "Over")%>%select(OU18_7, CurrentAge)%>%count(CurrentAge)

file%>%filter(OU18_7 == "Under")%>%select(OU18_7, CurrentAge)%>%count(CurrentAge)

OU18_7_Unknown <- function(x) {
  if(is.na(x)){
    NA
  } else if (x >= 69) {
    "Over"
  } else if (x <= 53) {
    "Under"
  } else {
    "unknown"
  }
}

apply(test, 1, OU18_7_Unknown)

#for those whose OU18_7 is unknown but OU18_1, OU18_7 should be automatically Under
file2<-file%>%filter(OU18_1== "Under"&OU18_7 == "unknown")
file2<-replace.value(file2, "OU18_7", "unknown", "Under")

file3<-file%>%filter(!(OU18_1== "Under"&OU18_7 == "unknown"))%>%filter(OU18_7 == "unknown") #separate out people who have OU18_7 as unknown and weren't covered above
file3<-file3%>%select(-OU18_7) #get rid of old 'unknowns'

OU18_7<-apply(file3[, 3, drop=F], 1, OU18_7_Unknown) #generate new 'Over/Under' using function
file3<-cbind(file3, OU18_7) #put back into dataset
file3<-file3[, c(1:20, 68, 21:67)] #put in correct order
file3$OU18_7<-as.character(file3$OU18_7) #change from factor to character

file4<-file%>%filter(OU18_7 != "unknown"|(is.na(OU18_7)))
file5<-rbind(file2, file3)
file<-rbind(file4, file5)%>%arrange(BRC_ID)

################OU18_8#########################################################################################

#the only OU18_8, ou18_9, OU18_10 (same BRCID) was in between the two values so had to be left as unknown

write_csv(file, "participants_all_details_cleaned_OU18_unknowns_added")

