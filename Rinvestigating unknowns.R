library(tidyverse)
library(psych)
library(readxl)
library(sjmisc)
library(lubridate)
participants_child_details <- read_excel("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/participants_child_details.xlsx")
file<-participants_child_details

file%>%count(Has_children) #1940 No, 2006 Yes, 1227 NA - these 1227 are ones who didn't have children reported nor did they have any CareNotes returned for any of the searches so we just don't know
file%>%count(Children_reported) #4349 No, 824 Yes. Whether they were originally reported when searching the 'Contact' field (2 of them turned out to not actually have children)
file%>%count(Carenotes_returned_NC) #3775 No, 574 Yes, 824 NA. 824 didn't search because already knew they had children. 574 returned with 'no children'
file%>%count(Carenotes_returned_SD) #1426 No, 3173  Yes, 574 NA. 574 didn't search because already knew they had 'no children'. The 1227 who I'm not sure have children or not make up the majority of the 1426 and the rest is made up with participants who had children reported in their Contacts field but had no accompanying CareNotes
file%>%filter(Has_children == 0) %>% filter(No.ofchild == 0) #The two participants who were returned as having children in their Contacts fields but actually don't have children when reading CareNotes


####t-tests to see whether the NA groups are sig. diff from the non NA groups in terms of demographics??###
participants_demographics_and_child_details <- read_excel("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/participants_demographics_and_child_details.xlsx")
file<-participants_demographics_and_child_details

#changing all the unknowns to '1' and over and under to '0' so can use as a grouping variable to test if there is any difference in terms of gender or age. If not can try to determine these unknown ages
file%>%count(OU18_1)
file1<-file %>% mutate(OU18_1, OU18_1 = if_else(OU18_1 == "unknown", 1L, 0L))
#recode gender as 1 or 0
file1<-file1 %>% mutate(Gender_Value, Gender_Value = if_else(Gender_Value == "Male", 1L, 0L))
#change DOB into Current Age
file1<-mutate(file1, CurrentAge = as.integer((ymd(20190214) - ymd(Date_Of_Birth))/365.25))

t.test(Gender_Value ~ OU18_1, data = file1)
t.test(CurrentAge ~ OU18_1, data = file1)

#I can conclude therefore that neither Gender nor Age is significantly different between those who's child ages are and aren't unknown
#and therefore i can do some kind of computation to estimate whether their children are over or under 18 using the parent's ages

file2<-mutate(file, CurrentAge = as.integer((ymd(20190214) - ymd(Date_Of_Birth))/365.25))

file2%>%group_by(OU18_1)%>%summarise(sd = sd(CurrentAge, na.rm = TRUE), mean = mean(CurrentAge, na.rm = TRUE), quantile_75 = quantile(CurrentAge, probs = 0.75, na.rm = TRUE))
ggplot(data = file2, aes(x = OU18_1, y = CurrentAge)) + geom_violin()
#Can definitely say for everyone 38 and younger, they can get 'Under' and for everyone 62 and older they can get 'Over' but what to do for between 38 and 62? Leave as unknown? Use quartiles?
#Over - mean 62.4, sd 13.2. #Under mean - 38.3, sd - 8.24. Under+sd = 46.54, Over-sd = 49.2 - so they still don't overlap. Could use this as a way to decide and then leave the 47 and 48 year olds as unknowns.
file2%>%filter(CurrentAge>46&CurrentAge<49)%>%select(CurrentAge, OU18_1) %>% filter(OU18_1 == "unknown") #that leaves 12 people where we can't say or should I just find some way of saying


file2%>%filter(OU18_1 == "Over")%>%select(OU18_1, CurrentAge)%>%count(CurrentAge)
#doing this makes it looks like someone who's 21 has a child who's 'Over' - this definitely isn't right - sort

file2%>%filter(OU18_1 == "Under")%>%select(OU18_1, CurrentAge)%>%count(CurrentAge)
