library(naniar)

###function to select last in list###
last_non_NA <- function(x) {
  tail(x[!is.na(x)], 1)
}

###testing###
m <- matrix(c("cat", "whiskers", "dog", NA, "mouse", "india", "jess", NA, NA, NA, "megan", NA, "felicity", "alicia", NA), nrow = 5, ncol = 3)
colnames(m) <- c("a", "b", "c")
test <- as_tibble(m)

output<-apply(test, 1, last_non_NA)

################################DIAGNOSIS##############################################################
file<-read_csv("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/patient characteristics/diagnosis.csv")

output<-apply(file, 1, last_non_NA)

file2<-cbind(file, output)

#####Problem is that those without a diagnosis just had the BRC_ID reported rather than putting NA so i need to take them out and replace with BRC_ID with NA
file3<-file2%>%filter(str_length(output) > 8)
file3$output<-is.na(file3$output)
file4<-file3%>%replace_with_na_all(~str_length(.x)>8)

####and now I need to take out the ones that did report well and then rbind them together to make final database
file5<-file2%>%filter(str_length(output) < 8)
file6<-rbind(file4, file5)%>%arrange(BRC_ID)
file6<-file6[, c(1, 36)]

colnames(file6)<-c("BRC_ID", "Diagnosis")

last_diagnosis<-file6

#########################################EMPLOYMENT###############################################################
file<-read_csv("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/patient characteristics/employment.csv")

output<-apply(file, 1, last_non_NA)

file2<-cbind(file, output)

#####Problem is that those without a diagnosis just had the BRC_ID reported rather than putting NA so i need to take them out and replace with BRC_ID with NA
file3<-file2%>%filter(str_length(output) == 13)
file3$output<-is.na(file3$output)
file4<-file3%>%replace_with_na_all(~str_length(.x)==13)

####and now I need to take out the ones that did report well and then rbind them together to make final database
file5<-file2%>%filter(str_length(output) != 13)
file6<-rbind(file4, file5)%>%arrange(BRC_ID)
file6<-file6[, c(1, 17)]

colnames(file6)<-c("BRC_ID", "Employment")

last_employment<-file6

#########################################ACCOMMODATION###############################################################
file<-read_csv("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/patient characteristics/accommodation_new.csv")

output<-apply(file, 1, last_non_NA)

file2<-cbind(file, output)

#####Problem is that those without a diagnosis just had the BRC_ID reported rather than putting NA so i need to take them out and replace with BRC_ID with NA
View(file3<-file2%>%filter(str_length(output) == 13))
file3$output<-is.na(file3$output)
file4<-file3%>%count(BRC_ID)
file4<-file4[, 1]
output<-rep(NA, 2169)
file4<-cbind(file4, output)
##file4<-file3$output%>%replace_with_na_all(~str_length(.x)==13)

####and now I need to take out the ones that did report well and then rbind them together to make final database
file5<-file2%>%filter(str_length(output) != 13)
file5<-file5[, c(1, 17)]
file6<-rbind(file4, file5)%>%arrange(BRC_ID)

colnames(file6)<-c("BRC_ID", "Accommodation")

last_accommodation<-file6

#####SMOKING################
file<-read_csv("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/patient characteristics/smoking.csv")

output<-apply(file, 1, last_non_NA)

file2<-cbind(file, output)

#####Problem is that those without a diagnosis just had the BRC_ID reported rather than putting NA so i need to take them out and replace with BRC_ID with NA
file3<-file2%>%filter(str_length(output) == 13)
file3$output<-is.na(file3$output)
file4<-file3%>%count(BRC_ID)
file4<-file4[, 1]
output<-rep(NA, 2389)
file4<-cbind(file4, output)

####and now I need to take out the ones that did report well and then rbind them together to make final database
file5<-file2%>%filter(str_length(output) != 13)
file5<-file5[, c(1, 17)]
file6<-rbind(file4, file5)%>%arrange(BRC_ID)

colnames(file6)<-c("BRC_ID", "Smoking")

last_smoking<-file6


#####LOAD MARITAL STATUS AND DEMOGRAPHICS (DON'T NEED TO FIND LAST VALUE)###############
marital_status<-read_csv("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/patient characteristics/marital_status.csv")

demographics<-read_csv("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/patient characteristics/demographics.csv")
demographics$Date_Of_Birth<-as.Date(demographics$Date_Of_Birth, "%d/%m/%Y")


#########JOIN TOGETHER WITH MAIN FILE###################################
main <- read_excel("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/participants_child_details.xlsx", 
                                         col_types = c("text", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "text", "text", "text", "text", "text", 
                                                       "text", "text", "text", "text", 
                                                       "text", "text", "text", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "text", "text", 
                                                       "text", "text", "text", "text", "text", 
                                                       "text", "text", "text", 
                                                       "text", "text", "numeric", 
                                                       "date", "numeric", "date", "numeric", 
                                                       "date", "numeric", "date", "numeric", 
                                                       "date", "numeric", "date", 
                                                       "numeric", "date", "numeric", 
                                                       "date", "date", "date", "date", 
                                                       "date", "date", "date"))

#join databases
temp<-left_join(demographics, marital_status, by = "BRC_ID")
temp<-left_join(temp, last_diagnosis, by = "BRC_ID")
temp<-left_join(temp, last_accommodation, by = "BRC_ID")
temp<-left_join(temp, last_employment, by = "BRC_ID")
temp<-left_join(temp, main, by = "BRC_ID")
all_details<-temp
View(all_details)
write_csv(all_details, "participant_all_details")

###after changing where I found accommodation from (from CPA_discharge to CPA_review)
write_csv(last_accommodation, "new_accommodation")

write_csv(last_smoking, "smoking")

