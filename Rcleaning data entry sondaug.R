#import file
library(tidyverse)
library(readxl)
library(lubridate)
data_entry_sonordaug_clinnotes_new_BRCIDs <- read_excel("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/Children in notes/reported_data_entry_sondaug_clinnotes.xlsx")#,
#col_types = c("text", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text",
#"numeric", "date", "numeric", "date", "numeric", "date", "numeric", "date", "numeric", "date", 
#"numeric", "date", "numeric", "date", "numeric", "date", "text", "text", "text", "text", "text", 

file<-data_entry_sonordaug_clinnotes_new_BRCIDs
file<-as_tibble(file)
file$DateDoc1<-as_date(file$DateDoc1)
file$DateDoc2<-as_date(file$DateDoc2)
file$DateDoc3<-as_date(file$DateDoc3)
file$DateDoc4<-as_date(file$DateDoc4)
file$DateDoc5<-as_date(file$DateDoc5)
file$DateDoc6<-as_date(file$DateDoc6)
file$DateDoc7<-as_date(file$DateDoc7)
file$DateDoc8<-as_date(file$DateDoc8)

file$DOB1<-as_date(file$DOB1)
file$DOB2<-as_date(file$DOB2)
file$DOB3<-as_date(file$DOB3)
file$DOB4<-as_date(file$DOB4)
file$DOB5<-as_date(file$DOB5)
file$DOB6<-as_date(file$DOB6)
file$DOB7<-as_date(file$DOB7)
file$DOB8<-as_date(file$DOB8)
file$DOB9<-as_date(file$DOB9)
file$DOB10<-as_date(file$DOB10)

#had to rename the OU18 bits because when i originally wrote the code for the first file they were named something 
#different so i changed the names here so i didn't have to rewrite the code

file$`O/U18.1`<-file$OU18.1
file$`O/U18.2`<-file$OU18.2
file$`O/U18.3`<-file$OU18.3
file$`O/U18.4`<-file$OU18.4
file$`O/U18.5`<-file$OU18.5
file$`O/U18.6`<-file$OU18.6
file$`O/U18.7`<-file$OU18.7
file$`O/U18.8`<-file$OU18.8
file$`O/U18.9`<-file$OU18.9
file$`O/U18.10`<-file$OU18.10
file$`O/U18.11`<-file$OU18.11
file$`O/U18.12`<-file$OU18.12

file<-select(file, -OU18.1, -OU18.2, -OU18.3, -OU18.4, -OU18.5, -OU18.6, -OU18.7, -OU18.8, -OU18.9, -OU18.10, -OU18.11, -OU18.12)

#count to check for errors
file%>%count(Gender1)

#select certain patients and checking they have all data that should be completed
#and don't have any data that shouldn't be completed

#Those without a child have nothing reported
file%>%filter(Evidofchild == 0) %>% filter(No.ofchild > 1|(!is.na(`O/U18.1`))|(!is.na(`O/U18.2`))|(!is.na(`O/U18.3`))|(!is.na(`O/U18.4`))
                                           |(!is.na(`O/U18.5`))|(!is.na(`O/U18.6`))|(!is.na(`O/U18.7`))|(!is.na(`O/U18.8`))
                                           |(!is.na(Age1))|(!is.na(Age2))|(!is.na(Age3))|(!is.na(Age4))|(!is.na(Age5))|(!is.na(Age6))
                                           |(!is.na(Age7))|(!is.na(Age8))|(!is.na(DateDoc1))|(!is.na(DateDoc2))|(!is.na(DateDoc3))
                                           |(!is.na(DateDoc4))|(!is.na(DateDoc5))|(!is.na(DateDoc6))|(!is.na(DateDoc7))|(!is.na(DateDoc8))
                                           |(!is.na(Gender1))|(!is.na(Gender2))|(!is.na(Gender3))|(!is.na(Gender4))|(!is.na(Gender5))|(!is.na(Gender6))|(!is.na(Gender7))
                                           |(!is.na(Gender8))|(!is.na(DOB1))|(!is.na(DOB2))|(!is.na(DOB3))|(!is.na(DOB4))|(!is.na(DOB5)))

#No. of child matches right number of O/U18 columns
file%>%filter(No.ofchild == 1)%>%filter(is.na(`O/U18.1`)|!(is.na(`O/U18.2`))|!(is.na(`O/U18.3`))|!(is.na(`O/U18.4`))|!(is.na(`O/U18.5`))|!(is.na(`O/U18.6`))|!(is.na(`O/U18.7`))|!(is.na(`O/U18.8`)))
file%>%filter(No.ofchild == 2)%>%filter(is.na(`O/U18.1`)|is.na(`O/U18.2`)|!(is.na(`O/U18.3`))|!(is.na(`O/U18.4`))|!(is.na(`O/U18.5`))|!(is.na(`O/U18.6`))|!(is.na(`O/U18.7`))|!(is.na(`O/U18.8`)))
file%>%filter(No.ofchild == 3)%>%filter(is.na(`O/U18.1`)|is.na(`O/U18.2`)|is.na(`O/U18.3`)|!(is.na(`O/U18.4`))|!(is.na(`O/U18.5`))|!(is.na(`O/U18.6`))|!(is.na(`O/U18.7`))|!(is.na(`O/U18.8`)))
file%>%filter(No.ofchild == 4)%>%filter(is.na(`O/U18.1`)|is.na(`O/U18.2`)|is.na(`O/U18.3`)|is.na(`O/U18.4`)|!(is.na(`O/U18.5`))|!(is.na(`O/U18.6`))|!(is.na(`O/U18.7`))|!(is.na(`O/U18.8`)))
file%>%filter(No.ofchild == 5)%>%filter(is.na(`O/U18.1`)|is.na(`O/U18.2`)|is.na(`O/U18.3`)|is.na(`O/U18.4`)|is.na(`O/U18.5`)|!(is.na(`O/U18.6`))|!(is.na(`O/U18.7`))|!(is.na(`O/U18.8`)))
file%>%filter(No.ofchild == 6)%>%filter(is.na(`O/U18.1`)|is.na(`O/U18.2`)|is.na(`O/U18.3`)|is.na(`O/U18.4`)|is.na(`O/U18.5`)|is.na(`O/U18.6`)|!(is.na(`O/U18.7`))|!(is.na(`O/U18.8`)))
file%>%filter(No.ofchild == 7)%>%select(BRC_ID, No.ofchild, `O/U18.1`, `O/U18.2`, `O/U18.3`, `O/U18.4`, `O/U18.5`, `O/U18.6`, `O/U18.7`, `O/U18.8`, `O/U18.9`,`O/U18.10`, `O/U18.11`, `O/U18.12`)
file%>%filter(No.ofchild == 8)%>%select(BRC_ID, No.ofchild, `O/U18.1`, `O/U18.2`, `O/U18.3`, `O/U18.4`, `O/U18.5`, `O/U18.6`, `O/U18.7`, `O/U18.8`, `O/U18.9`,`O/U18.10`, `O/U18.11`, `O/U18.12`)
file%>%filter(No.ofchild == 9)%>%select(BRC_ID, No.ofchild, `O/U18.1`, `O/U18.2`, `O/U18.3`, `O/U18.4`, `O/U18.5`, `O/U18.6`, `O/U18.7`, `O/U18.8`, `O/U18.9`,`O/U18.10`, `O/U18.11`, `O/U18.12`)
file%>%filter(No.ofchild == 10)%>%select(BRC_ID, No.ofchild, `O/U18.1`, `O/U18.2`, `O/U18.3`, `O/U18.4`, `O/U18.5`, `O/U18.6`, `O/U18.7`, `O/U18.8`, `O/U18.9`,`O/U18.10`, `O/U18.11`, `O/U18.12`)
file%>%filter(No.ofchild == 11)%>%select(BRC_ID, No.ofchild, `O/U18.1`, `O/U18.2`, `O/U18.3`, `O/U18.4`, `O/U18.5`, `O/U18.6`, `O/U18.7`, `O/U18.8`, `O/U18.9`,`O/U18.10`, `O/U18.11`, `O/U18.12`)
file%>%filter(No.ofchild == 12)%>%select(BRC_ID, No.ofchild, `O/U18.1`, `O/U18.2`, `O/U18.3`, `O/U18.4`, `O/U18.5`, `O/U18.6`, `O/U18.7`, `O/U18.8`, `O/U18.9`,`O/U18.10`, `O/U18.11`, `O/U18.12`)
#found errors doing this. Have highlighted to check on excel file.
#Now check excel file against clinical notes to resolve error
#Done

#No. of child matches right number of Gender columns
file%>%filter(No.ofchild == 1)%>%filter(is.na(`Gender1`)|!(is.na(`Gender2`))|!(is.na(`Gender3`))|!(is.na(`Gender4`))|!(is.na(`Gender5`))|!(is.na(`Gender6`))|!(is.na(`Gender7`))|!(is.na(`Gender8`)))%>%select(BRC_ID, No.ofchild, Gender1, Gender2, Gender3, Gender4, Gender5, Gender6, Gender7, Gender8)
file%>%filter(No.ofchild == 2)%>%filter(is.na(`Gender1`)|is.na(`Gender2`)|!(is.na(`Gender3`))|!(is.na(`Gender4`))|!(is.na(`Gender5`))|!(is.na(`Gender6`))|!(is.na(`Gender7`))|!(is.na(`Gender8`)))%>%select(BRC_ID, No.ofchild, Gender1, Gender2, Gender3, Gender4, Gender5, Gender6, Gender7, Gender8)
file%>%filter(No.ofchild == 3)%>%filter(is.na(`Gender1`)|is.na(`Gender2`)|is.na(`Gender3`)|!(is.na(`Gender4`))|!(is.na(`Gender5`))|!(is.na(`Gender6`))|!(is.na(`Gender7`))|!(is.na(`Gender8`)))%>%select(BRC_ID, No.ofchild, Gender1, Gender2, Gender3, Gender4, Gender5, Gender6, Gender7, Gender8)
file%>%filter(No.ofchild == 4)%>%filter(is.na(`Gender1`)|is.na(`Gender2`)|is.na(`Gender3`)|is.na(`Gender4`)|!(is.na(`Gender5`))|!(is.na(`Gender6`))|!(is.na(`Gender7`))|!(is.na(`Gender8`)))%>%select(BRC_ID, No.ofchild, Gender1, Gender2, Gender3, Gender4, Gender5, Gender6, Gender7, Gender8)
file%>%filter(No.ofchild == 5)%>%filter(is.na(`Gender1`)|is.na(`Gender2`)|is.na(`Gender3`)|is.na(`Gender4`)|is.na(`Gender5`)|!(is.na(`Gender6`))|!(is.na(`Gender7`))|!(is.na(`Gender8`)))%>%select(BRC_ID, No.ofchild, Gender1, Gender2, Gender3, Gender4, Gender5, Gender6, Gender7, Gender8)
file%>%filter(No.ofchild == 6)%>%filter(is.na(`Gender1`)|is.na(`Gender2`)|is.na(`Gender3`)|is.na(`Gender4`)|is.na(`Gender5`)|is.na(`Gender6`)|!(is.na(`Gender7`))|!(is.na(`Gender8`)))%>%select(BRC_ID, No.ofchild, Gender1, Gender2, Gender3, Gender4, Gender5, Gender6, Gender7, Gender8)
file%>%filter(No.ofchild == 7)%>%select(BRC_ID, No.ofchild, Gender1, Gender2, Gender3, Gender4, Gender5, Gender6, Gender7, Gender8, Gender9, Gender10, Gender11, Gender12)
file%>%filter(No.ofchild == 8)%>%select(BRC_ID, No.ofchild, Gender1, Gender2, Gender3, Gender4, Gender5, Gender6, Gender7, Gender8, Gender9, Gender10, Gender11, Gender12)
file%>%filter(No.ofchild == 9)%>%select(BRC_ID, No.ofchild, Gender1, Gender2, Gender3, Gender4, Gender5, Gender6, Gender7, Gender8, Gender9, Gender10, Gender11, Gender12)
file%>%filter(No.ofchild == 10)%>%select(BRC_ID, No.ofchild, Gender1, Gender2, Gender3, Gender4, Gender5, Gender6, Gender7, Gender8, Gender9, Gender10, Gender11, Gender12)
file%>%filter(No.ofchild == 11)%>%select(BRC_ID, No.ofchild, Gender1, Gender2, Gender3, Gender4, Gender5, Gender6, Gender7, Gender8, Gender9, Gender10, Gender11, Gender12)
file%>%filter(No.ofchild == 12)%>%select(BRC_ID, No.ofchild, Gender1, Gender2, Gender3, Gender4, Gender5, Gender6, Gender7, Gender8, Gender9, Gender10, Gender11, Gender12)



#Age column always has DateDoc column
file%>%filter(!is.na(`Age1`)&is.na(DateDoc1))%>%select(BRC_ID, Age1, DateDoc1)
file%>%filter(!is.na(`Age2`)&is.na(DateDoc2))%>%select(BRC_ID, Age2, DateDoc2)
file%>%filter(!is.na(`Age3`)&is.na(DateDoc3))%>%select(BRC_ID, Age3, DateDoc3)
file%>%filter(!is.na(`Age4`)&is.na(DateDoc4))%>%select(BRC_ID, Age4, DateDoc4)
file%>%filter(!is.na(`Age5`)&is.na(DateDoc5))%>%select(BRC_ID, Age5, DateDoc5)
file%>%filter(!is.na(`Age6`)&is.na(DateDoc6))%>%select(BRC_ID, Age6, DateDoc6)
file%>%filter(!is.na(`Age7`)&is.na(DateDoc7))%>%select(BRC_ID, Age7, DateDoc7)
file%>%filter(!is.na(`Age8`)&is.na(DateDoc8))%>%select(BRC_ID, Age8, DateDoc8)

#either Age or DOB is completed but not both
file%>%filter(!is.na(`Age1`)&!is.na(DOB1))%>%select(BRC_ID, Age1, DOB1)
file%>%filter(!is.na(`Age2`)&!is.na(DOB2))%>%select(BRC_ID, Age2, DOB2)
file%>%filter(!is.na(`Age3`)&!is.na(DOB3))%>%select(BRC_ID, Age3, DOB3)
file%>%filter(!is.na(`Age4`)&!is.na(DOB4))%>%select(BRC_ID, Age4, DOB4)
file%>%filter(!is.na(`Age5`)&!is.na(DOB5))%>%select(BRC_ID, Age5, DOB5)
file%>%filter(!is.na(`Age6`)&!is.na(DOB6))%>%select(BRC_ID, Age6, DOB6)
file%>%filter(!is.na(`Age7`)&!is.na(DOB7))%>%select(BRC_ID, Age7, DOB7)

#function to convert DOB/Age into Under or Over
convert_toOU <- function(x) {
  if(x >= 18) "Over" else "Under"
}

#made a test vector to see if I could make function loop in this 
test<-c(14, 12, 17, 2)

output <- vector("character", length(test)) 
for (i in seq_along(test)) {
  output[[i]] <- convert_toOU(test[[i]])
  }

#realise that it's working for normal numbers but not for NA entries so made second function that works for NAs

convert_toOUNA <- function(x) {
  if(is.na(x)) {
    "NA"
  } else if (x >= 18) {
    "Over"
  } else {
    "Under"
  }
}

test<-c(18, 12, 17, 2, NA)


output <- vector("character", length(test)) 
for (i in seq_along(test)) {
  output[[i]] <- convert_toOUNA(test[[i]])
}

#and now the converting to Over/Under or NA function works for the file!!
output <- vector("character", length(file$Age1))
for (i in seq_along(file$Age1)) {
  output[[i]] <- convert_toOUNA(file$Age1[[i]])
}

#Now I need to calculate current age from DOB
as.integer((ymd(20190213) - (ymd(`DateDocx`) - years(`Agex`)))/365.25)

#Mutate file to add 'CurrentAge' columns
file<-mutate(file, 
       CurrentAge1a = as.integer((ymd(20190214) - (ymd(`DateDoc1`) - years(`Age1`)))/365.25),
       CurrentAge2a = as.integer((ymd(20190214) - (ymd(`DateDoc2`) - years(`Age2`)))/365.25),
       CurrentAge3a = as.integer((ymd(20190214) - (ymd(`DateDoc3`) - years(`Age3`)))/365.25),
       CurrentAge4a = as.integer((ymd(20190214) - (ymd(`DateDoc4`) - years(`Age4`)))/365.25),
       CurrentAge5a = as.integer((ymd(20190214) - (ymd(`DateDoc5`) - years(`Age5`)))/365.25),
       CurrentAge6a = as.integer((ymd(20190214) - (ymd(`DateDoc6`) - years(`Age6`)))/365.25),
       CurrentAge7a = as.integer((ymd(20190214) - (ymd(`DateDoc7`) - years(`Age7`)))/365.25),
       CurrentAge1b = as.integer((ymd(20190214) - ymd(`DOB1`))/365.25),
       CurrentAge2b = as.integer((ymd(20190214) - ymd(`DOB2`))/365.25),
       CurrentAge3b = as.integer((ymd(20190214) - ymd(`DOB3`))/365.25),
       CurrentAge4b = as.integer((ymd(20190214) - ymd(`DOB4`))/365.25),
       CurrentAge5b = as.integer((ymd(20190214) - ymd(`DOB5`))/365.25),
       CurrentAge6b = as.integer((ymd(20190214) - ymd(`DOB6`))/365.25),
       CurrentAge7b = as.integer((ymd(20190214) - ymd(`DOB7`))/365.25),
       CurrentAge8 = as.integer((ymd(20190214) - ymd(`DOB8`))/365.25),
       CurrentAge9 = as.integer((ymd(20190214) - ymd(`DOB9`))/365.25),
       CurrentAge10 = as.integer((ymd(20190214) - ymd(`DOB10`))/365.25))

#Need to also work out Current Age from DOBs known. Apparently can use 'dplyr:: coalesce' 
#to make new column out of Current Age from the Age column and Current Age from the DOB column

file<-file%>%mutate(CurrentAge1 = coalesce(CurrentAge1a, CurrentAge1b), 
                    CurrentAge2 = coalesce(CurrentAge2a, CurrentAge2b),
                    CurrentAge3 = coalesce(CurrentAge3a, CurrentAge3b),
                    CurrentAge4 = coalesce(CurrentAge4a, CurrentAge4b),
                    CurrentAge5 = coalesce(CurrentAge5a, CurrentAge5b),
                    CurrentAge6 = coalesce(CurrentAge6a, CurrentAge6b),
                    CurrentAge7 = coalesce(CurrentAge7a, CurrentAge7b))


#Next step - create new O/U18.1 etc... columns by using function on file$CurrentAge1, file$CurrentAge2 etc...
output <- vector("character", length(file$CurrentAge1))
for (i in seq_along(file$CurrentAge1)) {
  output[[i]] <- convert_toOUNA(file$CurrentAge1[[i]])
}
x<-as_tibble(output)
colnames(x) <- "OU18.1"

output2 <- vector("character", length(file$CurrentAge2))
for (i in seq_along(file$CurrentAge2)) {
  output2[[i]] <- convert_toOUNA(file$CurrentAge2[[i]])
}
x2<-as_tibble(output2)
colnames(x2) <- "OU18.2"

output3 <- vector("character", length(file$CurrentAge3))
for (i in seq_along(file$CurrentAge3)) {
  output3[[i]] <- convert_toOUNA(file$CurrentAge3[[i]])
}
x3<-as_tibble(output3)
colnames(x3) <- "OU18.3"

output4 <- vector("character", length(file$CurrentAge4))
for (i in seq_along(file$CurrentAge4)) {
  output4[[i]] <- convert_toOUNA(file$CurrentAge4[[i]])
}
x4<-as_tibble(output4)
colnames(x4) <- "OU18.4"

output5 <- vector("character", length(file$CurrentAge5))
for (i in seq_along(file$CurrentAge5)) {
  output5[[i]] <- convert_toOUNA(file$CurrentAge5[[i]])
}
x5<-as_tibble(output5)
colnames(x5) <- "OU18.5"

output6 <- vector("character", length(file$CurrentAge6))
for (i in seq_along(file$CurrentAge6)) {
  output6[[i]] <- convert_toOUNA(file$CurrentAge6[[i]])
}
x6<-as_tibble(output6)
colnames(x6) <- "OU18.6"

output7 <- vector("character", length(file$CurrentAge7))
for (i in seq_along(file$CurrentAge7)) {
  output7[[i]] <- convert_toOUNA(file$CurrentAge7[[i]])
}
x7<-as_tibble(output7)
colnames(x7) <- "OU18.7"

output8 <- vector("character", length(file$CurrentAge8))
for (i in seq_along(file$CurrentAge8)) {
  output8[[i]] <- convert_toOUNA(file$CurrentAge8[[i]])
}
x8<-as_tibble(output8)
colnames(x8) <- "OU18.8"

output9 <- vector("character", length(file$CurrentAge9))
for (i in seq_along(file$CurrentAge9)) {
  output9[[i]] <- convert_toOUNA(file$CurrentAge9[[i]])
}
x9<-as_tibble(output9)
colnames(x9) <- "OU18.9"

output10 <- vector("character", length(file$CurrentAge10))
for (i in seq_along(file$CurrentAge10)) {
  output10[[i]] <- convert_toOUNA(file$CurrentAge10[[i]])
}
x10<-as_tibble(output10)
colnames(x10) <- "OU18.10"

file<-cbind(file, x, x2, x3, x4, x5, x6, x7, x8, x9, x10)
#file now contains new columns called 'OU18.1, OU18.2 etc...'

#Realised I actually did character NA rather than true missing so below is necessary:
file$OU18.1<-na_if(file$OU18.1, "NA")
file$OU18.2<-na_if(file$OU18.2, "NA")
file$OU18.3<-na_if(file$OU18.3, "NA")
file$OU18.4<-na_if(file$OU18.4, "NA")
file$OU18.5<-na_if(file$OU18.5, "NA")
file$OU18.6<-na_if(file$OU18.6, "NA")
file$OU18.7<-na_if(file$OU18.7, "NA")
file$OU18.8<-na_if(file$OU18.8, "NA")
file$OU18.9<-na_if(file$OU18.9, "NA")
file$OU18.10<-na_if(file$OU18.10, "NA")

#Get Over/Under information from original columns that was worked out by implication within free text
#rather than using Age or DOB column

file<-file%>%mutate(OU18_1 = coalesce(OU18.1, `O/U18.1`), 
              OU18_2 = coalesce(OU18.2, `O/U18.2`), 
              OU18_3 = coalesce(OU18.3, `O/U18.3`), 
              OU18_4 = coalesce(OU18.4, `O/U18.4`), 
              OU18_5 = coalesce(OU18.5, `O/U18.5`), 
              OU18_6 = coalesce(OU18.6, `O/U18.6`), 
              OU18_7 = coalesce(OU18.7, `O/U18.7`), 
              OU18_8 = coalesce(OU18.8, `O/U18.8`),
              OU18_9 = coalesce(OU18.9, `O/U18.9`),
              OU18_10 = coalesce(OU18.10, `O/U18.10`))

#These two below didn't have any input in Age/DateDoc or DOB- I just cleaned from context whether they were over or under

file$OU18_11<-file$`O/U18.11`
file$OU18_12<-file$`O/U18.12`


#Remove columns I no longer need
file<-select(file, -`O/U18.1`, -`O/U18.2`, -`O/U18.3`, -`O/U18.4`, -`O/U18.5`, -`O/U18.6`, -`O/U18.7`, -`O/U18.8`, -`O/U18.9`, -`O/U18.10`, -`O/U18.11`, -`O/U18.12`,  
             -OU18.1, -OU18.2, -OU18.3, -OU18.4, -OU18.5, -OU18.6, -OU18.7, -OU18.8, -OU18.9, -OU18.10, 
          -CurrentAge1a, -CurrentAge2a, -CurrentAge3a, -CurrentAge4a, -CurrentAge5a, -CurrentAge6a, -CurrentAge7a,
          -CurrentAge1b, -CurrentAge2b, -CurrentAge3b, -CurrentAge4b, -CurrentAge5b, -CurrentAge6b, -CurrentAge7b)

file<-select(file, -DOB_patient)

#Change order of columns
#for first file (cleaned_data_entry_sondaug_clinnotes)
file<-file[,c(1, 2, 3, 42, 43, 44, 45, 46, 47, 48, 49, 36, 37, 38, 39, 40, 41, 34, 35, 20, 21, 22, 23, 24, 25, 26, 27, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 28, 29, 30, 31, 32, 33)]

#for second file (cleaned_reported_data_entry_sondaug_clinnotes)
file<-file[,c(1, 2, 3, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 43, 44, 45, 46, 47, 48, 49, 40, 41, 42, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39)]

write_csv(file, "cleaned_data_entry_sondaug_clinnotes")
write_csv(file, "cleaned_reported_data_entry_sondaug_clinnotes")
file%>%count(Evidofchild)
