####Packages#####################
library(anchors)
library(tidyverse)
library(readxl)
library(lubridate)

main <- read_excel("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/participant_all_details.xlsx", 
                   col_types = c("text", "date", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", 
                   "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", 
                   "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", 
                   "date", "numeric", "date", "numeric", "date", "numeric", "date", "numeric", "date", "numeric", "date", "numeric", "date", "numeric", "date", 
                   "date", "date", "date", "date", "date", "date", "numeric", "text", "text", "text", "numeric", "numeric"))

main<-mutate(main, CurrentAge = as.integer((ymd(20190214) - ymd(Date_Of_Birth))/365.25))

y<-main
######RECODE VARIABLES############################################
######Ethnicity###############################################
x<-main%>%select(Ethnicity_Value)%>%count(Ethnicity_Value)

#asian 
y<-replace.value(y, "Ethnicity_Value", c("Asian or Asian British - Any other Asian background", "Asian or Asian British - Kashmiri", "Asian or Asian British - Bangladeshi", "Asian or Asian British - British", "Asian or Asian British - Chinese", "Asian or Asian British - Indian", "Asian or Asian British - Kasmiri", "Asian or Asian British - Mixed Asian", "Asian or Asian British - Other/Unspecified", "Asian or Asian British - Pakistani", "Asian or Asian British - Punjabi", "Asian or Asian British - Sri Lanka"), "Asian")
#black
y<-replace.value(y, "Ethnicity_Value", c("Black or Black British - African", "Black or Black British - Any other Black background", "Black or Black British - British", "Black or Black British - Caribbean", "Black or Black British - Mixed", "Black or Black British - Nigerian", "Black or Black British - Other/Unspecified", "Black or Black British - Somali"), "Black")
#mixed
y<-replace.value(y, "Ethnicity_Value", c("Mixed - Any other mixed background", "Mixed - Asian and Chinese", "Mixed - Black and Asian", "Mixed - Black and White", "Mixed - Other/Unspecified", "Mixed - White and Asian", "Mixed - White and Black African", "Mixed - White and Black Caribbean"), "Mixed")
#unknown
y<-replace.value(y, "Ethnicity_Value", c("Not Known (Not Requested)", "Not Known (Uble to Request)", "Not Stated (Client Refused)", "Not Stated (Client uble to Choose)", "Not Stated (Not Requested)", "Refused"), NA)
#any other group
y<-replace.value(y, "Ethnicity_Value", c("Any Other Group", "Other Ethnic Groups - Any other Ethnic Group", "Other Ethnic Groups - Arab", "Other Ethnic Groups - Chinese", "Other Ethnic Groups - Filipino", "Other Ethnic Groups - Israeli", "Other Ethnic Groups - Japanese", "Other Ethnic Groups - Kurdish", "Other Ethnic Groups - Latin American", "Other Ethnic Groups - Malaysian", "Other Ethnic Groups - Maur/SEyc/Mald/StHelen", "Other Ethnic Groups - North African", "Other Ethnic Groups - Other Middle East", "Other Ethnic Groups - Vietmese"), "Any Other Group")
#white - british
y<-replace.value(y, "Ethnicity_Value", c("White - British", "White - Scottish", "White - English", "White - Irish", "White - Irish Traveller", "White - Cornish", "White - Welsh", "White - Traveller"), "White - British")
#white - other
y<-replace.value(y, "Ethnicity_Value", c("White - Albanian", "White - All Republics of former USSR", "White - Other European", "White - Any other background", "White - Any other White background", "White - Croatian", "White - Cypriot (part not stated)", "White - Greek", "White - Greek Cypriot", "White - Gypsy/Romany", "White - Italian", "White - Mixed White", "White - Other Republics of former Yugoslavia", "White - Polish", "White - Serbian", "White - Turkish", "White - Turkish Cypriot"), "White - Other")

y%>%dplyr::select(Ethnicity_Value)%>%count(Ethnicity_Value)

#####Marital Status#####################################################
x<-main%>%select(Marital_Status_Value)%>%count(Marital_Status_Value)

#married/civil partner
y<-replace.value(y, "Marital_Status_Value", c("Civil Partnership", "Co-habiting", "Married", "Married/Civil Partner"), "Married/Civil Partner")

#divorced/separated/widowed
y<-replace.value(y, "Marital_Status_Value", c("Divorced/Person whose Civil Partnership has been dissolved", "Separated", "Widowed/Surviving Civil Partner"), "Divorced/Separated/Widowed")

#unknown
y<-replace.value(y, "Marital_Status_Value", c("Not Applicable", "Not Disclosed", "Not Known"), NA)

y%>%dplyr::select(Marital_Status_Value)%>%count(Marital_Status_Value)


#####Accommodation#####################################################
x<-main%>%select(Accommodation)%>%count(Accommodation)

#Supported Living
y<-replace.value(y, "Accommodation", c("Extra care sheltered housing", "MH Registered Care Home", "Non-MH Registered Care Home", "Non MH Accommodation with care", "Nursing Home for older persons", "Other sheltered housing", "Supported accommodation", "Supported group home", "Supported lodgings"), "Supported Living")

#Owning
y<-replace.value(y, "Accommodation", c("Owner occupier", "Shared ownership scheme"), "Owning")

#Renting
y<-replace.value(y, "Accommodation", c("Other mainstream housing", "Settled mainstream housing", "Tent - Housing Association", "Tent - Local Authority", "Tent - private landlord"), "Renting")

#Other
y<-replace.value(y, "Accommodation", c("Bail/Probation hostel", "Foyer", "NHS acute psychiatric ward", "Other homeless", "Other MH accommodation", "Other NHS facilities/hospital", "Prison", "Refuge", "Secure psychiatric unit", "Sheltered housing (older person)", "Sofa surfing", "Squatting", "Staying with friends/family", "Temporary accommodation", "Independent hospital/clinic", "Other Acute/long stay healthcare", "Specialist rehab/recovery"), "Other")

#Unknown
y<-replace.value(y, "Accommodation", c("Not applicable", "Not elsewhere classified", "Not known"), NA)

y%>%dplyr::select(Accommodation)%>%count(Accommodation)

#####Employment#########################################################
x<-main%>%select(Employment)%>%count(Employment)

#Student
y<-replace.value(y, "Employment", c("Students who are undertaking full (at least 16 hours per week) or part-time (less than 16 hours per week) education or training and who are not working or actively seeking work"), "Student")

#Unemployed
y<-replace.value(y, "Employment", c("Homemaker looking after the family or home and who are not working or actively seeking work", "Not receiving benefits and who are not working or actively seeking work", "Unemployed and Seeking Work", "Unpaid voluntary work who are not working or actively seeking work"), "Unemployed")

#Benefits
y<-replace.value(y, "Employment", c("Long-term sick or disabled, those who are receiving Incapacity Benefit, Income Support or both; or Employment and Support Allowance"), "Benefits")

#Unknown
y<-replace.value(y, "Employment", "Not Stated (PERSON?? asked but declined to provide a response", NA)

y%>%dplyr::select(Employment)%>%count(Employment)


write_csv(y, "participant_all_details_cleaned")

#####Ward Stay#########################################################
wardstay <- read_excel("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/patient characteristics/ward_stay_long.xlsx", col_types = c("text", "date", "date"))
#separate into those without any ward stay documented and those with
y<-wardstay%>%filter(is.na(Actual_Start_Date))
y<-y[, 1]
zero<-rep(0, 2459)
y<-cbind(y, zero)
colnames(y)<-c("BRC_ID", "n")

#those with a ward stay
x<-anti_join(wardstay, y)
x<-x%>%count(BRC_ID)

wardstayno<-rbind(y, x)%>%arrange(BRC_ID)

write_csv(wardstayno, "wardstayno")

#####Smoking###############################################
y<-replace.value(y, "Smoking", c("Never Smoked", "Non-Smoker (History unknown)"), "Non-smoker")
y<-replace.value(y, "Smoking", c("NA", "Not Stated (PERSONÂ asked but declined to provide a response)"), NA)

write_csv(y, "smoking_clean")

#####LSOA#########################################################
lsoa <- read_excel("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/patient characteristics/lsoa.xlsx")
lsoa%>%count(Post_Code_added_lsoa01)
lsoa%>%count(Post_Code_added_msoa01)
lsoa%>%count(Post_Code_added_lsoa11)
lsoa%>%count(Post_Code_added_msoa11)
