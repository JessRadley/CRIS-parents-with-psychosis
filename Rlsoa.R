lsoa<-read_csv("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/patient characteristics/lsoa_wide.csv")
lsoa<-lsoa[, c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31)]

last_non_NA <- function(x) {
  tail(x[!is.na(x)], 1)
}

output<-apply(lsoa, 1, last_non_NA)

lsoa2<-cbind(lsoa, output)

#####Problem is that those without a diagnosis just had the BRC_ID reported rather than putting NA so i need to take them out and replace with BRC_ID with NA
lsoa3<-lsoa2%>%filter(str_length(output) == 13)
lsoa3<-lsoa3[, 1]
output<-rep(NA, 84)
lsoa3<-cbind(lsoa3, output)
colnames(lsoa3)<-c("BRC_ID", "output")

####and now I need to take out the ones that did report well and then rbind them together to make final database
lsoa4<-lsoa2%>%filter(str_length(output) != 13)
lsoa4<-lsoa4[, c(1, 17)]
lsoa5<-rbind(lsoa3, lsoa4)
lsoa5$BRC_ID<-as.character(lsoa5$BRC_ID)
lsoa5$output<-as.character(lsoa5$output)
lsoa5%>%arrange(BRC_ID)
lsoa<-lsoa5

colnames(lsoa)<-c("BRC_ID", "LSOA11CD")


imd<-read_csv("S:/Parentswithpsychosis - Patients with psychosis who are parents to children under/UKCRIS Docs/patient characteristics/index_of_multiple_deprivation.csv")
imd<-imd[, c(1, 2, 5)]

lsoa_imd<-left_join(lsoa, imd, by = "LSOA11CD")%>%arrange(BRC_ID)

lsoa_imd%>%filter(is.na(IMD19))%>%filter(!is.na(LSOA11CD)) 
#12 some live in Wales now - just leave them

lsoa_imd<-lsoa_imd%>%arrange(IMD19)
(5173-84-12)/9 
#564.1111

groupone<-lsoa_imd[c(1:564), ]
group<-rep(1, 564)
groupone<-cbind(groupone, group)

grouptwo<-lsoa_imd[c(565:1128), ]
group<-rep(2, 564)
grouptwo<-cbind(grouptwo, group)

groupthree<-lsoa_imd[c(1129:1692), ]
group<-rep(3, 564)
groupthree<-cbind(groupthree, group)

groupfour<-lsoa_imd[c(1693:2256), ]
group<-rep(4, 564)
groupfour<-cbind(groupfour, group)

groupfive<-lsoa_imd[c(2257:2820), ]
group<-rep(5, 564)
groupfive<-cbind(groupfive, group)

groupsix<-lsoa_imd[c(2821:3384), ]
group<-rep(6, 564)
groupsix<-cbind(groupsix, group)

groupseven<-lsoa_imd[c(3385:3948), ]
group<-rep(7, 564)
groupseven<-cbind(groupseven, group)

groupeight<-lsoa_imd[c(3949:4512), ]
group<-rep(8, 564)
groupeight<-cbind(groupeight, group)

groupnine<-lsoa_imd[c(4513:5077), ]
group<-rep(9, 565)
groupnine<-cbind(groupnine, group)

groupna<-lsoa_imd[c(5078:5173), ]
group<-rep(NA, 96)
groupna<-cbind(groupna, group)

lsoa_imd<-rbind(groupone, grouptwo, groupthree, groupfour, groupfive, groupsix, groupseven, groupeight, groupnine, groupna)

lsoa_imd<-lsoa_imd%>%arrange(BRC_ID)

write_csv(lsoa_imd, "lsoa_imd")

