freq (as.numeric(curr_case$IP_Address))

n_distinct(case_fact$person_key)

casefact <-case_fact

casefact <- casefact %>% 
    group_by(dwelling_key) %>%
    mutate(participantid2=participantid[case_type=2])

qtyp1 <- subset(case_fact, case_type ==1 )

qtype2 <- subset(case_fact,case_type ==2)

qtype12 <- mutate(qtyp1, participant3=ifelse(participantid > 0,participantid,22))
require(dplyr)
joined <- left_join(qtyp1,qtype2 %>% dplyr::select(participantid),by = 'dwelling key')

joined <- merge(x= qtyp1,y= qtype2[,c("participantid")], by = "dwelling_key", all.x=T)
class(qtyp1$dwelling_key)

vars <- qtype2 %>% select(dwelling_key, participantid)
joined <- left_join(qtyp1,vars,by="dwelling_key") 




joined %>% 
    arrange(
        standid
        ,segmentid
        ,dwellingid
        ,familyid
        ,case_type
         , participantid.x
    )
case_fact <-case_fact[,-c(23)]

freq(as.numeric(sc3$participant_selected_as_SP))    

test<- data.frame(race=c(1,2,3,4)
                  ,gender=c(1,2)
                  ,selected_as_sp=c(1,2))
test2<-gather(test)
test2<-unique(test2)
test3 <-as.data.frame(unique(participant_deatails[,c("participant_selected_as_SP","participant_gender","participant_race")]))
test4 <-as.data.frame(na.rm(unique(gather(test3))))
remove(test,test2,test3,test4,participant_demographics_slicer2)                      


participant_deatails$slicer_ID <- rownames(participant_deatails)


participant_deatails$slicer_ID <- rownames(participant_deatails)

participant_demographics_slicer <- as.data.frame(
    participant_deatails[,c(
        "slicer_ID"
        ,"participant_selected_as_SP"
        ,"participant_gender"
        ,"participant_race"
        ,"participant_ethnicity"
        ,"participant_active_mil")])

participant_demographics_slicer <-melt(participant_demographics_slicer,id.vars=c('slicer_ID'))

fwrite(participant_demographics_slicer, file="L://v0.3/participant_demographics_slicer.csv")


Completed_Screening_forcast <- case_fact %>% 
    group_by(standid,date(case_fact$case_date))%>%
    summarise(number_of_complete_screenings =sum(completed_screener,na.rm=T))


Completed_Screening_forcast <- case_fact %>% 
    group_by(standid,date(case_fact$case_date))%>%
    summarise(N_aggragated_obs = n(),
        number_of_complete_screenings =sum(completed_screener,na.rm=T)) %>%
    mutate(cumsum(number_of_complete_screenings))

fwrite(Completed_Screening_forcast, file="l://v0.3/completed_screeening_forcast.csv")

#' figuring out the SP counts.   We can use the person table Ri1 which has 152,xxx people 
#' on it.  Trying not to have duplicate date in the data model for the visual.  instead 
#' use the case table 


dbWriteTable(write_con, name = "test", value = Completed_Screening_forcast, row.names = FALSE)

check695<- subset(participant_deatails, standid==695)
case_check695<- subset(case_fact, standid==695)
