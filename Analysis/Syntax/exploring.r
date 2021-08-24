freq(case_fact$case_disposition)

huh<- filter(stack.joined.qtype2, partcomplex %in%c(6,7))

freq(
    stack.joined.qtype2$partcomplex
)
unique(screener_ri7$standid)
unique(stand_demensions$standid)
freq(EROC$personid)
freq(EROC$who_contacted_participantid)
freq(EROC$interpreter_needed)
freq(EROC$survey_module)

SMS_du<-dbGetQuery(westat_con, "SELECT * FROM VN_ANL_SMS_vBrowseCaseDU")

freq(SMS_du$SBAPSUA)
colnames(screener_case_fact)

substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
}
SMS_du$duid<-substrRight(SMS_du$SBAPSUID,4)

SMS_du$duid<- str_remove(SMS_du$duid, "^0+")

# form joining at the dwelling level 
SMS_du$dwelling_key <- paste(
    SMS_du$SBAPSU
    ,SMS_du$SBASEGMT
    ,SMS_du$duid, sep = "_"
)
fwrite(SMS_du, file= "L:/2021/DHANES dashboard/V0.5/Flat files/SMS_DU.csv")

screener_case_fact$in_eroc_flag<-ifelse(screener_case_fact$dwelling_key %in% EROC$dwelling_key,1,0
)
EROC1 <-filter(EROC,EROC$question_type==1)

screener_case_fact$screener_not_worked <- ifelse(
    screener_case_fact$released_dwelling==1
     & screener_case_fact$case_type == 1
     & screener_case_fact$case_disposition %notin% c(
        '10','11','12','13','30','31','32','33','50','51','52','53','54','55','56')
     & screener_case_fact$person_key %notin% EROC$person_key[which(EROC$survey_module %in% c(26,27))], 
    1,0)
    
           
    
    #
        
        )
        & screener_case_fact$in_eroc_flag==1
       #  & screener_case_fact$person_key %notin% EROC$person_key
        )
unique(screener_case_fact$caseid[which(screener_case_fact$in_eroc_flag==1 & screener_case_fact$standid==695)])

freq(screener_case_fact$in_eroc_flag[which(screener_case_fact$standid==695)])

freq(screener_case_fact$is_sampleframe_dwelling[which(screener_case_fact$standid==695)])        
freq(screener_case_fact$released_dwelling[which(screener_case_fact$standid==695)])        

screener_case_fact$interm_refusal      
        
         
        ifelse(
            (case_type == 1)
            & (released_dwelling==1)
            & (case_disposition %in%
                   c(
                       '10','11','12','13','30','31','32','33','50','51','52','53','54','55','56'))
            & screener_case_fact$dwelling_key %in% EROC$dwelling_key
            ,0,NA)))



test <- as.data.frame(
    unique(
        screener_case_fact$caseid[which(
            screener_case_fact$interm_refusal==1 & screener_case_fact$standid==695)]))

freq(screener_case_fact$screener_not_worked[which(screener_case_fact$standid==695)])
freq(screener_case_fact$screener_in_progress[which(screener_case_fact$standid==695)])
freq(screener_case_fact$interm_refusal[which(screener_case_fact$standid==695)])
freq(screener_case_fact$screener_pending3cat[which(screener_case_fact$standid==695 )])
freq(screener_case_fact$total_pending_screener[which(screener_case_fact$standid==695 )])

problem_pending <- subset(screener_case_fact,
        screener_case_fact$total_pending_screener==1 
        & screener_case_fact$case_type==1 
        & screener_case_fact$screener_pending3cat %notin% c(1,2,3)
        & screener_case_fact$standid==695)


problem_pending$in_eroc1 <- ifelse(problem_pending$person_key %in% EROC1$person_key,1,0)

EROC1a<- subset(EROC1, EROC1$person_key %in% problem_pending$person_key)

freq(problem_pending$in_eroc1)
freq(problem_pending$case_disposition)
freq(problem_pending$released_dwelling)
freq(problem_pending$release_status)
freq(problem_pending$case_type)
freq(problem_pending$case_disposition)

freq(EROC1a$status_code)
     [which(EROC1a$survey_module==2)])
freq(EROC1a$pending)
freq(EROC1a$interm)
freq(EROC1a$survey_module)
freq(EROC1a$first)
interim <-subset(screener_case_fact,(screener_case_fact$standid==695 & screener_case_fact$screener_pending3cat==3))
