################################################
#syntax info:
# Created by :Allan Uribe 
# date created Dec 1, 2020
# date last modified: Feb 17, 2021 
# last modified by: Allan Uribe 
# purpose:   crating a dynamic dashboard in PowerBI with datamanagement and 
# backend tables created in R. 
# Status: 
################################################
################################################

install.packages("easypackages")
library('easypackages')
################################################
# install packages needed to connect and run queries  to a SQL server---- 
#such as the ANL ----
install_packages('DBI', 'odbc','RODBC','sqldf','Hmisc', 'expss','summarytools','openxlsx','lubridate')
libraries('DBI', 'odbc','RODBC','sqldf','Hmisc', 'expss', 'summarytools', 'openxlsx','lubridate')
# connecting to the ANL----
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "DSPP-HANE-1601",
                 Database = "ANL1_db",
                 Trusted_Connection = "True",
                 UID = "qsj2",
                 PWD = rstudioapi::askForPassword("Database password"),
                 Port = 1433)
################################################
# pull in the ri1 -person table----
ri1_varkeep <- 
    c("")
ri1_query <- paste("SELECT", paste(ri1varkeep, sep = "",collapse= ","),
                   "FROM VN_ANL_RI1_NH_Person")
cat(ri1_query)

ri1_k <- dbGetQuery(con, ri1_query)
ri1 <- dbGetQuery(con,
    "SELECT * FROM VN_ANL_RI1_NH_Person"
    )
################################################
 # pull in the SC2 table data-----
# list of variables to keep from SC2 
sc2_DU_keep <- c("SDASTAND"
                 , "SDASEGMT"
                 , "SDASERAL"
                 , "SCAREFPT"
                 , "SCQ090"        
                 , "SCQ040"
                 , "SCQ302"
                 , "SCANLIVE"
                 , "SCQ025"
                 , "SCAISPAN"
                 , "SCAWHERE"
                 , "SCARECNT"
                 , "SCQ600"
                 , "SCQ610"
                 , "SCQ620"
                 , "SCQ630"
                 , "SCQ640")

# the SQL code must be put into its own R object in order to use the past 
#function and not have to rewrite the entire var list to keep 
##sc2 is a table with all the Dwelling units processed or not
SC2_query <- paste("SELECT", paste(sc2_DU_keep, sep = "",collapse= ","),
                   "FROM ANL_SC2_SAM_INTV_DU")
#this prints out the query above as it will be entered into the dbgetquery below 
cat(SC2_query)
# query of only the variables need of the SAMPLE DU table 
sc2_DU_k <- dbGetQuery(con, SC2_query)
# query complete copy of the SAMPLE DU table
sc2_DU <- dbGetQuery(con, "SELECT * FROM ANL_SC2_SAM_INTV_DU")
################################################
# Pull in the Sc3 table ----
#some additional demographics about the cases.
sc3_sam_part_keep <- c("SDASTAND"
                       , "SDASEGMT"
                       , "SDASERAL"
                       , "SCAPARTN"
                       , "SCAFAMNO"        
                       , "SCAPERSN"
                       , "SCQ290A"
                       , "SCQ290B"
                       , "SCQ290C"
                       , "SCQ131"
                       , "SCQ270D"
                       , "SCQ260D"
                       , "SCQ302"
                       , "SCQCK305"
                       ,"SCQ291A"
                       , "SCQ291B"
                       , "SFQ190"
                       , "SCAPRACE"
                       , "SCAMARTL")

#                 , "SCQHISP"

sc3_query <- paste("SELECT", paste(sc3_sam_part_keep, sep = "",collapse= ","),
                   "FROM VN_ANL_SC3_SAM_part")
cat(sc3_query)
sc3_sam_part_k <- dbGetQuery(con, sc3_query)
sc3_sam_part <- dbGetQuery(con, "SELECT * FROM VN_ANL_SC3_SAM_part")

################################################
# Pull in the RI7 table ----
#QUESTION : what does this table represent
ri7_sam_case1_keep <- c("SCACASE"
                        , "SDASTAND"
                        , "SDASEGMT"
                        , "SDASERAL"
                        , "STAQTYP"
                        , "SCAFAMNO"        
                        , "SCAPERSN"
                        , "STARSPNO"
                        , "STAINTUS"
                        , "STAPXUSE"
                        , "STAPSUSE"
                        , "STAINTDT"
                        , "MAQ300"
)

ri7_query <- paste("SELECT ", paste(ri7_sam_case1_keep, sep = "",collapse= ","),
                   " FROM ANL_RI7_SAM_Case")
cat(ri7_query)
ri7_sam_case_k <- dbGetQuery(con, ri7_query)

ri7_sam_case <-dbGetQuery(con, "SELECT * FROM ANL_RI7_SAM_Case")

################################################
# Pull in the Ri8 table----
RI8_Sam_Case_Detail_keep <- c("SCACASE"
                        , "STACASDT"
                        , "STAEMPID"
                        ,"STAQDISP"
                        )

ri8_query <- paste("SELECT ", paste(RI8_Sam_Case_Detail_keep, sep = "",collapse= ","),
                   " FROM V_ANL_RI8_Sam_Case_Detail")
cat(ri8_query)
RI8_Sam_Case_Detail_k <- dbGetQuery(con, ri8_query)

RI8_Sam_Case_Detail_k$date1 <- as.Date(RI8_Sam_Case_Detail_k$STACASDT)
RI8_Sam_Case_Detail_k$time <- format(as.POSIXct(RI8_Sam_Case_Detail_k$STACASDT,"%y-%m-%d-%H:%M:%S",tz=""), 
                        format= "%H:%M:%S")

################################################
# Dedup the Ri8 table SB suggests using the current case table that has the ----
#latest reccord----
# these dedups my no be accurate because i dont have the qtype varaible linked 
# to the EMPID but it is close enough for now
# FIXME need to follow up on EMP ID for the screener 
RI8_Sam_Case_Detail_k_deduped <-sqldf('SELECT   SCACASE, STAQDISP, 
                                                min(STACASDT)  as date,  
                                                min(STAEMPID) as EMPID 
                                      FROM RI8_Sam_Case_Detail_k  
                                      WHERE STAEMPID > 0
                                      Group by SCACASE')
ri8_deduped_notworked <- sqldf('SELECT  SCACASE,STAQDISP, min(STACASDT) as date, 
                                      min(STAEMPID) as EMPID 
                                FROM RI8_Sam_Case_Detail_k
                                WHERE SCACASE NOT IN 
                                (SELECT SCACASE FROM RI8_Sam_Case_Detail_k_deduped)
                                 GROUP BY SCACASE')
ri8_dedc <- rbind(ri8_deduped_notworked,RI8_Sam_Case_Detail_k_deduped)



################################################
#Pull in the current case table ----
#For now importing this just to get the disp variable.  
curr_case_keep <- c("SCACASE"
                    ,"stacasdt"
                    ,"STAQDISP")

curr_case_query <- paste("SELECT ", paste(curr_case_keep, sep = "",collapse= ","),
                         " FROM V_Current_Case")
cat(curr_case_query)
curr_case_k <- dbGetQuery(con, curr_case_query)
curr_case <- dbGetQuery(con,"select * FROM V_Current_Case")
#################################################
# joining the ri7 table to the curr_case table----
ri7 <- sqldf("SELECT A.*, B.* FROM ri7_sam_case_k A LEFT JOIN curr_case B
             ON A.SCACASE=B.case_ID ")
#################################################
# Create all the flag variables for the screener level stuff----

ri7$dwelling_key <- paste(ri7$SDASTAND
                                     ,ri7$SDASEGMT
                                     ,ri7$SDASERAL, sep = "_")

ri7$segment_key <- paste(ri7$SDASTAND
                                    ,ri7$SDASEGMT
                                    ,sep = "_")

ri7$isdu <- ifelse(ri7$STAQTYP==1,1,0)

ri7$isreleaseddu <- 
    ifelse((ri7$STAQTYP==1) 
        & (ri7$question_disp %in% c("1", 
        "2", "3", "10", "11", 
        "12", "13", "22", "30", "31", "32", "50", 
        "52", "53", "54", "55", "56","70", "71", 
        "72", "73", "74")),1,0)

ri7$isvreleaseddu <- 
    ifelse((ri7$STAQTYP==1) 
        & (ri7$question_disp %in% c( "2", 
        "3", "10", "11", 
        "12", "13", "22", "30", "50", "52",
        "53", "54", "55", "56","70", "71", 
        "72", "73", "74")),1,0)

ri7$is_screen_assigned <- 
    ifelse((ri7$STAQTYP==1) 
        & (ri7$emp_ID > 0),1,0)

'%notin%' <- Negate('%in%')

ri7$is_screen_pending <- with(ri7, 
    ifelse ((STAQTYP==1) 
        & (isreleaseddu==1)
        & (question_disp %notin% 
        c('10','11','12','13','30','31',
        '32','33','50','51',
        '52','53','54','55','56')),1, 
    ifelse((STAQTYP==1) 
        & (isreleaseddu==1)
        & (question_disp %in% 
        c('10','11','12','13','30','31',
        '32','33','50','51',
        '52','53','54','55','56')),0,NA)))

ri7$is_screen_pending_no_worked <-with(ri7, 
        ifelse((STAQTYP==1)
            & (is_screen_pending == 1)
            & (question_disp %in% c('0','1'))
            ,1,
        ifelse((STAQTYP==1)
            & (is_screen_pending == 1)
            & (question_disp %notin% 
            c('0','1')),0,NA)))


# fix This does not work without the EROC table, which i dont have access to.  
ri7$is_screen_pending_in_progress <- with(ri7, 
        ifelse((STAQTYP==1)
             & (is_screen_pending =1)
             & (question_disp %in% c('2','3'))
             ,1,
        ifelse((STAQTYP==1)
            & (is_screen_pending =1)
            & (question_disp %in% c('2','3')),0,NA)))

#'This ought to work now as long as the current case continues getting updated
#' properly
ri7$interm_refusal <- with(ri7, 
        ifelse((STAQTYP==1)
            & (is_screen_pending ==1)       
            & (question_disp %in% c('22')),1,
        ifelse ((STAQTYP==1)
                & (is_screen_pending ==1)       
                & (question_disp %notin% c('22')),0,NA)))
                
ri7$vaccantNDU5 <- 
    ifelse((ri7$STAQTYP==1)
        & (ri7$isreleaseddu == 1)
        & (ri7$question_disp %in% 
        c('30','31','32','33')),1,
    ifelse((ri7$STAQTYP==1)
        & (ri7$isreleaseddu == 1)
        & (ri7$question_disp %notin% 
        c('30','31','32','33')),0,NA))

ri7$scompleted6 <- 
    ifelse((ri7$STAQTYP==1)
        & (ri7$isreleaseddu == 1)
        & (ri7$question_disp %in% c('10','11','12','13')),1,
    ifelse((ri7$STAQTYP==1)
           & (ri7$isreleaseddu == 1)
           & (ri7$question_disp %notin% c('10','11','12','13')),0,NA))
           
ri7$scompleted6a <- 
    ifelse((ri7$STAQTYP==1)
        & (ri7$scompleted6 ==1)
        & (ri7$question_disp %in% c('10')),1,
    ifelse((ri7$STAQTYP==1)
       & (ri7$scompleted6 ==1)
       & (ri7$question_disp %notin% c('10')),0,NA))

#'QUESTION : why does the codebook say disp=12 means limited info, but in the 
#'production report it means neighbor?
ri7$scompleted6b <- 
    ifelse((ri7$STAQTYP==1)
           & (ri7$scompleted6 ==1)
           & (ri7$question_disp %in% c('12')),1,
   ifelse((ri7$STAQTYP==1)
          & (ri7$scompleted6 ==1)
          & (ri7$question_disp %notin% c('12')),0,NA))

ri7$scompleted6c <- 
    ifelse((ri7$STAQTYP==1)
        & (ri7$scompleted6 ==1)
        & (ri7$question_disp %in% c('11')),1,
    ifelse((ri7$STAQTYP==1)
       & (ri7$scompleted6 ==1)
       & (ri7$question_disp %notin% c('11')),0,NA))

ri7$scompleted6d <- 
    ifelse((ri7$STAQTYP==1)
        & (ri7$scompleted6 ==1)
        & (ri7$question_disp %in% c('13')),1,
    ifelse((ri7$STAQTYP==1)
       & (ri7$scompleted6 ==1)
       & (ri7$question_disp %notin% c('13')),0,NA))    

ri7$snonresponse7 <- 
    ifelse((ri7$STAQTYP==1)
        & (ri7$isvreleaseddu==1)
        & (ri7$question_disp %in% c('50','51','52','53','54','55','56')),1,
    ifelse((ri7$STAQTYP==1)
       & (ri7$isvreleaseddu==1)
       & (ri7$question_disp %notin% c('50','51','52','53','54','55','56')),0,NA))  
######################################################
# person stats fro teh productions report----
# without the EROC table it is hard to complete 


#ri1$SPTRGT<- #no idea how this gets created.  

ri1$SPIDED <- 
    ifelse(ri1$RIAPCODE==2
        & ri1$SCQCK305 %in% c(1,4),1,
    ifelse((ri1$RIAPCODE==2)
        & (ri1$SCQCK305 %notin% c('1','4')),0,NA))

ri7$sp_pending <- 
    with(ri7, 
     ifelse ((STAQTYP ==4) 
        & (question_disp %notin% c('14','50','51','52','53','54','55','56','70'))
        ,1,
    ifelse ((STAQTYP ==4) 
        & (question_disp %in% c('14','50','51','52','53','54','55','56','70')),0,NA)))
# fix cant calculate 11a-d from the production report with out the eroc table 
ri7$sp_pendinga <- 
    with(ri7, 
        ifelse ((STAQTYP ==4) 
            & (sp_pending=1 )
            & (question_disp %notin% c('14','50','51','52','53','54','55','56','70'))
            ,1,
        ifelse ((STAQTYP ==4) 
            & (question_disp %in% c('14','50','51','52','53','54','55','56','70')),0,NA)))

ri7 <- sqldf(
    "SELECT A.*, B.* FROM ri7 A LEFT JOIN ri1 B ON
            A.SDAstand = B.SDAstand 
        and A.SDAsegmt = B.SDAsegmt 
        and A.SDAseral = B.SDAseral 
        and A.SCAfamno = B.SCAfamno 
        and A.SCApersn = B.SCApersn" 
                            )

ri7$date <- with(ri7, as_date(STAINTDT))
ri7$time <- Hours <- format(as.POSIXct(ri7$`STAINTDT`, "%Y-%m-%d %H:%M:%S", tz = ""), format = "%H:%M:%S")

ri7$sp_Q_completed <- 
    with(ri7, 
        ifelse ((STAQTYP ==4) 
            & (SCQCK305 %in% c(1,4))
            & (question_disp %in% c('14'))
            ,1,
        ifelse ((STAQTYP ==4) 
                 & (SCQCK305 %in% c(1,4))
                 & (question_disp %notin% c('14')),0,NA)))

ri7$sp_F_nonresponse <- 
    with(ri7, 
         ifelse ((STAQTYP ==4) 
            & (SCQCK305 %in% c(1,4))
            & (question_disp %in% c('50','51','52','53','54','55','56'))
            ,1,
         ifelse ((STAQTYP ==4) 
            & (SCQCK305 %in% c(1,4))
            & (question_disp %notin% c('50','51','52','53','54','55','56')),0,NA)))

ri7$Families_ID <- 
    with(ri7, 
        ifelse (STAQTYP ==3,1,0)) 
            
ri7$Desampled_Families <- 
    with(ri7, 
         ifelse ((STAQTYP ==3)
                 & (SCQCK305 %in% c(1,4,'NA')),1,0)) 
         
ri7$ct_famlies <- with(ri7, Families_ID - Desampled_Families)
                
ri7$family_q_pending <- with(
    ri7, ifelse((STAQTYP == 3) 
                & (question_disp %notin% c('14','50','51','52','53','54','55','56','70')),1, 
                ifelse((STAQTYP == 3) 
                       & (question_disp %in% c('14','50','51','52','53','54','55','56','70'))
                       ,0,NA))
)                
            

# cant create flag from 15a-15c with out the eroc table 
# 1= line 16 family completed, 0= ;ine 17 family non-response
ri7$family_q_completed <- with(
    ri7, ifelse((STAQTYP == 3) 
                & (question_disp ==14)
                & (SCQCK305 %in% c(1,4,NA)),1,
         ifelse((STAQTYP == 3) 
                & (question_disp !=14) 
                & (SCQCK305 %in% c(1,4,NA)),0,NA)))

#18 cant be caclulated either,   missing the table from which it is created.  
# ask Stephen where the targets are drawn from 

# Pulling in nh_appointment for the last 3  datapoints in the production report----
nh_appointment <- dbGetQuery(con,"SELECT * FROM V_NH_Appointment")

ri1a <- sqldf("SELECT A.*, B.* FROM ri1 a 
              LEFT JOIN (SELECT * FROM nh_appointment where appt_type =1) B 
              ON A.SP_ID = B.SP_ID")

ri1a$appt_exam_pend <- 
    with(ri1a, 
         ifelse((appt_status == 1) 
                & (RIAPCODE ==2)
                & (SCQCK305 %in% c(1,4 )),1,0
                   ))

ri1a$broken_appt <- 
    with(ri1a, 
         ifelse((appt_status %in% c(3,4)) 
                & (RIAPCODE ==2)
                & (SCQCK305 %in% c(1,4 )),1,0
         ))

ri1a$MEC_examined <- 
    with(ri1a, 
         ifelse((appt_status %in% c(2)) 
                & (RIAPCODE ==2)
                & (SCQCK305 %in% c(1,4 )),1,0
         ))


# FIX cant calculate line 22 on the daily production with out the appt_history table.



write.csv2(ri7, file = "l:\\ri7.csv")    
write.csv2(ri1a, file="l:\\ri1a.csv")

#fixme this will only work a the end of a stand.  it does not account for cases that will change status over time.
#probably need EROC data to fix.   
ri7$interview_appointment <- ifelse((ri7$STAQTYP==1)
                                    & (ri7$question_disp %in% 
                                           c('15')),1,0)


#Joining ri8 to ri7 data.  using the ri7 as the backbone.   
# grabbing the case disposition code from the current case table and joining 
#with the RI7
# RI7 has cases, defined by the portion of the survey being completed.
# case table can be joined with du table after filtering dow using the type 
#variable to include only the screener.  

ri7_sam_case_k <- sqldf("SELECT A.*,b.STAQDISP, 
                                    b.date ,
                                    b.date1 ,
                                    b.date2 ,
                                    b.time , 
                                    B.EMPID 
                        FROM ri7_sam_case_k A
                        LEFT JOIN ri8_dedc B
                        ON a.SCACASE = b.SCACASE")



#Question Should we add a data point for the number that is incomplete 
subset1 <- ri1$SDASTAND ==398
subset1a <- ri1a$SDASTAND ==398
subset7 <- ri7$SDASTAND ==398
freq(ri7$isdu[subset7])
freq (ri7$isreleaseddu[subset7])
freq (ri7$isvreleaseddu[subset7])
freq(ri7$is_screen_pending[subset7])
freq(ri7$is_screen_pending_no_worked[subset7])
freq(ri7$is_screen_pending_in_progress[subset7])
freq(ri7$interm_refusal[subset7])

freq(ri7$vaccantNDU5[subset7],report.nas = F)
freq(ri7$scompleted6[subset7])
freq(ri7$scompleted6a[subset7])
freq(ri7$scompleted6b[subset7])
freq(ri7$scompleted6c[subset7])
freq(ri7$scompleted6d[subset7])
freq(ri7$snonresponse7[subset7])

freq(ri1$SPIDED[subset1])
freq(ri7$sp_pending[subset7])
freq(ri7$sp_Q_completed[subset7])
freq(ri7$sp_F_nonresponse[subset7])
freq(ri7$Families_ID[subset7])
freq(ri7$family_q_pending[subset7])
freq(ri7$family_q_completed[subset7])
freq(ri1a$appt_exam_pend[subset1a])
freq(ri1a$broken_appt[subset1a])
freq(ri1a$MEC_examined[subset1a])

write.csv(ri7_sam_case_k, file = "l:\\ri7_sam_case_k.csv")

write.xlsx (ri8_dedc, file = "l:\\test.xlsx")





sc2_DU_k$dwelling_key <- paste(sc2_DU_k$SDASTAND
                               ,sc2_DU_k$SDASEGMT
                               ,sc2_DU_k$SDASERAL, sep = "_")

sc2_DU_k$segment_key <- paste(sc2_DU_k$SDASTAND
                              ,sc2_DU_k$SDASEGMT
                              ,sep = "_")

sc3_sam_part_k$dwelling_key <- paste(sc3_sam_part_k$SDASTAND
                                     ,sc3_sam_part_k$SDASEGMT
                                     ,sc3_sam_part_k$SDASERAL, sep = "_")

sc3_sam_part_k$segment_key <- paste(sc3_sam_part_k$SDASTAND
                                    ,sc3_sam_part_k$SDASEGMT
                                    ,sep = "_")


sc2_DU_k = apply_labels(sc2_DU_k, 
                        SDASTAND = "MEC location ID/City"
                        , SDASEGMT = "Neighborhood within the city"
                        , SDASERAL = "Dwelling structure within the Neighborhood"
                        , SCAREFPT = "ID of the screening informant"
                        , SCQ090 = "No. of people in the houshold"
                        
                        , SCQ040 = "Dwelling biult after 4/1/1989"
                        , SCQ040 = c("YES"=1, "NO"=2, "REFUSED"=7, "DON'KNOW"=9)
                        
                        , SCQ302 = "Screening infromant pregnant"
                        , SCQ302 = c("YES"=1, "NO"=2, "REFUSED"=7, "DON'KNOW"=9)
                        
                        , SCANLIVE = "No. of people in the houshold"
                        , SCQ025 = "Dwelling is a mobile home"
                        
                        , SCAISPAN = "Completed in Spanish"
                        , SCAISPAN = c("Conducted in Spanish"=1
                                       ,"Not conducted in Spanish"= 2)
                        
                        , SCAWHERE = "UNKNOWN"
                        , SCARECNT = "No. of times the questionaire was entered"
                        
                        , SCQ600 = " Self-Reported General health of screening informant"
                        , SCQ600 = c("Excellent" = 1
                                     , "Very good" = 2
                                     , "Good" = 3
                                     , "Fair" = 4
                                     , "Poor" = 5
                                     , "REFUSED" = 7
                                     , "DON'T KNOW" = 9)
                        
                        , SCQ610 = "Self-reported medications currently taking"
                        , SCQ610 = c("YES" = 1
                                     ,"NO" = 2
                                     ,"REFUSED" = 7
                                     ,"DON'T KNOW" = 9)
                        
                        , SCQ620 = "Self-reported No. of meds screening informant"
                        , SCQ620 = c("1 to 2" = 1
                                     ,"3 to 5" = 2
                                     ,"6 or more" =3
                                     ,"REFUSED" = 7
                                     ,"DON'T KNOW" =9
                        )
                        
                        , SCQ630 = "Self-reported diabetes status"
                        , SCQ630 = c("YES"=1
                                     ,"NO"=2
                                     ,"BORDERLINE OR PREDIABETES"=3
                                     ,"REFUSED" =7
                                     ,"DON'T KNOW"=9
                        )
                        
                        , SCQ640 = "Self-reported high blood pressure"
                        , SCQ640 = c("YES"= 1
                                     ,"NO"= 2
                                     ,"REFUSED"=7 
                                     ,"DON'T KNOW"=9)
)


