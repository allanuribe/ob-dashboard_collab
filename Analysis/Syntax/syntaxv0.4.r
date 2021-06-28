#Syntax Notes ###############################################
#syntax info:
# Created by :Allan Uribe 
# date created March 8, 2021
# date last modified: June 28, 2021
# last modified by: Allan Uribe 
#' purpose:   crating version 0.3 of  a dynamic dashboard in PowerBI with data 
#' management and back end tables created in R. 
# Server used : DSPP-HANE-1601
# DataBases used: ANL1

# tables used:----
#' 1. ANL_RI7_SAM_Case
#' 2. V_Current_Case
#' 3. VN_ANL_SC3_SAM_part

# Outcome variables: 
# Explanatory variables: 

# Functions: N/A

#Status: ----
#' the third version of this dashboard was created and presented to the group about 
#' 2 weeks ago.  The data model consists of a single fact table with all the cases needed
#' and many dimension tables.  In addition there are several tables that contain data 
#' labels.  We currently  dont have access to data more granular than a case. We are 
#' waiting on IT to get access to EROC data and appointment history data.
#' currently, the analytic tables that are feeding the dashboard are currently flat files,
#'  and need to be adjusted and written to SQL

# install & Load #########################################
# install and load package to manage all other needed packages
install.packages("easypackages")
library('easypackages')
# R packages needed in this syntax----
# only need to install packages once, comment out once its installed.  
# combak and add code to regularly check for updates to packages 
install_packages(
#     'DBI'
#     , 'odbc' 
#     ,'RODBC'
#     ,'sqldf'
#     ,'Hmisc'
#     ,'expss'
#     ,'summarytools'
#     ,'openxlsx'
#     ,'lubridate'
#     ,'RH2'
#     ,'tidyverse'
#     ,'plyr'
#     ,'reshape2'
#     ,'zoo'
    )
libraries(
    'RH2' # expands the functions of SQLDF but you have to specify the drv= option
    ,'DBI'# connect to SQL
    ,'odbc' # needed to connect to SQL 
    ,'RODBC' # needed to connect to SQL 
    ,'sqldf'# can use sql code to query R tables 
    ,'Hmisc'
    ,'expss' # easily add labels to dataframes 
    ,'summarytools' # introduces functionality similar to proc freq in sas 
    ,'openxlsx' # still trying to find the fastest way to write excel sheets. 
    # COMBAK try using the fwrite function.
    ,'lubridate' # makes working with dates much easier in R 
    , 'tidyverse'
#   , 'plyr'
    ,'reshape2'
    ,'zoo'
    )

options(scipen = 30) # dont display long numbers in R as exponents

# Connection to the ANL----
con <- dbConnect(
    odbc(),
    Driver = "SQL Server",
    Server = "DSPP-HANE-1601",
    Database = "ANL1_db",
    Trusted_Connection = "True",
    UID = "qsj2",
    PWD = rstudioapi::askForPassword("Database password"),
    Port = 1433
    )


# Pull i ri7 table from the ANL----
#'Ri7 table is going to the foundation for the fact table in the dashboard.  there is 
#'a record for each case.  a case is created for each part of the study that is completed
#'bu a person,  so a single person can have up to 6 cases.  

# list of variables to keep when we import
ri7_var_keep <- c(
#     "convert(varchar,SCACASE) as caseid"
    "SCACASE as caseid"
    , "SDASTAND as standid"
    , "SDASEGMT as segmentid"
    , "SDASERAL as dwellingid"
    , "STAQTYP as case_type"
    , "SCAFAMNO as familyid"        
    , "SCAPERSN as personid"
    , "STARSPNO as participantid"
    , "AAASESSN as mec_session_id"
    , "STAINTUS as interpreter_used"
    , "STAPXUSE as proxy_used"
    , "STAPSUSE as spanish_interview"
    , "STAINTDT as case_date"
    , "MAQ300 as case_where_completed"
)

# create R object with the SQL query
ri7_query <- paste(
    "SELECT ", paste(ri7_var_keep, sep = "", collapse= ", "),
    " FROM ANL_RI7_SAM_Case")

cat(ri7_query)
ri7 <- dbGetQuery(con, ri7_query)
 # use this SQL select statement to get all the columns with the original names 
##ri7_all <-dbGetQuery(con, "SELECT * FROM ANL_RI7_SAM_Case")

# Pull in the Current case table ---- 
#'  we need the current case table from the ANL because the case disposition is not in Ri7
#'  we also grab the case date, and emp ID 
curr_case_keep <- c(
    "case_id as caseid"
    , "case_dt case_date"
    , "question_disp case_disposition"
    ,"emp_id as employee_id"
    , "ip_address as case_ip_address")

curr_case_query <- paste(
    "SELECT ", paste(
        curr_case_keep, sep = "",collapse= ","),
    " FROM V_Current_Case")

cat(curr_case_query)
curr_case <- dbGetQuery(con, curr_case_query)

# use only if you want all columns on this table 
## curr_case <- dbGetQuery(con,"select * FROM V_Current_Case")

# Pull in the Sc3 table ----
#some additional demographics about the cases.
#' the Sc3 table is sometimes also called the person table and is needed to grab 
#' demographics. this will be treated as a dimensions table in the dashboard

# renaming variables for clarity in the dashboard.  the names is what will be deisplayed 
#' to the end users when they are personalizing visuals  
sc3_var_keep <- c(
    "SDASTAND as standid"
    , "SDASEGMT as segmentid"
    , "SDASERAL as dwellingid"
    , "SCAPARTN as participantid"
    , "SCAFAMNO as familyid"        
    , "SCAPERSN as personid"
    , "SCQ290A as participant_dob_month"
    , "SCQ290B as participant_dob_day"
    , "SCQ290C as participant_dob_year"
    , "SCQ131 as participant_gender"
    , "SCQ270D as participant_race"
    , "SCQ260D as participant_ethnicity"
    , "SCQ220 as participant_active_mil"
    , "SCQ302 as participant_pregnant"
    , "SCQCK305 as participant_selected_as_SP"
    ,"SCQ291A as participant_age_interview_years"
    , "SCQ291B as participant_age_interview_months"
    , "SFQ190 as participant_has_spouse_in_home"
)
# putting the query together in an R object so it can be refrenced in other R functions. 

sc3_query <- paste("SELECT", paste(sc3_var_keep, sep = "",collapse= ","),
                   "FROM VN_ANL_SC3_SAM_part")

cat(sc3_query)
# this actually pulls the data from the server into R 
sc3 <- dbGetQuery(con, sc3_query)
# you can use this code to pull the entire table in, rather than just select columns 
##sc3 <- dbGetQuery(con, "SELECT * FROM VN_ANL_SC3_SAM_part")
V_Stand_Dates_var_keep <- c(
    "stand_id as standid"
    , "stand_name as stand_name"
    , "Stand_Start_Dt as stand_start_date"
    , "Stand_End_Dt as stand_end_date"
    , "foactive"        
    , "mecactive"
    , "fosite"
    , "mecsite"
)
# putting the query together in an R object so it can be refrenced in other R functions. 

V_Stand_Dates_query <- paste("SELECT", paste(V_Stand_Dates_var_keep, sep = "",collapse= ","),
                   "FROM V_Stand_Dates")

cat(SAM_stand_query)
# this actually pulls the data from the server into R with columns renamed  
V_Stand_Dates <- dbGetQuery(con, V_Stand_Dates_query)

# pull all data in SAM stand in with original var names 
#V_Stand_Dates <- dbGetQuery(con, "SELECT * FROM V_Stand_Dates")

# Don't forget to disconnect from the SQL server after pulling in the data needed----
dbDisconnect(con)

# Join the case table with the current case table to get the dispositions and more 
case_fact <- sqldf(('SELECT A.*
                   , B.case_date as curr_case_date
                   , B.case_disposition
                   , B.employee_id
                   , B.case_ip_address
                   FROM ri7 as A 
                   LEFT JOIN curr_case as B
                   ON A.caseid=B.caseid'),drv="SQLite")

# deleting source tables.   no longer needed since we joined them together. 
remove(curr_case,ri7)

#format the date from current case properly 
# Question : why are there dates for sum cases in curr-case that doesnt have dates in ri7?
case_fact$curr_case_date <-as_datetime(case_fact$curr_case_date, tz="EST5EDT")


# these ID are needed bc powerbi does not allow us to link tables by more than one column----
# powerBI only accepts single variable keys

# for joinig at the segment level 
case_fact$segment_key <- paste(
    case_fact$standid
    ,case_fact$segmentid
    ,sep = "_"
)

# form joining at the dwelling level 
case_fact$dwelling_key <- paste(
    case_fact$standid
    ,case_fact$segmentid
    ,case_fact$dwellingid, sep = "_"
    )

# for joining at the family level 
case_fact$family_key <- paste(
    case_fact$standid
    ,case_fact$segmentid
    ,case_fact$dwellingid
    ,case_fact$familyid,
    sep = "_"
    )

# for joining at the person(SP) level
case_fact$person_key <- paste(
    case_fact$standid
    ,case_fact$segmentid
    ,case_fact$dwellingid
    ,case_fact$familyid
    ,case_fact$personid
    ,sep = "_"
)


# this var can be used to join to screener records. Family is left out b/c that is not 
#'identified until later in the survey.
#'for joining at the participant level.    
case_fact$participant_key <- paste(
    case_fact$standid
    ,case_fact$segmentid
    ,case_fact$dwellingid
    ,case_fact$participantid
    ,sep = "_"
)
#creating flags for various daily production reports----
# case records that are a dwelling
case_fact$is_dwelling <- ifelse(case_fact$case_type==1,1,0)

# organize columns in the order that is easier to create the various keys that we need.----
# Also trying to understand the relationship between these columns.   
# Family number is not assigned to screening or relationship case records.  
case_fact <- case_fact[,c(2,3,4,6,5,8,7,1,9:23)]

# sort the rows by (see below)
# checking some stuff to understand the relationship between these columns
case_fact<- case_fact %>% 
    arrange(
        standid
        ,segmentid
        ,dwellingid
        ,familyid
        ,case_type
         ,participantid
    )

# grab all cases that are type 1 (screener)
qtyp1 <- subset(case_fact, case_type ==1 )

# grab all cases that are type 2 (relationship)
qtype2 <- subset(case_fact,case_type ==2)
#create a new table with just the participant for type 2 to join to type 1 records
qtype2.vars <- qtype2 %>% select(dwelling_key, participantid)
joined <- left_join(qtyp1,qtype2.vars,by="dwelling_key") 

# original participantid has to be renamed xx.x and new col xx.y because that is how cols
#with the same name are treated in R during joins 
qtype2<-qtype2 %>% rename(participantid.x = participantid)
qtype2$participantid.y <- qtype2$participantid

# add the type 2 rows with teh two additional participant columns to the part 1 rows that 
#have the same two additional columsn now 
stack.joined.qtype2 <-bind_rows(joined,qtype2)

# recode the new participant ID to replace )s and NA's in part one whnere there is a 
#value in part 2
stack.joined.qtype2$participantid2 <- 
    ifelse(is.na(stack.joined.qtype2$participantid.x) & is.na(stack.joined.qtype2$participantid.y),NA,
    ifelse(stack.joined.qtype2$participantid.x > 0 & is.na(stack.joined.qtype2$participantid.y),stack.joined.qtype2$participantid.x,
    ifelse(is.na(stack.joined.qtype2$participantid.x) & stack.joined.qtype2$participantid.y > 0,stack.joined.qtype2$participantid.y,
    ifelse(stack.joined.qtype2$participantid.x == stack.joined.qtype2$participantid.y,stack.joined.qtype2$participantid.x,
    ifelse(stack.joined.qtype2$participantid.x>0 & stack.joined.qtype2$case_type ==1, stack.joined.qtype2$participantid.x,
    ifelse(stack.joined.qtype2$participantid.y>0 & stack.joined.qtype2$case_type==2, stack.joined.qtype2$participantid.y,
    ifelse(stack.joined.qtype2$participantid.x==0 & stack.joined.qtype2$participantid.y>0, stack.joined.qtype2$participantid.y,9876)))))))
# check that all combinations where accounted for 
freq(stack.joined.qtype2$participantid2)
#pull type 3 and greater cases, add the two columns and join them back together
stack.type3M <- subset(case_fact,case_type > 2)
stack.type3M<-stack.type3M %>% rename(participantid.x = participantid)
stack.type3M$participantid.y <- stack.type3M$participantid
stack.type3M$participantid2 <- stack.type3M$participantid

case_fact <-bind_rows(stack.joined.qtype2,stack.type3M)

#sort columsn to visually inspect that the assignment has worked as intended
case_fact <- case_fact[,c(1:6,24,25,7:23)]
#sort rows for visual insopection
case_fact<- case_fact %>% 
    arrange(
        standid
        ,segmentid
        ,dwellingid
        ,familyid
        ,case_type
        ,participantid.x
    )
# check that there are no anomolies
freq(case_fact$participantid2)
remove(joined,qtyp1,qtype2.vars,qtype2,stack.joined.qtype2,stack.type3M)

case_fact$released_dwelling <- 
    ifelse((case_fact$case_type==1)& (case_fact$case_disposition %in% c(
        "1", "2", "3", "10", "11", 
        "12", "13", "22", "30", "31", "32", "50", 
        "52", "53", "54", "55", "56","70", "71", 
        "72", "73", "74")),1,0)

case_fact$available_released_dwelling <- 
    ifelse((case_fact$case_type==1) & (case_fact$case_disposition %in% c(
        "2","3", "10", "11"
        ,"12", "13", "22", "30", "50", "52"
        ,"53", "54", "55", "56","70", "71"
        ,"72", "73", "74")),1,0)
# create a function that negates the %in% function in R which is not included
'%notin%' <- Negate('%in%')

# count how many case records that are a vacant or other non valid dwelling
case_fact$vaccant_and_not_dwellings <- 
    ifelse((case_fact$case_type==1)
        & (case_fact$released_dwelling == 1)
        & (case_fact$case_disposition %in% 
             c('30','31','32','33')),1,
    ifelse((case_fact$case_type==1)
        & (case_fact$released_dwelling == 1)
        & (case_fact$case_disposition %notin% 
             c('30','31','32','33')),0,NA))

# count how many case records that are a vacant 
# depends on the counbt above
case_fact$vaccant_dwellings <- 
    ifelse((case_fact$case_type==1)
        & (case_fact$released_dwelling == 1)
        & (case_fact$vaccant_and_not_dwellings==1)
        & (case_fact$case_disposition %in% 
             c('31')),1,
    ifelse((case_fact$case_type==1)
         & (case_fact$released_dwelling == 1)
         & (case_fact$vaccant_and_not_dwellings==1)
         & (case_fact$case_disposition %notin% 
             c('31')),0,NA))

# count how many case records that are other non valid dwelling
# depends on the count 2 above
case_fact$not_dwellings <- 
    ifelse((case_fact$case_type==1)
       & (case_fact$released_dwelling == 1)
       & (case_fact$vaccant_and_not_dwellings==1)
       & (case_fact$case_disposition %in% 
          c('30','32','33')),1,
    ifelse((case_fact$case_type==1)
       & (case_fact$released_dwelling == 1)
       & (case_fact$vaccant_and_not_dwellings==1)
       & (case_fact$case_disposition %notin% 
              c('30','32','33')),0,NA))               

case_fact$fi_assigned_screener <- 
    ifelse((case_fact$case_type==1)
           &(case_fact$released_dwelling==1)
           & (case_fact$employee_id > 0),1,0)

case_fact$total_pending_screener <- with(
    case_fact,
    ifelse ((case_type==1) 
            & (released_dwelling==1)
            & (case_disposition %notin% c(
                '10','11','12','13','30','31',
                '32','33','50','51',
                '52','53','54','55','56')),1, 
            ifelse((case_type==1) 
                   & (released_dwelling==1)
                   & (case_disposition %in% c(
                       '10','11','12','13','30','31',
                       '32','33','50','51',
                       '52','53','54','55','56')),0,NA)))


#fixme  This code will not properly count the cases not worked until i have access to 
#eroc Table 
case_fact$screener_not_worked <- with(
    case_fact,
    ifelse((case_type == 1)
           & (total_pending_screener == 1)
           & (case_disposition %in% c('0', '1')),1,
           ifelse((case_type == 1)
                  & (total_pending_screener == 1)
                  & (case_disposition %notin%
                         c('0', '1')),0,NA)))

case_fact$screener_in_progress <- with(
    case_fact, 
    ifelse((case_type==1)
           & (total_pending_screener =1)
           & (case_disposition %in% c('2','3')),1,
           ifelse((case_type==1)
                  & (total_pending_screener =1)
                  & (case_disposition %in% c('2','3')),0,NA)))

case_fact$interm_refusal <- with(
    case_fact, 
    ifelse((case_type==1)
           & (total_pending_screener ==1)       
           & (case_disposition %in% c('22')),1,
           ifelse ((case_type==1)
                   & (total_pending_screener ==1)       
                   & (case_disposition %notin% c('22')),0,NA)))

case_fact$screener_pending3cat <-with(
    case_fact,
    ifelse(screener_not_worked==1,1,
    ifelse(screener_in_progress==1,2,
    ifelse(interm_refusal==1,3,
    ifelse(fi_assigned_screener==1) 
    & (screener_not_worked==0)
    & (screener_in_progress==0)
    & (interm_refusal==0),4,NA
))))

case_fact$completed_screener <- with(
    case_fact, 
    ifelse((case_type==1)
           & (fi_assigned_screener == 1)
           & (case_disposition %in% c('10','11','12','13')),1,
           ifelse((case_type==1)
                  & (fi_assigned_screener == 1)
                  & (case_disposition %notin% c('10','11','12','13')),0,NA)))


case_fact$complected_screener_without_SP <- with(
    case_fact,
    ifelse(
        (case_type==1)
        & (completed_screener ==1)
        & (case_disposition %in% c('10')),1,
        ifelse(
            (case_type==1)
            & (completed_screener ==1)
            & (case_disposition %notin% c('10')),0,NA)))

#'QUESTION : why does the codebook say disp=12 means limited info, but in the 
#'production report it means neighbor?
case_fact$completed_screener_without_SP_neighbor <- with(
    case_fact,
    ifelse(
        (case_type==1)
        & ( completed_screener==1)
        & (case_disposition %in% c('12')),1,
        ifelse(
            (case_type==1)
            & (completed_screener ==1)
            & (case_disposition %notin% c('12')),0,NA)))

# the N is out of all those in a pending status
case_fact$completed_screener_with_sp <-with(
    case_fact, 
    ifelse(
        (case_type==1)
           & (completed_screener ==1)
           & (case_disposition %in% c('11')),1,
           ifelse(
               (case_type==1)
                  & (completed_screener ==1)
                  & (case_disposition %notin% c('11')),0,NA)))

# the N is out of all those in a pending status
case_fact$completed_screener_with_sp_neighbor <- with(
    case_fact, 
    ifelse(
        (case_type==1)
           & (completed_screener ==1)
           & (case_disposition %in% c('13')),1,
           ifelse(
               (case_type==1)
                  & (completed_screener ==1)
                  & (case_disposition %notin% c('13')),0,NA)))    

# a 4 cat variable of the dummies above for select visual simplicity
case_fact$completed_screener4cat <- with(
    case_fact,
    ifelse(completed_screener_without_SP_neighbor==1,1,
    ifelse(completed_screener_with_sp==1,2,
    ifelse(completed_screener_with_sp_neighbor==1,3,
    ifelse(complected_screener_without_SP==1,4,
    
    ifelse((completed_screener_without_SP_neighbor==0)
    & (completed_screener_with_sp==0)
    & (completed_screener_with_sp_neighbor==0)
    & (complected_screener_without_SP==0)
    & (fi_assigned_screener=1),5,NA))))))


#Question : why cant i just do the assigned minus the completes
case_fact$screener_nonresponse <-with(
    case_fact, 
    ifelse((case_type==1)
           & (released_dwelling==1)
           & (case_disposition %in% c('50','51','52','53','54','55','56')),1,
           ifelse((case_type==1)
                  & (released_dwelling==1)
                  & (case_disposition %notin% c('50','51','52','53','54','55','56')),0,NA)))


# i remove the ineligible dwellings from the denominator category of this variable.  
case_fact$screener_nonresponse_available_dwellings <-with(
    case_fact,  
    ifelse((case_type==1)
           & (available_released_dwelling==1)
           & (case_disposition %in% c('50','51','52','53','54','55','56')),1,
           ifelse((case_type==1)
                  & (available_released_dwelling==1)
                  & (case_disposition %notin% c('50','51','52','53','54','55','56')),0,NA)))

# this var can be used to join to screener records. Family is left out b/c that is not 
#identified until later in the survey.  
case_fact$participant_key2 <- paste(
    case_fact$standid
    ,case_fact$segmentid
    ,case_fact$dwellingid
    ,case_fact$participantid2
    ,case_fact$case_type
    ,sep = "_"
)
# creating the same key in the sc3 table
sc3$participant_key2 <- paste(
    sc3$standid
    ,sc3$segmentid
    ,sc3$dwellingid
    ,sc3$participantid
    ,"1"
    ,sep = "_"
)
# just giving this table a meaningful name 
participant_deatails <- sc3

remove(sc3)



# creating a stand dimensions table
# Dedup list of stands 
stand_demensions <- as.data.frame(unique((case_fact$standid)))
# just addingf a pretend stand that would be active based on my logic below.
stand_extra <-data.frame(standid=500)
# renaming standID to join with case fact table
stand_demensions<-stand_demensions %>% dplyr::rename(standid = 'unique((case_fact$standid))')
#adding fake stand 500 to test the logic 
stand_demensions <-rbind(stand_demensions,stand_extra)
#setting the threshhold for ineligable stands flag 
stand_demensions$vacannt_threshhold <- .2
# active not active stand logic 
stand_demensions$active_stand <- with(stand_demensions,
    ifelse(
        (standid>420)
        &(standid<800),1,2))

# label table for active not active stands 
Active_stand_label <-data.frame(
    active_stand_status=c(1,2),
    active_stand_label=c("Active","Inactive")
)

# creating an ID column needed for slicer join 
participant_deatails$slicer_ID <- rownames(participant_deatails)

# keeping the variables that will be in the slicer
participant_demographics_slicer <- as.data.frame(
    participant_deatails[,c(
        "slicer_ID"
        ,"participant_selected_as_SP"
        ,"participant_gender"
        ,"participant_race"
        ,"participant_ethnicity"
        ,"participant_active_mil")])

# melt the columns to rows
participant_demographics_slicer <-melt(
    participant_demographics_slicer,id.vars=c('slicer_ID'))

# creating a label column with a label for each variable and value
participant_demographics_slicer$valuelabel <- with(
    participant_demographics_slicer,
    ifelse(
        (variable =="participant_selected_as_SP") 
        & (value==2 ),"Not selected as SP",
        ifelse(
            (variable =="participant_selected_as_SP") 
            & (value==1 ),"Selected as SP",
                ifelse(
                (variable =="participant_selected_as_SP") 
                & (value==0 ),"Inconclusive SP",
                ifelse(
                    (variable =="participant_selected_as_SP") 
                    & (value==3 ),"Originally selected, no longer selected",
                    ifelse(
                        (variable =="participant_selected_as_SP") 
                        & (value==4 ),"Appointment made, not yet uploaded",
        ifelse(
        (variable=="participant_gender")
        & (value== 1),"Male",
        ifelse(
            (variable=="participant_gender")
            & (value== 2),"Female",
            ifelse(
                (variable=="participant_gender")
                & (value== 7),"Refused",
                ifelse(
                    (variable=="participant_gender")
                    & (value== 9),"Don't know",
            ifelse(
            (variable=="participant_race")
            & (value== 1),"White",
            ifelse(
                (variable=="participant_race")
                & (value== 2),"Black",
                ifelse(
                    (variable=="participant_race")
                    & (value== 3),"Other",
                    ifelse(
                        (variable=="participant_race")
                        & (value== 4),"Asian",
                        ifelse(
                            (variable=="participant_race")
                            & (value== 7),"Refused",
                        ifelse(
                            (variable=="participant_race")
                            & (value== 9),"Don't know",
    ifelse(
        (variable=="participant_ethnicity")
        & (value== 2),"Latino",
        ifelse(
            (variable=="participant_ethnicity")
            & (value== 4),"Not Latino",
            ifelse(
                (variable=="participant_ethnicity")
                & (value== 7),"Refused",
                ifelse(
                    (variable=="participant_ethnicity")
                    & (value== 9),"Don't know",
    ifelse(
        (variable=="participant_active_mil")
        & (value== 1),"Active military",
        ifelse(
            (variable=="participant_active_mil")
            & (value== 2),"Not active military",
            ifelse(
                (variable=="participant_active_mil")
                & (value== 7),"Refused",
                ifelse(
                    (variable=="participant_active_mil")
                    & (value== 9),"Don't know",
                    ifelse(
                        is.na(value),#fixme : double check this logic 
                           "Contact the dashboard manager there is  missing data",
                           "Contact the dashboard manager there is a recode error"
                           )))))))))))))))))))))))))

# fixme: need to double check that this is working as intended.  crowd source the potential errors.   
participant_demographics_slicer$valuelabel[is.na(participant_demographics_slicer$valuelabel)] <-"Contact the dashboard manager there is  missing data"

freq(participant_demographics_slicer$valuelabel)

# creating gender label table
gender_labels<-data.frame(value=c(1,2,7,9)
                          ,description=c(
                              "1. Male"
                              ,"2. Female"
                              ,"3. Refused"
                              ,"4. Don't Know"))
# create the race label table 
race_labels<-data.frame(value=c(1,2,3,4,7,9)
                          ,description=c(
                              "1. White"
                              ,"2. Black"
                              , "3. Other"
                              , "4. Asian"
                              ,"5. Refused"
                              ,"6. Don't Know"))

# create the screening pending label catagory
screener_pending3cat_labels <- data.frame(
    value=c(1,2,3,4,NA)
    ,screener_pending3cat_description=c(
        "Screener Not Worked"
        ,"Screener in Progress"
        , "interm Screening refusal"
        , "contact dashboard manager about recoding error"
        , "contact dashboard manager about missing data"))

# create the completed screening label catagory
completed_screener4cat <- data.frame(
    value=c(1,2,3,4,5,NA)
    ,completed_screener4cat_description=c(
        "completed screener without SP neighbor"
        ,"completed screener with sp"
        , "completed screener with sp neighbor"
        , "completed screener without SP"
        , "contact dashboard manager about recoding error"
        , "contact dashboard manager about missing data"))

#Various  tables and transformations needed needed to populate the visio 
# getting the count of released dwellings by stand.   
visio_released_dwelling <- case_fact %>%
    group_by(standid) %>% 
    summarise_at(
        vars(
            released_dwelling),
        list(count=sum))
# assigning a ID based on the steps in the screening process
visio_released_dwelling$visioid <-3

visio_fi_assigned_screening <- case_fact %>%
    group_by(standid) %>% 
    summarise_at(
        vars(
            fi_assigned_screener),
        list(count=sum))

visio_fi_assigned_screening$visioid <-4

#fix this is returning all NA and should be total of 714 
visio_total_pending_screener <- case_fact %>%
    group_by(standid) %>% 
        summarise(
            count=sum(total_pending_screener,na.rm=T))

visio_total_pending_screener$visioid <-5

visio_empty_dwellings <- case_fact %>%
    group_by(standid) %>% 
    summarise(
        count= sum(vaccant_and_not_dwellings,na.rm = T))
        
visio_empty_dwellings$visioid <-6

visio_completed_screener <- case_fact %>%
    group_by(standid) %>% 
    summarise(
        count= sum(completed_screener,na.rm = T))

visio_completed_screener$visioid <-7

visio_screener_nonresponse <- case_fact %>%
    group_by(standid) %>% 
    summarise(
        count= sum(screener_nonresponse,na.rm = T))

visio_screener_nonresponse$visioid <-8

visio_screener_process <- rbind(
    visio_released_dwelling
    ,visio_fi_assigned_screening
    ,visio_total_pending_screener
    ,visio_empty_dwellings
    ,visio_completed_screener
    ,visio_screener_nonresponse)

remove(visio_released_dwelling
       ,visio_fi_assigned_screening
       ,visio_total_pending_screener
       ,visio_empty_dwellings
       ,visio_completed_screener
       ,visio_screener_nonresponse)


visio_screener_process_inbetween <- data.frame(
    screenerstage=c(1:9)
)


# adding a rolling average calculation for the number of completed screenings 
case_fact <- case_fact %>% 
    arrange(desc(standid,curr_case_date))%>%
    group_by(standid)%>%
    mutate(completed_roll_avg_3D= rollmean(completed_screener,k=3, fill=NA),
           completed_roll_avg_5D= rollmean(completed_screener,k=5, fill=NA),
           completed_roll_avg_7D= rollmean(completed_screener,k=7, fill=NA),
           completed_roll_avg_9D= rollmean(completed_screener,k=9, fill=NA))%>%
    ungroup()

V_Stand_Dates$stand_cycle <- with(
    V_Stand_Dates,
    ifelse(year(stand_start_date) %in% c(1999,2000),"99-00",
    ifelse(year(stand_start_date) %in% c(2001,2002),"01-02",
    ifelse(year(stand_start_date) %in% c(2003,2004),"03-04",
    ifelse(year(stand_start_date) %in% c(2005,2006),"05-06",
    ifelse(year(stand_start_date) %in% c(2007,2008),"07-08",
    ifelse(year(stand_start_date) %in% c(2009,2010),"09-10",
    ifelse(year(stand_start_date) %in% c(2011,2012),"11-12",
    ifelse(year(stand_start_date) %in% c(2013,2014),"13-14",       
    ifelse(year(stand_start_date) %in% c(2015,2016),"15-16",       
    ifelse(year(stand_start_date) %in% c(2017,2018),"17-18",
    ifelse(year(stand_start_date) %in% c(2019,2020),"19-20",
    ifelse(year(stand_start_date) %in% c(2021,2022),"21-22",9999
           )))))))))))))

freq(V_Stand_Dates$stand_cycle)

#I have been trying to figure out how to subset when running some other procedure.
#finally got it 

attach(case_fact)

subset1 <- case_fact$standid ==398
freq(case_fact$released_dwelling[subset1])
freq(case_fact$available_released_dwelling[subset1])
freq(case_fact$vaccant_and_not_dwellings[subset1])
freq(case_fact$vaccant_dwellings[subset1])
freq(case_fact$not_dwellings[subset1])
freq(case_fact$fi_assigned_screener[subset1])
freq(case_fact$total_pending_screener[subset1])
freq(case_fact$screener_not_worked[subset1])
freq(case_fact$completed_screener[subset1])
freq(case_fact$completed_screener_without_SP[subset1])

freq(case_fact$completed_screener_with_sp_neighbor[subset1])
freq(case_fact$completed_screener_with_sp[subset1])

freq(case_fact$completed_screener_without_SP_neighbor[subset1])
freq(case_fact$screener_nonresponse[subset1])
freq(case_fact$screener_nonresponse_available_dwellings[subset1])
freq(case_factv04$completed_screener4cat[subset1])



freq(case_fact$released_dwelling[which(case_fact$standid ==398)])
freq(case_fact$available_released_dwelling[which(case_fact$standid ==398)])

# fast writing CSV to locker for import to powerBI
fwrite(case_fact, file= "L:/2021/DHANES dashboard/V0.4/case_factv0.4.csv")

fwrite(participant_deatails, file="L:/2021/DHANES dashboard/V0.4/participant_detailsv0.4.csv")

fwrite(stand_demensions,file = 'L:/2021/DHANES dashboard/V0.4/stand_demensions.csv')
fwrite(participant_demographics_slicer, 
       file = "L://v0.3/participant_demographics_slicer.csv")
fwrite(gender_labels,file = "L://v0.3/gender_labels.csv")
fwrite(race_labels,file = "L://v0.3/race_labels.csv")
fwrite(Active_stand_label, file = "L:/2021/DHANES dashboard/v0.4/active_stand_label.csv" )
fwrite(screener_pending3cat_labels, file = "L://v0.3/screener_pending3cat_labels.csv")
fwrite(completed_screener4cat, file = "L://v0.3/completed_screener4cat.csv")
fwrite(participant_demographics_slicer, file="L://v0.3/participant_demographics_slicer.csv")
fwrite(visio_screener_process, file="L://v0.3/visio_screener_process.csv")
fwrite(visio_screener_process_inbetween, file="L://v0.3/visio_screener_process_inbetween.csv")
fwrite(V_Stand_Dates, file = "L:/2021/DHANES dashboard/V0.4/V_Stand_Dates.csv" )





