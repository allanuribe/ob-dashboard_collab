#Syntax Notes ###############################################
#syntax info:
# Created by :Allan Uribe 
# date created March 8, 2021
# date last modified: July 10, 2021
# last modified by: Allan Uribe 
#' purpose:   crating version 0.5 of  a dynamic dashboard in PowerBI with data 
#' management and back end tables created in R. 
# Server used : DSPP-HANE-1601; 158.111.202.203
# DataBases used: ANL1, HANES_DBD_DEV; MSDELIVR_db

# tables used:----
#' 1. ANL_RI7_SAM_Case
#' 2. V_Current_Case
#' 3. VN_ANL_SC3_SAM_part

# Outcome variables: 
# Explanatory variables: 

# Functions: N/A

#Status: ----
#' the 4th  version of this dashboard was created and presented to the group July 2021.
#' The data model consists of a single fact table with all the cases needed
#' and many dimension tables.  In addition there are several tables that contain data 
#' labels.  We currently  dont have access to data more granular than a case. We are 
#' waiting on IT to get access to EROC data and appointment history data.
#' currently, the analytic tables that are feeding the dashboard are currently flat files,
#'  and need to be adjusted and written to SQL

# INSTALL AND LOAD R PACKAGES ----
# install and load package to manage all other needed packages
install.packages("easypackages")
library('easypackages')
# R packages needed in this syntax
# only need to install packages once, comment out once its installed.  
# combak and add code to regularly check for updates to packages 
# install_packages(
#         'DBI'
#         ,'odbc'
#         ,'RODBC'
#         ,'sqldf'
#         ,'Hmisc'
#         ,'expss'
#         ,'summarytools'
#         ,'openxlsx'
#         ,'lubridate'
#         ,'RH2'
#         ,'tidyverse'
#         ,'plyr'
#         ,'reshape2'
#         ,'zoo'
# )

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
# R OPTIONS TO SET FOR THE SESSION----
options(scipen = 30) # dont display long numbers in R as exponents

# CONNECTIONS TO THE DATASEVERS USED IN THIS PROGRAM----
# Connection to the ANL
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

# Connection to the SandBox space on the DHANES server
write_con <- dbConnect(
    odbc(),
    Driver = "SQL Server",
    Server = "DSPP-HANE-1601",
    Database = "HANES_DBD_DEV",
    Trusted_Connection = "True",
    UID = "qsj2",
    PWD = rstudioapi::askForPassword("Database password"),
    Port = 1433
)
# Connection to the Westat server
westat_con <- dbConnect(
    odbc(),
    Driver = "ODBC Driver 17 for SQL Server",
    Server = "158.111.202.203",
    Database = "MSDELIVR_db",
    UID = "NCHSDASH",
    PWD = rstudioapi::askForPassword("Database password")
)

westat_con <- dbConnect(
  odbc(),
  Driver = "ODBC Driver 17 for SQL Server",
  Server = "nhqprod3",
  Database = "MSDELIVR_db",
  UID = "NCHSDASH",
  PWD = rstudioapi::askForPassword("Database password")
)

# FUNCTIONS USED IN THIS CODE----
# create a function that negates the %in% function in R which is not included
#notin----
'%notin%' <- Negate('%in%')

# function to grab characters to the right
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
# PULL IN TABLES FROM WESTAT----
# Pull in RI7 ----
#'There is a record for each case. A case is created for each part of the study that is 
#'completed. A single person can have up to 6 cases. The screener is completed by a single
#'person at the house hold level.  the screener and relationship do not need to be 
#'completed by the same person.  After that the records are specific to a participant.  

# list of variables to keep when we import
ri7_var_keep <- c(
    #     "convert(varchar,SCACASE) as caseid"
    "SCACASE as caseid"
    , "convert (varchar,SDASTAND) as standid_c"
    , "SDASTAND as standid"
    , "convert (varchar,SDASEGMT) as segmentid_c"
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
    , "RIQ004 as interview_mode"
    , "RIAPAPMD as paper_mode"
    , "RIAPHNMD as Phone_mode"
)

# create R object with the SQL query
screener_ri7_query <- paste(
    "SELECT ", paste(ri7_var_keep, sep = "", collapse= ", "),
    " FROM ANL_RI7_SAM_Case
      WHERE STAQTYP in(1,2)")# selecting only the screener and relationship cases d

# see what the sql query looks like 
cat(screener_ri7_query)

screener_ri7 <- dbGetQuery(westat_con, screener_ri7_query)
# use this SQL select statement to get all the columns with the original names 
##ri7_all <-dbGetQuery(con, "SELECT * FROM ANL_RI7_SAM_Case")

# Pull in Current case ---- 
#'  we need the current case table from the ANL because the case disposition is not in Ri7
#'  we also grab the case date, and emp ID 
curr_case_keep <- c(
    "case_id as caseid"
    , "case_dt case_date_cc"
    , "question_disp case_disposition"
    ,"emp_id as employee_id"
    , "ip_address as case_ip_address"
    , "interview_mode as interview_mode_cc"
    , "Paper_mode as paper_mode_cc"
    , "Phone_mode as Phone_mode_cc"
    , "Mode_text as mode_text"
    )
screener_curr_case_query <- paste(
    "SELECT ", paste(
        curr_case_keep, sep = "",collapse= ","),
    "FROM V_Current_Case WHERE case_id IN (SELECT SCACASE FROM ANL_RI7_SAM_Case WHERE STAQTYP in (1,2))")

cat(screener_curr_case_query)
screener_curr_case <- dbGetQuery(westat_con, screener_curr_case_query)

# Pull in DU release status ----
du_released_status_keep <- c("*"
  )
du_released_status_query <- paste(
  "SELECT ", paste(
    du_released_status_keep, sep = "",collapse= ","),
  "FROM v_anl_du_released_status")

cat(du_released_status_query)
du_released_status <- dbGetQuery(con, du_released_status_query) #COMbak  this table is not on the westat server

#combak Consider changing var names in the sql statement  
du_released_status<-du_released_status %>% rename(standid = SDASTAND)
du_released_status<-du_released_status %>% rename(segmentid = SDASEGMT)
du_released_status<-du_released_status %>% rename(dwellingid = SDASERAL)
du_released_status<-du_released_status %>% rename(release_status = SCADURST)

# use only if you want all columns on this table 
## curr_case <- dbGetQuery(con,"select * FROM V_Current_Case")

# Pull in the SC3 table ----
# SC3 lists all the members of the DU
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
  , "SCQ291A as participant_age_interview_years"
  , "SCQ291B as participant_age_interview_months"
  , "SFQ190 as participant_has_spouse_in_home"
  , "SCAPRACE as informant_race"
  , "SFQ050 as related_to_others_in_house"
  , "SCAMARTL as marital_status_informant"
 )

# putting the query together in an R object so it can be referenced in other R functions. 

sc3_query <- paste("SELECT", paste(sc3_var_keep, sep = "",collapse= ","),
                   "FROM VN_ANL_SC3_SAM_part")

cat(sc3_query)
# this actually pulls the data from the server into R 
sc3 <- dbGetQuery(westat_con, sc3_query)

# Pull in stand dates----
v_stand_dates_var_keep <- c(
  "stand_id as standid"
  , "stand_name as stand_name"
  , "Stand_Start_Dt as stand_start_date"
  , "Stand_End_Dt as stand_end_date"
  , "foactive"        
  , "mecactive"
  , "fosite"
  , "mecsite"
)
# putting the query together in an R object so it can be referenced in other R functions. 

v_stand_dates_query <- paste("SELECT", paste(v_stand_dates_var_keep, sep = "",collapse= ","),
                             "FROM V_Stand_Dates")

cat(v_stand_dates_query)
# this actually pulls the data from the server into R with columns renamed  
v_stand_dates <- dbGetQuery(westat_con, v_stand_dates_query)

# Pull in web mail---- 
#' combak does this include the returned mail?

Web_Mailing_keep <- c(
  "SDASTAND as standid"
  , "SDASEGMT as segmentid"
  , "SDASERAL as dwellingid"
  , "RIAMALSP as mailing_type"
  , "RIAMALS as mailing_status"        
  , "RIAMALWP as web_passcode"
  , "RIAMALEM as employee_id"
)

Web_Mailing_query <- paste(
  "SELECT", paste(
    Web_Mailing_keep, sep = "",collapse= ","),
  "FROM V_ANL_NH_Web_Mailing")

cat(Web_Mailing_query)

V_ANL_NH_Web_Mailing <- dbGetQuery(westat_con, Web_Mailing_query)


# Pull in SMS DU----
#' bring in records from the SMS DU view.  the view currently does not go 
#' back to stand 101. starts at stand 350 something. 

SMS_DU_keep <- c(
    "SBADSUID as DU_ID"
  , "SBAPSUID as DU_ID2"
  , "SBAINTNM as FI_name"
  , "SBASUCST as SP_intvw_disp"
  , "SBASTCD as SP_intvw_disp_desc"
  , "SBACMPDT as Last_activity_date"
  , "SBARIADT as region_inactive_date"
  , "SBACOHYN as Case_on_hold"
  , "SBAEMPID as interviewer_ID"
  , "RIASEADD as res_dev_sec_add"
  , "RIAPCITY as city"
  , "RIAPST as state"
  , "RIAPZIP as zipcode"
  , "SBAQTYP as question_type"
  , "SBAREGN as FO_team_ID"
  , "SBAREGNA as FO_team_IDA"
  , "SBAPSU as standid"
  , "SBAPSUA as standida"
  , "SBASEGMT as segmentid"
  , "SBAACTIV as active_case"
  , "SBARDACT as round_active"
  , "SBASUPID as employeeid"
  , "SBAFMID as Familyid"
  , "SBAQDISP as final_SP_disp"
  , "SBALABEL as Stand_name"
  , "SBADDOFW as appt_date_day_of_week"
  , "SBACTRLC as case_control"
  , "SBACTLAN as contact_language"
  , "SBALANG as language_description"
  , "SBAEVRRF as ever_refused"
  , "SBAUNASD as unassigned"
  , "SBAFLDBT as field_bucket"
  , "SBAPRLBL as priority_label"
  , "SBASTDYR as stand_yr"
  , "SBAEVRAP as ever_appointed"
  , "SBAAGDYS as aging_days"
  , "SBANUMCT as number_of_contacts"
  , "SBAERCST as EROC_status_code"
  , "SBAERCCD as EROC_status_date"
  , "SBASTDEC as EROC_status_description"
  , "SBAIPFNR as in_progress"
  , "SBALTRDT as letter_date"
  , "SBARFSEV as refusal_severity"
  , "SBAOBS as DU_observation"
  , "SBAOBSST as DU_observation_status"
  , "SBAPRIVL as priority_value"
  , "SBAPRCMT as priority_comment"
  , "SBAPHNUM as phone_number"
  , "SBAOTHPH as other_phone_number"
  , "SBAACISS as ever_access_issue"
  , "SBABARIR as access_barrier"
  , "SBAPRPAD as screen_incentive_prepaid"
  , "SBAPRMSD as screen_incentive_promised"
  , "SBAACCPT as incentive_accepted"
  , "SBARFRSN as EROC_Refusal_Description"
  , "SBAERCMT as EROC_comment"
  , "SBARFSAD as refusal_said"
  , "SBACNHLP as refusal_conversion_help"
  , "SBALLN as Listing_line_number"
  , "SBASLTEL"
  , "SBASMPEL"
  , "SBASMPCL"
  , "SBAERRF2 as second_refusal_reason"
  , "SBAERRF3 as third_refusal_reason"
  , "SBASPCNT as SP_count"
  , "SBASUCMT as SU_comments"
)

SMS_DU_query<- paste(
  "SELECT", paste(
    SMS_DU_keep, sep="", collapse = ","),
  "FROM VN_ANL_SMS_vBrowseCaseDU"
)
 
SMS_du<-dbGetQuery(westat_con, SMS_DU_query)

#Pull in web screener----
Web_screener_keep <- c(
  "SDASTAND as standid"
  ,"SDASEGMT as segmentid"
  ,"SDASERAL as dwellingid"
  ,"RIASCNDT as screener_date"
  ,"RIASCNRS as screener_status"
  ,"RIAMALSP as Mail_step"
  ,"RIALUPDT as last_date_updated"
  
)
Web_screener_query<- paste(
  "SELECT", paste(
    Web_screener_keep, sep="", collapse = ","),
  "FROM V_ANL_NH_Web_Screener"
)

web_screener <- dbGetQuery(westat_con, Web_screener_query)





# Pull in SC2 ----
# this table lists all the DUs 
SC2_keep <- c(
  "SDASTAND as standid"
  ,"SDASEGMT as segmentid"
  ,"SDASERAL dwellingid"
  ,"SCQ090 num_of_peeps_HH"
  ,"SCAREFPT as participantid"
  ,"SCQ070 as correct_address"
  ,"SCQ040 as biult_after_1989"
  ,"SCANLIVE as enumerated_persons_screener"
  ,"SCQ025 as is_mobile_home"
  ,"SCANEIGH as neigh_info_used"
  ,"SCQ195 as other_home"
  ,"SCAISPAN as spanish_interview"
  ,"SCQCK030 as check_missed_DU"
  ,"SCAWHERE as where_completed"
  ,"SCARECNT as times_screener_entered"
  ,"SCAINCL as low_income_DU"
  ,"SCAINCLA as low_income_threshhold"
  ,"SCQ250 as HH_income" 
  ,"SCQ560 as missed_DU_in_area"
  ,"SCQ600 as general_health"
  ,"SCQ610 as take_meds"
  ,"SCQ620 as num_meds"
  ,"SCQ630 as diabetes"
  ,"SCQ640 as hypertension"
    )

SC2_query<- paste(
  "SELECT", paste(
    SC2_keep, sep="", collapse = ","),
  "FROM V_ANL_SC2_SAM_Intv_DU"
)

SC2 <- dbGetQuery(westat_con, SC2_query)

# Pull in EROC----
EROC_keep <- c(
  "ERAHHCT as household_contact_times"
  ,"ERACTNO as family_contact_times"
  ,"ERASPCT as sp_contact_times"
  ,"SDASTAND as standid"
  ,"SDASEGMT as segmentid"
  , "SDASERAL as dwellingid"
  ,"SCAFAMNO as familyid"
  ,"SCAPERSN as personid"
  , "SP_ID"
  , "ERACTDT as contact_date"
  , "ERAQTYPE as question_type"
  , "ERAAPPNM as application_name"
  , "ERACTMOD as survey_module"
  , "ERACTENT as contact_event"
  , "ERACTDES as contact_description"
  , "ERASTAT as status_code"
  , "ERAREFUS as refusal_code"
  , "STAEMPID as employeeid"
  , "ERACTOUT as contact_outcome"
  , "ERACOINI as who_initiated_contact"
  , "ERAWHOCO as who_contacted"
  , "ERAWHOOT as who_contacted_other"
  , "ERAWHOPN as who_contacted_participantid"
  , "ERACOMOD as contact_mode"
  , "ERAATGRP as activity_group"
  , "ERALNGCD as contact_language"
  , "ERACTLAN as contact_language_desc"
  , "ERAREFGD as refusal_gender"
  , "ERAREFAG as refusal_age_range"
  , "ERAREFRN as refusal_reason"
  , "ERAREFSV as refusal_severity"
  , "ERAREFRC as refusal_race"
  , "ERAREFOT as refusal_race_other"
  , "ERAAPTDT as appointment_date"
  , "ERAINTND as interpreter_needed"
  , "ERAREDND as reader_needed"
)
EROC_query<- paste(
  "SELECT", paste(
    EROC_keep, sep="", collapse = ","),
  "FROM VN_ANL_EROC_Contacts WHERE ERACTMOD in (1,2,26,27)"
)
cat(EROC_query)
EROC<- dbGetQuery(westat_con,EROC_query)

#DISCONNECT FROM THE SQL SERVERS----
dbDisconnect(con)
dbDisconnect(write_con)
dbDisconnect(westat_con)


# CREATE ALL THE KEYS NEEDED ON EACH TABLE ----
# screener case fact keys ----

# for joining at the segment level
screener_ri7$segment_key <- paste(
  screener_ri7$standid
  ,screener_ri7$segmentid
  ,sep = "_"
)

# form joining at the dwelling level 
screener_ri7$dwelling_key <- paste(
  screener_ri7$standid
  ,screener_ri7$segmentid
  ,screener_ri7$dwellingid, sep = "_"
)

# this var can be used to join to screener records. Family is left out b/c that is not 
#'identified until later in the survey.
#'for joining at the participant level.    
screener_ri7$participant_key <- paste(
  screener_ri7$standid
  ,screener_ri7$segmentid
  ,screener_ri7$dwellingid
  ,screener_ri7$participantid
  ,sep = "_"
)

screener_ri7$person_key <- paste(
  screener_ri7$standid
  ,screener_ri7$segmentid
  ,screener_ri7$dwellingid
  ,screener_ri7$familyid
  ,screener_ri7$personid
  ,sep = "_"
)

# current case keys----
# none needed,  current case is joined to ri7 with the caseid

#released dwelling keys----
# Creating join vars at various levels in the stat_DU
# for joining at the segment level
du_released_status$segment_key <- paste(
  du_released_status$standid
  ,du_released_status$segmentid
  ,sep = "_"
)

# form joining at the dwelling level 
du_released_status$dwelling_key <- paste(
  du_released_status$standid
  ,du_released_status$segmentid
  ,du_released_status$dwellingid, sep = "_"
)


#SC3 key creation ----
# creating the same key in the sc3 table
sc3$participant_key2 <- paste(
  sc3$standid
  ,sc3$segmentid
  ,sc3$dwellingid
  ,sc3$participantid
  ,"1"
  ,sep = "_"
)

# stand dates key creation----
#no key needed.

#web mailing key creation----
# form joining at the dwelling level 
V_ANL_NH_Web_Mailing$dwelling_key <- paste(
  V_ANL_NH_Web_Mailing$standid
  ,V_ANL_NH_Web_Mailing$segmentid
  ,V_ANL_NH_Web_Mailing$dwellingid, sep = "_"
)



#SMS DU key creation----

SMS_du$duid<-substrRight(SMS_du$DU_ID,4)

SMS_du$duid<- str_remove(SMS_du$duid, "^0+")

# form joining at the dwelling level 
SMS_du$dwelling_key <- paste(
  SMS_du$standid
  ,SMS_du$segmentid
  ,SMS_du$duid, sep = "_"
)

# web screener key creation ----
web_screener$dwelling_key <- paste(
  web_screener$standid
  ,web_screener$segmentid
  ,web_screener$dwellingid, sep = "_"
)


# sc2 key creation----

SC2$dwelling_key <- paste(
  SC2$standid
  ,SC2$segmentid
  ,SC2$dwellingid, sep = "_"
)

#EROC table key creation----
# for joining at the segment level
EROC$segment_key <- paste(
  EROC$standid
  ,EROC$segmentid
  ,sep = "_"
)

# form joining at the dwelling level 
EROC$dwelling_key <- paste(
  EROC$standid
  ,EROC$segmentid
  ,EROC$dwellingid, sep = "_"
)

# this var can be used to join to screener records. Family is left out b/c that is not 
#'identified until later in the survey.
#'for joining at the participant level.    
EROC$participant_key <- paste(
  EROC$standid
  ,EROC$segmentid
  ,EROC$dwellingid
  ,EROC$who_contacted_participantid
  ,sep = "_"
)

EROC$person_key <- paste(
  EROC$standid
  ,EROC$segmentid
  ,EROC$dwellingid
  ,EROC$familyid
  ,EROC$personid
  ,sep = "_"
)




#Join RI7 to current case----
# Join the case table with the current case table to get the dispositions and more
screener_case_fact <- sqldf(('SELECT A.*
                   , B.case_date_cc
                   , B.case_disposition
                   , B.employee_id
                   , B.case_ip_address
                   , B.interview_mode_cc
                   , B.paper_mode_cc
                   , B.Phone_mode_cc
                   , B.mode_text
                   FROM screener_ri7 as A 
                   LEFT JOIN screener_curr_case as B
                   ON A.caseid=B.caseid'),drv="SQLite")

#Logic to join informant info to case record----
# grab all cases that are type 1 (screener)
qtype1 <- subset(screener_case_fact, case_type ==1 )

# grab all cases that are type 2 (relationship)
qtype2 <- subset(screener_case_fact,case_type ==2)

#create a new table with just the needed variables for for type 2 cases to join to type 1
qtype2.vars <- qtype2 %>% select(dwelling_key, participantid,participant_key)

# contains type 1 cases with 2 additional columns, the participantid and participantkey
# from the type 2 cases.   
joined <- left_join(qtype1,qtype2.vars,by="dwelling_key")

#' to stack the type 2 cases to the joined table we need the same columns exactly.  
#' Below i create 2 new columns and rename one to match joined 
qtype2<-qtype2 %>% rename(participantid.x = participantid)
qtype2<-qtype2 %>% rename(participant_key.x = participant_key)
qtype2$participantid.y <- qtype2$participantid
qtype2$participant_key.y <- qtype2$participant_key

# add the type 2 rows with teh two additional participant columns to the part 1 rows that 
#have the same two additional columsn now 
stack.joined.qtype2 <-bind_rows(joined,qtype2)

# use this var to check my logic for assigning a partnumber to the type one records to 
# faciliate matching to a person.  
stack.joined.qtype2$partcomplex <- 
  ifelse(is.na(stack.joined.qtype2$participantid.x) & is.na(stack.joined.qtype2$participantid.y),9999,
         
  ifelse(stack.joined.qtype2$participantid.x == 0 & is.na(stack.joined.qtype2$participantid.y),1, 
  
  ifelse(is.na(stack.joined.qtype2$participantid.x) & stack.joined.qtype2$participantid.y == 0,2,
  
  ifelse(stack.joined.qtype2$participantid.x > 0 & is.na(stack.joined.qtype2$participantid.y),3,
  ifelse(is.na(stack.joined.qtype2$participantid.x) & stack.joined.qtype2$participantid.y > 0,4,
  
  ifelse((stack.joined.qtype2$participantid.x >0 & stack.joined.qtype2$participantid.y >0) & (stack.joined.qtype2$participantid.x == stack.joined.qtype2$participantid.y),5,
  
  ifelse((stack.joined.qtype2$participantid.x >0 & stack.joined.qtype2$participantid.y >0 & stack.joined.qtype2$participantid.x != stack.joined.qtype2$participantid.y) & stack.joined.qtype2$case_type==1,6,         
  ifelse((stack.joined.qtype2$participantid.x >0 & stack.joined.qtype2$participantid.y >0 & stack.joined.qtype2$participantid.x != stack.joined.qtype2$participantid.y) & stack.joined.qtype2$case_type==2,7,
 
  ifelse(stack.joined.qtype2$participantid.x==0 & stack.joined.qtype2$participantid.y ==0, 8,
  
  ifelse(stack.joined.qtype2$participantid.x == 0 & stack.joined.qtype2$participantid.y >0,9,       
  ifelse(stack.joined.qtype2$participantid.x >0 & stack.joined.qtype2$participantid.y == 0,10,9876
  )))))))))))
# recode the new participant ID to replace )s and NA's in part one whnere there is a 
#value in part 2
stack.joined.qtype2$participantid2 <- 
  ifelse(is.na(stack.joined.qtype2$participantid.x) & is.na(stack.joined.qtype2$participantid.y),stack.joined.qtype2$participantid.y,
         
  ifelse(stack.joined.qtype2$participantid.x == 0 & is.na(stack.joined.qtype2$participantid.y),stack.joined.qtype2$participantid.x, 
  ifelse(is.na(stack.joined.qtype2$participantid.x) & stack.joined.qtype2$participantid.y == 0,stack.joined.qtype2$participantid.y,
                       
  ifelse(stack.joined.qtype2$participantid.x > 0 & is.na(stack.joined.qtype2$participantid.y),stack.joined.qtype2$participantid.x,
  ifelse(is.na(stack.joined.qtype2$participantid.x) & stack.joined.qtype2$participantid.y > 0,stack.joined.qtype2$participantid.y,
                                     
  ifelse((stack.joined.qtype2$participantid.x >0 & stack.joined.qtype2$participantid.y >0) & (stack.joined.qtype2$participantid.x == stack.joined.qtype2$participantid.y),stack.joined.qtype2$participantid.x,
                                            
  ifelse((stack.joined.qtype2$participantid.x >0 & stack.joined.qtype2$participantid.y >0 & stack.joined.qtype2$participantid.x != stack.joined.qtype2$participantid.y) & stack.joined.qtype2$case_type==1,stack.joined.qtype2$participantid.x,         
  ifelse((stack.joined.qtype2$participantid.x >0 & stack.joined.qtype2$participantid.y >0 & stack.joined.qtype2$participantid.x != stack.joined.qtype2$participantid.y) & stack.joined.qtype2$case_type==2,stack.joined.qtype2$participantid.y,
                                                        
  ifelse(stack.joined.qtype2$participantid.x==0 & stack.joined.qtype2$participantid.y ==0,stack.joined.qtype2$participantid.x,
                                                               
  ifelse(stack.joined.qtype2$participantid.x == 0 & stack.joined.qtype2$participantid.y >0,stack.joined.qtype2$participantid.y,       
  ifelse(stack.joined.qtype2$participantid.x >0 & stack.joined.qtype2$participantid.y == 0,stack.joined.qtype2$participantid.x,9876
  )))))))))))



screener_case_fact<-stack.joined.qtype2

remove(joined,qtype1,qtype2.vars,qtype2,stack.joined.qtype2,screener_curr_case,screener_ri7)

# Screener case fact Participant key2  
# this var can be used to join to screener records. Family is left out b/c that is not 
# identified until later in the survey. This is the key to use after collapsing the 
# screener and relationship to ID the actual human that did the screener.  when the 
# screener and the relatipnship are done at the same time, the informant is IDd on the 
# Relationship,  not the screener.   
screener_case_fact$participant_key2 <- paste(
  screener_case_fact$standid
  ,screener_case_fact$segmentid
  ,screener_case_fact$dwellingid
  ,screener_case_fact$participantid2
  ,screener_case_fact$case_type
  ,sep = "_"
)

screener_case_fact <- sqldf(('SELECT A.*
                   , B.release_status
                   FROM screener_case_fact as A 
                   LEFT JOIN du_released_status as B
                   ON A.dwelling_key=B.dwelling_key'),drv="SQLite")

remove(du_released_status)


 #CREATING FLAG VARIABLES ON TABLES THAT WILL BE IMPORTATED TO POWERBI----
 #Flags Screener case fact----
# case records that are a dwelling
#QUESTION : can we account for returned mail or other processes that indicate it was an invalid dwelling?
screener_case_fact$is_sampleframe_dwelling <- ifelse(screener_case_fact$case_type==1,1,0)

screener_case_fact$released_dwelling <- 
  ifelse((screener_case_fact$release_status =="1" & screener_case_fact$case_type==1 
         # &
           # (screener_case_fact$case_disposition %in% c(
   # "0", "1", "2", "3", "10", "11", 
    # "12", "13", "22", "30", "31", "32", "50", 
    # "52", "53", "54", "55", "56","70", "71", 
    # "72", "73", "74")
   ),1,0)
#count dwellings that are no empty or vacant.
screener_case_fact$available_released_dwelling <- 
  ifelse((screener_case_fact$case_type==1) & (screener_case_fact$case_disposition %in% c(
    "2","3", "10", "11"
    ,"12", "13", "22", "30", "50", "52"
    ,"53", "54", "55", "56","70", "71"
    ,"72", "73", "74")),1,0)
# count how many case records that are a vacant or other non valid dwelling
screener_case_fact$vaccant_and_not_dwellings <- 
  ifelse((screener_case_fact$case_type==1)
         & (screener_case_fact$released_dwelling == 1)
         & (screener_case_fact$case_disposition %in% 
              c('30','31','32','33')),1,
         ifelse((screener_case_fact$case_type==1)
                & (screener_case_fact$released_dwelling == 1)
                & (screener_case_fact$case_disposition %notin% 
                     c('30','31','32','33')),0,NA))

# count how many case records that are a vacant 
# depends on the counbt above
screener_case_fact$vaccant_dwellings <- 
  ifelse((screener_case_fact$case_type==1)
         & (screener_case_fact$released_dwelling == 1)
         & (screener_case_fact$vaccant_and_not_dwellings==1)
         & (screener_case_fact$case_disposition %in% 
              c('31')),1,
         ifelse((screener_case_fact$case_type==1)
                & (screener_case_fact$released_dwelling == 1)
                & (screener_case_fact$vaccant_and_not_dwellings==1)
                & (screener_case_fact$case_disposition %notin% 
                     c('31')),0,NA))

# count how many case records that are other non valid dwelling
# depends on the count 2 above
screener_case_fact$not_dwellings <- 
  ifelse((screener_case_fact$case_type==1)
         & (screener_case_fact$released_dwelling == 1)
         & (screener_case_fact$vaccant_and_not_dwellings==1)
         & (screener_case_fact$case_disposition %in% 
              c('30','32','33')),1,
         ifelse((screener_case_fact$case_type==1)
                & (screener_case_fact$released_dwelling == 1)
                & (screener_case_fact$vaccant_and_not_dwellings==1)
                & (screener_case_fact$case_disposition %notin% 
                     c('30','32','33')),0,NA))  
# case has been assigned to a field interviewer
screener_case_fact$fi_assigned_screener <- 
  ifelse((screener_case_fact$case_type==1)
         &(screener_case_fact$released_dwelling==1)
         & (screener_case_fact$employee_id > 0),1,0)

# total number of cases in a pending status
screener_case_fact$total_pending_screener <- with(
  screener_case_fact,
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

# combak : westat code uses the du release status. I am using the assigned screener 
# col that I created.  Check this once I have access to teh correct table.  
screener_case_fact$completed_screener <- with(
  screener_case_fact, 
  ifelse((case_type==1)
         & (fi_assigned_screener == 1)
         & (case_disposition %in% c('10','11','12','13')),1,
         ifelse((case_type==1)
                & (fi_assigned_screener == 1)
                & (case_disposition %notin% c('10','11','12','13')),0,NA)))

#' combak  all of these are dependent on the completed screener var created above.
#' check once access to the correct table is granted
screener_case_fact$complected_screener_without_SP <- with(
  screener_case_fact,
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
screener_case_fact$completed_screener_without_SP_neighbor <- with(
  screener_case_fact,
  ifelse(
    (case_type==1)
    & ( completed_screener==1)
    & (case_disposition %in% c('12')),1,
    ifelse(
      (case_type==1)
      & (completed_screener ==1)
      & (case_disposition %notin% c('12')),0,NA)))

# the N is out of all those in a pending status
screener_case_fact$completed_screener_with_sp <-with(
  screener_case_fact, 
  ifelse(
    (case_type==1)
    & (completed_screener ==1)
    & (case_disposition %in% c('11')),1,
    ifelse(
      (case_type==1)
      & (completed_screener ==1)
      & (case_disposition %notin% c('11')),0,NA)))

# the N is out of all those in a pending status
screener_case_fact$completed_screener_with_sp_neighbor <- with(
  screener_case_fact, 
  ifelse(
    (case_type==1)
    & (completed_screener ==1)
    & (case_disposition %in% c('13')),1,
    ifelse(
      (case_type==1)
      & (completed_screener ==1)
      & (case_disposition %notin% c('13')),0,NA)))    

# a 4 cat variable of the dummies above for select visual simplicity
screener_case_fact$completed_screener4cat <- with(
  screener_case_fact,
  ifelse(completed_screener_without_SP_neighbor==1,1,
         ifelse(completed_screener_with_sp==1,2,
                ifelse(completed_screener_with_sp_neighbor==1,3,
                       ifelse(complected_screener_without_SP==1,4,
                              
                              ifelse((completed_screener_without_SP_neighbor==0)
                                     & (completed_screener_with_sp==0)
                                     & (completed_screener_with_sp_neighbor==0)
                                     & (complected_screener_without_SP==0)
                                     & (fi_assigned_screener=1),5,NA))))))

screener_case_fact$screener_nonresponse <-with(
  screener_case_fact, 
  ifelse((case_type==1)
         & (released_dwelling==1)
         & (case_disposition %in% c('50','51','52','53','54','55','56')),1,
         ifelse((case_type==1)
                & (released_dwelling==1)
                & (case_disposition %notin% c('50','51','52','53','54','55','56')),0,NA)))

#fixme  This code will not properly count the cases not worked until i have access to 
#eroc Table 

screener_case_fact$screener_not_worked <- ifelse(
  screener_case_fact$released_dwelling==1
  & screener_case_fact$case_type == 1
  & screener_case_fact$case_disposition %notin% c(
    '10','11','12','13','30','31','32','33','50','51','52','53','54','55','56')
  & screener_case_fact$person_key %notin% EROC$person_key[which(EROC$survey_module %in% c(26,27))], 
  1,0)


# EROC flags needed for SCreener Case fact----
EROC$row_id <-seq.int(nrow(EROC))
EROC1 <-EROC[which(EROC$survey_module %in% c(26,27)),] %>% group_by(person_key) %>% slice_max(contact_date)
EROC$first <- ifelse(EROC$row_id %in% EROC1$row_id,1,0)
remove(EROC1)


EROC$pending <- ifelse((EROC$survey_module %in% c(26,27)
                       & EROC$first==1
                       & EROC$status_code %notin% c(22,71)) 
                       ,1,0)

EROC$interm <- ifelse(EROC$survey_module %in% c(26,27)
                      & EROC$first==1 
                      & EROC$status_code %in% c(22,71),1,0)

#flags screener case fact dependent on the EROC FLAGS----

screener_case_fact$screener_in_progress <- ifelse(
  screener_case_fact$released_dwelling==1
  & screener_case_fact$case_type == 1
  & screener_case_fact$case_disposition %in% c(
    0,1,2,3,99,20,21,22,23,24,25,26)
  & screener_case_fact$person_key %in% EROC$person_key[which(EROC$pending==1)],
  1,0)


screener_case_fact$interm_refusal<- ifelse(
  screener_case_fact$released_dwelling==1
  & screener_case_fact$case_type == 1
  & screener_case_fact$case_disposition %notin% c(
    10,11,12,13,30,31,32,33,50,51,52,53,55)
  & screener_case_fact$person_key %in% EROC$person_key[which(EROC$interm==1)], 
  1,0)



screener_case_fact$screener_pending3cat <-with(
  screener_case_fact,
    ifelse(screener_not_worked==1,1,
           ifelse(screener_in_progress==1,2,
                  ifelse(interm_refusal==1,3,
                         ifelse((screener_not_worked ==0 
                                & screener_in_progress==0 
                                & interm_refusal== 0
                                & completed_screener == 1
                                & case_type==1),NA,5)))))
  


# count how many case records that are a vacant or other non valid dwelling
screener_case_fact$vaccant_and_not_dwellings <- 
  ifelse((screener_case_fact$case_type==1)
         & (screener_case_fact$released_dwelling == 1)
         & (screener_case_fact$case_disposition %in% 
              c('30','31','32','33')),1,
         ifelse((screener_case_fact$case_type==1)
                & (screener_case_fact$released_dwelling == 1)
                & (screener_case_fact$case_disposition %notin% 
                     c('30','31','32','33')),0,NA))

# count how many case records that are a vacant 
# depends on the counbt above
screener_case_fact$vaccant_dwellings <- 
  ifelse((screener_case_fact$case_type==1)
         & (screener_case_fact$released_dwelling == 1)
         & (screener_case_fact$vaccant_and_not_dwellings==1)
         & (screener_case_fact$case_disposition %in% 
              c('31')),1,
         ifelse((screener_case_fact$case_type==1)
                & (screener_case_fact$released_dwelling == 1)
                & (screener_case_fact$vaccant_and_not_dwellings==1)
                & (screener_case_fact$case_disposition %notin% 
                     c('31')),0,NA))

# count how many case records that are other non valid dwelling
# depends on the count 2 above
screener_case_fact$not_dwellings <- 
  ifelse((screener_case_fact$case_type==1)
         & (screener_case_fact$released_dwelling == 1)
         & (screener_case_fact$vaccant_and_not_dwellings==1)
         & (screener_case_fact$case_disposition %in% 
              c('30','32','33')),1,
         ifelse((screener_case_fact$case_type==1)
                & (screener_case_fact$released_dwelling == 1)
                & (screener_case_fact$vaccant_and_not_dwellings==1)
                & (screener_case_fact$case_disposition %notin% 
                     c('30','32','33')),0,NA))


# adding a rolling average calculation for the number of completed screenings 
#combak check if the release status column gets duplicated again tomorrow.  the rolling avg 
# failed to run until i deleted the duplicate release status column (row 37 ) when you print colnames 
screener_case_fact <- screener_case_fact %>% 
  arrange(desc(standid,case_date_cc))%>%
  group_by(standid)%>%
  mutate(completed_roll_avg_3D= rollmean(completed_screener,k=3, fill=NA),
         completed_roll_avg_5D= rollmean(completed_screener,k=5, fill=NA),
         completed_roll_avg_7D= rollmean(completed_screener,k=7, fill=NA),
         completed_roll_avg_9D= rollmean(completed_screener,k=9, fill=NA))%>%
  ungroup()



screener_case_fact$complete_relationship <- with(
  screener_case_fact,ifelse(
    case_type==2 
    & case_disposition %in% c('14','15'),1,
  ifelse(case_type==2 
         & case_disposition %notin% c('14','15'),0,NA
  )
  )
)
# Flags Web mailings

V_ANL_NH_Web_Mailing$advance_letter <- ifelse(
  (V_ANL_NH_Web_Mailing$mailing_type== 1 & 
     V_ANL_NH_Web_Mailing$mailing_status==2),1,0
  
)

V_ANL_NH_Web_Mailing$advance_Postcard <- ifelse(
  (V_ANL_NH_Web_Mailing$mailing_type== 2 & 
     V_ANL_NH_Web_Mailing$mailing_status==2),1,0
  
)

V_ANL_NH_Web_Mailing$xpress_delivery <- ifelse(
  (V_ANL_NH_Web_Mailing$mailing_type== 3 & 
     V_ANL_NH_Web_Mailing$mailing_status==2),1,0
  
)


V_ANL_NH_Web_Mailing$final_reminder <- ifelse(
  (V_ANL_NH_Web_Mailing$mailing_type== 4 & 
     V_ANL_NH_Web_Mailing$mailing_status==2),1,0
  
)

V_ANL_NH_Web_Mailing$supplemental <- ifelse(
  (V_ANL_NH_Web_Mailing$mailing_type== 5 & 
     V_ANL_NH_Web_Mailing$mailing_status==2),1,0
  
)

V_ANL_NH_Web_Mailing$mailing4cat <- with( 
  V_ANL_NH_Web_Mailing,
  ifelse((advance_letter==1 & 
            advance_Postcard==0 &
            xpress_delivery==0 &
            final_reminder==0 &
            supplemental==0),1,
         ifelse((advance_letter==0 & 
                   advance_Postcard==1 &
                   xpress_delivery==0 &
                   final_reminder==0 &
                   supplemental==0),2,
                ifelse((advance_letter==0 & 
                          advance_Postcard==0 &
                          xpress_delivery==1 &
                          final_reminder==0 &
                          supplemental==0),3,
                       ifelse((advance_letter==0 & 
                                 advance_Postcard==0 &
                                 xpress_delivery==0 &
                                 final_reminder==1 &
                                 supplemental==0),4,
                              ifelse((advance_letter==0 & 
                                        advance_Postcard==0 &
                                        xpress_delivery==0 &
                                        final_reminder==0 &
                                        supplemental==1),5,
                                     ifelse(mailing_status==1,6, 999
                                     )))))))



#CREATING OUTPUT TABLES ----
# Renaming SC3 and SC2----
# just giving this table a meaningful name 
informant_DU_details <- sc3
informant_details<-SC2

remove(sc3,SC2)

#Create dwelling demensions tables----
dwelling_demensions<- as_tibble(unique(screener_case_fact$dwelling_key))
dwelling_demensions <- dwelling_demensions%>% rename(dwelling_key=value)


# creating a stand dimensions table----
# Dedup list of stands 

stand_demensions <- as.data.frame(unique((screener_case_fact$standid)))
# renaming standID to join with case fact table
stand_demensions<-stand_demensions %>% dplyr::rename(standid = 'unique((screener_case_fact$standid))')
#setting the threshhold for ineligable stands flag 
stand_demensions$vacannt_threshhold <- .2
# active not active stand logic 
stand_demensions$active_stand <- with(stand_demensions,
                                      ifelse(
                                        (standid>420)
                                        &(standid<800),1,2))
stand_demensions1<- full_join(v_stand_dates,stand_demensions,by = "standid")

stand_demensions1$stand_cycle <- with(
  stand_demensions1,
  ifelse(year(stand_start_date) %in% c(1999,2000),"1999-2000",
  ifelse(year(stand_start_date) %in% c(2001,2002),"2001-2002",
  ifelse(year(stand_start_date) %in% c(2003,2004),"2003-2004",
  ifelse(year(stand_start_date) %in% c(2005,2006),"2005-2006",
  ifelse(year(stand_start_date) %in% c(2007,2008),"2007-2008",
  ifelse(year(stand_start_date) %in% c(2009,2010),"2009-2010",
  ifelse(year(stand_start_date) %in% c(2011,2012),"2011-2012",
  ifelse(year(stand_start_date) %in% c(2013,2014),"2013-2014",       
  ifelse(year(stand_start_date) %in% c(2015,2016),"2015-2016",       
  ifelse(year(stand_start_date) %in% c(2017,2018),"2017-2018",
  ifelse(year(stand_start_date) %in% c(2019,2020),"2019-2020",
  ifelse(year(stand_start_date) %in% c(2021,2022),"2021-2022",9999
  )))))))))))))

stand_demensions1$standid_c<-stand_demensions1$standid
remove(stand_demensions)

# CREATE VISUAL TABLES ( tables that reshape data just for a parrticular visual)----
# create data for forcasting visual----
#creating a count of completed screenings.  Originally created for forecasting models.
Completed_Screening_forcast <- screener_case_fact %>% 
  group_by(standid,date(screener_case_fact$case_date))%>%
  summarise(number_of_complete_screenings =sum(completed_screener,na.rm=T))


Completed_Screening_forcast <- screener_case_fact %>% 
  group_by(standid,date(screener_case_fact$case_date))%>%
  summarise(N_aggragated_obs = n(),
            number_of_complete_screenings =sum(completed_screener,na.rm=T)) %>%
  mutate(cumsum(number_of_complete_screenings))


#create visio tables----
#Various  tables and transformations needed needed to populate the visio 
# getting the count of released dwellings by stand.   
visio_released_dwelling <- screener_case_fact %>%
  group_by(standid) %>% 
  summarise_at(
    vars(
      released_dwelling),
    list(count=sum))
# assigning a ID based on the steps in the screening process
visio_released_dwelling$visioid <-3

visio_fi_assigned_screening <- screener_case_fact %>%
  group_by(standid) %>% 
  summarise_at(
    vars(
      fi_assigned_screener),
    list(count=sum))

visio_fi_assigned_screening$visioid <-4

#fix this is returning all NA and should be total of 714 
visio_total_pending_screener <- screener_case_fact %>%
  group_by(standid) %>% 
  summarise(
    count=sum(total_pending_screener,na.rm=T))

visio_total_pending_screener$visioid <-5

visio_empty_dwellings <- screener_case_fact %>%
  group_by(standid) %>% 
  summarise(
    count= sum(vaccant_and_not_dwellings,na.rm = T))

visio_empty_dwellings$visioid <-6

visio_completed_screener <- screener_case_fact %>%
  group_by(standid) %>% 
  summarise(
    count= sum(completed_screener,na.rm = T))

visio_completed_screener$visioid <-7

visio_screener_nonresponse <- screener_case_fact %>%
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

# create dynamic slicer tables----
# creating an ID column needed for slicer join 
informant_DU_details$slicer_ID <- rownames(informant_DU_details)

# keeping the variables that will be in the slicer
participant_demographics_slicer <- as.data.frame(
  informant_DU_details[,c(
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




#navigation data table----
# Create a navigation table for the dropdown navigation of the dashboard.  
dropdown_navigation <-data.frame("page_names" = c(
  "Screening Operations"
  , "Screener Informant & Dwelling DrillThough"
  , "Screener Refusal DrillThrough"
  , "Screener Study Participant DrillThrough"
  , "Information Page"
  , "Custom questions"
)
)



# CREATE LABEL TABLES 
# label table for active not active stands 
labels_Active_stand <-data.frame(
  active_stand_status=c(1,2),
  active_stand_label=c("Active","Inactive"))

# create the completed screening label catagory
labels_completed_screener4cat <- data.frame(
  value=c(1,2,3,4,5,NA)
  ,completed_screener4cat_description=c(
    " W/O SP neighbor"
    ," W/ sp"
    , "W/ sp neighbor"
    , "W/o SP"
    , "Contact dashboard manager about recoding error"
    , "Contact dashboard manager about missing data"))

labels_mailing_type <- data.frame(
  value=c(1,2,3,4,5)
 ,mailing_type=c(
    '1. Advance letter'
    ,'2. Postcard'
    ,'3. Xpress mail'
    ,'4. Final letter'
    ,'5. Supplemental'
  ))

labels_mailing_type2 <- data.frame(
  value=c(1,2,3,4,5)
  ,mailing_type=c(
    '1. Advance letter'
    ,'2. Postcard'
    ,'3. Xpress mail'
    ,'4. Final letter'
    ,'5. Supplemental'
  ))


labels_participant_ethnicity <- data.frame(
  value=c(1,2,3,4,7,9)
  ,ethnicity_description=c(
    '1. Not in codebook'
    ,'2. Latino'
    ,'3. Not Latino'
    ,'4. not in codebook'
    ,'7. Refused'
    ,"9. Don't Know" 
  ))

labels_Interview_type <- data.frame(
  value=c(1,2,3,4)
  ,int_type_description=c(
    '1. In-Person'
    ,'2. Phone'
    ,'3. Paper'
    ,'4. Web'
  ))

# creating gender label table
labels_gender<-data.frame(value=c(1,2,7,9)
                          ,gender_description=c(
                            "1. Male"
                            ,"2. Female"
                            ,"3. Refused"
                            ,"4. Don't Know"))
# create the race label table 
labels_race<-data.frame(value=c(1,2,3,4,7,9)
                        ,race_description=c(
                          "1. White"
                          ,"2. Black"
                          , "3. Other"
                          , "4. Asian"
                          ,"5. Refused"
                          ,"6. Don't Know"))

# create the screening pending label catagory


labels_screener_pending3cat <- data.frame(
  value=c(1,2,3,4,NA)
  ,screener_pending3cat_description=c(
    "Not Worked"
    ,"In Progress"
    , "Interim refusal"
    , "Completed"
    , "contact dashboard manager about missing data"))

labels_spanish_interview <- data.frame(
  value=c(1,2)
  ,spanish_interview_description=c(
    'Spanish'
    ,"Language other than Spanish"
  ))
labels_case_where_completed <- data.frame(
  value=c(1,2,3,4,5)
  ,case_where_completed_description=c(
    'Home'
    ,'NHANES Office'
    ,'Work'
    ,'Library'
    ,'Other'
  )
)

labels_severity <- data.frame(
  value=c(1,2,3,4)
  ,Severity_desc=c(
    '1. Mild'
    ,'2. Determined'
    ,'3. Firm'
    ,'4. Adamant'
    
  )
)

labels_case_disposition <- data.frame(
  value= c(
    0,1,2,3,10,11,12,13,14,15,22,30,31,32,35,36,50,52,53,54,55,56,65,66,67,68,70,71,72,73
    ,74,75,76,77,78,79,80,81,82,99),
  case_disposition = c(
    "Not Worked"	 
    ,"Not Worked Reassigned"	 
    ,"Partially Worked Reassigned"	 
    ,"Partially Worked"	 
    ,"Complete, No SPs ( final )"	 
    ,"Complete, with SPs ( final )"	 
    ,"Complete, Limited Info: No SPs"	 
    ,"Complete, Limited Info: SPs"	 
    ,"Complete ( final )"	 
    ,"Scheduled ( final )"	 
    ,"Refusal ( reassign )"	 
    ,"Out of Scope ( final )"	 
    ,"Vacant ( final )"	 
    ,"Not a DU ( final )"	 
    ,"Broken Appointment - No Show ( reassign )"	 
    ,"Broken Appointment - Cancel ( reassign )"	 
    ,"Illness ( final )"	 
    ,"Refusal ( final )"	 
    ,"Not at home after multiple attempts ( final )"	 
    ,"Unavailable during field period ( final )"	 
    ,"DU access/entry problem ( final )"	 
    ,"Other Specify ( final )"	 
    ,"Broken Appointment - No Show ( final )"	 
    ,"Broken Appointment - Cancel ( final )"	 
    ,"Refused Because of Consent ( final )"	 
    ,"Dust Incomplete (final)"	 
    ,"Complete, Eligibility Problems ( final )"	 
    ,"Complete, Problems ( final )"	 
    ,"Not Confirmed, Address/Phone Number Problem ( final )"	 
    ,"Not Confirmed, Falsification ( final )"	 
    ,"Complete, No Problems"	 
    ,"R does not recall contact"	 
    ,"R recalls contact/address problem"	 
    ,"R recalls contact/incorrect demographics info"	 
    ,"R recalls contact/not sureabout questions/incorrect quex inf"	 
    ,"Incomplete, not at home/no answer"	 
    ,"Incomplete, not at home/not available"	 
    ,"Incomplete, not at home/RF/breakoff"	 
    ,"Incomplete, not at home/other nonresponse"	 
    ,"Other (reassign)"  
  )
)

labels_survey_module <- data.frame(
  value= c(
    1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27),
  case_disposition = c(
"Screener"	 
,"Relationship Questionnaire"	 
,"Family Questionnaire"	 
,"SP Questionnaire"	 
,"MEC Appointment Scheduling"	 
,"Consent"	 
,"Water Collection"	 
,"24 Hour Urine"	 
,"Letter Managment"	 
,"Text Messaging"	 
,"MEC Exam"	 
,"HMHS"	 
,"TB"	 
,"HUC"	 
,"Dietary"	 
,"Hep C"	 
,"STD/HIV"	 
,"FFQ"	 
,"PAM"	 
,"PSA"	 
,"Allergy"	 
,"ER"	 
,"FCBS"	 
,"ROF"	 
,"Validation"	 
,"iROC"	 
,"tROC"
))











# dwelling_demensions_case_fact<-data.frame(unique(screener_case_fact$dwelling_key))
# dwelling_demensions_case_fact<-dwelling_demensions_case_fact %>% rename(
# dwelling_key = "unique.screener_case_fact.dwelling_key.") 
# 
# dwelling_demensions1 <-unique(rbind(dwelling_demensions,dwelling_demensions_case_fact))
# 
# screener_case_fact_extra <- data.frame(rep(c(NA),times= NROW(dwelling_demensions$dwelling_key))
#                                        ,c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA)
#                                        ,c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA)
#                                        ,c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA)
#                                        ,c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA)
#                                        ,c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA)
#                                        ,c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA)
#                                        ,c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA),c(NA)
#                                        ,c(NA),c(NA)
# )
# names(screener_case_fact_extra)<- c(
#   "caseid", "standid_c", "standid",
#   "segmentid_c",                            "segmentid",                              "dwellingid",                            
#   "case_type"  ,                            "familyid"            ,                   "personid" ,                             
#   "participantid.x",                        "mec_session_id"      ,                   "interpreter_used" ,                     
#   "proxy_used",                             "spanish_interview"    ,                  "case_date"         ,                    
#   "case_where_completed",                   "interview_mode"                        , "paper_mode"         ,                   
#   "Phone_mode"         ,                    "case_date_cc"                         ,  "case_disposition"    ,                  
#   "employee_id"         ,                   "case_ip_address"                     ,   "interview_mode_cc"    ,                 
#   "paper_mode_cc"        ,                  "Phone_mode_cc"                      ,    "mode_text"             ,                
#   "segment_key"           ,                 "dwelling_key"                      ,     "participant_key.x"      ,               
#   "participantid.y"        ,                "participant_key.y"                ,      "partcomplex"             ,              
#   "participantid2"           ,              "DU_state"                         ,      "DU_released_status"    ,                
#   "DU_fasting_requirement"   ,              "DU_deselection_factor"          ,        "DU_subsample_factor"   ,                
#   "stand_name"                ,             "FIPSst"                        ,         "FIPSarea"               ,               
#   "segment_density_strata"     ,            "TRACTBNA"                     ,          "is_sampleframe_dwelling" ,              
#   "released_dwelling"           ,           "available_released_dwelling" ,           "vaccant_and_not_dwellings",             
#   "vaccant_dwellings"            ,          "not_dwellings"              ,            "fi_assigned_screener"      ,            
#   "total_pending_screener"        ,         "completed_screener"        ,             "complected_screener_without_SP",        
#   "completed_screener_without_SP_neighbor", "completed_screener_with_sp",             "completed_screener_with_sp_neighbor",   
#   "completed_screener4cat"                , "screener_nonresponse")
# 
# 
# 
# screener_case_fact_extra$dwelling_key <-dwelling_demensions$dwelling_key
# screener_case_fact_extra$standid_c <- substr(screener_case_fact_extra$dwelling_key,1,3)
# screener_case_fact_extra$standid<-as.numeric(screener_case_fact_extra$standid_c)
# screener_case_fact <-rbind(screener_case_fact,screener_case_fact_extra)  



# WRITING TABLES TO LOCKER(SQL IS WRITING VERY SLOW)----
# Write data tables----
fwrite(screener_case_fact, file= "L:/2021/DHANES dashboard/V0.5/Flat files/screener_case_factv0.5.csv")
fwrite(informant_DU_details, file="L:/2021/DHANES dashboard/V0.5/flat files/informant_DU_detailsv0.5.csv")
fwrite(informant_details, file = "L:/2021/DHANES dashboard/V0.5/Flat files/informant_details.csv")
fwrite(Completed_Screening_forcast, file= "L:/2021/DHANES dashboard/V0.5/Flat files/completed_screening_forcast.csv")
fwrite(stand_demensions1, file= "L:/2021/DHANES dashboard/V0.5/Flat files/stand_demensions.csv")
fwrite(V_ANL_NH_Web_Mailing, file= "L:/2021/DHANES dashboard/V0.5/Flat files/Web_Mailing.csv")
fwrite(dwelling_demensions, file= "L:/2021/DHANES dashboard/V0.5/Flat files/dwelling_demensions.csv")
fwrite(dropdown_navigation, file= "L:/2021/DHANES dashboard/V0.5/Flat files/dropdown_navigation.csv")
fwrite(visio_screener_process, file="L:/2021/DHANES dashboard/V0.5/Flat files/visio_screener_process.csv")
fwrite(visio_screener_process_inbetween, file="L:/2021/DHANES dashboard/V0.5/Flat files/visio_screener_process_inbetween.csv")
fwrite(EROC,file="L:/2021/DHANES dashboard/V0.5/Flat files/EROC_screen_rel.csv")
fwrite(SMS_du, file= "L:/2021/DHANES dashboard/V0.5/Flat files/SMS_DU.csv")
fwrite(participant_demographics_slicer, file = "L:/2021/DHANES dashboard/V0.5/Flat files/slicer_participant_demographics.csv")
fwrite(web_screener, file = "L:/2021/DHANES dashboard/V0.5/Flat files/web_screener.csv")




#output label tables----
fwrite(labels_Active_stand, file= "L:/2021/DHANES dashboard/V0.5/Flat files/labels_active_stand.csv")
fwrite(labels_case_where_completed, file = "L:/2021/DHANES dashboard/V0.5/Flat files/labels_case_where_completed.csv")
fwrite(labels_completed_screener4cat, file= "L:/2021/DHANES dashboard/V0.5/Flat files/labels_completed_screener4cat.csv")
fwrite(labels_gender, file = "L:/2021/DHANES dashboard/V0.5/Flat files/labels_gender.csv")
fwrite(labels_Interview_type, file = "L:/2021/DHANES dashboard/V0.5/Flat files/labels_Interview_type.csv")
fwrite(labels_mailing_type, file= "L:/2021/DHANES dashboard/V0.5/Flat files/labels_mailing_type.csv")
fwrite(labels_mailing_type2, file= "L:/2021/DHANES dashboard/V0.5/Flat files/labels_mailing_type2.csv")
fwrite(labels_participant_ethnicity, file = "L:/2021/DHANES dashboard/V0.5/Flat files/labels_participant_ethnicity.csv")
fwrite(labels_race, file = "L:/2021/DHANES dashboard/V0.5/Flat files/labels_race.csv")
fwrite(labels_screener_pending3cat, file = "L:/2021/DHANES dashboard/V0.5/Flat files/labels_screener_pending3cat.csv")
fwrite(labels_spanish_interview, file = "L:/2021/DHANES dashboard/V0.5/Flat files/labels_spanish_interview.csv")
fwrite(labels_severity, file = "L:/2021/DHANES dashboard/V0.5/Flat files/labels_severity.csv")
fwrite(labels_case_disposition, file = "L:/2021/DHANES dashboard/V0.5/Flat files/labels_case_disposition.csv")
fwrite(labels_survey_module, file = "L:/2021/DHANES dashboard/V0.5/Flat files/labels_survey_module.csv")


 

# WRITING TABLES TO THE NHANES SANDBOX SERVER----
dbWriteTable(write_con, name = "screener_case_fact", value = screener_case_fact, row.names = FALSE, overwrite=TRUE)
