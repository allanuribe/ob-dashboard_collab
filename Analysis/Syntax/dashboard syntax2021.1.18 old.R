install.packages("easypackages")
library('easypackages')
################################################
# install packages needed to connect and run queries  to a SQL server 
#such as the ANL 
install_packages('DBI', 'odbc','RODBC','sqldf','Hmisc', 'expss','summarytools')
libraries('DBI', 'odbc','RODBC','sqldf','Hmisc', 'expss', 'summarytools')
# connecting to the ANL----
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "DSPP-HANE-1601",
                 Database = "ANL1_db",
                 Trusted_Connection = "True",
                 UID = "qsj2",
                 PWD = rstudioapi::askForPassword("Database password"),
                 Port = 1433)
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
############################################################################### 
# the SQL code must be put into its own R object in order to use the past 
#function and not have to rewrite the entire var list to keep 
SC2_query <- paste("SELECT ", paste(sc2_DU_keep, sep = "",collapse= ","),
                   " FROM ANL_SC2_SAM_INTV_DU")
#this prints out the query above as it will be entered into the dbgetquery below 
cat(SC2_query)
# query of only the variables need of the SAMPLE DU table ----
sc2_DU_k <- dbGetQuery(con, SC2_query)
# query complete copy of the SAMPLE DU table ----
sc2_DU <- dbGetQuery(con, "SELECT * FROM ANL_SC2_SAM_INTV_DU")
###############################################################################
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

#QUESTION : what does this table represent----
ri7_sam_case <-dbGetQuery(con, "SELECT * FROM ANL_RI7_SAM_Case")


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

sc3_query <- paste("SELECT ", paste(sc3_sam_part_keep, sep = "",collapse= ","),
                   " FROM VN_ANL_SC3_SAM_part")
cat(sc3_query)
sc3_sam_part_k <- dbGetQuery(con, sc3_query)

#some additional demographics about the cases.  
sc3_sam_part <- dbGetQuery(con, "SELECT * FROM VN_ANL_SC3_SAM_part")


#For now importing this just to get the disp variable.  
curr_case_keep <- c("SCACASE"
                    ,"STAQDISP" 
                    )

curr_case_query <- paste("SELECT ", paste(curr_case_keep, sep = "",collapse= ","),
                   " FROM V_ANL_Current_Case")
cat(curr_case_query)
curr_case_k <- dbGetQuery(con, curr_case_query)
curr_case <- dbGetQuery(con,"select * FROM V_ANL_Current_Case")

# grabbing the case disposition code from the current case table and joining 
#with the RI7
# RI7 has cases, defined by the portion of the survey being completed.
# case table can be joined with du table after filtering dow using the type 
#variable to include only the screener.  
ri7_sam_case1 <- sqldf("SELECT A.*, B.STAQDISP
                        FROM ri7_sam_case_k A
                        LEFT JOIN curr_case_k B
                        ON a.SCACASE = b.SCACASE")


ri7_sam_case_k$dwelling_key <- paste(ri7_sam_case_k$SDASTAND
                            ,ri7_sam_case_k$SDASEGMT
                            ,ri7_sam_case_k$SDASERAL, sep = "_")

ri7_sam_case_k$segment_key <- paste(ri7_sam_case_k$SDASTAND
                                     ,ri7_sam_case_k$SDASEGMT
                                     ,sep = "_")

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






# this table is a complete list of peoplt that are part of the sample frame----
person <- dbGetQuery(con, "SELECT * FROM vn_anl_ri1_nh_person WHERE riapcode=2 AND scqck305=1")
#QUESTION : What does this table represent----
mec_exam <-  dbGetQuery(con, "SELECT * FROM v_mec_exam")
#QUESTION :What does this table represent----
family <-  dbGetQuery(con, "SELECT * FROM ANL_SC1_SAM_Family")

#################################################################

# SCREENING TABLES
# Screening records completed or not. 
# QUESTION : shouldnt this be the same as the total records on SC2.  was off by one and steven found the issue, 1 record from stand 152. 
# COMBAK : make sure the extra ecord is removed by steven from SC2.
screening_record <- subset(ri7_sam_case,STAQTYP==1)
#Screening happens in 2 steps STAQTYP/ STARSPNO is automatically assigned
Screened_person <- subset(ri7_sam_case,
                          (STAQTYP==1 & STARSPNO > 0) | (STAQTYP==2 & STARSPNO > 0))


DUwScreener <- subset(sc2_DU,SCAREFPT >0)






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





                        
                        

sc2_DU_k <- sc2_DU[sc2_DU_keep]
sc3_sam_part_k <- sc3_sam_part[sc3_sam_part_keep]
ri7_sam_case1_k <- ri7_sam_case1[ri7_sam_case1_keep]

ri7_sam_case1_k$SCACASE <- as.character(ri7_sam_case1_k$SCACASE)

print(freq(as.numeric(sc2_DU_k$SCAWHERE) ), method = 'render')

freq(as.numeric(sc3_sam_part_k$SCQ302),plain.ascii = T, style = 'rmarkdown',big.mark= ',')

sqldf('SELECT SDASTAND, SCQ600, count(SCQ600) as COUNT FROM sc2_DU_k GROUP BY SDASTAND, SCQ600' )

gen_health <- aggregate(sc2_DU_k$SCQ600, by=list(Stand = sc2_DU_k$SDASTAND, "value" = sc2_DU_k$SCQ600),FUN ="sum")
gen_health$var <- "Self- reported general health status" 
meds <- aggregate(sc2_DU_k$SCQ610, by=list(Stand = sc2_DU_k$SDASTAND, "value" = sc2_DU_k$SCQ610),FUN ="sum")
meds$var <- "Taking medication"
qmeds <- aggregate(sc2_DU_k$SCQ620, by=list(Stand = sc2_DU_k$SDASTAND, "value" = sc2_DU_k$SCQ620),FUN ="sum")
qmeds$var <- "No. of Meds being taken"
dia <- aggregate(sc2_DU_k$SCQ630, by=list(Stand = sc2_DU_k$SDASTAND, "value" = sc2_DU_k$SCQ630),FUN ="sum")
dia$var <- "Self-reported diabetes"
htn <- aggregate(sc2_DU_k$SCQ640, by=list(Stand = sc2_DU_k$SDASTAND, "value" = sc2_DU_k$SCQ640),FUN ="sum")
htn$var <- "self-reported high blood pressure"
healthq <- rbind(gen_health,meds,qmeds,dia,htn)

freq(as.numeric(sc2_DU_k$SCQ600))

freq(as.numeric(sc2_DU_k$SCQ610))



write.csv2(ri7_sam_case1_k, file = "l:\\ri7_sam_case1_k.csv")
write.table(sc3_sam_part_k, file = "l:\\sc3_sam_part_k.txt")
write.table(sc2_DU_k, file = "l:\\sc2_DU_k.txt")


write.csv2(gen_health, file= 'l:\\gen_health.csv')
write.csv2(meds, file= 'l:\\meds.csv')
write.csv2(qmeds, file= 'l:\\qmeds.csv')
write.csv2(dia, file= 'l:\\dia.csv')
write.csv2(htn, file= 'l:\\htn.csv')
write.csv2(healthq, file= 'l:\\healthq.csv')


steven1 <- dbGetQuery(con,"select *from ANL_RI7_SAM_Case sc 
                            inner join V_ANL_Current_Case v 
                                on sc.SCACASE = v.SCACASE and 
                                v.STAQDISP in (10,11) 
                            left outer join ANL_RI7_SAM_Case sc2 
                                on sc2.SDASTAND = sc.SDASTAND and 
                                sc2.SDASEGMT = sc.SDASEGMT and 
                                sc2.SDASERAL = sc.SDASERAL and 
                                sc2.STAQTYP = 2 
                            left outer join VN_ANL_SC3_SAM_part sp 
                                on sc.SDASTAND = sp.SDASTAND and 
                                sc.SDASEGMT = sp.SDASEGMT and 
                                sc.SDASERAL = sp.SDASERAL and 
                                sp.SCAPARTN = case when sc.STARSPNO > 0 
                    then sc.STARSPNO else sc2.STARSPNO end where sc.STAQTYP = 1")
