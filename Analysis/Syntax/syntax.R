source("C:/Users/qsj2/OneDrive - CDC/MY WORK/Projects/connect to the ANL database.R")

## import the paradata tables----
sp_paradata <- dbGetQuery(con, "select * from ANL_PARADATA_SP")
paradata <- dbGetQuery(con, "select * from ANL_PARADATA")

# use and R package to create codebooks of the paradata----
makeCodebook(data= sp_paradata, vol = 1, reportTitle = "ParaData SP codeBook")
makeCodebook(data= paradata, vol = 1, reportTitle = "ParaData codeBook")

## Try to figure out which EROC table in ANL matched the portal codebooks----
ANL_EROC_DU_Type <- dbGetQuery(con, "select max(ERASTAND) as MAX, 
                               Min(ERASTAND) as Min 
                               from ANL_EROC_DU_Type")

ANL_EROC_Nonresponse <- dbGetQuery(con, "select max(ERASTAND) as MAX, 
                               Min(ERASTAND) as Min 
                               from ANL_EROC_Nonresponse")


ANL_EROC_ROC_Attempts <- dbGetQuery(con, "select max(ERASTAND) as MAX, 
                               Min(ERASTAND) as Min 
                               from ANL_EROC_ROC_Attempts")

ANL_EROC_ROC_DU <- dbGetQuery(con, "select max(ERASTAND) as MAX, 
                               Min(ERASTAND) as Min 
                               from ANL_EROC_ROC_DU")



