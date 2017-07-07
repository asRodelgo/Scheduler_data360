############################################################
# Queries Operations data to populate the Portfolio and Operations piece in TCdata360 Country reports
# Adapted from TCMN sheet/TCMN_Projects.R that read from Composite database using Composite driver
#
# Alberto Sanchez, July 7 2017, asanchezrodelgo@worldbank.org
############################################################


# Packages --------------------------------------------------------
library(RODBC)
library(tidyverse)

# -------------------------------------------------------------------------
# TABLE 1 -----------------------------------------------------------------
# T&C ibrd projects -------------------------------------------------------

# connection
EFI_ANALYTICS <- odbcDriverConnect('DRIVER={ODBC Driver 13 for SQL Server};
                                   SERVER=WBGMSSQLEOA001,5800;Database=WBG;uid=EFI_User;pwd=Efi2017go!')

# No run: Explore the tables 
# tables <- sqlTables(EFI_ANALYTICS) %>% filter(!(tolower(TABLE_SCHEM) == "sys"), TABLE_TYPE == "TABLE")

projects <- sqlQuery(EFI_ANALYTICS,
"SELECT [WB Project ID],[WB Practice Code], [WB Product Line Type Name],[WB Project Name],[WB Product Line Code],
[WB Board Approval Date],[WB Begin Appraisal Date],[WB Begin Appraisal Date Status],[WB ACS Date],[WB Original Delivery to Client Date],
[WB Original Final Delivery Date],[WB Management Approval Concept Date],[WB Country Key],[WB Country Short Name],[WB Country Long Name],
[WB Region Abbreviation],[WB Region Code],[WB Region Name],[WB Project Status Name],[WB TTL UPI],[WB TTL],[WB IBRD Cmt USD Amt],
[WB IDA CMT Amount],[WB Grant USD Amt],[WB Current FY Cost],[WB Cumulative FY Cost],[WB Current BB Cost],[WB Cumulative BB Cost],[WB Lending Instrument Type Name],
[WB FY Probability Type Code],[WB FY Probability Name],[WB IEG Outcome],[WB DO Rating],[WB IP Rating]
                     FROM Operations.Project 
                     WHERE [WB Practice Code] = 'TAC'
                     ")
names(projects) <- gsub(" ","_",names(projects))

sort <- sqlQuery(EFI_ANALYTICS,"SELECT [Project ID],[Indicator Name],[Rating Code]
                 FROM Operations.Project_Risk_SORT WHERE [Practice Code] = 'TAC'")

names(sort) <- gsub(" ","_",names(sort))

supervision <- sqlQuery(EFI_ANALYTICS,"SELECT 
[WB Supervision # of months in problem status],[WB Supervision FY],[WB Supervision project age in year],[WB Supervision Project ID],
[WB Supervision Total Commitment Amount],[WB Supervision undisbursed balance at begining FY], [WB Revised Closing Date],[WB Fee Based Flag]
                 FROM Operations.EFI_MOU")

names(supervision) <- gsub(" ","_",names(supervision))

supervision <- supervision %>%
  group_by(WB_Supervision_Project_ID) %>%
  filter(WB_Supervision_FY == max(WB_Supervision_FY)) %>% as.data.frame()

sector_theme <- sqlQuery(EFI_ANALYTICS,"SELECT  [WB Project ID],[WB Sector Code],[WB Sector Name],[WB Sector Seq Nbr],[WB Sector Pct],
[WB Major Sector Code], [WB Major Sector Name],[WB Theme Code],[WB Theme Name],[WB Theme Seq Nbr],[WB Theme Pct],
[WB Theme Lvl1 Code],[WB Theme Lvl2 Code],[WB Theme Lvl3 Code],[WB Theme Lvl1 Name],[WB Theme Lvl2 Name],[WB Theme Lvl3 Name]
                 FROM Operations.Sector_Theme")

names(sector_theme) <- gsub(" ","_",names(sector_theme))
#projects_extra <- sqlQuery(EFI_ANALYTICS,"SELECT top 100 * FROM Operations.Project")
#efi_mou <- sqlQuery(EFI_ANALYTICS,"SELECT top 100 * FROM Operations.EFI_MOU")

projects <- left_join(projects,sort,c("WB_Project_ID" = "Project_ID")) %>% 
  left_join(supervision, c("WB_Project_ID" = "WB_Supervision_Project_ID")) %>% 
  left_join(sector_theme, c("WB_Project_ID" = "WB_Project_ID"))

TCprojects <- mutate_at(projects,vars(ends_with("_Date")),as.character)
TCprojects <- mutate_if(TCprojects,is.factor,as.character)

odbcClose(EFI_ANALYTICS) # close connection

#merge(projects, team, by="PROJ_ID", all.x=TRUE)
write.csv(TCprojects, "outputFiles/TCprojects.csv",row.names=FALSE)


# -------------------------------------------------------------------------
# TABLE 2 -----------------------------------------------------------------
# IFC projects ------------------------------------------------------------

# fil <- filter(ifc_gpDEL, PROJ_ID=="593247" & Report_Date == "2015-12-31 EST")
# write.csv(fil, "C:/Users/asanchezrodelgo/Desktop/egyptIFC.csv",row.names = FALSE)
# connection
ch <- odbcConnect("WBG_Portfolio_Operations")

# IFC projects: write query so it returns latest Report_date group_by(PROJ_ID)
ifc_gp <- sqlQuery(ch,"select PROJ_ID, FY_OF_ASIP_APPROVAL, PROJECT_STAGE, PROJECT_NAME, PROJECT_STATUS, PROJECT_TYPE, 
                   PROJECT_CLASSIFICATION_TYPE, PRODUCT_NAME, PROJECT_LEADER, IMPLEMENTATION_END_DATE, 
                   PRORATED_TOTAL_FYTD_EXPENSE,ITD_EXPENDITURES,
                   REGION_CODE, ASIP_APPROVAL_DATE, COUNTRY_CODE, IFC_WBG_JOINT_PROJECT, TOTAL_PROJECT_SIZE, 
                   PRIMARY_BUSINESS_LINE_CODE, TOTAL_FUNDING, TOTAL_FUNDS_MANAGED_BY_IFC, Report_Date
                   from ifc_gp
                   ")
ifc_gp$IMPLEMENTATION_END_DATE <- as.character(ifc_gp$IMPLEMENTATION_END_DATE)
ifc_gp$ASIP_APPROVAL_DATE <- as.character(ifc_gp$ASIP_APPROVAL_DATE)
ifc_gp$Report_Date <- as.character(ifc_gp$Report_Date)
ifc_gp <- ifc_gp %>%
  group_by(PROJ_ID) %>%
  filter(Report_Date == max(Report_Date))

# filter project in active, closed or pipeline status?
# ifc_gp <- filter(ifc_gp, (PROJECT_STAGE=="PIPELINE") | (PROJECT_STATUS %in% c("ACTIVE","CLOSED")))

# create data table
#write.csv(ifc_gp, "outputFiles/ifc_gpTable.csv",row.names=FALSE)

odbcClose(ch) # close connection

# -------------------------------------------------------------------------
# TABLE 3 -----------------------------------------------------------------
# Team (people) -----------------------------------------------------------

# connection
ch <- odbcConnect("WBG_Portfolio_Operations")

#upi_nbr_c, PROJ_ID, UPI_ROLE_DESC 
wbg_team <- sqlQuery(ch,"select upi_nbr_c, PROJ_ID, UPI_ROLE_DESC 
                     from team
                     ")

odbcClose(ch) # close connection

# Match names with UPI ---------------------

# connection
ch <- odbcConnect("WBG_Portfolio_Corporate") 

employee <- sqlQuery(ch,"select upi_nbr_c, upi_9,FULL_NAME, job_title, WORK_ALPHA,CITY_NAME,CITY_CODE,COMPANY
                     from Corporate.Person.employee
                     ")

employee_positition_summary <- sqlQuery(ch,"select upi_9, position_type, grade, duty_country, supervisor, manager, practice_manager, practice, emplyment_type
                                        from Corporate.Person.employee_position_summary
                                        ")
# merge employee and employee_position_summary by= upi_9
employee <- merge(employee, employee_positition_summary, by="upi_9")

# merge with teams by upi_nbr_c
team <- merge(wbg_team, employee, by="upi_nbr_c", all.x=TRUE)

# create data table
#write.csv(team, "outputFiles/teamTable.csv",row.names=FALSE)

odbcClose(ch) # close connection


# -----------------------------------------------------------------------------
# TABLE FINAL -----------------------------------------------------------------
# Team ------------------------------------------------------------------------
# Merge T&C IBRD and IFC projects in two tables

### One table does not make sense as the PROJ_ID are exclusive to each organization, but here's the code just in case:
# 
# # list unique PROJ_IDs to filter team_all and remove the junk
# project_list <- unique(c(as.character(projects$PROJ_ID),as.character(ifc_gp$PROJ_ID)))
# 
# team_all <- merge(team, projects, by="PROJ_ID", all.x=TRUE)
# team_all <- merge(team_all, ifc_gp, by="PROJ_ID", all.x=TRUE)
# 
# team_all <- filter(team_all, PROJ_ID %in% project_list)
# 
# # create data table
# write.csv(team_all, "outputFiles/team_allTable.csv",row.names=FALSE)

TCprojects <- merge(projects, team, by="PROJ_ID", all.x=TRUE)
write.csv(TCprojects, "outputFiles/TCprojects.csv",row.names=FALSE)

# # TCprojects gets too big. Split it in pieces for easier manipulation
# TCprojectsList <- list()
# pieces <- 5
# split_size <- round(nrow(TCprojects)/pieces)
# for (i in 1:pieces){
#   TCprojectsList[[i]] <- TCprojects[((i-1)*split_size+1):(i*split_size),]
# }
# #write the multiple dataframes into as many csv files
# for (i in 1:pieces){
#   write.csv(TCprojectsList[[i]], paste0("outputFiles/TCprojects",i,".csv"),row.names=FALSE)
# }

IFCprojects <- merge(ifc_gp, team, by="PROJ_ID", all.x=TRUE)
write.csv(IFCprojects, "outputFiles/IFCprojects.csv",row.names=FALSE)

# -----------------------------------------------------------------------------------
# SCD/CPF DOCUMENTS -----------------------------------------------------------------
# -----------------------------------------------------------------------------------
# Web scraping documents:
# 1. Get to the documents page:
# http://documents.worldbank.org
# Use the API to construct a query: http://search.worldbank.org/api/v2/wds
# 
# #
#library(rvest)
library(readxl)
library(stringr)
library(RCurl)
library(RJSONIO) # read JSON files
# #
countries <- read.csv("C:/Users/asanchezrodelgo/Box Sync/BES data scraping/CountryClassification.csv")
myCountries <- filter(countries, !(substr(Country,1,1)=="("))
# remove blanks on ISO2 codes column
myCountries <- filter(myCountries, !(CountryCodeISO2 == ""))

# -----------------------------
## Pull CPF documents:

# # populate latest document for each country
docJsonCPF <- data.frame()
#names(docJson) <- c("Product","url","pdfurl","disclosure_date")

i <- 1
for (cou in myCountries$CountryCodeISO2){
  #for (cou in c("AF")){  
  # build the URL
  #http://search.worldbank.org/api/v2/wds?format=json&fl=abstracts,admreg,alt_title,authr,available_in,bdmdt,chronical_docm_id,closedt,colti,count,credit_no,disclosure_date,disclosure_type,disclosure_type_date,disclstat,display_title,docdt,docm_id,docna,docty,dois,entityid,envcat,geo_reg,geo_reg,geo_reg_and_mdk,historic_topic,id,isbn,issn,keywd,lang,listing_relative_url,lndinstr,loan_no,majdocty,majtheme,ml_abstract,ml_display_title,new_url,owner,pdfurl,prdln,projn,repnb,repnme,sectr,src_cit,subsc,subtopic,teratopic,theme,topic,topicv3,totvolnb,trustfund,txturl,unregnbr,url_friendly_title,versiontyp,versiontyp_key,virt_coll,vol_title,volnb&countcode=BI&majdocty_exact=Board+Documents^Country+Focus&docty_exact=CAS+Progress+Report^Country+Assistance+Strategy+Document^Interim+Strategy+Note&srt=docdt&order=desc  
  getDoc <- fromJSON(paste0("http://search.worldbank.org/api/v2/wds?format=json&fl=abstracts,admreg,alt_title,authr,available_in,bdmdt,chronical_docm_id,closedt,colti,count,credit_no,disclosure_date,disclosure_type,disclosure_type_date,disclstat,display_title,docdt,docm_id,docna,docty,dois,entityid,envcat,geo_reg,geo_reg,geo_reg_and_mdk,historic_topic,id,isbn,issn,keywd,lang,listing_relative_url,lndinstr,loan_no,majdocty,majtheme,ml_abstract,ml_display_title,new_url,owner,pdfurl,prdln,projn,repnb,repnme,sectr,src_cit,subsc,subtopic,teratopic,theme,topic,topicv3,totvolnb,trustfund,txturl,unregnbr,url_friendly_title,versiontyp,versiontyp_key,virt_coll,vol_title,volnb&countcode=",cou,"&majdocty_exact=Board+Documents^Country+Focus&docty_exact=CAS+Progress+Report^Country+Assistance+Strategy+Document^Interim+Strategy+Note&lang_exact=English^Englsih&srt=docdt&order=desc"))
  this_j <- 1
  #print(paste(cou,"Successful"))
  if (!(getDoc$total=="0")){
    # rid of header metadata
    getDoc <- getDoc[['documents']] # get rid of metadata
    
    # make sure we extract the right volume (some reports release different volumes/versions)
    if (as.numeric(getDoc[[1]]$totvolnb) > 1){
      j <- 2
      while (getDoc[[j]]$repnb == getDoc[[1]]$repnb){
        if (getDoc[[j]]$volnb=="1"){
          this_j <- j
        } 
        j <- j + 1
      }
      getDoc <- getDoc[[this_j]]
    } else  getDoc <- getDoc[[1]] # extract the first list
    
    # add to file
    docJsonCPF[i,1] <- as.character(getDoc$repnme[[1]])
    docJsonCPF[i,1] <- gsub("\n","",docJsonCPF[i,1],fixed = TRUE)
    docJsonCPF[i,1] <- gsub("\031","",docJsonCPF[i,1],fixed = TRUE)
    docJsonCPF[i,1] <- gsub("ô","o",docJsonCPF[i,1],fixed = TRUE)
    docJsonCPF[i,1] <- gsub("\\s+"," ",docJsonCPF[i,1])
    docJsonCPF[i,1] <- trimws(strsplit(docJsonCPF[i,1],":",fixed=TRUE)[[1]][1])
    docJsonCPF[i,2] <- as.character(getDoc$url)
    docJsonCPF[i,3] <- ifelse(length(as.character(getDoc$pdfurl))==1,as.character(getDoc$pdfurl),"")
    docJsonCPF[i,4] <- substr(as.character(getDoc$disclosure_date),1,10)
    docJsonCPF[i,5] <- cou
    i <- i + 1
  } 
  
}
names(docJsonCPF) <- c("Report","url", "pdfurl", "Date", "CountryCodeISO2")
# get ISO3 codes 
docJsonCPF <- merge(docJsonCPF, myCountries[,c("CountryCodeISO3","CountryCodeISO2")], by="CountryCodeISO2")
docJsonCPF <- as.data.frame(docJsonCPF)
docJsonCPF <- select(docJsonCPF, - CountryCodeISO2)

# -----------------------------
## Pull SCD documents:

# # populate latest document for each country
docJsonSCD <- data.frame()
#names(docJson) <- c("Product","url","pdfurl","disclosure_date")

i <- 1
for (cou in myCountries$CountryCodeISO2){
  #for (cou in c("AF")){  
  # build the URL
  #http://search.worldbank.org/api/v2/wds?format=json&fl=abstracts,admreg,alt_title,authr,available_in,bdmdt,chronical_docm_id,closedt,colti,count,credit_no,disclosure_date,disclosure_type,disclosure_type_date,disclstat,display_title,docdt,docm_id,docna,docty,dois,entityid,envcat,geo_reg,geo_reg,geo_reg_and_mdk,historic_topic,id,isbn,issn,keywd,lang,listing_relative_url,lndinstr,loan_no,majdocty,majtheme,ml_abstract,ml_display_title,new_url,owner,pdfurl,prdln,projn,repnb,repnme,sectr,src_cit,subsc,subtopic,teratopic,theme,topic,topicv3,totvolnb,trustfund,txturl,unregnbr,url_friendly_title,versiontyp,versiontyp_key,virt_coll,vol_title,volnb&countcode=BI&majdocty_exact=Board+Documents^Country+Focus&docty_exact=CAS+Progress+Report^Country+Assistance+Strategy+Document^Interim+Strategy+Note&srt=docdt&order=desc  
  getDoc <- fromJSON(paste0("http://search.worldbank.org/api/v2/wds?format=json&fl=abstracts,admreg,alt_title,authr,available_in,bdmdt,chronical_docm_id,closedt,colti,count,credit_no,disclosure_date,disclosure_type,disclosure_type_date,disclstat,display_title,docdt,docm_id,docna,docty,dois,entityid,envcat,geo_reg,geo_reg,geo_reg_and_mdk,historic_topic,id,isbn,issn,keywd,lang,listing_relative_url,lndinstr,loan_no,majdocty,majtheme,ml_abstract,ml_display_title,new_url,owner,pdfurl,prdln,projn,repnb,repnme,sectr,src_cit,subsc,subtopic,teratopic,theme,topic,topicv3,totvolnb,trustfund,txturl,unregnbr,url_friendly_title,versiontyp,versiontyp_key,virt_coll,vol_title,volnb&countcode=",cou,"&majdocty_exact=Board+Documents^Country+Focus&docty_exact=Systematic+Country+Diagnostic&lang_exact=English^Englsih&srt=docdt&order=desc"))
  if (!(getDoc$total=="0")){
    # extract
    getDoc <- getDoc[['documents']] # get rid of metadata
    getDoc <- getDoc[[1]] # extract the first list
    # add to file
    docJsonSCD[i,1] <- as.character(getDoc$repnme[[1]])
    docJsonSCD[i,1] <- gsub("\n","",docJsonSCD[i,1],fixed = TRUE)
    docJsonSCD[i,1] <- gsub("\031","",docJsonSCD[i,1],fixed = TRUE)
    docJsonSCD[i,1] <- gsub("ô","o",docJsonSCD[i,1],fixed = TRUE)
    docJsonSCD[i,1] <- gsub("\\s+"," ",docJsonSCD[i,1])
    docJsonSCD[i,1] <- trimws(strsplit(docJsonSCD[i,1],":",fixed=TRUE)[[1]][1])
    docJsonSCD[i,2] <- as.character(getDoc$url)
    docJsonSCD[i,3] <- ifelse(length(as.character(getDoc$pdfurl))==1,as.character(getDoc$pdfurl),"")
    docJsonSCD[i,4] <- substr(as.character(getDoc$disclosure_date),1,10)
    docJsonSCD[i,5] <- cou
    i <- i + 1
  } 
  
}
names(docJsonSCD) <- c("Report","url", "pdfurl", "Date", "CountryCodeISO2")
# get ISO3 codes 
docJsonSCD <- merge(docJsonSCD, myCountries[,c("CountryCodeISO3","CountryCodeISO2")], by="CountryCodeISO2")
docJsonSCD <- as.data.frame(docJsonSCD)
docJsonSCD <- select(docJsonSCD, - CountryCodeISO2)

# --------------
# Append together
docJson <- rbind(docJsonSCD,docJsonCPF)

write.csv(docJson,"outputFiles/SCDCPFdocuments.csv",row.names=FALSE)

##############################################

### Planned Documents: --------------------------------------------
# out_file <- paste0('L:\\portfolioReport\\','operationsPlanned.xlsx')
# wrapperURL <- "http://operationsmonitoring.worldbank.org/activitiestable.do?indicatorcode=0201&includeallprojects=true&org=ibrd&level=global&code=allprojects&params=%24cpfactualorpipeline%24cpfcurfy%24cpfboardappdate%24cpfcurfybyboardappdate%24&tabcode=5"
# 
# encoded_url <- base64enc::base64encode(charToRaw(wrapperURL))
# 
# # powershell command removes the security authentication
# wrapperCMD <- paste0('powershell.exe -command L:\\iSearch\\get_iSearch2.ps1 -encodedURL \"',
#                      encoded_url, '\" -OUT_FILE \"', out_file, '\"')
# 
# try(system(wrapperCMD, intern = FALSE, show.output.on.console = TRUE, invisible=TRUE))
# file.copy("L:\portfolioReport\operationsPlanned.xlsx","inputFiles/operationsPlanned.xlsx")

# -----------------------------------------------------------------

# If automation fails, manually do this:
# 1. Go to: http://operationsmonitoring.worldbank.org/view/index.html#details/global/allprojects/cpfproddetails/0201/$cpfactualorpipeline$cpfcurfy$cpfboardappdate$cpfcurfybyboardappdate/
# 2. Download Excel file and save it as inputFiles/operationsPlanned.xlsx
# planned <- read_excel("inputFiles/operationsPlanned.xlsx",3) # comes with 2 hidden sheets 

# -----------------------------------------------------------------

# Ramin downloads Excel files from Standard Reports daily 7:00 am. Read file from:
# \\Gtcfile\gtc\Strategy_Analysis\PMSO\Data_Analytics\Data

planned <- read_excel("//Gtcfile/gtc/Strategy_Analysis/PMSO/Data_Analytics/Data/StandardReports/CPFoperationsPlanned.xlsx",3) # comes with 2 hidden sheets 
planned <- as.data.frame(planned[5:nrow(planned),c(2,4,10,15)])
planned <- planned[!(duplicated(planned)),]
names(planned) <- c("Country","Product","ConceptReviewDate","BoardDate")
planned <- filter(planned, !is.na(Country))
docs2 <- merge(planned, countries[,c("CountryAlternat","CountryCodeISO3")], by.x="Country",by.y="CountryAlternat")
# dates in appropriate format
docs2 <- mutate(docs2, BoardDate = as.Date(as.numeric(BoardDate),origin = "1899-12-30"),
                ConceptReviewDate = as.Date(as.numeric(ConceptReviewDate),origin = "1899-12-30"))
docs2 <- filter(docs2, BoardDate > Sys.Date())

write.csv(docs2,"outputFiles/Planneddocuments.csv",row.names=FALSE)


