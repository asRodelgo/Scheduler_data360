# Functions that filter data according to different output categories --------------------------
#
# filter IBRD T&C relevant projects ---------------
.filterTCProjects <- function(){
  
  # calculate total amount per project
  dataTC <- TCprojects %>%
    group_by(WB_Project_ID) %>%
    mutate(Project_Amount = (WB_IBRD_Cmt_USD_Amt+WB_IDA_CMT_Amount+WB_Grant_USD_Amt)/1000000,
           Prod_Line = ifelse(tolower(substr(WB_Product_Line_Type_Name,1,4))=="lend","Financing",
                              ifelse(tolower(substr(WB_Product_Line_Type_Name,1,3))=="aaa",
                                     "Advisory Services and Analytics (ASA) IBRD",WB_Product_Line_Type_Name)),
           ProjectOrder = ifelse(WB_Project_Status_Name=="Active",1,ifelse(WB_Project_Status_Name=="Pipeline",2,3)),
           url = paste0("http://operationsportal2.worldbank.org/wb/opsportal/ttw/about?projId=",WB_Project_ID),
           RAS = ifelse(is.na(WB_Fee_Based_Flag),"N","Y")) %>%
    select(-c(WB_IBRD_Cmt_USD_Amt,WB_IDA_CMT_Amount,WB_Grant_USD_Amt)) %>%
    filter(WB_Project_Status_Name %in% c("Closed","Active","Pipeline")) %>%
    mutate(ProjectOrder = ifelse(is.na(WB_Revised_Closing_Date),ProjectOrder,ifelse(WB_Revised_Closing_Date<Sys.Date(),3,ProjectOrder)))
  
  return(dataTC)
}

# filter IFC T&C relevant projects ---------------
.filterIFCProjects <- function(){
  
  # projects in active, pipeline or closed status
  dataIFC <- filter(IFCprojects, (PROJECT_STAGE %in% c("PIPELINE","PORTFOLIO")) | (PROJECT_STATUS %in% c("ACTIVE", "HOLD", "CLOSED")),
                    PROJECT_TYPE == "AS PROJECTS WITH CLIENT(S)")
  dataIFC <- mutate(dataIFC, Prod_Line = "Advisory Services and Analytics (ASA) IFC",
                    Project_Status = ifelse(PROJECT_STATUS=="CLOSED","Closed",ifelse(PROJECT_STAGE=="PIPELINE","Pipeline","Active")),
                    Hold = ifelse((PROJECT_STAGE=="PORTFOLIO") & (PROJECT_STATUS=="HOLD"), "Y","N"))
  dataIFC <- mutate(dataIFC, ProjectOrder = ifelse(Project_Status=="Active",1,ifelse(Project_Status=="Pipeline",2,3)),
                    url = paste0("http://ifcext.ifc.org/ifcext/spiwebsite1.nsf/%20AllDocsAdvisory?SearchView&Query=(FIELD ProjectId=",PROJ_ID))
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  #dataIFC <- mutate(dataIFC, ProjectOrder = ifelse(is.na(IMPLEMENTATION_END_DATE),ProjectOrder,ifelse(IMPLEMENTATION_END_DATE<Sys.Date(),3,ProjectOrder)))
  
  return(dataIFC)
}


## ---- lendingPipeline ----
projectsTableLendingPipeline <- function(){
  
  dataTC <- .filterTCProjects()
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID=WB_Project_ID, Prod_Line, Project_Name = WB_Project_Name, WB_Country_Key,
                   Team_Leader = WB_TTL, Approval_Date = WB_Board_Approval_Date, 
                   Lending_Inst_Type = WB_Lending_Instrument_Type_Name,
                   Begin_Appraisal = WB_Begin_Appraisal_Date,Project_Amount,
                   Latest_Sort = Rating_Code, FY_Expenses = WB_Current_FY_Cost,
                   Cum_Expenses = WB_Cumulative_FY_Cost,FY_Prob = WB_FY_Probability_Type_Code,
                   ProjectOrder,url)
  # Financing products in Pipeline (ProjectOrder==2)
  dataTC <- filter(dataTC, Prod_Line == "Financing" & ProjectOrder==2)
  count_ibrd <- nrow(dataTC) # will determine the size of the table
  # filter by date range
  #dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Approval_Date))
  dataTC <- select(dataTC,-ProjectOrder, -Prod_Line)
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  #data <- select(data, -url)
  
  # scale Expenses to thousands 
  data$FY_Expenses <- data$FY_Expenses/1000
  data$Cum_Expenses <- data$Cum_Expenses/1000
  
  # format Amount
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  data$FY_Expenses <- format(data$FY_Expenses, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  data$Cum_Expenses <- format(data$Cum_Expenses, digits=0, decimal.mark=".",
                              big.mark=",",small.mark=".", small.interval=3)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  nrow <- nrow(data)
  data <- sapply(data, function(x) gsub("NA", "---", x, fixed=TRUE))
  if (nrow==1){
    data <- as.data.frame(t(data))
  } else{
    data <- as.data.frame(data)
  }
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("#", "\\#", data$Project_Name, fixed=TRUE)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataTC)-2)))
  }
  
  names(data) <- c("Project ID", "Project Name", "CountryCodeISO2","Team Leader", "Approval Date", "Lending Inst. Type",
                   "Begin Appraisal", "Commitment (US\\$M)","Latest Sort Overall Risk Rating","FY Expenses (US\\$K)",
                   "Cum Expenses (US\\$K)","FY Prob","URL")
  
  return(data)
  
}


## ---- portfolioActive ----
projectsTablePortfolioActive <- function(){
  
  dataTC <- .filterTCProjects()
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Name = PROJ_SHORT_NME, WBG_CNTRY_KEY,
                   Team_Leader = FULL_NME, Approval_Date = BD_APPRVL_DATE, 
                   Lending_Inst_Type = LENDING_INSTR_TYPE_NME,
                   Closing_Date = REVISED_CLS_DATE,Project_Amount,
                   Undisb_Bal = total_undis_balance,DO_RATING, IP_RATING,
                   Latest_Sort = rate_indicator,
                   Months_Problem = No_of_Months_in_problem_status,
                   ProjectOrder,url)
  # Financing products in Active (ProjectOrder==1)
  dataTC <- filter(dataTC, Prod_Line == "Financing" & ProjectOrder==1 & !(is.na(Approval_Date)))
  count_ibrd <- nrow(dataTC) # will determine the size of the table
  # filter by date range
  #dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Approval_Date))
  dataTC <- select(dataTC,-ProjectOrder, -Prod_Line)
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  #data <- select(data, -url)
  
  # scale Expenses to thousands 
  data$Undisb_Bal <- data$Undisb_Bal/1000000
  
  # format Amount
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  data$Undisb_Bal <- format(data$Undisb_Bal, digits=0, decimal.mark=".",
                            big.mark=",",small.mark=".", small.interval=3)
  data$Months_Problem <- format(data$Months_Problem, digits=1, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  nrow <- nrow(data)
  data <- sapply(data, function(x) gsub("NA", "---", x, fixed=TRUE))
  if (nrow==1){
    data <- as.data.frame(t(data))
  } else{
    data <- as.data.frame(data)
  }
  
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("#", "\\#", data$Project_Name, fixed=TRUE)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataTC)-2)))
  }
  
  names(data) <- c("Project ID", "Project Name", "CountryCodeISO2","Team Leader", "Approval Date", "Lending Inst. Type",
                   "Closing Date", "Commitment (US\\$M)","Undisbursed Balance (US\\$M)",
                   "Project Rating DO", "Project Rating IP","Overall Risk",
                   "Months in Problem Status", "URL")
  return(data)
}


## ---- portfolioClosed ----
projectsTablePortfolioClosed <- function(){
  
  dataTC <- .filterTCProjects()
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Name = PROJ_SHORT_NME, WBG_CNTRY_KEY,
                   Team_Leader = FULL_NME, Approval_Date = BD_APPRVL_DATE, 
                   Lending_Inst_Type = LENDING_INSTR_TYPE_NME,
                   Closing_Date = REVISED_CLS_DATE,Project_Amount,
                   DO_RATING, IP_RATING, ieg_Outcome,
                   ProjectOrder,url)
  # Financing products in Closed (ProjectOrder==3)
  dataTC <- filter(dataTC, Prod_Line == "Financing" & ProjectOrder==3)
  # compute IEG_Ouctome codes:
  dataTC <- mutate(dataTC, ieg_Outcome = ifelse(is.na(ieg_Outcome),"---",
                                                ifelse(substr(ieg_Outcome,1,1)=="H",
                                                       ifelse(substr(ieg_Outcome,8,8)=="S","HS","HU"),
                                                       ifelse(substr(ieg_Outcome,1,1)=="M",
                                                              ifelse(substr(ieg_Outcome,12,12)=="S","MS","MU"),
                                                              ifelse(substr(ieg_Outcome,1,1)=="N",
                                                                     ifelse(substr(ieg_Outcome,5,6)=="AP","NAP",ifelse(substr(ieg_Outcome,5,6)=="AV","NA","NR")),
                                                                     substr(ieg_Outcome,1,2))))))
  
  # filter by date range. Last 2 years
  dataTC <- filter(dataTC, (Closing_Date >= (Sys.Date() - 730)) | (is.na(Closing_Date)))
  count_ibrd <- nrow(dataTC) # will determine the size of the table
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Closing_Date),desc(Approval_Date))
  dataTC <- select(dataTC,-ProjectOrder, -Prod_Line)
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  #data <- select(data, -url)
  
  # format Amount
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  nrow <- nrow(data)
  data <- sapply(data, function(x) gsub("NA", "---", x, fixed=TRUE))
  if (nrow==1){
    data <- as.data.frame(t(data))
  } else{
    data <- as.data.frame(data)
  }
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("#", "\\#", data$Project_Name, fixed=TRUE)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataTC)-2)))
  }
  #
  names(data) <- c("Project ID", "Project Name", "CountryCodeISO2","Team Leader", "Approval Date", "Lending Inst. Type",
                   "Closing Date", "Commitment (US\\$M)",
                   "Project Rating DO", "Project Rating IP", "IEG Outcome Rating", "URL")
  return(data)
}


## ---- ASAActive ----
projectsTableASAActive <- function(){
  
  dataTC <- .filterTCProjects()
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Project_Name = PROJ_SHORT_NME, WBG_CNTRY_KEY,
                   Team_Leader = FULL_NME, Concept_Appr_Date = MGT_APPR_CONCEPT_DATE,
                   Approval_Date = BD_APPRVL_DATE, 
                   Prod_Line, PROD_LINE_CODE, RAS, Current_ExpBB = CURRENT_BB_COST,
                   FY_Expenses = CURRENT_FY_COST, Cum_ExpBB = CUMULATIVE_BB_COST,
                   Cum_Expenses = CUMULATIVE_FY_COST,
                   ProjectOrder,url)
  # AAA in Active (ProjectOrder==1)
  dataTC <- filter(dataTC, Prod_Line %in% c("Advisory Services and Analytics (ASA) IBRD","STANDARD PRODUCT") 
                   & ProjectOrder==1 & !(is.na(Approval_Date)))
  count_ibrd <- nrow(dataTC) # will determine the size of the table
  # filter by date range
  #dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Approval_Date))
  dataTC <- select(dataTC,-ProjectOrder,-Prod_Line)
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # Attach a link to Project ID
  data <- as.data.frame(data)
  #data <- select(data, -url)
  
  # scale Expenses to thousands 
  data$Current_ExpBB <- data$Current_ExpBB/1000
  data$Cum_ExpBB <- data$Cum_ExpBB/1000
  data$FY_Expenses <- data$FY_Expenses/1000
  data$Cum_Expenses <- data$Cum_Expenses/1000
  
  # format Amount
  data$FY_Expenses <- format(data$FY_Expenses, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  data$Cum_Expenses <- format(data$Cum_Expenses, digits=0, decimal.mark=".",
                              big.mark=",",small.mark=".", small.interval=3)
  data$Current_ExpBB <- format(data$Current_ExpBB, digits=0, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
  data$Cum_ExpBB <- format(data$Cum_ExpBB, digits=0, decimal.mark=".",
                           big.mark=",",small.mark=".", small.interval=3)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  nrow <- nrow(data)
  data <- sapply(data, function(x) gsub("NA", "---", x, fixed=TRUE))
  if (nrow==1){
    data <- as.data.frame(t(data))
  } else{
    data <- as.data.frame(data)
  }
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("#", "\\#", data$Project_Name, fixed=TRUE)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataTC)-2)))
  }
  
  names(data) <- c("Task ID", "Task Name", "CountryCodeISO2","Team Leader", "Concept Approval Date", "Output Approval Date", 
                   "Product Line","RAS (Y/N)","Current Expenditure BB (US\\$K)", "Current Expenditure Total (US\\$K)",
                   "Lifetime Expenditure BB (US\\$K)","Lifetime Expenditure Total (US\\$K)", "URL")
  return(data)
}


## ---- ASAClosed ----
projectsTableASAClosed <- function(){
  
  dataTC <- .filterTCProjects()
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Project_Name = PROJ_SHORT_NME, WBG_CNTRY_KEY,
                   Team_Leader = FULL_NME, Concept_Appr_Date = MGT_APPR_CONCEPT_DATE,
                   Approval_Date = BD_APPRVL_DATE, 
                   Prod_Line, PROD_LINE_CODE, RAS, Current_ExpBB = CURRENT_BB_COST,
                   FY_Expenses = CURRENT_FY_COST, Cum_ExpBB = CUMULATIVE_BB_COST,
                   Cum_Expenses = CUMULATIVE_FY_COST,
                   ProjectOrder,url)
  # AAA in Active (ProjectOrder==1)
  dataTC <- filter(dataTC, Prod_Line %in% c("Advisory Services and Analytics (ASA) IBRD","STANDARD PRODUCT") 
                   & ProjectOrder==3)
  count_ibrd <- nrow(dataTC) # will determine the size of the table
  # filter by date range
  #dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Approval_Date))
  dataTC <- select(dataTC,-ProjectOrder, -Prod_Line)
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # Attach a link to Project ID
  data <- as.data.frame(data)
  #data <- select(data, -url)
  
  # scale Expenses to thousands 
  data$Current_ExpBB <- data$Current_ExpBB/1000
  data$Cum_ExpBB <- data$Cum_ExpBB/1000
  data$FY_Expenses <- data$FY_Expenses/1000
  data$Cum_Expenses <- data$Cum_Expenses/1000
  
  # format Amount
  data$FY_Expenses <- format(data$FY_Expenses, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  data$Cum_Expenses <- format(data$Cum_Expenses, digits=0, decimal.mark=".",
                              big.mark=",",small.mark=".", small.interval=3)
  data$Current_ExpBB <- format(data$Current_ExpBB, digits=0, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
  data$Cum_ExpBB <- format(data$Cum_ExpBB, digits=0, decimal.mark=".",
                           big.mark=",",small.mark=".", small.interval=3)
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  nrow <- nrow(data)
  data <- sapply(data, function(x) gsub("NA", "---", x, fixed=TRUE))
  if (nrow==1){
    data <- as.data.frame(t(data))
  } else{
    data <- as.data.frame(data)
  }
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("#", "\\#", data$Project_Name, fixed=TRUE)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataTC)-2)))
  }
  
  names(data) <- c("Task ID", "Task Name", "CountryCodeISO2","Team Leader", "Concept Approval Date", "Output Approval Date", 
                   "Product Line","RAS (Y/N)","Current Expenditure BB (US\\$K)", "Current Expenditure Total (US\\$K)",
                   "Lifetime Expenditure BB (US\\$K)","Lifetime Expenditure Total (US\\$K)", "URL")
  
  return(data)
}


## ---- ASA_IFC Active, Closed or Pipeline ----
projectsTableASA_IFC <- function(status){
  
  pOrder <- ifelse(status=="Active",1,ifelse(status=="Closed",3,2))
  ### IFC projects ----------
  dataIFC <- .filterIFCProjects()
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Project_Name = PROJECT_NAME, COUNTRY_CODE,
                    Team_Leader = PROJECT_LEADER, PRODUCT_NAME, ClassType = PROJECT_CLASSIFICATION_TYPE,
                    Approval_Date = ASIP_APPROVAL_DATE, Closing_Date = IMPLEMENTATION_END_DATE,
                    Project_Status, Project_Amount = TOTAL_FUNDING, ITD_EXPENDITURES,
                    Current_Exp = PRORATED_TOTAL_FYTD_EXPENSE, ProjectOrder,url,Hold)
  dataIFC <- filter(dataIFC, ProjectOrder == pOrder)
  #dataIFC <- filter(dataIFC, (Approval_Date >= fromDate) & (Approval_Date <= toDate)) #select country
  count_ifc <- nrow(dataIFC) # will determine the size of the table
  # arrange
  dataIFC <- arrange(as.data.frame(dataIFC), desc(Approval_Date))
  dataIFC <- select(dataIFC,-ProjectOrder, -Project_Status) # drop ProjectOrder
  # remove duplicates
  data <- dataIFC[!duplicated(paste0(dataIFC$PROJ_ID,dataIFC$PRODUCT_NAME)),]
  # add amounts per PROJ_ID
  data <- data %>%
    group_by(PROJ_ID) %>%
    mutate(Project_Amount = sum(Project_Amount, na.rm=TRUE), 
           ITD_EXPENDITURES = sum(ITD_EXPENDITURES, na.rm=TRUE),
           Current_Exp = sum(Current_Exp, na.rm=TRUE))
  
  data <- data[!duplicated(data$PROJ_ID),]
  data <- select(data,-PRODUCT_NAME, -ClassType) # drop Product and class info
  
  # Map IFC country codes to ISO2 codes
  countries <- read.csv("C:/Users/asanchezrodelgo/Box Sync/BES data scraping/CountryClassification.csv")
  data <- merge(data, countries[,c("CountryCodeISO2","ISO3_IFC")], by.x = "COUNTRY_CODE", by.y="ISO3_IFC", all.x = TRUE)
  data <- select(data, PROJ_ID, Project_Name, CountryCodeISO2,
                 Team_Leader, Approval_Date, Closing_Date, Project_Amount, ITD_EXPENDITURES,
                 Current_Exp, url, Hold)
  
  # Scale amounts
  data$Project_Amount <- data$Project_Amount/1000
  data$Current_Exp <- data$Current_Exp/1000
  data$ITD_EXPENDITURES <- data$ITD_EXPENDITURES/1000
  
  data <- mutate(as.data.frame(data), PROJ_ID = ifelse(Hold == "Y",
                                                       paste0(PROJ_ID,' (Hold)'),
                                                       PROJ_ID))
  
  data <- select(data, -Hold)
  
  # format Amount
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  data$Current_Exp <- format(data$Current_Exp, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  data$ITD_EXPENDITURES <- format(data$ITD_EXPENDITURES, digits=0, decimal.mark=".",
                                  big.mark=",",small.mark=".", small.interval=3)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  nrow <- nrow(data)
  data <- sapply(data, function(x) gsub("NA", "---", x, fixed=TRUE))
  if (nrow==1){
    data <- as.data.frame(t(data))
  } else{
    data <- as.data.frame(data)
  }
  
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("#", "\\#", data$Project_Name, fixed=TRUE)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(data)-1)))
  }
  
  names(data) <- c("Project ID", "Project Name", "CountryCodeISO2","Team Leader","IP Approval Date", 
                   "Expected End Date","Approval Value (in US\\$K)", "Total Expenditures (in US\\$K)", 
                   "Current FY Expenditure (in US\\$K)", "URL")
  
  return(data)
}

