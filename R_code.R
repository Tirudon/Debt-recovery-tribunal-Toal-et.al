
# setwd("C:/Users/51950005/Desktop/Empirical Corp Finance/class3_bankruptcy laws and credit access/Assignment1/Final1")

# install required packages
install.packages("tidyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("robustHD")

library(tidyr)
library(lubridate)
library(ggplot2)
library(robustHD)


#============================================================================================================
#***** STEP1 - DOWNLOAD COMPANY NAMES & STATES *****
#============================================================================================================

# From ProwessIQ, download the list of non-financial firms as on 31-March-2001, together with other fonromation such as CIN code, prowess code, state, entity type, industry, etc.
# Import this data: Companydata.csv 

companydata <- read.csv("Companydata.csv", header = TRUE, sep =",",  dec =".", stringsAsFactors = FALSE)
companydata$state <- NULL # add a new blank column for State
companydata$state <- companydata$Registered.ofc.state # use the registered office state as the 'state' of the company
company_state <- companydata[!(is.na(companydata$state)|companydata$state==""),] # delete the firms (rows) for which state is not available (NA or blanks)
unique(company_state$Entity.type) # check the type of entities present in our data. Does it have govt entities?
company_state <- company_state[!(company_state$Entity.type=="Governments"),] # remove the givt firms
names(company_state$Company.Name) <- "CompanyName"
save(company_state, file="company_state.RData")
#write.csv(company_state, file = "company_state.csv")

#***Output: this file has the list of companies (11,536 firms) as on 31-March-2001 which are non financial and non govt entities, and which have a valid state populated in prowess IQ



#=========================================================================================================
#***** STEP 2: FIND TANGIBLE ASSET *****
#=========================================================================================================
# Download from prowess the asset information for all the filtered firms from the previous step. Import this sheet into R

tangasset <- read.delim("tangibleasset.txt", header = TRUE, sep ="|",  dec =".", stringsAsFactors = FALSE)
#View(tangasset) # the header and the top 5 rows in the downloaded file are misplaced in R. clean the rows
column_names <- tangasset[5,]
colnames(tangasset) <- column_names
labels(tangasset) # check the labels again if they are named correctly, in-line with prowess
tangasset <- tangasset[c(6:nrow(tangasset)),] # remove the first 5 rows fo the downloaded file, which are redundant
nrow(tangasset) #38279 records
tang_asset <- tangasset[!(is.na(tangasset$`Gross fixed assets`)|tangasset$`Gross fixed assets`==""),] # remvoe the firms for which the 1991 gross asset is zero
nrow(tang_asset) # 1796 firms with non blank asset values
#write.csv(tang_asset, "tang_nonzero_GrossAssets.csv")

# for the above 1796 firms (in tang_nonzero_GrossAssets.csv) extract the detailed asset information from prowess in the text file 'tagible assets2.txt'
# and than calculate tangible asset = total assets - intangible assets
tang <- read.delim("tangible assets2.txt", header = TRUE, sep="|", dec=".", stringsAsFactors = FALSE)
tang_col_names <- tang[5,]
colnames(tang) <- tang_col_names
#View(tang)
tang$TangibleAsset <- NULL
typeof(tang$`Total assets`) # character. This needs ti be converted to numbers
tang[,c(2:3)] <- sapply(tang[,c(2:3)], as.numeric) # convert character to numbers
tang[is.na(tang)] <- 0
tang <- tang[c(6:nrow(tang)),]
tang$TangibleAssets <- tang$`Total assets` - tang$`Intangible assets, gross` # calculate tangible assets = Total assets - Intangible assets
#View(tang)
tang <- tang[!(tang$TangibleAssets==0),] # reove the firms for which tangibleassets is zero
tang <- tang[!(tang$`Total assets`==0),] # remove the firms for which total assets is zero
nrow(tang) #1528 rows
save(tang, file="tangibleassets.RData")
#write.csv(tang, "tangibleassets.csv")

#***Output - This file contains the tangible assets for all the eligible firms (1528 firms)



#=====================================================================================================
#***** STEP 3 - FIND CHANGE IN LONG TERM BORROWINGS (LTB) *****
#---------------------------------------------------------------------------------------------
# STEP 3.1 - Find change in LTB from cash flow statements in Prowess
# STEP 3.2 - Find change in LTB from Balance Sheet (Liabilities section) in Prowess
# STEP 3.3 - COmbine LTB from both the sources
#=====================================================================================================

# download the borrowings data from prowess for the lsit of all firms from the previous step (for year 1988 to 2003)
# the prowess download should contain relevant borrowing line items from Cash flow as well as balance shet (liabilities section)
# import this file into R

AllBorrowings <- read.delim("Borrowings.txt", header = TRUE, sep ="|",  dec =".", stringsAsFactors = FALSE)
#View(AllBorrowings) 

# note in this download from prowess, each firm is present in a row in the data frame. and each row has 192 columns which represent 16 years x 12 borrowings data per year

# STEP 3.1 - LTB from CF is retreieved. 
# Reitreived direclty from the attribute "Cash inflow due to proceeds from long term borrowings", if this attribute is >0 and is available. 
# Else LTB from CF is calculated as {"cash inflow due to proceeds from total borrowings" - "cash inflow due to proceeds from short term borrowings"}
# if both the methods does not provide any data for LTB from CF, than LTB is retreived from BS data

# STEP 3.2 - LTB from BS is retreived 
# Prowess is not populated with long term borrowings for firms
# so these 7 columns are used to back calculate long term borrowings: (1)Total liabilities	(2)Total capital	(3)Reserves and funds	(4)Share application money & suspense account	(5)Deposits (accepted by commercial banks)	(6)Non-current liabilities	(7)Current liabilities & provisions
# I calculate Long term borrowings = Total Liabilities - Total capital - Reserves & funds - share appln money - Deposits - CUrrent liabilites

# STEP 3.3 - Cimbine LTB from CF and BS
# If Step 3.1 yields positive results than use LTB from CF. 
# If setp 3.1 does not yeild positive results but step 3.2 yields positive results, than use LTB from BS. 
# If both the steps yield negative results than update the LTB as 0. 
# If both the steps do not yield results for any year in the study, than drop the firm from the study


ncol(AllBorrowings) # 193
AllBorrowings[,c(2:ncol(AllBorrowings))] <- sapply(AllBorrowings[,c(2:ncol(AllBorrowings))], as.numeric) # convert the borrowing amounts to numeric
# warnings() # NAs created wherever the data was missing for any firm-year
colnames(AllBorrowings) <- c(0:192) # give placeholder column names as 0 to 192. 
#Note: here 192 columns corresponds to 12 borrowing attributes each year, for 16 years of data for each firm
# Out of the 12 columns for each year, first 6 columns have the CF attributes for borrowings, and the next 6 columns have the BS(Liab) attributes for borrowings
AllBorrowings <- AllBorrowings[c(6:nrow(AllBorrowings)),] # remove the top 5 rows which are redundant
#View(AllBorrowings)
typeof(AllBorrowings$`1`) # double

#----------------------------------------------------------------------
# STEP 3.1 - LTB FROM CASH FLOW 
#--------------------------------------------------------------------
# create a blank matrix with firm names and the long term borrowings for each year from 1988 to 2003
B_CF_LTB1 <- matrix(, nrow=nrow(AllBorrowings), ncol=17)

for (i in 1:16) {
  j = 2 + 12*(i-1)
  B_CF_LTB1[,i+1] = AllBorrowings[,j]
}


B_CF_LTB2 <- matrix(, nrow=nrow(AllBorrowings), ncol=17)

for (i in 1:16) {
  j = 4 + 12*(i-1)
  #B_CF_LTB2[,i+1] = AllBorrowings[,j] - AllBorrowings[,j+1]
  B_CF_LTB2[,i+1] = AllBorrowings[,j] - ifelse(is.na(AllBorrowings[,j+1]),0,AllBorrowings[,j+1])
  B_CF_LTB2[,i+1] = ifelse(B_CF_LTB2[,i+1]<0,0,B_CF_LTB2[,i+1])
  #B_CF_LTB4[,i+1] = ifelse(is.na(AllBorrowings[,j]),0,AllBorrowings[,j]) - ifelse(is.na(AllBorrowings[,j+1]),0,AllBorrowings[,j+1])
}

options(scipen = 999) # convert the resulting amount from scientific format to nuemric format

B_CF_LTB1 <- format(round(B_CF_LTB1, 2), nsmall = 2) # round up to two decimal places
B_CF_LTB2 <- format(round(B_CF_LTB2, 2), nsmall = 2) # round up to two decimal places

B_CF_LTB1 <- as.data.frame(B_CF_LTB1)
B_CF_LTB2 <- as.data.frame(B_CF_LTB2)

names(B_CF_LTB1)[1] <- "CompanyName"
names(B_CF_LTB2)[1] <- "CompanyName"
names(B_CF_LTB1)[c(2:17)] <- c(1988:2003) # name the columns
names(B_CF_LTB2)[c(2:17)] <- c(1988:2003) # name the columns

B_CF_LTB1$CompanyName <- AllBorrowings$`0`
B_CF_LTB2$CompanyName <- AllBorrowings$`0`


B_CF_LTB1 <- gather(B_CF_LTB1, key="Year", value="Borrowings", -CompanyName) # do pivoting to get data in panel data format
B_CF_LTB2 <- gather(B_CF_LTB2, key="Year", value="Borrowings", -CompanyName)

B_CF_LTB1 <- B_CF_LTB1[order(B_CF_LTB1$CompanyName),] # sort the panel data w.r.t. company name
B_CF_LTB2 <- B_CF_LTB2[order(B_CF_LTB2$CompanyName),]

# Now merge the LTB1 (direct cash inflwo from long term borrowings) and LTB2 (indirect value calulated as total cash inflow minum cash inflow from short term borrowings) values
Borrowings_CF <- merge(x=B_CF_LTB1, y=B_CF_LTB2, by.x=c("CompanyName", "Year"), by.y = c("CompanyName", "Year"))

typeof(Borrowings_CF$Borrowings.x) # the values are characters. need to convert to numbers
Borrowings_CF[,c(3,4)] <- sapply(Borrowings_CF[,c(3,4)], as.numeric)

#merge both values of LTB into one. Give precedence to LTB1 if available, else take the LTB2 values. Mark any negative value as 0.
Borrowings_CF$Borrowings <- ifelse(is.na(Borrowings_CF$Borrowings.x)== FALSE, 
                                   ifelse(Borrowings_CF$Borrowings.x>=0, Borrowings_CF$Borrowings.x, Borrowings_CF$Borrowings.y),
                                   ifelse( Borrowings_CF$Borrowings.y<=0, 0, Borrowings_CF$Borrowings.y))

Borrowings_CF <- Borrowings_CF[,c(1,2,5)] # this table has 1528*16 = 24448 rows (16 years data for each firm * 1528 firms)

Borrowings_CF_1980 <- filter(Borrowings_CF, Borrowings_CF$Year=="1988") # note that from prowess we do not have any CF LTB for year 1988. Hence I am dropping this year from the study

Borrowings_CF <- Borrowings_CF[!(Borrowings_CF$Year=="1988"),]
save(Borrowings_CF, file="Borrowings_CF.RData")
#write.csv(Borrowings_CF, file="Borrowings_CF.csv")

#***OUTPUT of step 3.1 - Pandel data of long term borrowings for the 1528 firms for each year, Obtained from CASH FLOW data from Prowess


#===================================================================================================
# STEP 3.2 - LTB FROM BALANCE SHEET (BS) LIABILITIES
#====================================================================================================

# download the borrowings data from Liabilities section in prowess for the lsit of all 1528 firms from the previous step (for year 1988 to 2004)
# the prowess download should contain relevant borrowing line items balance shet (liabilities section)
# import this file into R

AllBorrowings1 <- read.delim("BorrowingsBS.txt", header = TRUE, sep ="|",  dec =".", stringsAsFactors = FALSE)

# note in this download from prowess, each firm is present in a row in the data frame. and each row has 102 columns which represent 17 years x 6 borrowings data per year

# STEP 3.2 - LTB from BS is retreived 
# Prowess is not populated with long term borrowings for firms
# so these 7 columns are used to back calculate long term borrowings: (1)Total liabilities	(2)Total capital	(3)Reserves and funds	(4)Share application money & suspense account	(5)Deposits (accepted by commercial banks)	(6)Non-current liabilities	(7)Current liabilities & provisions
# I calculate Long term borrowings = Total Liabilities - Total capital - Reserves & funds - share appln money - Deposits - CUrrent liabilites

# If Step 3.1 yields positive results than use LTB from CF. 
# If setp 3.1 does not yeild positive results but step 3.2 yields positive results, than use LTB from BS. 
# If both the steps yield negative results than update the LTB as 0. 
# If both the steps do not yield results for any year in the study, than drop the firm from the study

AllBorrowings1[,c(2:ncol(AllBorrowings1))] <- sapply(AllBorrowings1[,c(2:ncol(AllBorrowings1))], as.numeric) # convert the borrowing amounts to numeric

# warnings() # NAs created wherever the data was missing for any firm-year

colnames(AllBorrowings1) <- c(0:102) # give placeholder column names as 0 to 102. 

#Note: here 102 columns corresponds to 6 borrowing attributes from BS each year, for 17 years of data for each firm

AllBorrowings1 <- AllBorrowings1[c(6:nrow(AllBorrowings1)),] # remove the top 5 rows which are redundant

typeof(AllBorrowings$`1`) # double

# AllBorrowings1[,c(2:ncol(AllBorrowings1))] <- sapply(AllBorrowings1[,c(2:ncol(AllBorrowings1))], as.numeric)
# AllBorrowings1[is.na(AllBorrowings1)] <- 0 # conver the missing or NA values of borrowings as zero. 

ncol(AllBorrowings1)

B_BS_level <- matrix(, nrow=nrow(AllBorrowings1), ncol=18) # create a blank matrix to store BS liabilties

for (i in 1:17)
{
  j = 2 + 6*(i-1)
  B_BS_level[,i+1] = AllBorrowings1[,j] - ifelse(is.na(AllBorrowings1[,j+1]),0,AllBorrowings1[,j+1])
  - ifelse(is.na(AllBorrowings1[,j+2]),0,AllBorrowings1[,j+2])
  - ifelse(is.na(AllBorrowings1[,j+3]),0,AllBorrowings1[,j+3])
  - ifelse(is.na(AllBorrowings1[,j+4]),0,AllBorrowings1[,j+4])
  - ifelse(is.na(AllBorrowings1[,j+5]),0,AllBorrowings1[,j+5])
}

options(scipen = 999) # convert the resulting amount from scientific format to nuemric format
#B_BS_level <- format(round(B_BS_level, 2), nsmall = 2) # round up to two decimal places

B_BS_level <- as.data.frame(B_BS_level)
names(B_BS_level)[1] <- "CompanyName"
names(B_BS_level)[c(2:18)] <- c(1988:2004)

B_BS_level$CompanyName <- AllBorrowings1$`0`

typeof(B_BS_level[1,4])
# B_BS_level[,c(2:ncol(B_BS_level))] <- sapply(B_BS_level[,c(2:ncol(B_BS_level))], as.numeric)


B_BS_change <- matrix(, nrow=nrow(AllBorrowings1), ncol=17) # create a blank matrix for storing borrowing changes from balance sheet

for (i in 1:16) {
  for (j in 1:nrow(AllBorrowings1)) {
    B_BS_change[j, i+1] = ifelse(is.na(B_BS_level[j,i+2]), NA, ifelse(is.na(B_BS_level[j,i+1]), NA, as.numeric(B_BS_level[j,i+2]) - as.numeric(B_BS_level[j,i+1])))
  }
}

options(scipen = 999) # convert the resulting amount from scientific format to nuemric format

B_BS_change[,1] <- AllBorrowings1$`0` # ass the first column to the matrix with the company names
B_BS_change <- as.data.frame(B_BS_change)

names(B_BS_change)[1] <- "CompanyName"
names(B_BS_change)[c(2:17)] <- c(1989:2004) # name the columns
B_BS_change <- gather(B_BS_change, key="Year", value="Borrowings", -CompanyName) # this has 1528 firms * 16 years = 24448 records
B_BS_change <- B_BS_change[order(B_BS_change$CompanyName),]
options(scipen = 999) # convert the resulting amount from scientific format to nuemric format
B_BS_change[,3] <- format(round(B_BS_change[,3], 2), nsmall = 2) # round up to two decimal places
B_BS_change[,3] <- sapply(B_BS_change[,3], as.numeric)

typeof(B_BS_change$Borrowings)
B_BS_change$Borrowings <- ifelse(B_BS_change$Borrowings<0,0,B_BS_change$Borrowings) # convert the negative change in BS borrowings to zero, which indicate no new long term loan availed in that record
Borrowings_BS <- B_BS_change
save(B_BS_change, file="Borrowings_BS.RData")
#write.csv(B_BS_change, file="Borrowings_BS.csv")

#***Output- The file contins the panel data of change in long term borrowings for firms, which is the balance sheet way of calculating new LTB availed by a firm in a aprticular year


#==============================================================================
# STEP 3.3 - COMBINE LTB FROM CASH FLOWS AND BALANCE SHEET
#==============================================================================

# join borrowings from CF and BS
# use CF borrowings if available, else use BS borrowings

Borrowing <- merge(x=Borrowings_CF, y=Borrowings_BS, by.x = c("CompanyName", "Year"), by.y = c("CompanyName", "Year"))
count((Borrowing[is.na(Borrowing$Borrowings.y),])) # 4043 out of 24448 records are NAs
count((Borrowing[is.na(Borrowing$Borrowings.x),])) # 15792 out of 24448 records are  NAs
# I use the CF derived data as my primary data and use BS data when CF data is unavailable
names(Borrowing)[3] <- "CF"
names(Borrowing)[4] <- "BS"
Borrowing <- Borrowing %>% 
  mutate(Borrowing = ifelse(is.na(BS) & is.na(CF), BS, ifelse(is.na(BS), CF, ifelse(is.na(CF), BS, CF) ) ))

Borrowing <- Borrowing[!(is.na(Borrowing$Borrowing)),]  # remove the NAs
Borrowing <- Borrowing[,c(1,2,5)] # this has 18927 firm year observations and each of this observation has non NA value for change in LTB
Borrowing$Year <- as.numeric(Borrowing$Year)

# each firm should have sufficient no of years of observations, say 13 or more for credible analysis 
freq <- as.data.frame(table(Borrowing$CompanyName)) # shows the frequency of each company in the borrowings data
remove_comp <- freq %>% filter(Freq < 14) %>% select(Var1)
as.vector(unlist(remove_comp))
count(remove_comp) # these 486 copanies need to be removed
Full_Borrowing <- Borrowing[!(Borrowing$CompanyName %in% remove_comp$Var1),] # 14606 records, where each firm has minimum 13 ovbsersation

# Now divide the Borrowing table into two different tables, for replicating Table 5 and Table & in the paper
Table5_Borrowing <- Borrowing[!(Borrowing$Year <= 1991),]
Table7_Borrowing <- Borrowing[!(Borrowing$Year >= 1994),]

# For table 5 specs, each firm should have 12 years of Borrowings data (i.e. 1992-2003). Remove the companies which have less than 12 observations per year from the Borrowings database
freq5 <- as.data.frame(table(Table5_Borrowing$CompanyName)) # the frequencies of each company in the borrowings data for Table 5 specs.
remove_comp_5 <- freq5 %>% filter(Freq<12) %>% select(Var1) # List of comapny names which do not have data from 1991-2003 for all the years
as.vector(unlist(remove_comp_5))
count(remove_comp_5) # 538 companies need to be dropped from the Borrowings data for Table 5 specifications
Table5_Borrowing <- Table5_Borrowing[!(Table5_Borrowing$CompanyName %in% remove_comp_5$Var1), ] # It has 11,880 obs. And each firm has 12 years of observations from 1991-2003

# For table 7 specs, each firm should have 5 years of Borrowings data (i.e. 1989-1993). Remove the companies which have less than 5 observations per year from the Borrowings database
freq7 <- as.data.frame(table(Table7_Borrowing$CompanyName)) # the frequencies of each company in the borrowings data for Table 5 specs.
remove_comp_7 <- freq7 %>%   filter(Freq < 5) %>%   select(Var1) 
as.vector(unlist(remove_comp_7))
count(remove_comp_7) # 1238 companies need to be dropped from the Borrowings data for Table 7 specifications
Table7_Borrowing <- Table7_Borrowing[!(Table7_Borrowing$CompanyName %in% remove_comp_7$Var1), ] # it has 1445 observation. And each firm has all the 5 years of observation i.e. 1989-1993

save(Borrowing, file="Borrowings_Final.RData")
#write.csv(Borrowing, file="Borrowings_Final.csv")

#***OUTPUT is the Borrowing file which has the increase in LTB borrowings for each firm each year after combining the LTB values from both CF and BS


#==============================================================================================
#***** STEP 4 - MERGE COMPANY, STATE, TANGIBLES, & BORROWINGS DATA INTO SINGLE PANEL DATA *****
#==============================================================================================

# first merge companystate table with the tangibles table
merge1 <- merge(x=tang, y=company_state, by.x = "Company Name", by.y = "Company.Name", all.x = TRUE)
merge1 <- merge1[,c(1,4,20,21,26)]
merge1 <- merge1[!(is.na(merge1$state)),]
merge1 <- merge1[!(is.na(merge1$TangibleAssets)),] # 1483 firms

# than merge the Full borrowings data with the merge1 table
merge2 <- merge(x=Full_Borrowing, y=merge1, by.x = "CompanyName", by.y = "Company Name", all.x = TRUE)
merge2 <- merge2[order(merge2[,1], merge2[,2]),]
merge2 <- merge2 %>% 
  filter(is.na(TangibleAssets)==FALSE | TangibleAssets == 0)

# merge borrowings data for Table 5 with the merge1 table
merge_table5 <- merge(x=Table5_Borrowing, y=merge1, by.x = "CompanyName", by.y = "Company Name", all.x = TRUE)
merge_table5 <- merge_table5[order(merge_table5[,1], merge_table5[,2]),]
merge_table5 <- merge_table5 %>% 
  filter(is.na(TangibleAssets)==FALSE | TangibleAssets == 0)

# merge borrowings data for Table 7 with the merge1 table
merge_table7 <- merge(x=Table7_Borrowing, y=merge1, by.x = "CompanyName", by.y = "Company Name", all.x = TRUE)
merge_table7 <- merge_table7[order(merge_table7[,1], merge_table7[,2]),]
merge_table7 <- merge_table7 %>% 
  filter(is.na(TangibleAssets)==FALSE | TangibleAssets == 0)


#***OUTPUT - Merged data of firm-years with tangible assets and LTBs separately for Table V and Table VII.


#==========================================================================
#***** STEP 5: ADD DRT DATA *****
#==========================================================================

# Import DRTdata.csv. THis file has the state id, borrower cluster id and the drtdate for each state
# join this with the panel data from step 4
# create the dummy drtown (=1, after DRT is passed)
# The drt date, needs to be converted in terms of FINANCIAL YEAR as per INdian accoutning to match with the Accoutning Year of the Borrowings 

DRTdata <- read.csv("DRTdata.csv", header = TRUE, sep =",",  dec =".", stringsAsFactors = FALSE)

# join DRT data with merge_all table
merge_all <- merge(x=merge2, y=DRTdata, by.x = "state", by.y = "state1", all.x = TRUE)
names(merge_all)[1] <- "StateName"
names(merge_all)[8] <- "state"
merge_all <- merge_all %>% mutate(acctdate = make_date(Year,"03","31")) # convert Year to accoutning Year (FY)
merge_all$bdrtdts <- as.Date(merge_all$bdrtdts, format="%d-%m-%Y") #convert bdrtdts to date format
# this gives the fianncial year rather than the actual year (FY in India ends on 31-March)
merge_all$drtyear <- ifelse(month(merge_all$bdrtdts)<=3, year(merge_all$bdrtdts), year(merge_all$bdrtdts)+1) 
# create the drt dummy
merge_all$drtown <- ifelse(merge_all$acctdate >= merge_all$bdrtdts, 1, 0)
merge_all <- merge_all[order(merge_all[,2], merge_all[,3]),]
merge_all <- merge_all[!(is.na(merge_all$state)),]


# join DRT data with merge_table5 data
merge_table5 <- merge(x=merge_table5, y=DRTdata, by.x="state", by.y="state1", all.x = TRUE)
names(merge_table5)[1] <- "StateName"
names(merge_table5)[8] <- "state"
merge_table5 <- merge_table5 %>% mutate(acctdate = make_date(Year,"03","31")) # convert Year to accoutning Year (FY)
merge_table5$bdrtdts <- as.Date(merge_table5$bdrtdts, format="%d-%m-%Y") #convert bdrtdts to date format
# derive the fianncial year rather than the actual year (FY in India ends on 31-March)
merge_table5$drtyear <- ifelse(month(merge_table5$bdrtdts)<=3, year(merge_table5$bdrtdts), year(merge_table5$bdrtdts)+1) 
# create the drt dummy
merge_table5$drtown <- ifelse(merge_table5$acctdate >= merge_table5$bdrtdts, 1, 0)
merge_table5 <- merge_table5[order(merge_table5[,2], merge_table5[,3]),]
merge_table5 <- merge_table5[!(is.na(merge_table5$state)),]

write.csv(merge_table5, file = "PanelData_table5.csv")

# join DRT data with merge_table7 data
merge_table7 <- merge(x=merge_table7, y=DRTdata, by.x="state", by.y="state1", all.x = TRUE)
names(merge_table7)[1] <- "StateName"
names(merge_table7)[8] <- "state"
merge_table7 <- merge_table7 %>% mutate(acctdate = make_date(Year,"03","31")) # convert Year to accoutning Year (FY)
merge_table7$bdrtdts <- as.Date(merge_table7$bdrtdts, format="%d-%m-%Y") #convert bdrtdts to date format
# this gives the fianncial year rather than the actual year (FY in India ends on 31-March)
merge_table7$drtyear <- ifelse(month(merge_table7$bdrtdts)<=3, year(merge_table7$bdrtdts), year(merge_table7$bdrtdts)+1) 
# create the drt dummy
merge_table7$drtown <- ifelse(merge_table7$acctdate >= merge_table7$bdrtdts, 1, 0)
merge_table7 <- merge_table7[order(merge_table7[,2], merge_table7[,3]),]
merge_table7 <- merge_table7[!(is.na(merge_table7$state)),]

write.csv(merge_table7, file = "PanelData_table7.csv")

#*** OUTPUT - FInal panel data which contains firm years, Tangibe assets, FIrm borrowings, DRT indicators



#=============================================================================================
#***** STEP 6 - GRAPHICAL EVIDENCE FOR EFFECT OF DRT *****
#=============================================================================================


#panel <- read.csv("PanelData_table.csv", header=TRUE, sep=",", dec=".", stringsAsFactors =FALSE)

table5_graphdata <- merge_table5

#graphdata <- rbind(merge_table5, merge_table7)

# table5 graph data (Avg Borrowings vs year w.r.t DRT)
table5_graphdata$eventyear <- NULL # define a attribute to capture the year w.r.t. DRT year as 0
table5_graphdata$eventyear <- table5_graphdata$Year - table5_graphdata$drtyear

table5_graphdata$Borrowing_w <- winsorize(table5_graphdata$Borrowing, probs = c(0.005, 0.995))
table5_graphdata$TangibleAssets_w <- winsorize(table5_graphdata$TangibleAssets, probs = c(0.005, 0.995))
quantile(table5_graphdata$TangibleAssets_w)

# graph1 - Avg of change in borrowing vs event year
graph1 <- 
  table5_graphdata %>% 
  select(CompanyName, Year, Borrowing, TangibleAssets, Borrowing_w, TangibleAssets_w, drtown, eventyear) %>% 
  group_by(eventyear) %>% 
  summarize(avg_chg_in_borrowing = mean(Borrowing)) %>% 
  ggplot(aes(x=eventyear, y=avg_chg_in_borrowing)) + geom_point() + geom_line() 

graph1

# graph2 - post drt period: change in Borrowing vs Initial Size of firms
graph2 <- table5_graphdata %>% 
  select(CompanyName, Borrowing, TangibleAssets, drtown, eventyear, Borrowing_w, TangibleAssets_w) %>% 
  filter(drtown == 1) %>% 
  # group_by(drtown) %>% 
  group_by(CompanyName) %>% 
  summarize(avg_chg_in_borrowing = mean(Borrowing), initial_size = mean(TangibleAssets), b_w = mean(Borrowing_w), a_w = mean(TangibleAssets_w)) %>% 
  mutate(big_small = ifelse(initial_size >= median(initial_size), "Big", "Small"))


graph2_smallfirms <- graph2 %>%  filter(big_small == "Small") %>% 
  ggplot(aes(x=initial_size, y=avg_chg_in_borrowing)) + geom_point() + geom_smooth(method = 'lm', se=FALSE)

graph2_bigfirms <- graph2 %>%  filter(big_small == "Big") %>% 
  ggplot(aes(x=initial_size, y=avg_chg_in_borrowing)) + geom_point() + geom_smooth(method = 'lm', se=FALSE)

graph2_smallfirms

graph2_bigfirms


#*** OUTPUT - Graphical evidence for Table V

