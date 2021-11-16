setwd("~/master/price_comparison/")
source("functions.R")
source("variables.R")

### CUSTOM RUN - Find sheet and define
gs4_find()
dailySheet <- "1ZswodizrcZlyhfHcjUs5KISTyzTLt2lZqs25eFXVXUA"

## SETUP DAILY SHEET
today <- Sys.Date() %>% as.character()
dailySheet <- gs4_find(as.character(today))[2] %>% toString()

## Create daily sheet
create_daily_sheet(today)

## Create meta data
create_metadata(today)

## MANUAL TELMORE WRAPUP
telmore_append_manual <- function() {
  
  range_clear(ss = telmore_operatorSheetId)
  header <- data_frame("web-scraper-order", 
                       "web-scraper-start-url", 
                       "product-list", 
                       "product-list-href", 
                       "subscriptionName", 
                       "subscriptionMonthlyPrice", 
                       "repaymentPeriod", 
                       "terminalModel", 
                       "terminalVariant", 
                       "terminalPrice", 
                       "terminalDiscount", 
                       "paymentMonthly", 
                       "paymentMinimum")
  sheet_append(ss = telmore_operatorSheetId, data = header)
  
  raw_df <- read_sheet(ss = telmore_operatorids[1])
  sheet_append(ss = telmore_operatorSheetId, data = raw_df)
  
  raw_df <- read_sheet(ss = telmore_operatorids[2])
  sheet_append(ss = telmore_operatorSheetId, data = raw_df)
  
  raw_df <- read_sheet(ss = telmore_operatorids[3])
  sheet_append(ss = telmore_operatorSheetId, data = raw_df)
  
  raw_df <- read_sheet(ss = telmore_operatorids[4])
  sheet_append(ss = telmore_operatorSheetId, data = raw_df)
  
  raw_df <- read_sheet(ss = telmore_operatorids[5])
  sheet_append(ss = telmore_operatorSheetId, data = raw_df)
}

## Daily update of all operators
dailyRun()


