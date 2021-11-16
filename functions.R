library(tidyverse)
library(stringr)
library(readr)
library(googlesheets4)
library(httr)
library(googledrive)
library(clipr)
library(chron)
library(magrittr)
library(scales)

## 3DK
scrape3dk <- function() {
  operatorName <- "3dk"
  raw_df <- read_sheet(ss = dk3_operatorSheetId)
  dataTrans3dk <- function(sh) {
    sh$operatorName <- operatorName
    names(sh)[names(sh) == "product-list-href"] <- "url"
    names(sh)[names(sh) == "product-list"] <- "urlClickText"
    sh$subscriptionNameFull <- sh$subscriptionName
    sh$subscriptionName <- str_extract(sh$subscriptionName, "[^Abonnement:].*") %>% tolower() %>% str_trim()
    sh$repaymentPeriodMonth <- str_extract(sh$repaymentPeriod, regex("\\d+")) %>% as.numeric()
    sh$terminalModelFull <- sh$terminalModel
    sh$terminalModel <- str_extract(sh$terminalModel, regex("[^,]+")) %>% tolower()
    sh$terminalModelBrand <- str_extract(sh$terminalModel, regex("[^ ]+")) %>% tolower()
    sh$terminalVariant <- gsub(" ", "", str_extract(sh$terminalVariant, regex("[^Størrelse: ].*")))
    sh$terminalDiscount <- str_replace(sh$terminalDiscount, "null", "0")
    sh$terminalDiscount <- gsub("\\.", "", str_extract(sh$terminalDiscount, regex("(?<=Rabat med abonnement: -)(.*)(?=kr)"))) %>% str_trim() %>% as.numeric() 
    sh$terminalPrice <- gsub(",", ".", gsub("\\.", "", str_extract(sh$terminalPrice, regex("(?<=Mobilens pris)(.*)(?=kr)")))) %>% str_trim() %>% as.numeric()
    sh$subscriptionMonthlyPrice <- str_extract(sh$subscriptionMonthlyPrice, regex("\\d+")) %>% as.numeric()
    sh$paymentMonthly <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMonthly, regex("[^kr]+")))) %>% as.numeric()
    sh$paymentMinimum <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMinimum, regex("(?<=\\n)(.*)(?=kr)")))) %>% str_trim() %>% as.numeric()
    sh$dateTime <- Sys.time()
    sh
  }
  df <- dataTrans3dk(raw_df)
  df <- df[,order(colnames(df))]
  sheet_write(ss = dailySheet, data = df, operatorName)
}

## Telenor
scrapeTelenor <- function() {
  operatorName <- "Telenor"
  raw_df <- read_sheet(ss = telenor_operatorSheetId)
  dataTransTelenor <- function(sh) {
    sh$operatorName <- operatorName
    names(sh)[names(sh) == "product-list-href"] <- "url"
    names(sh)[names(sh) == "product-list"] <- "urlClickText"
    sh$urlClickText <- "Vælg"
    sh$subscriptionNameFull <- sh$subscriptionName
    sh$subscriptionName <- sh$subscriptionName %>% tolower() %>% str_trim()
    sh$repaymentPeriodMonth <- str_extract(sh$repaymentPeriod, regex("\\d+")) %>% as.numeric() %>% replace_na(0)
    sh$terminalModelFull <- sh$terminalModel
    sh$terminalModel <- str_extract(sh$terminalModel, regex("[^,]+")) %>% tolower()
    sh$terminalModelBrand <- str_extract(sh$terminalModel, regex("[^ ]+")) %>% tolower()
    sh$terminalVariant <- gsub(" ", "", sh$terminalVariant)
    sh$terminalDiscount <- gsub("\\.", "", str_extract(sh$terminalDiscount, regex("[^,-]+"))) %>% str_trim() %>% as.numeric() 
    sh$terminalDiscount[is.na(sh$terminalDiscount)] <- 0
    sh$terminalPrice <- gsub(",", ".", gsub("\\.", "", str_extract(sh$terminalPrice, regex("[^,-]+")))) %>% str_trim() %>% as.numeric()
    sh$subscriptionMonthlyPrice <- str_extract(sh$subscriptionMonthlyPrice, regex("\\d+")) %>% as.numeric()
    sh$paymentMonthly <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMonthly, regex("[^,-]+")))) %>% as.numeric()
    sh$paymentMinimum <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMinimum, regex("[^,-]+")))) %>% as.numeric()
    sh$dateTime <- Sys.time()
    sh
  }
  df <- dataTransTelenor(raw_df)
  df <- df[,order(colnames(df))]
  sheet_write(ss = dailySheet, data = df, operatorName)
}

## Yousee

scrapeYouSee <- function() {
  operatorName <- "youSee"
  raw_df <- read_sheet(ss = yousee_operatorSheetId)
  dataTransYouSee <- function(sh) {
    sh$operatorName <- "youSee"
    names(sh)[names(sh) == "product-list-href"] <- "url"
    names(sh)[names(sh) == "product-list"] <- "urlClickText"
    sh$subscriptionNameFull <- sh$subscriptionName
    sh$subscriptionName <- str_extract(sh$subscriptionName, regex(".+?(?=\\d{3})")) %>% tolower()
    sh$repaymentPeriodMonth <- str_extract(sh$repaymentPeriod, regex("\\d+ mdr")) %>% str_extract(regex("\\d+")) %>% as.numeric() %>% replace_na(0)
    sh$terminalModelFull <- paste0(sh$terminalModelBrand, " ", sh$terminalModel)
    sh$terminalModel <- paste0(sh$terminalModelBrand, " ", sh$terminalModel) %>% tolower()
    sh$terminalModelBrand <- sh$terminalModelBrand 
    sh$terminalVariant <- gsub(" ", "", str_extract(sh$terminalVariant, regex("\\d.*"))) %>% str_trim()
    sh$terminalDiscount <- gsub("\\.", "", str_extract(sh$terminalDiscount, regex("(?<=-)(.*)(?=kr)"))) %>% str_trim() %>% as.numeric() 
    sh$terminalPrice <- gsub(",", ".", gsub("\\.", "", str_extract(sh$terminalPrice, regex("(.*)(?=kr)")))) %>% str_trim() %>% as.numeric()
    sh$subscriptionMonthlyPrice <- str_extract(sh$subscriptionMonthlyPrice, regex("\\d+")) %>% as.numeric()
    sh$paymentMonthly <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMonthly, regex("[^kr]+")))) %>% as.numeric()
    sh$paymentMinimum <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMinimum, regex("[^kr]+")))) %>% str_trim() %>% as.numeric()
    sh$dateTime <- Sys.time()
    sh
  }
  df <- dataTransYouSee(raw_df)
  df <- df[,order(colnames(df))]
  sheet_write(ss = dailySheet, data = df, operatorName)
}

## Telia
scrapeTelia <- function() {
  operatorName <- "Telia"
  raw_df <- read_sheet(ss = telia_operatorSheetId)
  dataTransTelia <- function(sh) {
    sh$operatorName <- operatorName
    names(sh)[names(sh) == "product-list-href"] <- "url"
    names(sh)[names(sh) == "product-list"] <- "urlClickText"
    sh$subscriptionNameFull <- sh$subscriptionName
    sh$subscriptionName <- str_extract(sh$subscriptionName, "[^-]+") %>% tolower() %>% str_trim()
    sh$repaymentPeriodMonth <- str_extract(sh$repaymentPeriod, regex("\\d+")) %>% as.numeric()
    sh$terminalModelFull <- sh$terminalModel
    sh$terminalModel <- str_extract(sh$terminalModel, regex("[^,]+")) %>% tolower()
    sh$terminalModelBrand <- str_extract(sh$terminalModel, regex("[^ ]+")) %>% tolower()
    sh$terminalVariant <- gsub(" ", "", str_extract(sh$terminalVariant, regex("\\d+[ ]?GB"))) %>% str_trim()
    sh$terminalDiscount <- str_replace(sh$terminalDiscount, "null", "0")
    sh$terminalDiscount <- gsub("\\.", "", str_extract(sh$terminalDiscount, regex("[^kr]+"))) %>% str_trim() %>% as.numeric() 
    sh$terminalPrice <- gsub(",", ".", gsub("\\.", "", str_extract(sh$terminalPrice, regex("[^kr]+")))) %>% str_trim() %>% as.numeric()
    sh$subscriptionMonthlyPrice <- str_extract(sh$subscriptionMonthlyPrice, regex("\\d+")) %>% as.numeric()
    sh$paymentMonthly <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMonthly, regex("[^kr]+")))) %>% as.numeric()
    sh$paymentMinimum <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMinimum, regex("[^kr]+")))) %>% as.numeric()
    sh$dateTime <- Sys.time()
    sh
  }
  df <- dataTransTelia(raw_df)
  df <- df[,order(colnames(df))]
  sheet_write(ss = dailySheet, data = df, operatorName)
}

## Telmore
scrapeTelmore <- function() {
  operatorName <- "Telmore"
  raw_df <- read_sheet(ss = telmore_operatorSheetId)
  dataTransTelmore <- function(sh) {
    sh$operatorName <- operatorName
    names(sh)[names(sh) == "product-list-href"] <- "url"
    names(sh)[names(sh) == "product-list"] <- "urlClickText"
    sh$subscriptionNameFull <- sh$subscriptionName
    sh$subscriptionName <- sh$subscriptionName
    sh$repaymentPeriodMonth <- str_extract(sh$repaymentPeriod, regex("\\d+ mdr")) %>% str_extract(regex("\\d+")) %>% as.numeric() %>% replace_na(0)
    sh$terminalModelFull <- sh$terminalModel
    sh$terminalModel <- str_extract(sh$terminalModel, regex("[^,]+")) %>% tolower()
    sh$terminalModelBrand <- str_extract(sh$terminalModel, regex("[^ ]+")) %>% tolower()
    sh$terminalVariant <- gsub(" ", "", str_extract(sh$terminalVariant, regex("[^Størrelse: ].*")))
    sh$terminalDiscount <- gsub(",", ".", gsub("\\.", "", str_extract(sh$terminalDiscount, regex("[^kr]+")))) %>% as.numeric()
    sh$terminalPrice <- gsub(",", ".", gsub("\\.", "", str_extract(sh$terminalPrice, regex("[^kr]+")))) %>% as.numeric()
    sh$subscriptionMonthlyPrice <- gsub("\\.", "", sh$subscriptionMonthlyPrice) %>% str_extract(regex("\\d+")) %>% as.numeric()
    sh$paymentMonthly <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMonthly, regex("[^kr]+")))) %>% as.numeric()
    sh$paymentMinimum <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMinimum, regex("[^kr]+")))) %>% str_trim() %>% as.numeric()
    sh$dateTime <- Sys.time()
    sh
  }
  df <- dataTransTelmore(raw_df)
  df <- df[,order(colnames(df))]
  df <- df %>% select(-`web-scraper-order`, -url, -urlClickText, -`web-scraper-start-url`) %>% unique()
  sheet_write(ss = dailySheet, data = df, operatorName)
}

## CallMe
scrapeCallMe <- function() {
  operatorName <- "CallMe"
  raw_df <- read_sheet(ss = callme_operatorSheetId)
  dataTransCallMe <- function(sh) {
    sh$operatorName <- operatorName
    raw_df <- read_sheet(ss = callme_operatorSheetId)
    names(sh)[names(sh) == "product-list-href"] <- "url"
    names(sh)[names(sh) == "product-list"] <- "urlClickText"
    sh$subscriptionNameFull <- sh$subscriptionName
    sh$subscriptionName <- sh$subscriptionName %>% tolower()
    sh$repaymentPeriodMonth <- str_extract(sh$repaymentPeriod, regex("\\d+ måneder")) %>% str_extract(regex("\\d+")) %>% as.numeric() %>% replace_na(0)
    sh$terminalModelFull <- sh$terminalModel
    sh$terminalModel <- str_extract(sh$terminalModel, regex("[^,]+")) %>% tolower()
    sh$terminalModelBrand <- str_extract(sh$terminalModel, regex("[^ ]+")) %>% tolower()
    sh$terminalVariant <- gsub(" ", "", str_extract(sh$terminalVariant, regex("[^Størrelse: ].*")))
    sh$terminalDiscount <- 0 
    sh$terminalPrice <- 0
    sh$subscriptionMonthlyPrice <- str_extract(sh$subscriptionMonthlyPrice, regex("\\d+")) %>% as.numeric()
    sh$paymentMonthly <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMonthly, regex("[^kr]+")))) %>% as.numeric()
    sh$paymentMinimum <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMinimum, regex("[^kr]+")))) %>% str_trim() %>% as.numeric()
    sh$dateTime <- Sys.time()
    sh
  }
  df <- dataTransCallMe(raw_df)
  df <- df[,order(colnames(df))]
  sheet_write(data = df, ss = dailySheet, operatorName)
}

## CBBMobil
scrapeCBBMobil <- function() {
  operatorName <- "CBBMobil"
  raw_df <- read_sheet(cbbmobil_operatorSheetId)
  
  dataTransCBBMobil <- function(sh) {
    sh$operatorName <- operatorName
    names(sh)[names(sh) == "product-list-href"] <- "url"
    names(sh)[names(sh) == "product-list"] <- "urlClickText"
    sh$subscriptionNameFull <- sh$subscriptionName
    sh$subscriptionName <- sh$subscriptionName %>% tolower()
    sh$repaymentPeriodMonth <- str_extract(sh$repaymentPeriod, regex("\\d+ måneder")) %>% str_extract(regex("\\d+")) %>% as.numeric() %>% replace_na(0)
    sh$terminalModelFull <- sh$terminalModel
    sh$terminalModel <- str_extract(sh$terminalModel, regex("[^,]+")) %>% tolower()
    sh$terminalModelBrand <- sh$url %>% str_extract(regex("(?<=mobiltelefoner\\/)(.*)(?=\\/)")) %>% str_extract(regex("^[^\\/]+"))
    sh$terminalVariant <- gsub(" ", "", sh$terminalVariant) %>% str_trim()
    sh$terminalDiscount <- 0 
    sh$terminalPrice <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMonthly, regex("[^kr]+")))) %>% as.numeric()
    sh$subscriptionMonthlyPrice <- str_extract(sh$subscriptionMonthlyPrice, regex("\\d+")) %>% as.numeric()
    sh$paymentMonthly <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMonthly, regex("[^kr]+")))) %>% as.numeric()
    sh$paymentMinimum <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMinimum, regex("[^kr]+")))) %>% str_trim() %>% as.numeric() %>% replace_na(0)
    sh$dateTime <- Sys.time()
    sh
  }
  df <- dataTransCBBMobil(raw_df)
  df <- df[,order(colnames(df))]
  sheet_write(ss = dailySheet, data = df, operatorName)
}

## GreenTel
scrapeGreenTel <- function() {
  operatorName <- "GreenTel"
  raw_df <- read_sheet(ss = greentel_operatorSheetId)
  dataTransGreenTel <- function(sh) {
    sh$operatorName <- operatorName
    names(sh)[names(sh) == "product-list-href"] <- "url"
    names(sh)[names(sh) == "product-list"] <- "urlClickText"
    sh$subscriptionNameFull <- sh$subscriptionName
    sh$subscriptionName <- sh$subscriptionName %>% tolower()
    sh$repaymentPeriodMonth <- str_extract(sh$repaymentPeriod, regex("\\d+ mdr")) %>% str_extract(regex("\\d+")) %>% as.numeric() %>% replace_na(0)
    sh$terminalModelFull <- sh$terminalModel
    sh$terminalModel <- str_extract(sh$terminalModel, regex("[^,]+")) %>% tolower()
    sh$terminalModelBrand <- str_extract(sh$terminalModel, regex("[^ ]+")) %>% tolower()
    sh$terminalVariant <- gsub(" ", "", str_extract(sh$terminalVariant, regex("\\d+[ ]?GB"))) %>% str_trim()
    sh$terminalDiscount <- 0 
    sh$terminalPrice <- 0
    sh$subscriptionMonthlyPrice <- str_extract(sh$subscriptionMonthlyPrice, regex("\\d+")) %>% as.numeric()
    sh$paymentMonthly <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMonthly, regex("[^kr]+")))) %>% as.numeric()
    sh$paymentMinimum <- gsub(",", ".", gsub("\\.", "", str_extract(sh$paymentMinimum, regex("[^kr]+")))) %>% str_trim() %>% as.numeric()
    sh$dateTime <- Sys.time()
    sh
  }
  df <- dataTransGreenTel(raw_df)
  df <- df[,order(colnames(df))]
  sheet_write(ss = dailySheet, data = df, operatorName)
}


## DAILY RUN
dailyRun <- function() {

for(i in 1:10){
  try({
    scrape3dk()
    scrapeYouSee()
    scrapeTelenor()
    scrapeTelia()
    scrapeTelmore()
    scrapeCallMe()
    scrapeCBBMobil()
    scrapeGreenTel()
    break
  }, silent = FALSE)
}
}

### CREATE METADATA
create_metadata <- function(sheetName) {
  sheetName <- gs4_find()[1,2] %>% as.character()
  metadata_template <- read.csv("~/master/price_comparison/metadata_template.csv", header=FALSE)
  metadata_template$V2[1] <- as.character(today)
  metadata_template$V2[2] <- format(Sys.time(), "%H:%M:%S")
  total_runtime <- chron(times=format(Sys.time(), "%H:%M:%S")) - chron(times="07:00:00")
  metadata_template$V2[3] <- as.character(total_runtime)
  sheet_write(ss = sheetName, data = metadata_template, "Metadata")
}

  ## CREATE DAILY SHEET
create_daily_sheet <- function(sheetName) {
  ## Create daily sheet
  gs4_create(name = sheetName, sheets = "Metadata")
  dailySheet <- gs4_find(sheetName)[2] %>% toString()
  
  ## Change permissions and add link to clipboard
  drive_get(as_id(dailySheet)) %>% drive_share(role = "reader", type = "anyone")
  drive_get(as_id(dailySheet))$drive_resource[[1]]$webViewLink %>% write_clip()
}

