setwd("~/master/price_comparison/")
source("functions.R")
source("variables.R")

## TELIA
scrapeTelia()

## 3.dk
scrape3dk()

## Telenor
scrapeTelenor()

## YOUSEE
scrapeYouSee()

## TELMORE
scrapeTelmore()

## CALLME
scrapeCallMe()

## CBBMOBIL
scrapeCBBMobil()

## GreenTel
scrapeGreenTel()