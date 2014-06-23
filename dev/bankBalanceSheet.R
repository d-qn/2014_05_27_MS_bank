############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")
font <- "Archivo Narrow"

widthFig <- 640
heightFig <- widthFig * 3

############################################################################################
###		Load data
############################################################################################
library(pxR)


datapx <- read.px("ae1d6c84-b36a-4cb0-b801-74580edc4101.px")
table <- datapx$DATA[[1]]

# get only all banks data
table <- table[table$Bank == "All banks",]

cc <- 'Switzerland'
#cc <- 'United States'
tt<- table[which(table$Country == cc & as.character(table$Year) == "2009"),]

sum(tt[6:10,'value']) - sum(tt[12:17,'value'], na.rm =T)

total <- table[table$Item == "25. End-year total" & as.character(table$Year) == "2009",]
total <- total[!is.na(total$value),]