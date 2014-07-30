############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")

widthFig <- 10
heightFig <- widthFig * 1.4

############################################################################################
### Treemap of the jobs in switzerland: colored by sectors, area by number, hue by
############################################################################################
library(treemap)

data.ch <- read.csv("laborsta2B_prod_cleaned.csv")

#Add line breaks

data.ch$Activity <- factor(sapply(as.character(data.ch$Activity), function(ac) {
	if(nchar(ac) > 18) {
		gsub('(.{1,18})(\\s|$)', '\\1\n', ac)
	}	else {
		ac
	}
}))

# Clustering by sector
#png("jobActivityCH_treemap.png", width = widthFig, height = heightFig)
pdf("jobActivityCH_treemap.pdf", width = widthFig, height = heightFig, pointsize = 14)
treemap(data.ch, index=c("Sector", "Activity"), vSize="Job",type="index", palette = swi_9palette[c(1,6,4)],
fontsize.labels=c(0, 18),   fontsize.title = 20, border.lwds = c(2,3), border.col = "#efe9e0",
	title = paste("Distribution of Switzerland's", round(sum(data.ch$Job) / 1000, 1), "million jobs by economic activity"),
	fontfamily.title = font, fontfamily.labels = font)
dev.off()


############################################################################################
### Compare percentage of population working in finance vs different countries
############################################################################################

data.table <- read.csv("laborsta2B-refined-csv.csv")

# discard Luxembourg
data.table <- data.table[data.table[,2] != 'LU',]


# split the data by country and return the ratio of job in finance intermediation for the latest year available

table <- do.call(rbind, by(data.table, data.table$COUNTRY, function(dd) {
	year <- max(dd$year)
	ddd <- dd[dd$year == year & dd$CLASSIFICATION == 'ISIC-Rev.3',]

	dddd <- cbind(ddd[1,c(1:2)], year = year, total = ddd[ddd$CODE.SUB.CLASSIFICATION == "00_",'value'], finance =
		ddd[ddd$CODE.SUB.CLASSIFICATION == "10_",'value'])

	dddd$ratio <- (dddd$finance / dddd$total) * 100
	dddd
}))
table$rratio <- round(table$ratio, 1)
table$COUNTRY <- reorder(table$COUNTRY, table$rratio)
table$color <- 'a'
table[table$COUNTRY=="Switzerland",'color'] <- 'b'


# png("jobInFinance_bar.png", width = widthFig, height = heightFig)
# p1 <- ggplot(table, aes(x = COUNTRY, y =  rratio)) + geom_bar(stat = "identity", aes(fill = color)) +
# 	ggtheme_xgrid + coord_flip ()+ scale_fill_manual(values = swi_22palette[c(3,9)]) +
# 	theme(text=element_text(family=font , size = 35), axis.ticks.y = element_blank()) + xlab("") +
# 	geom_text(data=table,aes(label=format(rratio)), hjust=1.5, size = 7.7, color = "#efe9e0", family = font) +
# 	ylab("Percentage of the total job in financial services (bank, pension funds, insurance, ...)") +
# 	theme(legend.position = "none", panel.border = element_blank())
# p1
# dev.off()

write.table(table, "jobInFinance.csv", sep = ",")

############################################################################################
### GDP composition
############################################################################################

data.table <- read.csv("SNA_TABLE1_Data_84542764-4f5f-4898-8095-77a64a35c7d7.tsv", sep="\t")
colnames(data.table)[6] <- 'value'
data.table$value <- as.numeric(gsub(" +", "", as.character(data.table$value)))

# split the data by country and return the ratio of job in finance  for the latest year available
table <- do.call(rbind, by(data.table, data.table$Country, function(dd) {
	cat("\n", as.character(dd[1,1]))
	year <- max(dd$Time)
	ddd <- dd[dd$Time == year,]
	#browser()
	dddd <- data.frame(Country = as.character(ddd[1,1]), year = year,
		total = ddd[ddd$Transaction == "Gross domestic product (output approach)",'value'],
		finance =ddd[ddd$Transaction == "Financial and insurance activities (ISIC rev4)",'value'])

	dddd$ratio <- (dddd$finance / dddd$total) * 100
	dddd
}))
countries_fullNames <- c('Australia', 'Austria', 'European Union (28 countries)', 'Ireland', 'Japan', 'Netherlands',
	'Italy','Germany', 'France','Portugal','Spain','United Kingdom', 'Switzerland')

table <- table[table$Country %in% countries_fullNames,]

table$rratio <- round(table$ratio, 1)
table$Country <- reorder(table$Country, table$rratio)
table$color <- 'a'
table[table$Country=="Switzerland",'color'] <- 'b'

# png("financeInGDP_bar.png", width = widthFig, height = heightFig)
# p2 <- ggplot(table, aes(x = Country, y =  rratio)) + geom_bar(stat = "identity", aes(fill = color)) +
# 	ggtheme_xgrid + coord_flip ()+ scale_fill_manual(values = swi_22palette[c(3,9)]) +
# 	theme(text=element_text(family=font , size = 35), axis.ticks.y = element_blank()) + xlab("") +
# 	geom_text(data=table,aes(label=format(rratio)), hjust=1.5, size = 7.7, color = "#efe9e0", family = font) +
# 	ylab("Contribution of the financial sector to the total GDP (output method) in %") +
# 	theme(legend.position = "none", panel.border = element_blank())
# p2
# dev.off()

write.table(table, "financeInGDP.csv", sep = ",")


###### Bank asset to GDP ########
data.table1 <- read.csv("../data/Bank Assets (As % Of GDP).csv", sep=",", stringsAsFactors = F)
data.table2 <- read.csv("../data/Bank Assets Per Capita (USD).csv", sep=",", stringsAsFactors = F)
data.table3 <- read.csv("../data/Bank Assets (USD).csv", sep=",", stringsAsFactors = F)

stopifnot(nrow(data.table1) == nrow(data.table2), nrow(data.table1) == nrow(data.table3))

countries_fullNames2 <- c('Ireland', 'Japan', "USA", 'Italy','Germany', 'France', 'Portugal','Spain',
'United Kingdom', 'Switzerland', 'Russia')

getLastdata <- function(d, colName) {
	d <- d[d$Countries %in% countries_fullNames2,]
	do.call(rbind, lapply( 1:nrow(d), function(i) {
		res <- d[i,c(1,max(which(!is.na(d[i,]))))]
		#browser()
		df <- data.frame(country = res[1,1], res = res[1,2], year = gsub("^X", "", names(res)[2]))
		colnames(df)[2] <- colName
		colnames(df)[3] <- paste('year', colName, sep = "_")
		df
	}))
}
dat <- cbind(getLastdata(data.table1, "assets as % of GDP"), getLastdata(data.table2, "assets per capita [USD millions]")[,2:3],
 getLastdata(data.table3, "assets [USD billions]")[,2:3] )
write.table(dat, "assetByGDP_Capita_abs.csv", sep = ",", row.names = F)


