############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")
font <- "Archivo Narrow"


############################################################################################
### Treemap of the jobs in switzerland: colored by sectors, area by number, hue by
############################################################################################
library(treemap)

data.ch <- read.csv("laborsta2B_prod.csv")

#Add line breaks

data.ch$Activity <- factor(sapply(as.character(data.ch$Activity), function(ac) {
	if(nchar(ac) > 30) {
		gsub('(.{1,30})(\\s|$)', '\\1\n', ac)
	}	else {
		ac
	}
}))

# Clustering by sector
pdf("jobActivityCH_treemap.pdf", width = 13, height = 10, pointsize = 14)
treemap(data.ch, index=c("Sector", "Activity"), vSize="Job",type="index", palette = swi_9palette[c(1,4,6)],
fontsize.labels=c(0, 20),   fontsize.title = 27,
	title = paste(round(sum(data.ch$Job) / 1000, 1), "millions of job in Switzerland, their repartition by economic activity"),
	fontfamily.title = font, fontfamily.labels = font)
dev.off()




############################################################################################
### Compare percentage of population working in finance vs different countries
############################################################################################

data.table <- read.csv("laborsta2B-refined-csv.csv")

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

# rCharts - NVD3
# n1 <- nPlot(rratio ~ COUNTRY, data = table, type = 'discreteBarChart')
# n1$chart(color = swi_22palette)
# n1$chart(staggerLabels = T)
# n1

pdf("jobInFinance_bar.pdf", width = 13, height = 10)
p1 <- ggplot(table, aes(x = COUNTRY, y =  rratio)) + geom_bar(stat = "identity", aes(fill = color)) +
	ggtheme_xgrid + coord_flip ()+ scale_fill_manual(values = swi_22palette[c(3,9)]) +
	theme(text=element_text(family=font , size=12), axis.ticks.y = element_blank()) + xlab("") +
	geom_text(data=table,aes(label=format(rratio)), hjust=1.5, size = 3.5, color = "#efe9e0", family = font) +
	ylab("Percentage of the total job in financial services (bank, pension funds, insurance, ...)") +
	theme(panel.background = element_rect(fill = '#f7f5ed'), plot.background=element_rect(fill="#f7f5ed"),
	legend.position = "none", panel.border = element_blank())
p1
dev.off()

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
countries_fullNames <- c('Australia', 'Austria', 'European Union (28 countries)', 'Ireland', 'Japan', 'Luxembourg', 'Netherlands',
	'China', 'Italy','Germany', 'France','Portugal','Spain','United Kingdom', 'Switzerland')

table <- table[table$Country %in% countries_fullNames,]

table$rratio <- round(table$ratio, 1)
table$Country <- reorder(table$Country, table$rratio)
table$color <- 'a'
table[table$Country=="Switzerland",'color'] <- 'b'

pdf("financeInGDP_bar.pdf", width = 13, height = 10)
p2 <- ggplot(table, aes(x = Country, y =  rratio)) + geom_bar(stat = "identity", aes(fill = color)) +
	ggtheme_xgrid + coord_flip ()+ scale_fill_manual(values = swi_22palette[c(3,9)]) +
	theme(text=element_text(family=font , size=12), axis.ticks.y = element_blank()) + xlab("") +
	geom_text(data=table,aes(label=format(rratio)), hjust=1.5, size = 3.5, color = "#efe9e0", family = font) +
	ylab("Contribution of the financial sector to the total GDP (output method) in %") +
	theme(panel.background = element_rect(fill = '#f7f5ed'), plot.background=element_rect(fill="#f7f5ed"),
	legend.position = "none", panel.border = element_blank())
p2
dev.off()

############################################################################################
### GDP composition
############################################################################################

library(quantmod)


banks <- c('WFC', 'UBS', 'CS')
getQuote(banks, what = yahooQF(c("Market Capitalization")))
sapply(banks, function(b) getFinancials(b, "IS", "A"))