############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")
font <- "Archivo Narrow"

widthFig <- 640 * 2
heightFig <- widthFig * 2/3

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
png("jobActivityCH_treemap.png", width = widthFig, height = heightFig)
#pdf("jobActivityCH_treemap.pdf", width = 13, height = 10, pointsize = 14)
treemap(data.ch, index=c("Sector", "Activity"), vSize="Job",type="index", palette = swi_9palette[c(1,6,4)],
fontsize.labels=c(0, 29),   fontsize.title = 33, border.lwds = c(2,3), border.col = "#efe9e0",
	title = paste(round(sum(data.ch$Job) / 1000, 1), "millions of job in Switzerland their repartition by economic activity"),
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


png("jobInFinance_bar.png", width = widthFig, height = heightFig)
p1 <- ggplot(table, aes(x = COUNTRY, y =  rratio)) + geom_bar(stat = "identity", aes(fill = color)) +
	ggtheme_xgrid + coord_flip ()+ scale_fill_manual(values = swi_22palette[c(3,9)]) +
	theme(text=element_text(family=font , size = 35), axis.ticks.y = element_blank()) + xlab("") +
	geom_text(data=table,aes(label=format(rratio)), hjust=1.5, size = 7.7, color = "#efe9e0", family = font) +
	ylab("Percentage of the total job in financial services (bank, pension funds, insurance, ...)") +
	theme(legend.position = "none", panel.border = element_blank())
p1
dev.off()

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
	'China', 'Italy','Germany', 'France','Portugal','Spain','United Kingdom', 'Switzerland')

table <- table[table$Country %in% countries_fullNames,]

table$rratio <- round(table$ratio, 1)
table$Country <- reorder(table$Country, table$rratio)
table$color <- 'a'
table[table$Country=="Switzerland",'color'] <- 'b'

png("financeInGDP_bar.png", width = widthFig, height = heightFig)
p2 <- ggplot(table, aes(x = Country, y =  rratio)) + geom_bar(stat = "identity", aes(fill = color)) +
	ggtheme_xgrid + coord_flip ()+ scale_fill_manual(values = swi_22palette[c(3,9)]) +
	theme(text=element_text(family=font , size = 35), axis.ticks.y = element_blank()) + xlab("") +
	geom_text(data=table,aes(label=format(rratio)), hjust=1.5, size = 7.7, color = "#efe9e0", family = font) +
	ylab("Contribution of the financial sector to the total GDP (output method) in %") +
	theme(legend.position = "none", panel.border = element_blank())
p2
dev.off()

write.table(table, "financeInGDP.csv", sep = ",")


###### Bank asset to GDP ########
data.table <- read.csv("../data/Bank Assets (As % Of GDP).csv", sep=",", stringsAsFactors = F)
countries_fullNames2 <- c('Australia', 'Austria', 'Ireland', 'Japan', "USA", 'Italy','Germany', 'France','Portugal','Spain',
'United Kingdom', 'Switzerland', 'Russia')


data.table<- data.table[data.table$Countries %in% countries_fullNames2,]

dat <- do.call(rbind, lapply( 1:nrow(data.table), function(i) {
	res <- data.table[i,c(1,max(which(!is.na(data.table[i,]))))]
	#browser()
	data.frame(country = res[1,1], value = res[1,2], year = gsub("^X", "", names(res)[2]))
}))

write.table(table, "asset2GDP.csv", sep = ",")



############################################################################################
### Bank capital to assets ratio : WDI FB.BNK.CAPA.ZS
############################################################################################
# The ratio of capital to assets measures bank solvency and resiliency— and the extent to which banks can deal with unexpected losses. With banks under stress in the global financial crisis, the likelihood and cost of bank failures led countries to review their banking regulations. Many major economies have required higher minimum capital ratios to ensure bank capacity to cover liabilities and protect depositors and other lenders. In the United States the average ratio of capital to assets rose to 11.2 percent in 2011, up from 9.3 percent in 2008. Also maintaining higher ratios were euro area countries (6.7 percent) and the United Kingdom (5.1 percent). Japan and Germany, by con- trast, kept rates below 5 percent because of their banking conditions.

#
# library(WDI)
#
# countries.iso2 <- c('AT', "CH", "DE", "GR", "ES", "FR", "IT", "PT", "GB", "US", 'JP', 'BR', 'RU', 'CN')
#
#
# bankRatio <- WDI(country = c(countries.iso2), indicator = "FB.BNK.CAPA.ZS",
#   start = 1991,  end = 2012, extra = FALSE, cache = NULL)
#
# #Clean up the data a bit
# bankRatio <- rename(bankRatio, replace = c("SL.UEM.1524.ZS" = "unemployment"))
# bankRatio$unemployment <- round(bankRatio$unemployment, 1)
#
#
# # Create the chart
# bankRatioPlot <- nPlot(
# 	unemployment ~ year,
# 	data = bankRatio,
# 	group = "country",
# 	type = "lineChart")
#
# # Add axis labels and format the tooltip
# bankRatioPlot$yAxis(axisLabel = "Youth (age 15-24) unemployement in %", width = 55)
# bankRatioPlot$xAxis(axisLabel = "Year")
# bankRatioPlot$chart(tooltipContent = "#! function(key, x, y){
#       return '<h3>' + key + '</h3>' +
#       '<p>' + y + ' in ' + x + '</p>'
#       } !#")
# ids <- unique(bankRatio$iso2c)
# country.selec <- as.logical(!ids %in% c('AT','CH','DE','ES','PT','GB', 'GR'))
# bankRatioPlot$set(disabled = country.selec)
# bankRatioPlot$publish("line chart World Bank youth unemployment", host = "rpubs")
# bankRatioPlot


