############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")
library(treemap)
font <- "Open Sans"

widthFig <- 10
heightFig <- widthFig * 1.5

############################################################################################
### Create the data to visualize
############################################################################################

data <- data.frame(
	what = c('UBS market\ncapitalisation', "UBS Private Banking's asset under management",
		"UBS\nyearly\noperating\nincome", 'UBS loss\ndue to\nsubprime\ncrisis in 2008', 'Switzerland GDP',
		'Switzerland total trade export', 'Global pharmaceuticals market'),
	value = c(78.06, 1705, 27.732,  37.7, 631.173, 308.4, 300),
	group = factor(c('market', 'market', 'market', 'market', 'country', 'country', 'pharma')),
	source = c('http://www.relbanks.com/worlds-top-banks/market-cap', 'Scorpio partnership private banking benchmark 2013',
		'http://www.ubs.com/global/de/about_ubs/investor_relations/annualreporting/2013/_jcr_content/par/teaserbox_6c86/teaser/linklist/link.644305709.file/bGluay9wYXRoPS9jb250ZW50L2RhbS9zdGF0aWMvZ2xvYmFsL2ludmVzdG9yX3JlbGF0aW9ucy9hbm51YWwyMDEzL0FubnVhbFJldmlldzIwMTMtZW4ucGRm/AnnualReview2013-en.pdf',
		'http://en.wikipedia.org/wiki/List_of_writedowns_due_to_subprime_crisis', 'World Bank',
		'https://www.cia.gov/library/publications/the-world-factbook/geos/sz.html',
		'http://www.who.int/trade/glossary/story073/en/')
	)
data$label <- paste(data$what, "\n\n$",  data$value, " bn", sep = "")

pdf(file = "UBS_treemap.pdf", width = widthFig, height = heightFig, family = font)
treemap(data[data$what != "UBS Private Banking's asset under management",], index=c("group", "label"),
	vSize="value",type="index", palette = swi_9palette[-3],
	fontsize.labels=c(0, 15),   fontsize.title = 0, border.lwds = c(3), border.col = "#efe9e0",
	fontfamily.title = font, fontfamily.labels = font, align.labels = c( "left", "top"))

treemap(data[data$what == "UBS Private Banking's asset under management",], index=c("group", "label"),
	vSize="value",type="index", palette = swi_9palette[-3],
	fontsize.labels=c(0, 15),   fontsize.title = 0, border.lwds = c(3), border.col = "#efe9e0",
	fontfamily.title = font, fontfamily.labels = font, align.labels =  c( "left", "top"))
dev.off()


