############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")
library(treemap)

widthFig <- 10
heightFig <- (widthFig * 1.4)

############################################################################################
### Create the data to visualize
############################################################################################

data <- data.frame(
	what = c('UBS market\ncapitalisation', "UBS Private Banking's assets under management (2012)",
		"UBS yearly\noperating\nincome\n(2013)", 'UBS loss\ndue to\nsubprime\ncrisis\nin 2008', 'Switzerland annual GDP (2013)',
		'Switzerland total trade export (2012)', 'Global pharmaceuticals market', 'European Union annual budget', 'Morocco annual GDP (2013)'),
	value = c(78.06, 1705, 27.732,  37.7, 650.8, 308.4, 300, 142 * 1.36, 104.4),
	group = factor(c('market', 'market', 'market', 'market', 'country', 'country', 'pharma', 'continent', 'country2')),
	source =
		c('http://www.relbanks.com/worlds-top-banks/market-cap', 'Scorpio partnership private banking benchmark 2013',
		'http://www.ubs.com/global/de/about_ubs/investor_relations/annualreporting/2013/_jcr_content/par/teaserbox_6c86/teaser/linklist/link.644305709.file/bGluay9wYXRoPS9jb250ZW50L2RhbS9zdGF0aWMvZ2xvYmFsL2ludmVzdG9yX3JlbGF0aW9ucy9hbm51YWwyMDEzL0FubnVhbFJldmlldzIwMTMtZW4ucGRm/AnnualReview2013-en.pdf',
		'http://en.wikipedia.org/wiki/List_of_writedowns_due_to_subprime_crisis', 'World Bank',
		'https://www.cia.gov/library/publications/the-world-factbook/geos/sz.html',
		'http://www.who.int/trade/glossary/story073/en/', 'http://europa.eu/pol/financ/index_en.htm', 'World Bank')
	)
data$label <- paste(data$what, "\n $",  data$value, " bn", sep = "")

whatpage <- "UBS Private Banking's assets under management (2012)"
balance <- data[data$what == whatpage,'value'] - sum(data[data$what != whatpage,'value'])
if(abs(balance) > 5) {
	stop("unbalanced values!\t", balance)
}

pdf(file = "UBS_treemap.pdf", width = widthFig, height = heightFig, family = font)
treemap(data[data$what != whatpage,], index=c("group", "label"),
	vSize="value",type="index", palette = swi_9palette[-(2)],
	fontsize.labels=c(0, 14),   fontsize.title = 10, title ="", border.lwds = c(3), border.col = "#efe9e0",
	fontfamily.title = font, fontfamily.labels = font, align.labels = c( "left", "top"), overlap.labels = 1)

treemap(data[data$what == whatpage,], index=c("group", "label"),
	vSize="value",type="index", palette = swi_9palette[-(2)],
	fontsize.labels=c(0, 14),   fontsize.title = 10, title = "", border.lwds = c(3), border.col = "#efe9e0",
	fontfamily.title = font, fontfamily.labels = font, align.labels =  c( "left", "top"))
dev.off()


pdf(file = "UBS_treemap_long.pdf", width = widthFig, height = heightFig, family = font)
treemap(data, index=c("group", "label"),
	vSize="value",type="index", palette = swi_9palette[-(2)],
	fontsize.labels=c(0, 16),   fontsize.title = 10, title ="1", border.lwds = c(3), border.col = "#efe9e0",
	fontfamily.title = font, fontfamily.labels = font, align.labels = c( "left", "top"), overlap.labels = 1)
dev.off()


