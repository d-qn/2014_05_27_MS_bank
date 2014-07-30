############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")

widthFig <- 10
heightFig <- widthFig * 1.35

############################################################################################
### Market cap banks
############################################################################################

data <- read.csv("inkscape/banks_byMarketCapAndOthers.csv")
dat <- data[data$type == "bank",]
dat$Bank <- reorder(dat$Bank, dat$Market.cap)

gridSize <- 100
y = seq(50, 250, by = gridSize)
scale.labs <- data.frame(             ## define scale labels
  x = rep(1, length(y)),
  y = y,
  labels = as.character(y)
)
scale.grid <- data.frame(            ## define grid for plot
  x = rep(1:(nlevels(factor(dat$Bank))), each = length(y)),
  y = rep(gridSize, nlevels(factor(dat$Bank)) * length(y))
)

plot <- ggplot(data = dat, aes(x = Bank, y = Market.cap)) + geom_bar(aes(fill = Country), stat ="identity", width = 1, color = "white") +
	ggtheme + scale_fill_manual(values = swi_22palette[-c(2,3)]) +  coord_polar(direction = -1) + xlab("") + ylab("") +
	geom_text(aes(y = Market.cap -15, label = Market.cap), hjust = 0.5, vjust = 2, size = 2, color="white") +
	theme(axis.text.x = element_text(
	   angle= 90 + (360 -5) / length(unique(dat$Bank)) * seq_along(dat$Bank)
	   )
	 ) + geom_bar(
	    data = scale.grid,              ## implement manual grid
	    aes(x = x, y = y, fill = NA), width = 1,
	    colour = "white", position = "stack", stat = "identity"
	) # +
# geom_text(
#     data = scale.labs, aes(x = x, y = y, label = labels, fill = NULL), size = 2.5
#     )
#

pdf("marketCap.pdf",  width = widthFig, height = heightFig, family = font)
print(plot)
dev.off()

