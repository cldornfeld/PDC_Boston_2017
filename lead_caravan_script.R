library(jsonlite)
library(RColorBrewer)
library(ggplot2)
library(hexbin)
library(reshape2)
library(plyr)
library(dplyr)
library(lattice)

#test jsonlite functionality
all.equal("/Users/catherinedornfeld/Desktop/UW-Madison/CPL/PDC_Boston/events-feb-1.json", fromJSON(toJSON("/Users/catherinedornfeld/Desktop/UW-Madison/CPL/PDC_Boston/events-feb-1.json")))

#read JSON to data frame
lead_data <- stream_in(file("/Users/catherinedornfeld/Desktop/UW-Madison/CPL/PDC_Boston/events-feb-1.json"))
flatten(lead_data, recursive = TRUE)
lead_data$timestamp <- as.POSIXct(lead_data$timestamp/1000, origin="1970-01-01")
head(lead_data$timestamp)
dim(lead_data)
str(lead_data)

#read CSV
lead_min <- read.csv("/Users/catherinedornfeld/Desktop/UW-Madison/CPL/PDC_Boston/lead_min.csv")
lead_short <- read.csv("/Users/catherinedornfeld/Desktop/UW-Madison/CPL/PDC_Boston/lead_short.csv")
lead_python <- read.csv("/Users/catherinedornfeld/Desktop/UW-Madison/CPL/PDC_Boston/lead_python.csv")
lead_feb4_r1 <- read.csv("/Users/catherinedornfeld/Desktop/UW-Madison/CPL/PDC_Boston/lead_feb4_r1_timecorrect.csv")

###trying to fix the data
# bad = "allGroups"
# bad = lead_data$allGroups
# lead_data2 <- select(lead_data, -allGroups)
# str(lead_data2)

#write to CSV keeping only first ten variables (_id, timestamp, key, gameCode, description, group, itemNo, item, oldPrice, newPrice)
write.csv(lead_data[,1:10], file = "/Users/catherinedornfeld/Desktop/UW-Madison/CPL/PDC_Boston/lead_short.csv", na="")
write.csv(lead_data2, file = "/Users/catherinedornfeld/Desktop/UW-Madison/CPL/PDC_Boston/lead_short2.csv", na="")

#SUBSETS - separate item prices and market prices
lead_item <- subset(lead_feb4_r1, newPrice < 11)
lead_market_r1 <- subset(lead_feb4_r1, gameCode == "51092" & newPrice > 11)
lead_market_r2 <- subset(lead_feb4_r1, gameCode == "2416" & newPrice > 11)
lead_trade <- subset(lead_feb4_r1, description == "TradeCausedUpdate")
lead_item_r1 <- subset(lead_feb4_r1, gameCode == "51092"& newPrice < 11)
lead_item_r2 <- subset(lead_feb4_r1, gameCode == "2416" & newPrice < 11)
lead_cash_r1 <- subset(lead_feb4_r1, gameCode == "51092" & key == "CashingOutResource")
lead_cash_r2 <- subset(lead_feb4_r1, gameCode == "2416" & key == "CashingOutResource")

#lead_concise$StockPrice <- ifelse(lead_concise$key == "StockPriceChange",1,0)
#lead_no555 <- subset(lead_short, newPrice > 0.01 | newPrice < 11) 
#lead_no555NA <- na.omit(lead_no555)  # deletes all rows with a missing (NA)

#individual items
lead_plat <- subset(lead_item, item = "platinum")
lead_pearl <- subset(lead_item, item = "pearl") 
lead_irid <- subset(lead_item, item = "iridium")
lead_sapp <- subset(lead_item, item = "sapphire")
lead_wood <- subset(lead_item, item = "wood")
lead_cott <- subset(lead_item, item = "cotton")
lead_cats <- subset(lead_item, item = "cats")
lead_food <- subset(lead_item, item = "food")
head(lead_item)

#generic plots for item and market prices
hist(lead_item$newPrice, col=brewer.pal(8,"Greys"), main="Histogram of Item Prices")
hist(lead_market$newPrice, col=brewer.pal(8,"Reds"), main="Historgram of Market Prices")
plot(lead_item$newPrice, type="l", ylab = "Item New Price")
plot(lead_market$newPrice, type="l", ylab = "Market New Price")
barplot(lead_market$newPrice) #upward trend over time
plot(newPrice ~ oldPrice, data = lead_item) #strong linear trend for item prices
boxplot(lead_item$newPrice, col = "blue")
boxplot(lead_market$newPrice, col = "red")
plot(newPrice ~ humanTime,data=lead_python, cex.lab=0.2)

#specific items
plot(lead_plat$newPrice, type="l")
plot(x=lead_plat$newPrice, ylab = "New Price", main = "Platinum Prices Over Time")

#hexbin plot for market price over time
a=hexbin(lead_market$newPrice,xbins=25)
rf <-colorRampPalette(rev(brewer.pal(12,"Set3"))) ##not that helpful
plot(a, ylab="", main = "Market Prices") ## colramp=rf)

#play with mosaic plots - mosaicplot(dataframe)
#lead_mos <- subset(lead_short, )
#mosaicplot(lead_mos) #not working

#heatmap of groups and traded items
heat_prep1 <- acast(lead_feb4_r1, group~item, value.var="humanTime")
heatmap(heat_prep1, ylab="Group Number", main="Prices by Group and Item") ##BEST GRAPH
heat_prep2 <- acast(lead_item, group~timestamp, value.var="newPrice")
heatmap(heat_prep2, main="Prices by Group and Time")
#who trades with whom
heat_prep3 <- acast(lead_feb4_r1, recipientGroup~requesterGroup)
heatmap(heat_prep3, main="Recipient and Requester")
#trades - groups over time
heat_prep4 <- acast(lead_trade, humanTime~group, value.var="item")
heatmap(heat_prep4, main="Item by Time and Group")
#cash for groups over time (in years) - Round 1
heat_prep5 <- acast(lead_cash_r1, group~year, value.var="cash")
heatmap(heat_prep5, main="R1: Group and Year by Cash")
#cash for groups over time (in years) - Round 2
heat_prep6 <- acast(lead_cash_r2, group~year, value.var="cash")
heatmap(heat_prep6, main="R2: Group and Year by Cash")

##GOOD GRAPHS - ggplot2
qplot(oldPrice, newPrice, data = lead_item, color = item) #nice
qplot(oldPrice, newPrice, data = lead_item, color = item, size = description) #nice
qplot(oldPrice, newPrice, data = lead_item, color = item, size = group, alpha = 0.7) #little easier to read
qplot(humanTime, item, data = lead_item, color = group, size = description, alpha = 0.50)
#market graph by group
qplot(humanTime, newPrice, data = lead_market_r1, color = group)
qplot(humanTime, newPrice, data = lead_market_r2, color = group)
#cash over time - DON'T SHOW
qplot(year, cash, data = lead_cash_r1, color = group, size = price)
qplot(year, cash, data = lead_cash_r2, color = group, size = price)
#cash over time by groups
lead_cash_r1$group2 <- factor(lead_cash_r1$group, levels = c('0','1','2','3','4','6','7'))
lead_cash_r2$group2 <- factor(lead_cash_r2$group, levels = c('0','1','2','3','4','6','7'))
ggplot(lead_cash_r1, aes(year,cash)) +
  geom_point(aes(color=group)) +
  geom_smooth(method="lm") +
  coord_cartesian() +
  scale_color_gradient()
ggplot(lead_cash_r2, aes(year,cash)) +
  geom_point(aes(color=group)) +
  geom_smooth(method="lm") +
  coord_cartesian() +
  scale_color_gradient()

#for CPL
qplot(newPrice, data = lead_item, color = item) #interesting
qplot(description, data = lead_item, color = item) #interesting - category size

