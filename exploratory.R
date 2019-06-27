#
beer_path="/Users/wailunchung/Documents/GitHub/MSDS-6306-Doing-Data-Science/Unit 7/Beers.csv"
brewery_path="/Users/wailunchung/Documents/GitHub/MSDS-6306-Doing-Data-Science/Unit 7/Breweries.csv"
beer_df=read.csv(beer_path)
brewery_df=read.csv(brewery_path)

summary(beer_df)
summary(brewery_df)

#Q
#ABV NA:62
#IBU NA:1005
#style need clean? e.g. India Pale Ale = IPA, blank, etc do we need this? 

#is Brewery_id = Brew_ID?
#Brewery name
#51 state? do we count DC as state? 

#??
#style 

library(dplyr)
distinct(beer_df,Style,sort)
?distinct
distinct(brewery_df,Name)
distinct(brewery_df,City)
distinct(brewery_df,State)

#library(plyr)
library(plyr)
state_freq <-count(brewery_df$State)
state_freq

library(ggplot2)
#2d ggplot 1-descending, 2-color by region, 3-legend
ggplot(state_freq, aes(x=reorder(x,-freq),y=freq,fill=x)) + 
  geom_bar(stat="identity") + 
  labs(title="state count",subtitle="",x="State",y="freq",caption="") +
  theme(plot.title=element_text(hjust=0.5),axis.text.x = element_text(angle = 90, hjust = 1))

#try merge (left join)
beer_brew_df<-merge(x=beer_df,y=brewery_df,by.x="Brewery_id",by.y="Brew_ID",all.x = TRUE)
head(beer_brew_df,10)
test_df<-beer_brew_df[which(is.na(beer_brew_df$ABV)),names(beer_brew_df)]
test_df

#2d ggplot 1-descending, 2-color by region, 3-legend
ggplot(beer_brew_df, aes(Brew_ID))

#show distribution
hist(beer_df$ABV)
hist(beer_df$IBU)
#show NA
summary(beer_df$ABV)
summary(beer_df$IBU)

#2410 obs
beer_df
beer_brew_df
test<-subset(beer_brew_df, is.na(beer_brew_df$Brewery_id) == TRUE, select = c("Brewery_id"))
test

boxplot(beer_df$ABV)
boxplot(beer_df$IBU)
plot(beer_df$ABV,beer_df$IBU)
abline(v=0.1, col="blue")
head(sort(beer_df$ABV,decreasing=TRUE), n = 50)

beer_df$Style_cat='Unknown'

#clean special characters 
beer_df$Style <- gsub('Kölsch', 'Kolsch', beer_df$Style)
beer_df$Style <- gsub('Märzen', 'Marzen', beer_df$Style)
beer_df$Style <- gsub('Bière', 'Biere', beer_df$Style)

#IPA
beer_df$Style_cat <- ifelse(grepl("India Pale Ale", ignore.case=TRUE, beer_df$Style), "IPA", beer_df$Style_cat)
beer_df$Style_cat <- ifelse(grepl("IPA", ignore.case=TRUE, beer_df$Style), "IPA", beer_df$Style_cat)
#Porters
beer_df$Style_cat <- ifelse(grepl("porter", ignore.case=TRUE, beer_df$Style), "Porters", beer_df$Style_cat)
#Stouts
beer_df$Style_cat <- ifelse(grepl("stout", ignore.case=TRUE, beer_df$Style), "Stouts", beer_df$Style_cat)

#Strong Ales
strong_ales_key <-c('red ale','wine ale', 'old ale', 'wine', 'strong ale','quadrupel','tripel','Scotch Ale','Wee Heavy')
beer_df$Style_cat <- ifelse(grepl(paste(strong_ales_key,collapse="|"), ignore.case=TRUE, beer_df$Style), "Strong Ales", beer_df$Style_cat)

#Wheat Beers
wheat_beer_key <- c('wheat', 'Witbier', 'Weissbier','Dunkelweizen','Hefeweizen')
beer_df$Style_cat <- ifelse(grepl(paste(wheat_beer_key,collapse="|"), ignore.case=TRUE, beer_df$Style), "Wheat Beers", beer_df$Style_cat)

#Wild or Sour Beers
wild_beer_key <-c('Gose', 'wild ale','oud bruin','Flanders red ale')
beer_df$Style_cat <- ifelse(grepl(paste(wild_beer_key,collapse="|"), ignore.case=TRUE, beer_df$Style), "Wild or Sour Beers", beer_df$Style_cat)

#Hybrid Beers
hybrid_beer_key <- c('california common', 'Garde', 'cream ale','kolsch','Altbier','braggot')
beer_df$Style_cat <- ifelse(grepl(paste(hybrid_beer_key,collapse="|"), ignore.case=TRUE, beer_df$Style), "Hybrid Beers", beer_df$Style_cat)

#Dark Lagers
dark_key <-c('schwarzbier','Marzen','Oktoberfest')
beer_df$Style_cat <- ifelse(grepl(paste(dark_key,collapse="|"), ignore.case=TRUE, beer_df$Style), "Dark Lagers", beer_df$Style_cat)

#Bocks
bock_key<-c('Weizen','bock')
beer_df$Style_cat <- ifelse(grepl(paste(bock_key,collapse="|"), ignore.case=TRUE, beer_df$Style), "Bocks", beer_df$Style_cat)

#Dark Ales
dark_ale_key<-c('black ale', 'dark ale', 'dubbel', 'roggenbier', 'scottish ale', 'winter warmer')
beer_df$Style_cat <- ifelse(grepl(paste(dark_ale_key,collapse="|"), ignore.case=TRUE, beer_df$Style), "Dark Ales", beer_df$Style_cat)

#Brown Ales
brown_key<-c('Brown Ale','Oud Bruin','Mild')
beer_df$Style_cat <- ifelse(grepl(paste(brown_key,collapse="|"), ignore.case=TRUE, beer_df$Style), "Brown Ales", beer_df$Style_cat)

#Specialty Beers
spec_key<-c('Shandy','cider','mead','radler','chile beer','low alcohol beer','black ale','Pumpkin Ale', 'Rye Beer', 'Smoked Beer', 'rauchbier', 'herbed','spiced','Fruit','Vegetable')
beer_df$Style_cat <- ifelse(grepl(paste(spec_key,collapse="|"), ignore.case=TRUE, beer_df$Style), "Specialty Beers", beer_df$Style_cat)

#Pale Ales
pal_key<-c('blonde ale', 'bitter', 'ESB', 'pale ale', 'Abbey Single Ale','saison','grisette')
beer_df$Style_cat <- ifelse(grepl(paste(pal_key,collapse="|"), ignore.case=TRUE, beer_df$Style), "Pale Ales", beer_df$Style_cat)

#Pilsener and Pale Lager
pil_key<-c('pilsener', 'pilsner','Helles','Keller Bier','Zwickel Bier','Malt Liquor','american lager')
beer_df$Style_cat <- ifelse(grepl(paste(pil_key,collapse="|"), ignore.case=TRUE, beer_df$Style), 'Pilsener and Pale Lager', beer_df$Style_cat)

#Lager
lager_key<-c(' lager')
beer_df$Style_cat <- ifelse(grepl(paste(lager_key,collapse="|"), ignore.case=TRUE, beer_df$Style), "Lager", beer_df$Style_cat)

df <- beer_df[which(beer_df$Style_cat==''),]
df$Style

beer_df$Style
beer_df$Style_cat

names(beer_df)

beer_brew_df<-merge(x=beer_df,y=brewery_df,by.x="Brewery_id",by.y="Brew_ID",all.x = TRUE)

names(beer_brew_df)


#Questions ######################3

#1.   How many breweries are present in each state?

beer_path="/Users/wailunchung/Documents/GitHub/MSDS-6306-Doing-Data-Science/Unit 7/Beers.csv"
brewery_path="/Users/wailunchung/Documents/GitHub/MSDS-6306-Doing-Data-Science/Unit 7/Breweries.csv"
beer_df=read.csv(beer_path)
brewery_df=read.csv(brewery_path)

count_by_state <- data.frame(table(brewery_df$State))

ggplot(count_by_state, aes(x=reorder(Var1,-Freq),y=Freq,fill=Var1)) + 
  geom_bar(stat="identity") + 
  labs(title="Brewery Count by State",subtitle="",x="State",y="freq",caption="") +
  theme(plot.title=element_text(hjust=0.5),axis.text.x = element_text(angle = 90, hjust = 1))

install.packages("maps")
library(maps)
require(maps)
namevec <- map(database = "state", col = "blue",fill=T, namesonly=TRUE)

map(database = "state",col = c("white", "blue")[1+(namevec %in% tolower(c) )],fill=T)

# using maps to get coordinate and use ggplot to plot maps
library(maps)
us_states <- map_data("state")
head(us_states,10)
p <- ggplot(data = us_states,
            aes(x = long, y = lat,
                group = group, fill = region))
p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)
p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 



install.packages("mapproj")

library(mapproj)

p <- ggplot(data = count_by_state,
            mapping = aes(x = Var1, y = Freq,
                          group = Var1, fill = Var1))
p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)



p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long, y = lat, group = group, fill = pct_trump))

p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

p1 + labs(title = "Trump vote") + theme_map() + labs(fill = "Percent")

p2 <- p1 + scale_fill_gradient(low = "white", high = "#CB454A") +
  labs(title = "Trump vote") 
p2 + theme_map() + labs(fill = "Percent")

#2. Merge beer data with the breweries data. Print the first 6 observations and the last six observations to check the merged file.

beer_brew_df<-merge(x=beer_df,y=brewery_df,by.x="Brewery_id",by.y="Brew_ID",all.x = TRUE)
head(beer_brew_df,6)
tail(beer_brew_df,6)

#3. Report the number of NA's in each column.

summary(beer_df$ABV) #62 na
summary(beer_df$IBU) #1005 na
beer_brew_df$

#4. Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.
library(dplyr)
beer_brew_df %>%
  group_by(beer_brew_df$State)%>% 
  summarise(Mean=mean(beer_df$ABV), Max=max(beer_df$ABV), Min=min(beer_df$ABV), Median=median(beer_df$ABV), Std=sd(beer_df$ABV))P



