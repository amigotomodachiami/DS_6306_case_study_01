#Questions ######################3

#'x' is the column of a data.frame that holds 2 digit state codes
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}

#1.   How many breweries are present in each state?

beer_path="/Users/wailunchung/Documents/GitHub/MSDS-6306-Doing-Data-Science/Unit 7/Beers.csv"
brewery_path="/Users/wailunchung/Documents/GitHub/MSDS-6306-Doing-Data-Science/Unit 7/Breweries.csv"
beer_df=read.csv(beer_path)
brewery_df=read.csv(brewery_path)

# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)
brewery_df$State <- trim(brewery_df$State)

# covert state to full state name
brewery_df$State_full<-stateFromLower(brewery_df$State)
count_by_state <- data.frame(table(brewery_df$State_full))

ggplot(count_by_state, aes(x=reorder(Var1,-Freq),y=Freq,fill=Var1)) + 
  geom_bar(stat="identity") + 
  labs(title="Brewery Count by State",subtitle="",x="State",y="freq",caption="") +
  theme(plot.title=element_text(hjust=0.5),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")

#install.packages("maps")
# using maps to get coordinate and use ggplot to plot maps
#install.packages('ggthemes')
library(maps)
library(ggplot2)
library(ggthemes)
us_states <- map_data("state")
head(us_states,10)
beer_brew_df<-merge(x=beer_df,y=brewery_df,by.x="Brewery_id",by.y="Brew_ID",all.x = TRUE)
#beer_brew_df_map<-merge(x=brewery_df,y=us_states,by.x="State_full",by.y="region",all.x = TRUE)
beer_brew_df_map<-merge(x=count_by_state,y=us_states,by.x="Var1",by.y="region",all.x = TRUE)
#count_by_state
#names(beer_brew_df_map)

#beer_brew_df_map

p <- ggplot(data = beer_brew_df_map,
            aes(x = long, y = lat,
                group = group, fill = Freq))
#p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)
p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  labs(title = "Number of Brewery") +
  theme_map()

#2. Merge beer data with the breweries data. Print the first 6 observations and the last six observations to check the merged file.

beer_brew_df<-merge(x=beer_df,y=brewery_df,by.x="Brewery_id",by.y="Brew_ID",all.x = TRUE)
head(beer_brew_df,6)
tail(beer_brew_df,6)

#3. Report the number of NA's in each column.

summary(beer_df$ABV) #62 na
summary(beer_df$IBU) #1005 na

#4. Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.
#library(dplyr)
#beer_brew_df %>%
#  group_by(State)%>% 
#  summarise(Mean=mean(ABV,na.rm=TRUE), Max=max(ABV,na.rm=TRUE), Min=min(ABV,na.rm=TRUE), Median=median(ABV,na.rm=TRUE), Std=sd(ABV,na.rm=TRUE))

library(dplyr)
median_IBU_by_state <- beer_brew_df %>%
  group_by(State_full)%>% 
  summarise(Median=median(IBU,na.rm=TRUE))

median_IBU_by_state[is.na(median_IBU_by_state$Median),]
# *** state "SD" no Median, All IBU are NA in SD
distinct(beer_brew_df,State)
beer_brew_df[beer_brew_df$State=='SD',]

ggplot(median_IBU_by_state, aes(x=reorder(State_full,-Median),y=Median,fill=State_full)) + 
  geom_bar(stat="identity") + 
  labs(title="IBU Median by State",subtitle="",x="State",y="Median",caption="") +
  theme(plot.title=element_text(hjust=0.5),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")

#5. Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?

ABV_by_state <- beer_brew_df %>%
  group_by(State_full)%>% 
  summarise(Mean=mean(ABV,na.rm=TRUE),Median=median(ABV,na.rm=TRUE))

ABV_by_state

IBU_by_state <- beer_brew_df %>%
  group_by(State_full)%>% 
  summarise(Mean=mean(IBU,na.rm=TRUE),Median=median(IBU,na.rm=TRUE))

IBU_by_state

#Top 10 ABV state (Mean)
top_n(ABV_by_state, n=10, Mean) %>%
  ggplot(., aes(x=reorder(State_full,-Mean),y=Mean,fill=State_full)) + 
  geom_bar(stat="identity") + 
  labs(title="Top 10 ABV Average by State",subtitle="",x="State",y="Average",caption="") +
  theme(plot.title=element_text(hjust=0.5),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")

#Top 10 IBU state (Mean)
top_n(IBU_by_state, n=10, Mean) %>%
  ggplot(., aes(x=reorder(State_full,-Mean),y=Mean,fill=State_full)) + 
  geom_bar(stat="identity") + 
  labs(title="Top 10 IBU Average by State",subtitle="",x="State",y="Average",caption="") +
  theme(plot.title=element_text(hjust=0.5),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")



#--------

summary(beer_brew_df)
median_ABV_by_state <- beer_brew_df %>%
  group_by(State_full)%>% 
  summarise(Median=median(ABV,na.rm=TRUE))

#Top 10 ABV state
top_n(median_ABV_by_state, n=10, Median) %>%
ggplot(., aes(x=reorder(State_full,-Median),y=Median,fill=State_full)) + 
  geom_bar(stat="identity") + 
  labs(title="Top 10 ABV Median by State",subtitle="",x="State",y="Median",caption="") +
  theme(plot.title=element_text(hjust=0.5),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")

#Top 10 IBU state
top_n(median_IBU_by_state, n=10, Median) %>%
  ggplot(., aes(x=reorder(State_full,-Median),y=Median,fill=State_full)) + 
  geom_bar(stat="identity") + 
  labs(title="Top 10 IBU Median by State",subtitle="",x="State",y="Median",caption="") +
  theme(plot.title=element_text(hjust=0.5),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")

#6. Summary statistics for the ABV variable.
summary(beer_brew_df$ABV)

#7. Is there an apparent relationship between the bitterness of the beer and its alcoholic content? Draw a scatter plot.

ggplot(beer_brew_df, aes(x=ABV, y=IBU)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue")

#Extra: ABV, IBU by style

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

beer_df[beer_df$Style_cat=='Unknown',]
beer_df

beer_brew_df<-merge(x=beer_df,y=brewery_df,by.x="Brewery_id",by.y="Brew_ID",all.x = TRUE)
ABV_by_style <- beer_brew_df %>%
  group_by(Style_cat)%>% 
  summarise(Mean=mean(ABV,na.rm=TRUE),Median=median(ABV,na.rm=TRUE))

ABV_by_style

IBU_by_style <- beer_brew_df %>%
  group_by(Style_cat)%>% 
  summarise(Mean=mean(IBU,na.rm=TRUE),Median=median(IBU,na.rm=TRUE))

IBU_by_style

#Top 10 ABV Style (Mean)
top_n(ABV_by_style, n=99, Mean) %>%
  ggplot(., aes(x=reorder(Style_cat,-Mean),y=Mean,fill=Style_cat)) + 
  geom_bar(stat="identity") + 
  labs(title="ABV Average by Style",subtitle="",x="Style",y="Average",caption="") +
  theme(plot.title=element_text(hjust=0.5),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")

#Top 10 IBU Style (Mean)
top_n(IBU_by_style, n=99, Mean) %>%
  ggplot(., aes(x=reorder(Style_cat,-Mean),y=Mean,fill=Style_cat)) + 
  geom_bar(stat="identity") + 
  labs(title="IBU Average by Style",subtitle="",x="Style",y="Average",caption="") +
  theme(plot.title=element_text(hjust=0.5),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")

p<-ggplot(beer_brew_df, aes(x=ABV, y=IBU,color=Style_cat)) + 
  geom_point()+
  geom_smooth(method=lm,linetype="dashed",
              color="darkred", fill="blue")
p <- p +  labs(color='Style') 
p <- p + theme(legend.position="bottom")
p <- p + guides(fill=guide_legend(nrow=2, byrow=TRUE))
p
#+ labs(color='NEW LEGEND TITLE') 

getwd()
file.path(getwd(), "Raw Data", "lib")
paste(getwd(),"/Raw Data")
file.path(getwd(), "Raw Data", "Beers.csv")
install.packages("ggforce")
