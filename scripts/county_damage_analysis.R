# Load necessary package
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(plyr)
library(data.table)
library(plotly)

# set working directory
setwd('/Users/hkim/Projects/precision_ag_charter/data/usda_croplayer')
getwd()

# load data set
farm_df <- read.table('colmnt16.txt', quote = "", sep = '|', fill = FALSE, strip.white = TRUE)

# change column names
cols <- c('year', 'state_code', 'state_abb', 'county_code', 'county', 'commidity_code', 'commodity',
          'insurance_code', 'insurance', 'stage', 'damage_code', 'damage', 'month', 'month_abb',
          'acreage', 'amount')

colnames(farm_df) <- cols

# convert digit for state/county code to five digit to make FIPS
farm_df$stcode_2d <- sprintf("%02d", farm_df$state_code)
farm_df$cocode_3d <- sprintf("%03d", farm_df$county_code)
farm_df$FIPS <- paste0(farm_df$stcode_2d, farm_df$cocode_3d)
# combine state_abb and county name 
farm_df$state_county <- 
  paste0(farm_df$state_abb, '-', farm_df$county)

# subset data frame
farm_df <- subset(farm_df, select = -c(2, 4, 6, 8:11, 17, 18))

# re-order data frame
farm_df_cln <- farm_df[c(1:3, 10, 11, 4:9)]

# aggregate data by state in order to find max-damaged state
state_damage_summary <- farm_df %>%
  #select(FIPS, state_abb, county, commodity, damage, month_abb, acreage, amount) %>% 
  group_by(state_abb) %>% 
  dplyr::summarise(acre_damaged = sum(acreage), 
                   amount_damage = sum(amount))

# find max-damaged state
max_state <- state_damage_summary[which.max(state_damage_summary$acre_damaged), ] 
# pass the information into a variable
maxstate <- as.factor(max_state$state_abb)
# filter dataframe just for the county
as.factor(max_state$state_abb)
##################################################################################################
# aggregate data by state & county to find max-damaged county
county_damage_summary <- farm_df_cln %>%
  #select(FIPS, state_abb, county, commodity, damage, month_abb, acreage, amount) %>% 
  group_by(state_abb, county) %>%    # should be group (state and county)
  dplyr::summarise(acre_damaged = sum(acreage), 
                   amount_damage = sum(amount))
# combine state_abb and county name 
county_damage_summary$state_county <- 
  paste0(county_damage_summary$state_abb, '-', county_damage_summary$county)

# check top 50 most damaged counties
ordered <- county_damage_summary[order(-county_damage_summary$acre_damaged), ]
top50 <- ordered[1:50, ]

# find maximum damaged county
max_county <- top50[which.max(top50$acre_damaged), ] 
# pass the information into a variable
maxcounty <- as.factor(max_county$state_county)
# filter dataframe just for the county
as.factor(farm_df$state_county)
one_county <- farm_df_cln %>% filter(state_county == maxcounty)

# pass one user-defined county
#one_county <- farm_df_cln %>% filter(state_county == 'ND-Ward')
#################################################################################################
damage_type <- one_county %>% 
  select(FIPS, commodity, damage, month_abb, acreage, amount) %>% 
  group_by(commodity, damage) %>% 
  dplyr::summarise(acre_damaged = sum(acreage), 
                   amount_damage = sum(amount))

damage_month <- one_county %>% 
  select(FIPS,  commodity, damage, month_abb, acreage, amount) %>% 
  group_by(commodity, month_abb) %>% 
  dplyr::summarise(acre_damaged = sum(acreage), 
                   amount_damage = sum(amount))

damage_crop <- one_county %>% 
  select(FIPS, commodity, damage, month_abb, acreage, amount) %>% 
  group_by(commodity) %>% 
  dplyr::summarise(acre_damaged = sum(acreage), 
                   amount_damage = sum(amount))
  

###################################################################################################
# plot results
bar <- ggplot(top50, aes(x = reorder(state_county, acre_damaged), y = acre_damaged)) +
  geom_bar(stat = 'identity') +
  ylab('Total Damaged Acre') + xlab('State-County') +
  coord_flip() +
  ggtitle('Top 50 Most Damaged Counties - 2016') +
  theme(plot.title = element_text(family = "Trebuchet MS", 
                                  color="#666666", face="bold", size=30, hjust=0.5),
        plot.margin = unit(c(1,1,1,2), "cm")) +
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size=20),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size=15))
bar

bar0 <- ggplot(damage_type, aes(x = commodity, y = acre_damaged, fill = damage)) +
  geom_bar(stat = 'identity') +
  ylab('acre_damaged') + xlab('Commodity') +
  coord_flip() +
  ggtitle('Morrow County, Oregon') +
  theme(plot.title = element_text(family = "Trebuchet MS", 
                                  color="#666666", face="bold", size=30, hjust=0.5),
        plot.margin = unit(c(1,1,1,2), "cm")) +
   theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20),
         axis.text.x = element_text(size = 15), axis.text.y = element_text(size=15),
         legend.title = element_text(size = 20, colour = 'black'),
         legend.key.height = unit(0.5, 'cm'),
         legend.text = element_text(size = 15, colour = "black")) 
bar0

bar1 <- ggplot(damage_month, aes(x = commodity, y = acre_damaged, fill = month_abb)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  ggtitle('Most Affected Month') +
  theme(plot.title = element_text(family = "Trebuchet MS", 
                                  color="#666666", face="bold", size=30, hjust=0.5),
        plot.margin = unit(c(1,1,1,2), "cm")) +
  theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20),
        axis.text.x = element_text(angle = 90, size = 15), axis.text.y = element_text(size=15),
        legend.title = element_text(size = 20, colour = 'black'),
        legend.key.height = unit(0.5, 'cm'),
        legend.text = element_text(size = 15, colour = "black")) 
bar1

bar2 <- ggplot(damage_crop, aes(x = reorder(commodity, acre_damaged), y = acre_damaged)) +
  geom_bar(stat = 'identity') +
  ylab('Total Damaged Acre') + #xlab('Crop') +
  coord_flip() +
  ggtitle('Damaged Crop - ND, Ward County') +
  theme(plot.title = element_text(family = "Trebuchet MS", 
                                  color="#666666", face="bold", size=25, hjust=0.5),
        plot.margin = unit(c(1,1,1,2), "cm")) +
  theme(axis.title.y=element_blank(),
        #axis.text.x=element_blank(),
        axis.title.x = element_text(size = 20), 
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size=13)) 
bar2

