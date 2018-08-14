# load necessary package
library(dplyr)
library(lubridate)
library(magrittr)
library(tidyr)

###################################################################################################
# This is a function to compute composite dates of every nth interval
# Motivation of this code is to find matching dates of daily weather station
# with satellite composite dates
# For example, MOD13 products have 16-day composite period, and
# MOD09 products have 8-day compote period
find_composite_date <- function(n, startDate, endDate) {
  sdoy <- as.numeric(strftime(startDate, format = "%j"))
  adjustmentStart <- ((sdoy %% n) - 1)
  startDoy <- sdoy - adjustmentStart
  
  edoy <- as.numeric(strftime(endDate, format = "%j"))
  adjustmentEnd <- (n - (edoy %% n)) + 1
  endDoy <- edoy + adjustmentEnd
  
  return(c(startDoy, endDoy))
}
###################################################################################################

# set working directory
setwd('/Users/hkim/Projects/data/precision_agriculture/weather_station')
getwd()

# load data set
site_weather <- read.csv('Mead_NE_2001_2016.csv')

# drop/delete unnecessary information
site_weather <- site_weather[ , c("STATION_NAME", "DATE", "PRCP")]

# set date format to plot time series
site_weather$DATE <- as.Date(as.character(site_weather$DATE), format='%Y%m%d')

# fill missing date with NA or zero (insert rows for missing date)
# create a time series date from beginning and end of the data frame; 
# and add some dummy data in order to make a full year data
ts <- seq.POSIXt(as.POSIXct("2001-01-01", '%Y%m%d'), as.POSIXct("2016-12-31", '%Y%m%d'), by = 'day')
ts <- seq.POSIXt(as.POSIXlt("2001-01-01"), as.POSIXlt("2016-12-31"), by = 'day')
ts <- format.POSIXct(ts, '%Y%m%d')
# convert it to data.frame
date_df <- data.frame(DATE=ts)
date_df$DATE <- as.Date(date_df$DATE, "%Y%m%d")

# join the "date" data frame with original data
prcp_df <- full_join(date_df, site_weather, by = "DATE")

# change site name with shorter version or insert the column with station name
prcp_df$STATION_NAME <- 
  gsub('MEAD 6 S NE US', 'Mead NE', prcp_df$STATION_NAME)

# change NA value of the station name colume with the station name
prcp_df <- 
  prcp_df %>% replace_na(list(STATION_NAME = 'Mead NE'))

#prcp_df$STATION_NAME [prcp_df$STATION_NAME %in% NA] <- 'Mead NE'

# just in case you need to handle yearly data
prcp_df$year <- year(prcp_df$DATE) # this is more efficient with using lubricate
prcp_df$month <- month(prcp_df$DATE)
prcp_df$mday <- mday(prcp_df$DATE)
prcp_df$doy <- as.numeric(strftime(prcp_df$DATE, format = '%j'))

# replace na value (-9999) with zero
prcp_df$PRCP[prcp_df$PRCP %in% -9999.00] <- 0
prcp_df[is.na(prcp_df)] <- 0

# re-order the data frame 
prcp_df <- prcp_df[c(2, 1, 4:7, 3)]
#######################################################################################################
# below is for year specific processing
# Subset specific year to check
prcp_year <- subset(prcp_df, format(DATE, '%Y') %in% 
                      c('2008'))
#######################################################################################################
# function for extracting every nth row
# data: data frame
# nSubsets: number of replicate, default is 1
# nSkip: value that you want to skip, every 8th = 8, every 16th = 16
# usage: datatest <- as.data.frame(nth_ext(site_weather_filled, 1, 16))
nth_ext = function(data, nSubsets, nSkip){
  lapply(1:nSubsets, function(n) data[seq(n, NROW(data), by = nSkip),])
}
#######################################################################################################
date16 <- as.data.frame(nth_ext(prcp_year, 1, 16))
date16 <- subset(date16, select = c(2))

# or create doy for the new data frame
#doy <- data.frame("doy" = (seq(1, 365, by = 16)))
#######################################################################################################
# composite precipitation for the same period
# select columns for composite: year, doy, and prcp
prcp_tmp <- subset(prcp_year, select = c(6, 7))
n <- 16
prcp_tmp <- aggregate(prcp_tmp, list(rep(1:(nrow(prcp_tmp) %/% n+1), 
                        each=n, len=nrow(prcp_tmp))), sum)[-1]; # function can be changed "sum", "mean"
# drop unnecessary columns
prcp_tmp <- subset(prcp_tmp, select = -c(1))

# combine doy and 16-day composite weather data
prcp_composite <- cbind(date16, prcp_tmp)
#######################################################################################################
library(ggplot2)
#par(mar = c(1,1,1,1))
p_p <- ggplot(prcp_composite, aes(DATE, PRCP)) +
  geom_bar(stat = 'identity', fill = 'grey50') + 
  ggtitle("Mead NE 2008") +
  ylab('Precipitation (inch)') +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=25, hjust=0.5),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size=20),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size=15),
        legend.title = element_text(size = 20, colour = 'black'),
        legend.text = element_text(size = 15, colour = "black")) 
  #theme(panel.background = element_rect(fill = NA))
#p_p
###############################################################################################################
# # for plotting monthly average
# month_weather <- year_weather %>%
#   group_by(year, month) %>%
#   summarise(monthly_mean_prcp = mean(PRCP))
# 
# # create color scale for monthly precipitation plot
# month_legend = as.factor(month_weather$month)
# 
# p_m <- ggplot(month_weather, aes(year, monthly_mean_prcp, group = month, 
#                                    colour = month_legend)) +
#   geom_bar(fill = "grey50", stat = 'identity', position = 'dodge') +
#   ggtitle("Monthly precipitation ") +
#   scale_fill_discrete(name = "month") +
#   theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0.5)) +
#   theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size=20),
#         axis.text.x = element_text(size = 15), axis.text.y = element_text(size=15),
#         legend.title = element_text(size = 20, colour = 'black'),
#         legend.text = element_text(size = 15, colour = "black"))
# p_m
# 
# # Combine all plots in one page
# grid.arrange(p_p, p_t, ncol = 1, nrow = 2, 
#              top=textGrob("Mead NE",gp=gpar(fontsize=25,font=3)))
