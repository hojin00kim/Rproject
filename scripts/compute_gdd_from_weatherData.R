# load necessary package
library(dplyr)
library(lubridate)
library(magrittr)
library(tidyr)

# list functions
#######################################################################################################
# This is a function to compute composite dates of every nth interval
# Motivation of this code is to find matching dates of daily weather station
# with satellite composite dates
# For example, MOD13 products have 16-day composite period, and
# MOD09 products have 8-day compote period
find_composite_date <- function(n, startDate, endDate) {
  s_doy <- as.numeric(strftime(startDate, format = '%j'))
  e_doy <- as.numeric(strftime(endDate, format = '%j'))
  
  adjustmentStart <- ((s_doy %% n) - 1)
  startDoy <- s_doy - adjustmentStart
  
  adjustmentEnd <- (n - (e_doy %% n)) + 1
  endDoy <- e_doy + adjustmentEnd
  
  return(c(startDoy, endDoy))
}
#######################################################################################################
# Planting and harvesting dates of crops for each states, these dates are based on midean of most 
# active planting and harvesting dates, these dates are based on the year of 2009
# IA; corn - planting (04/25 - 05/15), harvesting (10/05 - 11/09)
#     soybean - planting (05/08 - 06/02), harvesting (09/28 - 10/20)
#     corn_start = 05/05, corn_end = 10/23
#     soy_start = 05/21, soy_end = 10/8
# IL; corn - planting (04/21 - 05/23), harvesting (09/23 - 11/05)
#     soybean - planting (05/08 - 06/12), harvesting (09/26 - 10/26)
#     corn_start = 05/05, corn_end = 10/23
#     soy_start = 05/26, soy_end = 10/10
# KA; corn - planting (04/15 - 05/15), harvesting (09/10 - 10/25)
#     soybean - planting (05/15 - 06/20), harvesting (10/01 - 11/01)
#     corn_start = 04/30, corn_end = 10/02
#     soy_start = 06/03, soy_end = 10/16
# MN; corn - planting (04/26 - 05/19), harvesting (10/08 - 11/08)
#     soybean - planting (05/08 - 06/02), harvesting (09/27 - 10/20)
#     corn_start = 05/08, corn_end = 10/23
#     soy_start = 05/21, soy_end = 10/08
# NE; corn - planting (04/27 - 05/15), harvesting (10/04 - 11/10)
#     soybean - planting (05/11 - 05/31), harvesting (09/29 - 10/24)
#     corn_start = 05/06, corn_end = 10/23
#     soy_start = 05/21, soy_end = 10/11
#######################################################################################################

# set working directory
setwd('/Users/hkim/Projects/data/precision_agriculture/weather_station')
getwd()

# unload package 'tidyr'
#.rs.unloadPackage("tidyr")
# load data set
site_weather <- read.csv('USW_AMESMunicipal.csv')

# subset columns by names; "STATION_NAME", "DATE", "TMAX", "TMIN"
site_weather <- site_weather[ , c("STATION_NAME", "DATE", "TMAX", "TMIN")]

# set date format to plot time series
site_weather$DATE <- as.Date(as.character(site_weather$DATE), format='%Y%m%d')

# fill missing date with NA or zero (insert rows for missing date)
# create a time series date from beginning and end of the data frame; 
# and add some dummy data in order to make a full year data
ts <- seq.POSIXt(as.POSIXct("2001-01-01", '%Y%m%d'), as.POSIXct("2016-12-31", '%Y%m%d'), by = 'day')
ts <- seq.POSIXt(as.POSIXlt("2001-01-01"), as.POSIXlt("2016-12-31"), by = 'day')
ts <- format.POSIXct(ts, '%Y%m%d')
# convert it to data.frame
df <- data.frame(DATE=ts)
df$DATE <- as.Date(df$DATE, "%Y%m%d")

# join the "date" data frame with original data
site_weather_filled <- full_join(df, site_weather, by = "DATE")

# change site name with shorter version or insert the column with station name
site_weather_filled$STATION_NAME <- 
  gsub('AMES MUNICIPAL AIRPORT IA US', 'Brooksfield IA', site_weather_filled$STATION_NAME)

# change NA value of the station name colume with the station name
site_weather_filled <- 
  site_weather_filled %>% replace_na(list(STATION_NAME = 'Brooksfield IA'))

# just in case you need to handle yearly data
site_weather_filled$year <- year(site_weather_filled$DATE) # this is more efficient with using lubricate
site_weather_filled$month <- month(site_weather_filled$DATE)
site_weather_filled$mday <- mday(site_weather_filled$DATE)
site_weather_filled$doy <- as.numeric(strftime(site_weather_filled$DATE, format = '%j'))

# replace na value (-9999) with zero for arithmatic calculation
site_weather_filled$TMAX[site_weather_filled$TMAX %in% -9999] <- NA
site_weather_filled$TMIN[site_weather_filled$TMIN %in% -9999] <- NA

# insert columns for maxT and minT for computing GDD
# maxT and minT are threshold temperature for GDD computation,
# if Temperature >= 86, T == 86, if Temperature <= 50, T == 50
site_weather_filled <- site_weather_filled %>% mutate (gddTMAX = ifelse(TMAX <= 86, TMAX, 86))
site_weather_filled <- site_weather_filled %>% mutate (gddTMIN = ifelse(TMIN >= 50, TMIN, 50))

# compute GDD and insert it to a new column
site_weather_filled <- site_weather_filled %>% 
  mutate(GDD = ((site_weather_filled$gddTMAX + site_weather_filled$gddTMIN) / 2) - 50)

# subset data frame by column names
site_weather_filled <- site_weather_filled[ , c("STATION_NAME", "DATE", "year", "month", "mday",
                                                "doy", "GDD")]

# re-order data frame from long to wide
library(tidyr)

gdd <- subset(site_weather_filled, select = c("year", "doy", "GDD"))
gdd_yearly <- spread(gdd, year, GDD)

# change NA value to zero
# change na value of weather data with zero
#gdd_yearly[is.na(gdd_yearly)] <- 0
#######################################################################################################
# Before computing cumulative sum of each year, first few rows of GDD have to be removed due to 
# set up a first day of planting, ** dependent on crop life cycle ** 
# please refer to the 'us_planting_harvesting_2010.pdf' for the dates information of each states
#     ia_corn_start = 2009/05/05, ia_corn_end = 2009/10/23
#     ia_soy_start = 2009/05/21, ia_soy_end = 2009/10/8
#year <- '2009'
n <- 16
corn_start <- '2009/05/05'
corn_end <- '2009/10/23'

soy_start <- '2009/05/21'
soy_end <- '2009/10/08'

# compute start and end dates in "day of year' format
corn_start_end <- find_composite_date(n, corn_start, corn_end)
c_doy <- seq(corn_start_end[1], corn_start_end[2], by = n)
doy_corn <- data.frame("doy" = c_doy)

soy_start_end <- find_composite_date(n, soy_start, soy_end)
s_doy <- seq(soy_start_end[1], soy_start_end[2], by = n)
doy_soy <- data.frame("doy" = s_doy)

# subset GDD with start and end doy
corn_gdd <- gdd_yearly[ which(gdd_yearly$doy >= corn_start_end[1] & gdd_yearly$doy <= corn_start_end[2]), ]
soy_gdd <- gdd_yearly[ which(gdd_yearly$doy >= soy_start_end[1] & gdd_yearly$doy <= soy_start_end[2]), ]

#######################################################################################################
#n <- 16
# computing composite day sum of parameter
gdd16day_c <- aggregate(corn_gdd, list(rep(1:(nrow(corn_gdd) %/% n+1), 
                                each=n, len=nrow(corn_gdd))), sum)[-1]; # function can be changed "sum", "mean"
# delete the first column of weird doy
gdd16day_c <- subset(gdd16day_c, select = -c(1))
# combine doy and 16-day composite weather data
gdd16day_corn <- cbind(doy_corn, gdd16day_c)

gdd16day_s <- aggregate(soy_gdd, list(rep(1:(nrow(soy_gdd) %/% n+1), 
                                           each=n, len=nrow(soy_gdd))), sum)[-1]; # function can be changed "sum", "mean"
# delete the first column of weird doy
gdd16day_s <- subset(gdd16day_s, select = -c(1))
# combine doy and 16-day composite weather data
gdd16day_soy <- cbind(doy_soy, gdd16day_s)
#######################################################################################################
library(reshape)
# Compute cumulative sum of each year
# from wide to long format
gddCorn_gathered <- gdd16day_corn %>% gather(gdd16day_corn, gdd, 2:ncol(gdd16day_corn))
colnames(gddCorn_gathered)[2] <- 'year'
cumsum_gddCorn <- gddCorn_gathered %>% group_by(year) %>% mutate(cumsum_gddCorn = cumsum(gdd))

gddSoy_gathered <- gdd16day_soy %>% gather(gdd16day_soy, gdd, 2:ncol(gdd16day_soy))
colnames(gddSoy_gathered)[2] <- 'year'
cumsum_gddSoy <- gddSoy_gathered %>% group_by(year) %>% mutate(cumsum_gddSoy = cumsum(gdd))
#######################################################################################################
# below is for site specific processing
library(tidyr)
# Subset corn and soybean year for a site
corn_year_gdd <- cumsum_gddCorn[(cumsum_gddCorn$year %in% c('2002', '2004', '2006', '2008')), ]

soy_year_gdd <- cumsum_gddSoy[(cumsum_gddSoy$year %in% c('2001', '2003', '2005', '2007')), ]

colnames(corn_year_gdd)[4] <- "cumsum_gdd"
colnames(soy_year_gdd)[4] <- "cumsum_gdd"
###############################################################################################################
library(ggplot2)

p_c <- ggplot(corn_year_gdd, aes(doy, cumsum_gdd, group = year, colour = year)) +
  #geom_errorbar(aes(ymin = avg - std, ymax = avg + std), width=.2,
  #              position=position_dodge(0.05)) +
  ggtitle('Cumulative GDD for Corn') +
  geom_line() + 
  geom_point(size = 2) +
  xlab("Day of Year") + ylab("Cumulative GDD") +
  geom_line(linetype = 2) +
  geom_point(size = 4) +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=25, hjust=0.5)) +
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size=20),
        axis.text.x = element_text(size = 20), axis.text.y = element_text(size=20))
        #legend.title = element_text(size = 20, colour = 'black'),
        #legend.text = element_text(size = 15, colour = "black"))
p_c

p_s <- ggplot(soy_year_gdd, aes(doy, cumsum_gdd, group = year, colour = year)) +
  #geom_errorbar(aes(ymin = avg - std, ymax = avg + std), width=.2,
  #              position=position_dodge(0.05)) +
  ggtitle('Cumulative GDD for Soy') +
  geom_line() + 
  geom_point(size = 2) +
  xlab("Day of Year") + ylab("Cumulative GDD") +
  geom_line(linetype = 2) +
  geom_point(size = 4) +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=25, hjust=0.5)) +
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size=20),
        axis.text.x = element_text(size = 20), axis.text.y = element_text(size=20))
        #legend.title = element_text(size = 20, colour = 'black'),
        #legend.text = element_text(size = 15, colour = "black"))
p_s

library(gridExtra)
library(grid)

grid.arrange(p_c, p_s, ncol = 2, nrow = 1, 
             top=textGrob("Brooksfield, IA",gp=gpar(fontsize=25,font=3)))