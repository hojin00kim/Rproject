# load the appropriate libraries in R.
library(raster)
library(sp)
library(rgdal)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(grid)

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
setwd('/Users/hkim/Projects/data/precision_agriculture/modis/mod13q1/h11v04/geotif')
getwd()

# unload 'extract' package from 'tidyr', so that the code does not with the one in the raster package
.rs.unloadPackage("tidyr")

# or if shape file(s) are available, read-in shapefile data
#shp_dir <- '/Users/hkim/Projects/precision_ag_charter/data/modis/mod13q1/test'
#shapefileData <- readOGR(dsn = 'shp_dir', layer = 'meadNE_site')

# four sites has to be processed are
# Rosemount - MN cbind(-93.0898, 44.7143) - h11v04
# Brooks Field-IA cbind(-93.6936, 41.9747)
# Mead-NE cbind(-96.4396, 41.1797)
# Bondville-IL cbind(-88.2904, 40.0062)

# the location of site, ***must be longitude and latitude
site = cbind(-93.0898, 44.7143)

# define the projection system to use # lat-long in this case
latlon = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
# create a spatial object from the coordinate(s)
coordinates = SpatialPoints(site, latlon)

# Load tif files and convert them to raster objects
fnames <- list.files(getwd(), pattern = glob2rx('*.tif'))
rasterfiles <- lapply(fnames, raster)

# Extract pixel values and store them to a list
ext_value <- lapply(rasterfiles, extract, coordinates)
ndvi <- unlist(ext_value)

###############################################################################
# load library for tyding data
library(dplyr)
library(tidyr)
# Seperate year and day of year in order to compute yearly average
year <- substr(fnames, 19,22)
doy <- substr(fnames, 23,25)
# in order to make unclass doy column

df_long <- data.frame(year, doy, ndvi)
#colnames(df_year)[3] <- "ndvi"
df_long$doy <- as.numeric(as.character(df_long$doy))
df_long$year <- as.numeric(as.character(df_long$year))

# re-order date frame by year ; long to wide format
df_year <- spread(df_long, year, ndvi)
# another way of data frame long to wide format
#yearly <- dcast(df_year, doy ~ year, value = "ndvi")

corn <- subset(df_year, select = c('doy', '2003', '2005', '2007', '2009', '2011', '2013'))
corn$avg <- apply(corn[ , 2:ncol(corn)], 1, mean, na.rm = TRUE)
corn$std <- apply(corn[ , 2:ncol(corn)], 1, sd, na.rm = TRUE)

soybean <- subset(df_year, select = c('doy','2004', '2006', '2008', '2010', '2012', '2014'))
soybean$avg <- apply(soybean[ , 2:ncol(soybean)], 1, mean, na.rm = TRUE)
soybean$std <- apply(soybean[ , 2:ncol(soybean)], 1, sd, na.rm = TRUE)

# filled with zero 
corn[is.na(corn)] <- 0
soybean[is.na(soybean)] <- 0
###############################################################################
# Before computing cumulative sum of each year, first few rows of GDD have to be removed due to 
# set up a first day of planting, ** dependent on crop life cycle ** 
# please refer to the 'us_planting_harvesting_2010.pdf' for the dates information of each states
#     corn_start = 05/08, corn_end = 10/23
#     soy_start = 05/21, soy_end = 10/08
#year <- '2009'
n <- 16
corn_start <- '2009/05/08'
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

# subset NDVI with start and end doy
corn_ndvi <- corn[ which(corn$doy >= corn_start_end[1] & corn$doy <= corn_start_end[2]), ]
soy_ndvi <- soybean[ which(soybean$doy >= soy_start_end[1] & soybean$doy <= soy_start_end[2]), ]

###############################################################################
library(reshape)
# Compute cumulative sum of each year
#cumsum_out <- df_long %>% group_by(year) %>% mutate(cum_ndvi = cumsum(ndvi))
#yearlyCumsum <- cast(cumsum_out, doy ~ year, value = "cumsum")

# from wide to long format for corn
corn_gathered <- corn_ndvi %>% gather(corn_ndvi, ndvi, 2:ncol(corn))
colnames(corn_gathered)[2] <- 'year'
cumsum_corn <- corn_gathered %>% group_by(year) %>% mutate(cumsum = cumsum(ndvi))

# from wide to long format for soybean
soybean_gathered <- soy_ndvi %>% gather(soy_ndvi, ndvi, 2:ncol(soybean))
colnames(soybean_gathered)[2] <- 'year'
cumsum_soy <- soybean_gathered %>% group_by(year) %>% mutate(cumsum = cumsum(ndvi))

###############################################################################
# plot all years and average
p_corn <- ggplot(cumsum_corn, aes(doy, cumsum, group = year, colour = year)) +
  #geom_errorbar(aes(ymin = avg - std, ymax = avg + std), width=.2,
  #              position=position_dodge(0.05)) +
  ggtitle('Cumulative NDVI, Corn') +
  geom_line() + 
  geom_point(size = 2) +
  xlab("Day of Year") + ylab("Cumulative NDVI") +
  geom_line(linetype = 2) +
  geom_point(size = 4) +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=25, hjust=0.5)) +
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size=20),
        axis.text.x = element_text(size = 20), axis.text.y = element_text(size=20),
        legend.title = element_text(size = 20, colour = 'black'),
        legend.text = element_text(size = 15, colour = "black"))
p_corn

p_soy <- ggplot(cumsum_soy, aes(doy, cumsum, group = year, colour = year)) +
  #geom_errorbar(aes(ymin = avg - std, ymax = avg + std), width=.2,
  #              position=position_dodge(0.05)) +
  ggtitle('Cumulative NDVI, Soybean') +
  geom_line() + 
  geom_point(size = 2) +
  xlab("Day of Year") + ylab("Cumulative NDVI") +
  geom_line(linetype = 2) +
  geom_point(size = 4) +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=25, hjust=0.5)) +
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size=20),
        axis.text.x = element_text(size = 20), axis.text.y = element_text(size=20),
        legend.title = element_text(size = 20, colour = 'black'),
        legend.text = element_text(size = 15, colour = "black"))
p_soy

# Combine all plots in one page
grid.arrange(p_corn, p_soy, ncol = 2, nrow = 1, 
             top=textGrob("Rosemount, MN",gp=gpar(fontsize=25,font=3)))
