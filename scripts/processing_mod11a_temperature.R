# load the appropriate libraries in R.
library(raster)
library(sp)
library(rgdal)
library(ggplot2)
library(lubridate)
library(plotly)
library(rgeos)
library(robfilter)

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

setwd('/Users/hkim/Projects/data/LST/modis/h11v04/geotif')
getwd()

# read-in our shapefile data
#shp_dir <- '/Users/hkim/Projects/precision_ag_charter/data/modis/mod13q1/test'
#shapefileData <- readOGR(dsn = 'shp_dir', layer = 'meadNE_site')

# the location of site, ***must be longitude and latitude
site = cbind(-93.0898, 44.7143)

# four sites has to be processed are
# Rosemount - MN cbind(-93.0898, 44.7143)
# Brooks Field-IA cbind(-93.6936, 41.9747)
# Mead-NE cbind(-96.4396, 41.1797)
# Bondville-IL cbind(-88.2904, 40.0062)

# define the projection system to use # lat-long in this case
latlon = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
# create a spatial object from the coordinate(s)
coordinates = SpatialPoints(site, latlon)

# Load tif files and convert them to raster objects
fnames <- list.files(getwd(), pattern = glob2rx('*.tif'))
rasterfiles <- lapply(fnames, raster)

# Extract pixel values and store them to a list
ext_value <- lapply(rasterfiles, extract, coordinates)
temp <- unlist(ext_value)
lst_celsius <- temp * 0.02 - 273.15
# Date extract from file name
dates <- as.Date(substr(fnames, 18, 24), format = "%Y%j")

# create a data frame with dates and ndvi values
df_ts <- data.frame(dates, lst_celsius)

# remove na values from data frame
df_cln <- df_ts[complete.cases(df_ts), ]

###################################################################################################
# Apply Savitzky-Golay filter
library(signal)
sg <- sgolay(p = 6, n = 91, m = 0)
df_cln$sg <- formattable(filter(sg, df_cln$lst_celsius), digits = 2, format = "f")


# loess filtering
mod <- loess(df_cln$lst_celsius ~ df_cln$dates, span = 0.8, data.frame(x = df_cln$dates, y=df_cln$lst_celsius))

###################################################################################################
# below is for year specific processing
# Subset specific year to check
year_lst <- subset(df_cln, format(dates, '%Y') %in% 
                       c('2006'))

###################################################################################################
# Plot time series
require(cowplot)
require(grid)

p_temp <- ggplot(df_cln) +
          geom_point(aes(dates, lst_celsius), size = 2) +
          geom_line(aes(dates,sg), col = 'red', size = 1) +
          ylab('Land Surface Temperature')
p_temp

p_t <- ggplot(year_lst) + 
  ylab('Land Surface Temperature (Celsius) ') +
  ggtitle("MODIS Land Surface Temperature") +
  geom_point(aes(dates, lst_celsius), size = 2) +
  geom_line(aes(dates, sg), col = "red", size = 1) +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=25, hjust=0.5),
        plot.margin = unit(c(1,1.5,1,2), "cm")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size=20),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size=15))
  #theme(panel.background = element_rect(fill = NA))
p_t 




#outdir <- '/Users/hkim/Projects/precision_ag_charter/results/'
#out_png <- paste0(outdir, "ndvi_historical_01-15_brooksfield.png")
#dev.copy(png, out_png,width=12,height=7,units="in",res=100)
#dev.off()

###################################################################################################
# # plot two graphs side by side
# library(gridExtra)
# library(grid)
# #grid.arrange(p_p, p_ndvi, ncol = 2, nrow = 1, 
# #              top=textGrob("Rosemount, MN ; 2015",gp=gpar(fontsize=25,font=3)))
# 
# # plot two graphs vertically
# grid.newpage()
# grid.draw(rbind(ggplotGrob(p_p), ggplotGrob(p_ndvi), size = "last"))
###################################################################################################
