# Load necessary package
library(ggplot2)
library(rgdal)
library(dplyr)
library(rgeos)
library(tmap)

# set working directory
setwd('/Users/hkim/Projects/data/precision_agriculture/usda_croplayer')
getwd()

# load data set
farm_df <- read.table('colmnt11.txt', quote = "", sep = '|', fill = FALSE, strip.white = TRUE)

# change column names
cols <- c('year', 'stcode', 'state_abb', 'cocode', 'county', 'comcode', 'commodity',
          'insucode', 'insurance', 'stage', 'dmgcode', 'damage', 'month', 'month_abb',
          'acreage', 'amount')

colnames(farm_df) <- cols

# convert digit for state/county code to five digit to make FIPS
farm_df$stcode_2d <- sprintf("%02d", farm_df$stcode)
farm_df$cocode_3d <- sprintf("%03d", farm_df$cocode)
farm_df$FIPS <- paste0(farm_df$stcode_2d, farm_df$cocode_3d)

# re-order data frame
farm_df <- farm_df[c(1:5, 19, 6:18)]

#farm_df <- data.table(farm_df)

us.counties <- readOGR(dsn = '/Users/hkim/Projects/r_develop', layer = 'gz_2010_us_050_00_5m')

# leave out AK, HI, PR
us.counties <- us.counties[!(us.counties$STATE %in% c('02', '15', '72')), ]
us.counties$FIPS <- paste0(us.counties$STATE, us.counties$COUNTY)

county.data <- us.counties@data
county.data <- cbind(id=rownames(county.data), county.data)
county.data <- data.table(county.data)
county.data[, FIPS:=paste0(STATE, COUNTY)]

setkey(county.data, FIPS)

# set farm_df as data table
farm_df <- data.table(farm_df)
setkey(farm_df, FIPS)

county.data[farm_df, damaged_acre:=acreage]

us.damaged <- append_data(us.counties, farm_df, key.shp = 'FIPS', 
                          key.data = 'FIPS', ignore.duplicates = TRUE)
under_coverage()

qtm(us.damaged, fill = 'damage')

us.state <- unionSpatialPolygons(us.damaged, IDs = us.damaged$STATE)

dmgmap <- tm_shape(us.damaged, projection = '+init=epsg:2163') +
  tm_polygons('damage', border.col = 'grey30', title='type') +
  tm_shape(us.state) +
  tm_borders(lwd = 2, col='black', alpha = .5) +
  tm_layout(title = '2011 Cause of Damages',
            inner.margins=c(0, 0.2, 0, 0),
            title.position = c('center', 'top'),
            legend.text.size = 1)

dmgmap

# view map with default view options
tmap_mode("view")
dmgmap
