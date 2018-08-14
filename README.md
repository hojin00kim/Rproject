# Rproject

## Introduction
This is a personal repo for various R script that I have worked for mainly image processing and time series analysis for agricultural or natural resource applications. 

These tools are distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for more details.  

Author: Hojin Kim  


|file                           | Description   |
|:------------------------------|:--------------|
|raster_shift.py                | shift (affine transformation) a raster file in x/y direction with a user defined pixel counts in geographic coordinate systems |
|clip_raster_to_polygon.py      | clip a raster file to polygon either a single feature or  multiple features from the attribute table |
|rgb_image_rotator.py | rotate raster file (rgb geotiff format) to an arbitrary angle |
|update_raster_geotiff_header.py | update imagery geotiff header based on twf file |
|image_utmzone_finder.py | find matching utm zone from image with geographic coordinate system |
|image_chunker.py | chunk imagery data by user defined window size |
|quad_blob_estimation.py | estimate percentage of blob area from each quad section | 
|extract_validation_data_image.py | little automated qa/qc tool for checking output image and products match and the algorithm works and generated expected output |  
