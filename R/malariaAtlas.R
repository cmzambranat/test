# Malaria Atlas

install.packages('malariaAtlas')
library(malariaAtlas)
library(ggplot2)
library(rgdal)
library(raster)
library(gfcanalysis)
library(sp)
library(diffeR)
library(sf)
library(tiff)

PNG_shp <- getShp(ISO = "PNG", admin_level = "admin2")
PNG_shp <- as.MAPshp(PNG_shp)
autoplot(PNG_shp)
png(filename="PNG_shp.png")


# PNG_PfPR2_10 <- getRaster(surface = "Plasmodium falciparum PR2-10", shp = PNG_shp, year = 2015)
# PNG_PfPR2_10 <- as.MAPraster(PNG_PfPR2_10)
# autoplot(PNG_PfPR2_10)

# PNG adm boundaries, SRTM, worldclim

getData('ISO3')
ccodes()

PNG1 <- getData('GADM', country='PNG', level=1)

plot(PNG1, main="PNG Provincias")


PNG_shp <- readOGR('~/R_internship/PNG_shp/PNG_Boundaries/png_prov_boundaries_2011census_region.shp')

PNGshp <- shapefile("~/PNG_Boundaries/png_prov_boundaries_2011census_region.shp")

class(PNG_adm1)
crs(PNG_adm1)
extent(PNG_adm1)
PNG_adm1

# IDN1 <- getData('GADM', country='IDN', level=2)

# plot(IDN1, main="IDN Provincias")



#---------------------------------

# Elevation
# Res:
# Proj: 
# Units:


srtm <- getData('SRTM', lon=145, lat=-5)
plot(srtm)
plot(PNG1, add=TRUE)

srtm1 <- getData('SRTM', lon=141, lat=-5)
plot(srtm1)
plot(PNG1, add=TRUE)

srtm2 <- getData('SRTM', lon=141, lat=0)
plot(srtm2)
plot(PNG1, add=TRUE)

srtm3 <- getData('SRTM', lon=145, lat=0)
plot(srtm3)
plot(PNG1, add=TRUE)

srtm4 <- getData('SRTM', lon=150, lat=0)
plot(srtm4)
plot(PNG1, add=TRUE)

srtm5 <- getData('SRTM', lon=150, lat=-5)
plot(srtm5)
plot(PNG1, add=TRUE)

srtm6 <- getData('SRTM', lon=155, lat=0)
plot(srtm6)
plot(PNG1, add=TRUE)

srtm7 <- getData('SRTM', lon=155, lat=-5)
plot(srtm7)
plot(PNG1, add=TRUE)


srtmmosaic <- mosaic(srtm, srtm1, srtm2, srtm3, srtm4, srtm5, srtm6, srtm7, fun=mean)

plot(srtmmosaic, main="Elevation PNG (SRTM)")
plot(PNG1, add=TRUE)



#---------------------------------

# Climate
# Res:
# Proj: 
# Units:

climate <- getData('worldclim', var='bio', res=2.5)
plot(climate$bio1, main="Annual Mean Temperature")

climate <- getData('worldclim', var='bio', res=0.5, lon= 145, lat=0)
plot(climate$bio1_310, main="Annual Mean Temperature")
plot(PNG1, add=TRUE)


prec <- getData('worldclim', var='prec', res=0.5, lon= 145, lat=0)
plot(prec$prec1_310, main="Annual Precipitation")
plot(PNG1, add=TRUE)



#---------------------------------

# Global Forest Watch (Hansen)
# Res:
# Proj: 
# Units:

output_folder <- "R_internship"
forest_threshold <- 90



gfc_extract <- extract_gfc(PNG_shp, output_folder, filename="PNG_extract.tif")



#---------------------------------

# Land Cover change
# Res:
# Proj: 
# Units: 


TC_ruta <- "/Users/jorge/Desktop/Jorge/UGA/Tesis PhD/Internship/R_internship/Treecover/"
PNG.TC <- list.files(path = TC_ruta, pattern = "tif", all.files = TRUE)
PNG.TC

# (PNG.TC.1 <- paste(TC_ruta,PNG.TC[1],sep=""))

PNG_TC1 <- raster(paste(TC_ruta,PNG.TC[1],sep=""))
PNG_TC2 <- raster(paste(TC_ruta,PNG.TC[2],sep=""))
PNG_TC3 <- raster(paste(TC_ruta,PNG.TC[3],sep=""))
PNG_TC4 <- raster(paste(TC_ruta,PNG.TC[4],sep=""))

(PNG_TCall <- stack(PNG_TC1,PNG_TC2,PNG_TC3,PNG_TC4))

PNG.crop <- crop(PNG_TC1, extent(PNG_shp))

PNG.mask <- mask(x = PNG.crop, mask = PNG_shp)
plot(PNG.mask)


#---------------------------------

# PNG population 
# Res: 3 arc (approximately 100m at the equator) . 
# Proj: WGS84. 
# Units: The units are estimated number of male/female in each age group per grid square.

# Female

PNG_female_0 <- raster('png_f_0_2020.tif')
plot(PNG_female_0, main="Estimated number of female 0 y old per grid square")

PNG_female_5 <- raster('png_f_5_2020.tif')
plot(PNG_female_5, main="Estimated number of female 5 y old per grid square")

PNG_female_10 <- raster('png_f_10_2020.tif')
plot(PNG_female_10, main="Estimated number of female 10 y old per grid square")

PNG_female_15 <- raster('png_f_15_2020.tif')
plot(PNG_female_15, main="Estimated number of female 15 y old per grid square")

PNG_female_20 <- raster('png_f_20_2020.tif')
plot(PNG_female_20, main="Estimated number of female 20 y old per grid square")

PNG_female_25 <- raster('png_f_25_2020.tif')
plot(PNG_female_25, main="Estimated number of female 25 y old per grid square")

PNG_female_30 <- raster('png_f_30_2020.tif')
plot(PNG_female_30, main="Estimated number of female 30 y old per grid square")

PNG_female_35 <- raster('png_f_35_2020.tif')
plot(PNG_female_35, main="Estimated number of female 35 y old per grid square")

PNG_female_40 <- raster('png_f_40_2020.tif')
plot(PNG_female_40, main="Estimated number of female 40 y old per grid square")

PNG_female_45 <- raster('png_f_45_2020.tif')
plot(PNG_female_45, main="Estimated number of female 45 y old per grid square")

PNG_female_50 <- raster('png_f_50_2020.tif')
plot(PNG_female_50, main="Estimated number of female 50 y old per grid square")

PNG_female_55 <- raster('png_f_55_2020.tif')
plot(PNG_female_55, main="Estimated number of female 55 y old per grid square")

PNG_female_60 <- raster('png_f_60_2020.tif')
plot(PNG_female_60, main="Estimated number of female 60 y old per grid square")

PNG_female_65 <- raster('png_f_65_2020.tif')
plot(PNG_female_65, main="Estimated number of female 65 y old per grid square")

PNG_female_70 <- raster('png_f_70_2020.tif')
plot(PNG_female_70, main="Estimated number of female 70 y old per grid square")

PNG_female_75 <- raster('png_f_75_2020.tif')
plot(PNG_female_75, main="Estimated number of female 75 y old per grid square")

PNG_female_80 <- raster('png_f_80_2020.tif')
plot(PNG_female_80, main="Estimated number of female 80 y old per grid square")


# Male

PNG_male_0 <- raster('png_f_0_2020.tif')
plot(PNG_male_0, main="Estimated number of male 0 y old per grid square")

PNG_male_5 <- raster('png_f_5_2020.tif')
plot(PNG_male_5, main="Estimated number of male 5 y old per grid square")

PNG_male_10 <- raster('png_f_10_2020.tif')
plot(PNG_male_10, main="Estimated number of male 10 y old per grid square")

PNG_male_15 <- raster('png_f_15_2020.tif')
plot(PNG_male_15, main="Estimated number of male 15 y old per grid square")

PNG_male_20 <- raster('png_f_20_2020.tif')
plot(PNG_male_20, main="Estimated number of male 20 y old per grid square")

PNG_male_25 <- raster('png_f_25_2020.tif')
plot(PNG_male_25, main="Estimated number of male 25 y old per grid square")

PNG_male_30 <- raster('png_f_30_2020.tif')
plot(PNG_male_30, main="Estimated number of male 30 y old per grid square")

PNG_male_35 <- raster('png_f_35_2020.tif')
plot(PNG_male_35, main="Estimated number of male 35 y old per grid square")

PNG_male_40 <- raster('png_f_40_2020.tif')
plot(PNG_male_40, main="Estimated number of male 40 y old per grid square")

PNG_male_45 <- raster('png_f_45_2020.tif')
plot(PNG_male_45, main="Estimated number of male 45 y old per grid square")

PNG_male_50 <- raster('png_f_50_2020.tif')
plot(PNG_male_50, main="Estimated number of male 50 y old per grid square")

PNG_male_55 <- raster('png_f_55_2020.tif')
plot(PNG_male_55, main="Estimated number of male 55 y old per grid square")

PNG_male_60 <- raster('png_f_60_2020.tif')
plot(PNG_male_60, main="Estimated number of male 60 y old per grid square")

PNG_male_65 <- raster('png_f_65_2020.tif')
plot(PNG_male_65, main="Estimated number of male 65 y old per grid square")

PNG_male_70 <- raster('png_f_70_2020.tif')
plot(PNG_male_70, main="Estimated number of male 70 y old per grid square")

PNG_male_75 <- raster('png_f_75_2020.tif')
plot(PNG_male_75, main="Estimated number of male 75 y old per grid square")

PNG_male_80 <- raster('png_f_80_2020.tif')
plot(PNG_male_80, main="Estimated number of male 80 y old per grid square")