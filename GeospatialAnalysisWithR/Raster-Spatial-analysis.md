Spatial Analysis of rainfall and percipitation in Zambia
================

This uses districts, roads, and farmers datasets for spatial analysis within Zambia. 
It also uses geodata’s worldclim_country function to grab WorldClim’s mean temperature, 
precipitation dataset for other spatial analysis.

#### Task 1

``` r

library(geospaar)

# read in datasets for districts
districts <- system.file("extdata/districts.geojson", package = "geospaar") %>% 
  st_read
#> Reading layer `districts' from data source 
#>   `/packages/geospaar/extdata/districts.geojson' using driver `GeoJSON'
#> Simple feature collection with 72 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 21.99978 ymin: -18.0751 xmax: 33.6875 ymax: -8.226213
#> Geodetic CRS:  WGS 84
districts
#> Simple feature collection with 72 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 21.99978 ymin: -18.0751 xmax: 33.6875 ymax: -8.226213
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>         distName                       geometry
#> 1        Chadiza POLYGON ((32.3076 -14.3128,...
#> 2          Chama POLYGON ((33.6875 -10.57932...
#> 3        Chavuma POLYGON ((22.00014 -13.4776...
#> 4       Chibombo POLYGON ((28.89229 -14.7998...
#> 5        Chiengi POLYGON ((29.1088 -9.079298...
#> 6  Chililabombwe POLYGON ((28.06755 -12.3526...
#> 7        Chilubi POLYGON ((30.61853 -11.2516...
#> 8       Chingola POLYGON ((27.51766 -12.2984...
#> 9       Chinsali POLYGON ((32.34814 -9.74577...
#> 10       Chipata POLYGON ((32.95348 -13.2636...

# subsetting districts 22, 26, 53, and 54
districts_ss <- districts[c(22, 26, 53, 54), ]
districts_ss
#> Simple feature collection with 4 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 23.68492 ymin: -16.10729 xmax: 27.96308 ymax: -12.8613
#> Geodetic CRS:  WGS 84
#>    distName                       geometry
#> 22    Kaoma POLYGON ((25.59041 -14.5366...
#> 26  Kasempa POLYGON ((26.87956 -13.4341...
#> 53 Mufumbwe POLYGON ((25.57661 -12.9764...
#> 54   Mumbwa POLYGON ((27.18799 -14.3719...

# define a new raster with 0.1 resolution
r <- rast((ext(districts_ss)), res = 0.1, crs = crs(districts))
r
#> class       : SpatRaster 
#> dimensions  : 32, 43, 1  (nrow, ncol, nlyr)
#> resolution  : 0.1, 0.1  (x, y)
#> extent      : 23.68492, 27.98492, -16.10729, -12.90729  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326)

# Create new rasters called: rsamp and rrandn
set.seed(1) 
rsamp <- r
values(rsamp) <- sample(10:50, size = ncell(rsamp), replace = TRUE) 
rsamp 
#> class       : SpatRaster 
#> dimensions  : 32, 43, 1  (nrow, ncol, nlyr)
#> resolution  : 0.1, 0.1  (x, y)
#> extent      : 23.68492, 27.98492, -16.10729, -12.90729  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : lyr.1 
#> min value   :    10 
#> max value   :    50

rrandn <- r
values(rrandn) <- rnorm(n = ncell(rrandn), mean = 30, sd = 5)

# Stack the two rasters
l <- list(r, rsamp, rrandn)
s <- rast(l)
names(s) <- c("Random Sampled Raster", "Raster from Normal distribution")
s
#> class       : SpatRaster 
#> dimensions  : 32, 43, 2  (nrow, ncol, nlyr)
#> resolution  : 0.1, 0.1  (x, y)
#> extent      : 23.68492, 27.98492, -16.10729, -12.90729  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> names       : Random Sampled Raster, Raster from Normal distribution 
#> min values  :                    10,                        14.77318 
#> max values  :                    50,                        45.19517
#plot(s, nr = 1)

# Masking stack with and plot using no axes
mask_s <- mask(x = s, mask = districts_ss)

plot_noaxes(mask_s)
```

<img src="figure-gfm/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

#### Task 2: Disaggregate s[[1]] to a resolution of 0.025°, using bilinear interpolation. Create new raster where all areas of s2_1d that have values > 35 Set the values of s2_1gt35 that equal 0 to NA. Convert the resulting raster into an sf object and plot the resulting polygons over s2_1d.

``` r

# disaggregate the rsamp raster to a 0.025 resolution
s2_1d <- disagg(x = s[[1]], fact = 4, method = "bilinear")
s2_1d
#> class       : SpatRaster 
#> dimensions  : 128, 172, 1  (nrow, ncol, nlyr)
#> resolution  : 0.025, 0.025  (x, y)
#> extent      : 23.68492, 27.98492, -16.10729, -12.90729  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : Random Sampled Raster 
#> min value   :              10.12500 
#> max value   :              49.65625

# filter pixesl > 35
s2_1gt35 <- s2_1d > 35
s2_1gt35
#> class       : SpatRaster 
#> dimensions  : 128, 172, 1  (nrow, ncol, nlyr)
#> resolution  : 0.025, 0.025  (x, y)
#> extent      : 23.68492, 27.98492, -16.10729, -12.90729  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : Random Sampled Raster 
#> min value   :                 FALSE 
#> max value   :                  TRUE

# set all values in s2_1gt35 ==0 to NA
s2_1gt35[s2_1d == 0] <- NA

# convert filtered rasters to polygon and then convert polygon to sf 
s2poly <- as.polygons(x = s2_1gt35, dissolve = TRUE) 
s2poly
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 2, 1  (geometries, attributes)
#>  extent      : 23.68492, 27.98492, -16.10729, -12.90729  (xmin, xmax, ymin, ymax)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       : Random Sampled Raster
#>  type        :                 <int>
#>  values      :                     0
#>                                    1

# convert polygon to sf
s2poly <- st_as_sf(s2poly) %>% print()
#> Simple feature collection with 2 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 23.68492 ymin: -16.10729 xmax: 27.98492 ymax: -12.90729
#> Geodetic CRS:  WGS 84
#>   Random Sampled Raster                       geometry
#> 1                     0 MULTIPOLYGON (((27.48492 -1...
#> 2                     1 MULTIPOLYGON (((23.80992 -1...
s2poly
#> Simple feature collection with 2 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 23.68492 ymin: -16.10729 xmax: 27.98492 ymax: -12.90729
#> Geodetic CRS:  WGS 84
#>   Random Sampled Raster                       geometry
#> 1                     0 MULTIPOLYGON (((27.48492 -1...
#> 2                     1 MULTIPOLYGON (((23.80992 -1...

# plot the converted sf over the rsamp raster
par(mar = rep(0, 4))
plot(s2_1d, main = "N farmers per 0.025 cell")
plot(s2poly$geometry, add = TRUE)
```

<img src="figure-gfm/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

#### Task 3: Recreate new grids and sum all farmers within these grids

``` r

# create new grid from the extent of districts, with resolution of 0.5° 
# & assign all cells a value of 1
zamr <- rast(x = ext(districts), res = 0.5, crs = crs(districts))
values(zamr) <- 1:ncell(zamr)
zamr
#> class       : SpatRaster 
#> dimensions  : 20, 23, 1  (nrow, ncol, nlyr)
#> resolution  : 0.5, 0.5  (x, y)
#> extent      : 21.99978, 33.49978, -18.0751, -8.075097  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : lyr.1 
#> min value   :     1 
#> max value   :   460

# read in farmers dataset and create framers raster (farmersr)
farmers <- system.file("extdata/farmer_spatial.csv", package = "geospaar") %>%
  read_csv(show_col_types = FALSE) 

# Recreate raster that sums the number of farmers falling within each grid 
# cell
farmersr <- farmers %>% 
  distinct(uuid, .keep_all = TRUE) %>% 
  dplyr::select(x, y) %>% 
  mutate(count = 1) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  rasterize(x = ., y = zamr, field = "count", fun = sum) 
farmersr
#> class       : SpatRaster 
#> dimensions  : 20, 23, 1  (nrow, ncol, nlyr)
#> resolution  : 0.5, 0.5  (x, y)
#> extent      : 21.99978, 33.49978, -18.0751, -8.075097  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : sum 
#> min value   :   1 
#> max value   : 287

farmersr <- mask(farmersr, districts)

par(mar = c(1, 1, 3, 1))
#plot(districts$geometry, col = "grey", main = "Overlay of sf of pixels > 35
# over rsamp raster")
districts %>% 
  st_union %>% 
  plot(col = "grey", border = "grey", 
       main = expression(paste("N farmers per 0.5", degree, " cell")))
plot(farmersr, add = TRUE, ext = districts)
```

<img src="figure-gfm/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

#### Task 4 : convert farmers raster in to points and calculate distance form dristricts to farmers.

``` r

# convert farmersr raster into point using as.point
farmersrpts <- as.points(x = farmersr) %>% 
  st_as_sf


zamr <- rast(x = ext(districts), res = 0.05, crs = crs(districts))
values(zamr) <- 1:ncell(zamr)
#plot(zamr)

# Spatial analysis of distance from dristricts to farmers
dist_to_farmers <- distance(x = zamr, y = farmersrpts)
#> |---------|---------|---------|---------|=========================================                                          
dist_to_farmers <- mask(dist_to_farmers, districts)

# convert distance to km
s <- dist_to_farmers / 1000

# plot
par(mar = c(1, 1, 3, 1))
plot_noaxes(s, main = "Distance(km) from  farmers")
plot(farmersrpts$geometry, pch = 20, cex = 0.5, col = "black", add = TRUE)
```

<img src="figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />


#### Task 5: Download and classify temperature datasets within Zambia

``` r

install.packages("geodata")
library(geodata)

# download average monthly temperature dataset from WorldClim’s in my temporary
# directory
tmean <- geodata::worldclim_country(var = "tavg", res = 2.5, 
                                     country = "Zambia", path = tempdir())
tmean
#> class       : SpatRaster 
#> dimensions  : 1260, 1500, 12  (nrow, ncol, nlyr)
#> resolution  : 0.008333333, 0.008333333  (x, y)
#> extent      : 21.5, 34, -18.5, -8  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source      : ZMB_wc2.1_30s_tavg.tif 
#> names       : ZMB_w~avg_1, ZMB_w~avg_2, ZMB_w~avg_3, ZMB_w~avg_4, ZMB_w~avg_5, ZMB_w~avg_6, ... 
#> min values  :        12.4,        12.5,        12.3,        11.7,        10.2,         8.4, ... 
#> max values  :        28.7,        28.5,        28.3,        27.9,        27.0,        25.9, ...

# calculate annual temp. and mask it with districts
zamtmean <- mask(app(tmean, mean), districts)
zamtmean
#> class       : SpatRaster 
#> dimensions  : 1260, 1500, 1  (nrow, ncol, nlyr)
#> resolution  : 0.008333333, 0.008333333  (x, y)
#> extent      : 21.5, 34, -18.5, -8  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        :     mean 
#> min value   : 15.04167 
#> max value   : 27.19167

# plot
plot(zamtmean, main = "Average Temperature in Zambia")
```

<img src="figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

#### Task 6: Classify  the mean temperature into 3 zones

``` r

trng <- global(zamtmean, range, na.rm = TRUE)

reclmat <- cbind(unlist(c(floor(trng[1]), 20, 24)), unlist(c(20, 24, ceiling(trng[2]))), 1:3)

# classify the mean temp values
zamtclass <- classify(x = zamtmean, rcl = reclmat, include.lowest = TRUE)

# plot 
cols <- c("blue", "yellow2", "red")
plot_noaxes(zamtclass, legend = FALSE, 
            main = "Average Temperature zones in Zambia", col = cols, 
            mar = c(1, 1, 3, 1))
legend(x = "bottomright", legend = c("High", "Intermediate", "Low"), 
       pch = 15, pt.cex = 3, col = rev(cols), bty = "n")
```

<img src="figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

#### Task 7 : Calculate the mean precipitation within each temperature zone

``` r

# down load precipitation dataset
wcprec <- geodata::worldclim_country(var = "prec", res = 2.5, 
                                     country = "Zambia", path = tempdir())
zamprec <- mask(app(wcprec, sum), districts)

# mean precipitation within each temperature zones in zamtclass 1,2,3
z <- zonal(zamprec, z = zamtclass, fun = "mean", na.rm = TRUE)

# plot
zamprecz <- subst(x = zamtclass, from = z[, 1], to = z[, 2])
cols <- c( "yellow2", "green", "blue")
plot_noaxes(zamprecz, legend = FALSE, 
            main = "Mean Precipitaion in Temperature zones", 
            col = cols, 
            mar = c(1, 1, 3, 1))
legend(x = "bottomright", legend = round(z$mean), 
       pch = 15, pt.cex = 3, col = rev(cols), bty = "n")
```

<img src="figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />


#### Task 8: Get rainfall and elevation dataset, aggregate elevation data to rainfall and use these to calculate aspect

``` r

# download rainfall dataset
chirps <- rast(system.file("extdata/chirps.tif", package = "geospaar"))
chirps
#> class       : SpatRaster 
#> dimensions  : 197, 234, 28  (nrow, ncol, nlyr)
#> resolution  : 0.05, 0.05  (x, y)
#> extent      : 21.95, 33.65, -18.05, -8.200001  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source      : chirps.tif 
#> names       :   Y16299,   Y16300,   Y16301,   Y16302,   Y16303,  Y16304, ... 
#> min values  :  0.00000,  0.00000,  0.00000,  0.00000,  0.00000,  0.0000, ... 
#> max values  : 21.33322, 17.76521, 22.12555, 32.39063, 19.46936, 28.5387, ...

# download elevation dataset
dem <- geodata::elevation_30s(country = "ZMB", path = tempdir())
dem
#> class       : SpatRaster 
#> dimensions  : 1212, 1452, 1  (nrow, ncol, nlyr)
#> resolution  : 0.008333333, 0.008333333  (x, y)
#> extent      : 21.8, 33.9, -18.2, -8.1  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source      : ZMB_elv_msk.tif 
#> name        : ZMB_elv_msk 
#> min value   :         327 
#> max value   :        2286

# aggregate the elevation to that of chirps and mask by districts
dem5 <- aggregate(x = dem, fact = 6.00000024000001) %>% 
  mask(., mask = districts)
  
# calculate aspect from the aggregated elevation raster
aspect <- terrain(x = dem5, v = 'aspect', unit = 'degrees')
aspect
#> class       : SpatRaster 
#> dimensions  : 202, 242, 1  (nrow, ncol, nlyr)
#> resolution  : 0.05, 0.05  (x, y)
#> extent      : 21.8, 33.9, -18.2, -8.1  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        :      aspect 
#> min value   : 7.11038e-03 
#> max value   : 3.60000e+02

# filtering aspects to the west and east
west <- aspect > 247.5 & aspect < 292.5

east <- aspect > 67.5 & aspect < 112.5

# Stacking the all the 3 aspect rasters
l <- list(aspect, west, east)
s <- rast(l)
names(s) <- c("Aspect", "West", "East")
s
#> class       : SpatRaster 
#> dimensions  : 202, 242, 3  (nrow, ncol, nlyr)
#> resolution  : 0.05, 0.05  (x, y)
#> extent      : 21.8, 33.9, -18.2, -8.1  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> names       :      Aspect,  West,  East 
#> min values  : 7.11038e-03, FALSE, FALSE 
#> max values  : 3.60000e+02,  TRUE,  TRUE

# plot with no axes
plot_noaxes(s, mfrow = c(2, 2), mar = c(1, 1, 3, 1))
```

<img src="figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />


#### Task 9: Create random rasters, covert them to sf and extract mean temperature values

``` r

# set the cells equal to 0 in east and west to NA
west[west == 0] <- NA
east[east == 0] <- NA

# Create two random samples of 100 each and convert to sf by piping to st_as_sf. 
set.seed(1)
westpts <- spatSample(x = west, size = 100, 
                      cells = TRUE, xy = TRUE, na.rm = TRUE) %>% 
  st_as_sf(coords = c("x", "y")) 
westpts
#> Simple feature collection with 100 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 22.775 ymin: -17.525 xmax: 33.475 ymax: -8.925
#> CRS:           NA
#> First 10 features:
#>     cell aspect               geometry
#> 1  18134   TRUE POINT (33.075 -11.825)
#> 2  21615   TRUE POINT (25.625 -12.575)
#> 3  27892   TRUE POINT (24.875 -13.875)
#> 4  41170   TRUE POINT (23.275 -16.625)
#> 5  16165   TRUE POINT (31.425 -11.425)
#> 6  40732   TRUE POINT (25.575 -16.525)
#> 7  43392   TRUE POINT (25.475 -17.075)
#> 8  30787   TRUE POINT (24.425 -14.475)
#> 9  29816   TRUE POINT (24.275 -14.275)
#> 10  7642   TRUE  POINT (28.775 -9.675)

# convert sample rasters into points
eastpts <- spatSample(x = east, size = 100, 
                      cells = TRUE,  xy = TRUE, na.rm = TRUE)%>% 
  st_as_sf(coords = c("x", "y")) 
eastpts
#> Simple feature collection with 100 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 22.075 ymin: -17.675 xmax: 33.375 ymax: -8.575
#> CRS:           NA
#> First 10 features:
#>     cell aspect               geometry
#> 1  28175   TRUE POINT (26.925 -13.925)
#> 2  32260   TRUE POINT (25.475 -14.775)
#> 3  31114   TRUE POINT (28.675 -14.525)
#> 4  31310   TRUE POINT (26.375 -14.575)
#> 5  26082   TRUE POINT (31.175 -13.475)
#> 6  43863   TRUE POINT (24.825 -17.175)
#> 7  23589   TRUE POINT (27.525 -12.975)
#> 8  37590   TRUE POINT (25.775 -15.875)
#> 9  34143   TRUE POINT (22.825 -15.175)
#> 10 27696   TRUE POINT (27.175 -13.825)

# extract temperature values from zamtmean into a tibble temp_stats
temp_stats <- bind_rows(
  tibble(temp = terra::extract(zamtmean, westpts)$mean, dat = "West"), 
tibble(temp = terra::extract(zamtmean, eastpts)$mean, dat = "East")
)

# plot
bp_theme <- theme(legend.title = element_blank(), 
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(), 
                  panel.grid.major.x = element_blank(), 
                  panel.grid.minor.x = element_blank(), 
                  panel.background = element_rect(fill = "grey95"))
ggplot(temp_stats) +
  geom_boxplot(mapping = aes(y = temp, fill = dat), position = "dodge2") +
  scale_fill_manual(values = c("steelblue", "cadetblue")) + 
  ggtitle("Temperature distributions") + xlab(NULL) + ylab("Degrees") + 
  bp_theme
```

<img src="figure-gfm/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />


#### Task 10: Create districts centrioids and reproject and then extract temperature values of these points. Then create an inverse distance weighted model (IDW) of temperature values.

``` r

# read in roads dataset
roads <- system.file("extdata/roads.geojson", package = "geospaar") %>% st_read
#> Reading layer `roads' from data source `/packages/geospaar/extdata/roads.geojson' using driver `GeoJSON'
#> Simple feature collection with 473 features and 1 field
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -292996.9 ymin: -2108848 xmax: 873238.8 ymax: -1008785
#> Projected CRS: Africa_Albers_Equal_Area_Conic

# convert districts polgons to centroids
dcent <- st_centroid(districts) %>% 
  st_transform(., crs = st_crs(roads))

# reproject the mean temp raster(zamtmean) to roads crs and change resolution to
# 5000m
zamtmeanalb <- project(zamtmean, crs(roads), res = 5000, method = "bilinear")
#plot(zamtmeanalb)

# use dcent to extract temperature values from zamtmeanalb add it as a new 
# variable using mutate
dcent <- dcent %>% 
  mutate(temp = terra::extract(zamtmeanalb, dcent)$mean) 
  
# convert to tibble
dcent <- bind_cols(
  dcent %>% data.frame %>% dplyr::select(-geometry) %>% as_tibble, 
  st_coordinates(dcent) %>% as_tibble
) %>% rename(x = X, y = Y)
dcent
#> # A tibble: 72 × 4
#>    distName       temp        x         y
#>    <chr>         <dbl>    <dbl>     <dbl>
#>  1 Chadiza        22.0  785504. -1660651.
#>  2 Chama          23.5  804577. -1323913.
#>  3 Chavuma        19.6 -265706. -1561046.
#>  4 Chibombo       20.5  314855. -1748564.
#>  5 Chiengi        24.4  427626. -1035130.
#>  6 Chililabombwe  19.4  289720. -1461451.
#>  7 Chilubi        21.1  540126. -1314573.
#>  8 Chingola       19.3  280202. -1478732.
#>  9 Chinsali       20.0  712689. -1273197.
#> 10 Chipata        22.0  775301. -1600748.
#> # ℹ 62 more rows

# Install gstat library to use uses for the inverse distance weighted interpolation
library(gstat)

# create an empty raster that would be use in the model
r <- rast(ext(zamtmeanalb), res = res(zamtmeanalb), 
          crs = crs(zamtmeanalb), vals = 1)
#plot(r)

#names(dcent) <- "temp"
invdist <- gstat(id = "temp", formula = temp ~ 1, locations = ~x + y, data = dcent)

# IDW model() and mask it to zambia boundaries
invdistr <- interpolate(object = r, model = invdist, )
#> [inverse distance weighted interpolation]
#> [inverse distance weighted interpolation]
zamtidw <- mask(x = invdistr[[1]], mask = zamtmeanalb)
#plot(zamtidw)

# plot the real temperature (zamtmeanalb) and the interpolated temperature (zamtidw).
tempinterp <- c(list(zamtmeanalb, zamtidw))
titles <- c("Real Temperature", "IDW Temperature")

par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
for(i in 1:2) 
  plot_noaxes(tempinterp[[i]], main = titles[i])
```

<img src="figure-gfm/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />
