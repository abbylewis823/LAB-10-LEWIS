library(terra)
library(tmap)
library(sf)

# 1. Read and Explore the DEM

dem <- rast(paste0("unit2.img"))
dem
df_dem <- as.data.frame(dem, xy = TRUE)

# 2. Extract Slope and Aspect

slope <- terrain(dem, v = "slope", unit = "degrees", neighbors = 8)

aspect <- terrain(dem, v = "aspect", unit = "degrees")

ttm()

tm_shape(slope, alpha = 0.5) +
  tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)")

tm_shape(aspect) +
  tm_raster(style = "cont")

# 3. Reclassify Aspect

asp_class <- matrix(c(
  0, 45, 1,
  45, 90, 2,
  90, 175, 2,
  175, 180, 3,
  180, 225, 3,
  225, 270, 4,
  270, 315, 4,
  315, 360, 1
), ncol = 3, byrow = TRUE)

asp <- classify(aspect, asp_class)

ttm()
tm_shape(asp) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow", "red"),
            labels = c(NA, "North", "East", "South", "West"), alpha = 0.2)


# 4. Visualize Sample Forest Inventory Plots

sum_u2 <- read.csv(paste0("sum_u2.csv"))

svy_pts <- st_read(paste0("HEE_Overstory_Survey_Points_2017 - Copy.shp"))

svy_pts <- st_transform(svy_pts, 32616)

survey_pts <- subset(svy_pts, Unit == '2')

sum_u2 <- merge.data.frame(sum_u2, survey_pts, all.x = TRUE)

unique(sum_u2$Plot)
unique(survey_pts$Plot)

sum_u2 <- st_as_sf(sum_u2, coords = c("X", "Y"), crs = 32616)
sum_u2

# 5. Create Circular Plots

sf_plot <- st_buffer(sum_u2, dist = 17.83)

# 6. Unify Coordinate Systems

crs(sf_plot , proj=T)
crs(asp , proj=T)

asp_crs <- crs(asp, proj = TRUE)
sf_plot_crs <- st_transform(sf_plot, crs = asp_crs)

# 7. Visualization

ttm()
tm_shape(asp, alpha = 0.5) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow", "red"),
            showNA = FALSE, alpha = 0.2, labels = c(NA, "North", "East", "South", "West")) +
  tm_shape(sf_plot) +
  tm_polygons('Common.name') +
  tm_layout(legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9)


ttm()
tm_shape(slope, alpha = 0.5) +
  tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)") +
  tm_shape(sf_plot) +
  tm_polygons('Common.name', title = "Dom_Species", alpha = 0.6) +
  tm_layout(title = "Dominant trees by slope",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9, size = 1.2)

ttm()
tm_shape(sf_plot) +
  tm_polygons('BA', title = "Basal Area (sq_ft/acre)", palette = "brewer.spectral") +
  tm_layout(title = "Basal Area Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()


ttm()
tm_shape(sf_plot) +
  tm_polygons('TPA', title = "Trees Per Acre", palette = "brewer.spectral") +
  tm_layout(title = "TPA Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()

ttm()
tm_shape(sf_plot) +
  tm_polygons('bm_tonpa', title = "Biomass (tons/ac)", palette = "brewer.spectral") +
  tm_layout(title = "Biomass Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()





