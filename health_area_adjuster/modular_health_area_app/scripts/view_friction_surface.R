library(terra)
library(sf)
library(tmap)

# paths
friction_file  <- "data/friction/somalia_friction_100m.tif"
districts_file <- "data/districts_shp.Rds"

# load data
friction  <- terra::rast(friction_file)
districts <- readRDS(districts_file) |>
  sf::st_as_sf() |>
  sf::st_make_valid()

# optional: national boundary from districts
national_boundary <- districts |>
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
  sf::st_as_sf()

# optional: mask out extremely high / impassable values for prettier display
friction_plot <- friction
friction_plot[friction_plot >= 1e6] <- NA

# tmap v4 interactive mode
tmap_mode("view")

tm_shape(friction_plot) +
  tm_raster(
    col.scale = tm_scale_continuous(values = "-magma"),
    col.legend = tm_legend(title = "Friction"),
    col_alpha = 0.85
  ) +
  tm_shape(national_boundary) +
  tm_borders(col = "black", lwd = 2) +
  tm_shape(districts) +
  tm_borders(col = "white", lwd = 0.5, fill_alpha = 0.6) +
  tm_title("Somalia National Friction Surface") +
  tm_scalebar() +
  tm_compass()


library(terra)

pop_cost <- rast("data/friction/somalia_population_cost_100m.tif")
friction <- rast("data/friction/somalia_friction_100m.tif")

s <- spatSample(
  c(pop_cost, friction),
  size = 50000,
  method = "random",
  na.rm = TRUE
)

s <- as.data.frame(s)
names(s) <- c("pop_cost", "friction")

cor(s$pop_cost, s$friction, method = "spearman")



library(sf)
library(terra)

friction <- rast("data/friction/somalia_friction_100m.tif")

districts <- readRDS("data/districts_shp.Rds") |>
  st_as_sf() |>
  st_make_valid() |>
  st_transform(crs(friction))

# extract values directly along boundaries
boundary_vals <- extract(
  friction,
  vect(st_boundary(districts))
)

range(boundary_vals[,2], na.rm = TRUE)
summary(boundary_vals[,2])


