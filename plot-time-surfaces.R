# library(rasterVis)
library(sp)
library(glue)
library(geojsonio)
library(scales)

r_dir = 'raster-dl'
origin = 'Plaza de Armas'
origins = c('Plaza de Armas', 'Tobalaba', 'Lo Boza', 'Lo Barnechea', 'La Pintana', 'Puente Alto')
n_repli = 20
cv_stack = stack()
sd_stack = stack()
range_stack = stack()

draws = c(60, 120, 480, 960, 1200)
percentiles = c(5, 25, 50, 75, 95)
layer_names = c()

total_variance = tribble(~origin, ~draws, ~percentile, ~variance)

mean_cell_rmsd = tribble(~origin, ~draws, ~percentile, ~mrmsd)

max_range = tribble(~origin, ~draws, ~percentile, ~range)

for (i in 1:length(origins)) {
  origin = origins[i]
  print(origin)
  for (j in 1:length(draws)) {
    d = draws[j]
    for (k in 1:length(percentiles)) {
      p = percentiles[k]
      # Format names of files that have been written previously using code in time-surfaces
      cv_file = glue({r_dir}, '/', {origin}, '/', {d}, '_cv.tif')
      mean_file = glue({r_dir}, '/', {origin}, '/', {d}, '_mean.tif')
      range_file = glue({r_dir}, '/', {origin}, '/', {d}, '_range.tif')
      layer_names = c(layer_names, glue({d}, ' draws , percentile ', {percentiles[p]}))
      
      # Coefficient of variation
      cv_raster = raster(cv_file, band = k)
      cv_stack = addLayer(cv_stack, cv_raster)
      
      # Standard deviation
      sd_raster = cv_raster * raster(mean_file, band = k)
      sd_stack = addLayer(sd_stack, sd_raster)
      
      # Range
      range_raster = raster(range_file, band = k)
      range_stack = addLayer(range_stack, range_raster)
      
      # Record summary values
      total_variance = add_row(total_variance, 
                               origin = i, 
                               draws = d, 
                               percentile = p, 
                               variance = cellStats(sd_raster ^ 2, 'sum')
                              )
      
      mean_cell_rmsd = add_row(mean_cell_rmsd,
                               origin = i,
                               draws = d,
                               percentile = p,
                               mrmsd = cellStats((sd_raster * 19/20 / 100), 'sum') / cellStats(!is.na(sd_raster),'sum') # convert denominator from n-1 to n, and from percentage
                               )
      
      max_range = add_row(max_range, 
                          origin = i, 
                          draws = d, 
                          percentile = p, 
                          range = cellStats(range_raster, 'max')
                        )

    }
  }
}

pctile_labeller = function(num) { 
  glue('{num}th percentile')
}

origin_labeller = function(num) {
  origins[parse_number(num)]
}

breaks_scaler = function(x) {
  interval = max(x) - min(x)
  c(min(x) + 0.1 * interval, max(x) - 0.1 * interval) 
}


max_range %>%
  ggplot() +
  aes(draws, range, color=origin) +
  geom_point() +
  geom_line(size = 0.25, linetype = 'dashed') + 
  scale_x_continuous(limits = c(60, 1200), breaks = c(60, 480, 1200)) +
  scale_y_continuous(position = 'right', breaks = seq(0, 20, 10), minor_breaks = seq(0, 20, 2)) +
  expand_limits(y = c(0, 20)) +
  facet_grid(origin ~ percentile, labeller = labeller(origin = as_labeller(origin_labeller), percentile = as_labeller(pctile_labeller)), switch = 'y') + 
  labs(title='Maximum per-cell range \n (by origin, percentile travel time, and number of Monte Carlo draws)', y = 'Maximum range (minutes)', x= 'Monte Carlo draws') + 
  theme_grey() + 
  theme(legend.position = 'None', panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(hjust = 0.5))

total_variance %>%
  ggplot() +
  aes(draws, variance, color=origin) +
  geom_point() +
  geom_line(size = 0.25, linetype = 'dashed') + 
  scale_x_continuous(limits = c(60, 1200), breaks = c(120, 480, 960)) +
  scale_y_continuous(position = 'right', breaks = breaks_scaler, labels = function(x) format(x, scientific = TRUE, digits = 2)) +
  facet_grid(origin ~ percentile, labeller = labeller(origin = as_labeller(origin_labeller), percentile = as_labeller(pctile_labeller)), switch = 'y', scales = 'free_y') + 
  labs(title='Total variation \n (by origin, percentile travel time, and number of Monte Carlo draws)', y = 'Total variation', x= 'Monte Carlo draws') + 
  theme_grey() + 
  theme(legend.position = 'None', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5))

mean_cell_rmsd %>%
  ggplot() +
  aes(draws, mrmsd, color=origin) +
  geom_point() +
  geom_line(size = 0.25, linetype = 'dashed') + 
  scale_x_continuous(limits = c(60, 1200), breaks = c(60, 480, 1200)) +
  scale_y_continuous(position = 'right', breaks = seq(0, 2, 1), minor_breaks = seq(0, 2, 0.5)) +
  expand_limits(y = c(0, 2)) +
  facet_grid(origin ~ percentile, labeller = labeller(origin = as_labeller(origin_labeller), percentile = as_labeller(pctile_labeller)), switch = 'y') + 
  labs(title='RMSD, mean across raster cells reachable in all trials \n (by origin, percentile travel time, and number of Monte Carlo draws)', y = 'RMSD (Minutes)', x= 'Monte Carlo draws') + 
  theme_grey() + 
  theme(legend.position = 'None', panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(hjust = 0.5))

names(cv_stack) = layer_names

plot(range_stack, zlim=c(0, 10), nc = 3, nr = 3, axes = FALSE)

names(sd_stack) = layer_names

plot(sd_stack, zlim=c(0, 120), nc = 3, nr = 3, axes = FALSE)


save_contours = function(origin, draws, percentiles) {
  for (j in 1:length(draws)) {
    for (k in 1:length(percentiles)) {
      r_file = glue('{r_dir}/{origin}/{draws[j]}_stack_{k}.tif')
      r_band = raster(r_file, band = 1)
      r_band[is.na(r_band)] = 120
      sldf = rasterToContour(r_band, levels = c(30, 60))
      
      for (i in 2:20) {
        r_band = raster(r_file, band = i)
        r_band[is.na(r_band)] = 120
        sldf = rbind(sldf, rasterToContour(r_band, levels = c(30, 60)))  
      }
      
      c_file = glue('{r_dir}/{origin}/contour/{draws[j]}_contour_{percentiles[k]}.geojson')
      geojson_write(sldf, file = c_file, convert_wgs84 = TRUE)
    }
  }
}



