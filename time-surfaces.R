# Download travel time surface rasters (.geotiff format) and process them to inform comparisons of frequency-based feeds

# Install dependencies if needed
# install.packages('tidyverse')
# install.packages('httr')
# install.packages('raster')
# install.packages('jsonlite')

# Load dependencies
library('tidyverse')
library('httr')
library('raster')
library('jsonlite')

# Config values
server = 'https://staging-api.conveyal.com/api/analysis'

token = readLines(file('raster-config/.auth'))

n_draws = c(0, 60, 120, 240, 480, 960)
n_repli = 20

base_dir = 'raster-dl'

bookmarks = fromJSON(readLines(file('raster-config/bookmarks.json'))) %>% 
  remove_rownames() %>% 
  column_to_rownames('name')

profile_request = fromJSON(readLines(file('raster-config/profile_request.json')))

remove_zeros = function(x) {x[x == 0 ] = NA; return(x)}

# Download and process travel time surfaces for origin corresponding to bookmark name
download_origin = function(origin, n_draws) {

  # Set bookmark details and output directory
  profile_request$fromLat = bookmarks[origin,]$profileRequest$fromLat
  profile_request$fromLon = bookmarks[origin,]$profileRequest$fromLon
  profile_request$fromTime = bookmarks[origin,]$profileRequest$fromTime
  profile_request$toTime = bookmarks[origin,]$profileRequest$toTime
  origin_dir = paste(base_dir, origin, sep = '/')  # `paste` to concatenate in R
  
  # Base directory for results
  if (!dir.exists(base_dir)) {
    dir.create(base_dir)
  }
  
  # Sub-directory for this origin
  if (!dir.exists(origin_dir)) {
    dir.create(origin_dir)
  }

  # Loops to download sample geotiff files for each number of draws
  for (i in n_draws) {
    
    draw_dir = paste(origin_dir, i, sep = '/')
    
    if (!dir.exists(draw_dir)) {
      dir.create(draw_dir)
    }
    
    for (j in 1:n_repli) {
      raster_path = glue('{draw_dir}/{j}.geotiff')
  
      profile_request$monteCarloDraws = i
  
      print(
        POST(
          url = server,
          add_headers(
            Accept = 'image/tiff',
            Authorization = token
          ),
          body = toJSON(profile_request, auto_unbox = TRUE),
          httr::write_disk(path = raster_path, overwrite = TRUE)
        )
      )
      
      cat('\n')
      
    }
  
  }
}

process_downloads = function(origin, n_draws) {

  # Summarize downloaded files
  for (n_draw in n_draws) {
    mean_stack = stack()
    cv_stack = stack()
    min_stack = stack()
    max_stack = stack()
    
    raster_dir = paste(base_dir, origin, n_draw, sep = '/')
    print(raster_dir)
    
    for (percentileIndex in 1:5) {
      print(paste('Processing percentile:', percentileIndex, '/ 5'))
      
      stack_name = paste('combined', n_draw, 'draws', percentileIndex, sep = '_')
      assign(stack_name, stack())
      
      for (j in 1:n_repli) {
        current_raster = raster(paste(raster_dir, '/', j, '.geotiff', sep =''), band = percentileIndex)
        current_raster = calc(current_raster, remove_zeros)
        assign(stack_name, addLayer(get(stack_name), current_raster))
      }
  
      mean_stack = addLayer(mean_stack, mean(get(stack_name)))
      cv_stack = addLayer(cv_stack, cv(get(stack_name)))
      min_stack = addLayer(min_stack, min(get(stack_name)))
      max_stack = addLayer(max_stack, max(get(stack_name)))
      
      # 1 band per replication, all at same percentile
      writeRaster(get(stack_name), paste(raster_dir, 'stack', percentileIndex, sep = '_'), format = 'GTiff')
      
    }
    
    # summary statistics, 1 band per percentile
    writeRaster(mean_stack, paste(raster_dir, 'mean', sep = '_'), format = 'GTiff')
    writeRaster(cv_stack, paste(raster_dir, 'cv', sep = '_'), format = 'GTiff')
    writeRaster(max_stack - min_stack, paste(raster_dir, 'range', sep = '_'), format = 'GTiff')
  }
}

