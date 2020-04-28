# Load dependencies
library('tidyverse')
library('httr')
library('raster')
library('jsonlite')
library(glue)

base_dir = 'raster-dl'

accessibility_dir = 'accessibility'

server = 'https://staging-api.conveyal.com/api/regional/'

token = readLines(file('raster-config/.auth'))

# Parallel arrays of ids and names, using
# temp1.map(r => r._id) and temp1.map(r => r.name)

regional_analyses = fromJSON(readLines(file('raster-config/regional_analyses.json')))

raster_path = 'accessibility.geotiff'

if (!dir.exists(base_dir)) {
  dir.create(base_dir)
}

if (!dir.exists(glue('{base_dir}/{accessibility_dir}'))) {
  dir.create(glue('{base_dir}/{accessibility_dir}'))
}

names = vector()

for (n in 1:length(regional_analyses$ids)){
  
  analysis_id = regional_analyses$ids[n]
  name = str_replace_all(strsplit(regional_analyses$names[n],": ")[[1]][2], " ", "_")
  
  if (!dir.exists(glue('{base_dir}/{accessibility_dir}/{name}'))) {
    dir.create(glue('{base_dir}/{accessibility_dir}/{name}'))
  }
  
  if (!(name %in% names)) {
    names = append(names, name)
  }
  
  resp = content(
    GET(
      url = glue({server}, '/', {analysis_id}, '/grid/tiff?redirect=false'),
      add_headers(
        Authorization = token
      )
    )
  )
  
  GET(
    url = resp$url,
    httr::write_disk(path = glue('{base_dir}/{accessibility_dir}/{name}/{analysis_id}.geotiff'), overwrite = TRUE)
  )
}

for (name in names) {
  dir = glue('{base_dir}/{accessibility_dir}/{name}')
  
  if (!dir.exists(glue('{dir}/summary'))) {
    dir.create(glue('{dir}/summary'))
  }
  
  stack = stack()
  files = list.files(dir)
  
  for (file in list.files(dir)) {
    if (file != 'summary') {
      stack = addLayer(stack, raster(glue('{dir}/{file}'))) 
    }
  }
  
  writeRaster(mean(stack), glue('{dir}/summary/mean'), format = 'GTiff')
  writeRaster(cv(stack), glue('{dir}/summary/cv'), format = 'GTiff')
  writeRaster(max(stack)-min(stack), glue('{dir}/summary/range'), format = 'GTiff')
  
}