# Install packages. sf is finicky, try installing it first.
install.packages('sf')
install.packages('tidyverse')
install.packages('ggplot2')
install.packages('tidytransit')

library('sf')
library('tidyverse')
library('ggplot2')
library('tidytransit')

# Read GTFS
gtfs <- read_gtfs("data/calgary-2019/2019.zip")
gtfs <- set_date_service_table(gtfs)
gtfs <- set_servicepattern(gtfs)

date_table <- gtfs$.$date_servicepattern_table

service_table <- gtfs$.$service_pattern

# Paste service_id values below after inspecting two tables above
# get_stop_frequency is code built into the tidytransit package
am_freq <- get_stop_frequency(gtfs, start_hour = 7, end_hour = 8, service_ids = 
                                c('1BUSWK-Weekday-4-2019MA-1111100', 
                                  '1BUSWK-Weekday-4-2019MA-1111000',
                                  '1BUSWK-Weekday-4-2019MA-0010000',
                                  '1BUSWK-Weekday-4-2019MA-0000100'
                                  
))

stops <- left_join(gtfs$stops, am_freq, by='stop_id') %>%
  group_by(stop_id) %>%
  transmute(total_departures=sum(departures, na.rm = TRUE))

stops <- distinct(right_join(gtfs$stops, stops, by='stop_id'))