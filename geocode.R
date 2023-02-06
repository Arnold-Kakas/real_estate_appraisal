library(pacman)

p_load(tidyverse, rio, ggmap)

# load csv with municipalities
municipalities <- import("data/geospatial_data/municipalities.csv")
municipalities <- municipalities %>% 
  mutate(address = paste(Municipality, District, "Slovakia", sep = ", "))

# geocode
api_key <- ""
register_google(key = api_key)
places <- tibble(municipalities$address)
geocodes <- geocode(municipalities$address, output = "latlon", source = "google")
geocodes <- cbind(places, geocodes)

rm(api_key, places)

# write csv with geocodes
write.csv2(geocodes, "data/geospatial_data/geocodes.csv")
