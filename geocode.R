library(pacman)

p_load(tidyverse, ggmap, rio)

# load csv with municipalities
municipalities <- import("data/municipalities.csv")
municipalities <- municipalities %>% 
  mutate(address = paste(Municipality, District, "Slovakia", sep = ", "))

# geocode
api_key <- "AIzaSyCDWUgz8htt-DlNGH__ek1p1ycAaxrau6w"
register_google(key = api_key)
places <- tibble(municipalities$address)
geocodes <- geocode(municipalities$address, output = "latlon", source = "google")
geocodes <- cbind(places, geocodes)

rm(api_key, places)

# write csv with geocodes
write.csv2(geocodes, "data/geocodes.csv")
# try map()
# bbox <- c(bottom = 47.3, top = 50 , right = 23, left = 16.5)
# get_stamenmap(bbox, zoom = 6, maptype = "toner-lite") %>% ggmap() 
# # https://jtr13.github.io/cc19/stamen-maps-with-ggmap.html