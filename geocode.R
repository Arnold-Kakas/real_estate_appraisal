# Check if 'pacman' package is installed; if not, install it
if (!require("pacman")) {
  install.packages("pacman")
}

# Load required packages
pacman::p_load(tidyverse, tidygeocoder, sf)

# Read geospatial data for communes
communes <- readRDS("data/geospatial_data/communes.RDS") %>% 
  select(NAME_NSI)

# Read advertisements data
advertisements_complete <- readRDS("data/advertisements_complete.RDS") 

# Extract unique addresses from advertisements data
advertisements <- advertisements_complete %>% 
  select(address) %>% 
  rename(address1 = address) %>% 
  as_tibble() %>% 
  unique()

# Geocode addresses using OpenStreetMap method
geocoded_advertisements <- geocode(advertisements, 
                            address = address1, 
                            method = 'osm', 
                            lat = latitude , 
                            long = longitude)

# Identify missing geocoded addresses
geocoded_missing <- geocoded_advertisements %>% 
  filter(is.na(latitude)) %>%
  select(
    -latitude,
    -longitude
  ) %>% 
  mutate(address2 = str_extract(address1, 
                                "^[^,]+,[^,]+")
  ) %>% 
  unique()

# Geocode missing addresses using OpenStreetMap method
geocoded_advertisements2 <- geocode(geocoded_missing, 
                             address = address2, 
                             method = 'osm', 
                             lat = latitude2 , 
                             long = longitude2)

# Join geocoded addresses with advertisements data
advertisements_complete_geocoded <- advertisements_complete %>% 
  mutate(address2 = str_extract(address, 
                                "^[^,]+,[^,]+")
  ) %>% 
  left_join(geocoded_advertisements, by = c("address" = "address1"), keep = FALSE, multiple = "first") %>% 
  left_join(geocoded_advertisements2, by = "address2", keep = FALSE, multiple = "first") %>% 
  mutate(lat = coalesce(latitude, latitude2),
         long = coalesce(longitude, longitude2)) %>% 
  select(-latitude, -latitude2, -longitude, -longitude2) %>% 
  filter(lat <= 49.613611 & lat >= 47.75740 & long <= 22.565833 & long >= 16.833333) %>%  
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# Spatial join with communes data
advertisements_complete_geocoded <- communes %>% 
  st_join(advertisements_complete_geocoded, join = st_contains)

# Save the final geocoded advertisements data
saveRDS(advertisements_complete_geocoded, "data/advertisements_complete_geocoded.RDS")
