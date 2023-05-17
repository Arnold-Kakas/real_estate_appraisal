if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, tidygeocoder, sf)

#postcodes <- readRDS("data/geospatial_data/postcodes.RDS")
  
communes <- readRDS("data/geospatial_data/communes.RDS") %>% 
  select(NAME_NSI)


  
# alternative for communes  
  # gisco_get_lau(
  # year = "2020",
  # epsg = "4326",
  # country = "SK")

advertisements_complete <- readRDS("data/advertisements_complete.RDS") 

adverts <- advertisements_complete %>% 
  select(address) %>% 
  rename(address1 = address) %>% 
  as_tibble() %>% 
  unique()

geocoded_adverts <- geocode(adverts, 
                            address = address1, 
                            method = 'osm', 
                            lat = latitude , 
                            long = longitude)

geocoded_missing <- geocoded_adverts %>% 
  filter(is.na(latitude)) %>%
  select(
    -latitude,
    -longitude
         ) %>% 
  mutate(address2 = str_extract(address1, 
                               "^[^,]+,[^,]+")
  ) %>% 
  unique()

geocoded_adverts2 <- geocode(geocoded_missing, 
                address = address2, 
                method = 'osm', 
                lat = latitude2 , 
                long = longitude2)

  
advertisements_complete_geocoded <- advertisements_complete %>% 
  mutate(address2 = str_extract(address, 
                                "^[^,]+,[^,]+")
         ) %>% 
  left_join(geocoded_adverts, join_by(address == address1), keep = FALSE, multiple = "first") %>% 
  left_join(geocoded_adverts2, by = "address2", keep = FALSE, multiple = "first") %>% 
  mutate(lat = coalesce(latitude, latitude2),
         long = coalesce(longitude, longitude2)) %>% 
  select(-latitude, -latitude2, -longitude, -longitude2) %>% 
  filter(lat <= 49.613611 & lat >= 47.75740 & long <= 22.565833 & long >= 16.833333) %>%  # remove point beyond SK min/max values
  st_as_sf(coords = c("long", "lat"), crs = 4326)
  
advertisements_complete_geocoded <- communes %>% 
   st_join(advertisements_complete_geocoded, join = st_contains)




saveRDS(advertisements_complete_geocoded, "data/advertisements_complete_geocoded.RDS")