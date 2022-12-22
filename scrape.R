library(tidyverse)
library(rvest)

# library(usethis)
# usethis::use_git_config(user.name = "Arnold-Kakas", user.email = "kakasarnold@gmail.com")
# 
# install.packages("gitcreds")
# library(gitcreds)
# 
# gitcreds::gitcreds_set()
# 
# gitcreds::gitcreds_get()

# scrape through nehnutelnosti web page and retrieve data advertisements for the sale of apartments and houses

site <- "https://www.nehnutelnosti.sk/predaj/?p[categories][ids]=1.2&p[page]="

number_of_pages <- read_html(paste0(site, 1)) %>%
  html_nodes(xpath = '//*[@class="component-pagination__items d-flex align-items-center"]') %>%
  html_elements("li") %>%
  html_text(trim = TRUE) %>%
  as.numeric() %>%
  discard(is.na) %>%
  max()

# list of additional info we want to retrieve later on

info_names <- c("Stav", "Úžit. plocha", "Zast. plocha", "Plocha pozemku", "Provízia zahrnutá v cene")

# create empty dataframe for ads

advertisements <- data.frame()

# feed empty dataframe

for (i in 1:1) {
  page_content <- read_html(paste0(site, i))
  price <- page_content %>%
    html_nodes(xpath = '//*[@class="advertisement-item--content__price col-auto pl-0 pl-md-3 pr-0 text-right mt-2 mt-md-0 align-self-end"]') %>%
    html_attr("data-adv-price")
  
  type_of_real_estate <- page_content %>%
    html_nodes(xpath = '//*[@class="advertisement-item--content__info"]') %>%
    html_text2()
  
  address <- page_content %>%
    html_nodes(xpath = '//*[@class="advertisement-item--content__info d-block text-truncate"]') %>%
    html_text2()
  
  link <- page_content %>%
    html_nodes(xpath = '//*[@class="mb-0 d-none d-md-block"]') %>%
    html_nodes("a") %>%
    html_attr("href")
  
  advertisements <- rbind(advertisements, data.frame(price, type_of_real_estate, address, link, stringsAsFactors = FALSE))
}

# create empty dataframe for additional ads info

additional_info_df <- data.frame()

# feed empty dataframe

for (i in 1:nrow(advertisements)){
  temp <- read_html(advertisements[i, 4]) %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "col-md-4", " " ))]//div') %>%
    html_text2()
  
  info <- as.data.frame(temp) %>%
    separate(temp, sep = ": ", c("info", "status")) %>%
    filter(info %in% info_names) %>%
    pivot_wider(names_from = "info", values_from = "status")
  
  temp_col_names <- colnames(info)
  
  condition <- ifelse("Stav" %in% temp_col_names, info$Stav, NA)
  usable_area <- ifelse("Úžit. plocha" %in% temp_col_names, info$"Úžit. plocha", NA)
  built_up_area <- ifelse("Zast. plocha" %in% temp_col_names, info$"Zast. plocha", NA)
  land_area <- ifelse("Plocha pozemku" %in% temp_col_names, info$"Plocha pozemku", NA)
  commission_in_price <- ifelse("Provízia zahrnutá v cene" %in% temp_col_names, info$"Provízia zahrnutá v cene", NA)
  
  additional_info_df <- rbind(additional_info_df, data.frame(condition, usable_area, built_up_area, land_area, commission_in_price, stringsAsFactors = FALSE))
}

# bind ads and additional info dataframes

advertisements <- cbind(advertisements, additional_info_df)

# adjust data

advertisements <- advertisements %>%
  separate(type_of_real_estate, c("type", "area"), sep = " • ") %>%
  separate(address, c("a", "b", "c"), sep = ", ") %>%
  mutate(
    rooms = case_when(
      str_detect(type, "byt") ~ substr(type, 1, 1),
      str_detect(type, "Garsónka") ~ "1,5",
      str_detect(type, "Dvojgarsónka") ~ "2,5"
    ),
    price = str_replace(price, " €", ""),
    usable_area = str_replace(usable_area, " m2", ""),
    built_up_area = str_replace(built_up_area, " m2", ""),
    land_area = str_replace(land_area, " m2", ""),
    commission_in_price = ifelse(is.na(commission_in_price) == TRUE, "no", "yes"),
    type = case_when(
      !str_detect(type, "byt") ~ type,
      str_detect(type, "byt") ~ "Byt"
    )
  ) %>%
  unite("address", c(6, 5, 4), sep = ", ", na.rm = TRUE, remove = TRUE) %>%
  separate(address, c("district", "municipality", "street"), sep = ", ") %>%
  filter(price != "Cena dohodou", str_detect(district, "okres")) %>% 
  select(-area)

write.csv2(advertisements, "data/advertisements.csv")
