library(pacman)

p_load(rio, tidyverse, rvest, httr, doParallel, furrr, RSelenium, remotes)

# scrape through nehnutelnosti web page and retrieve data advertisements for the sale of apartments and houses
site <- "https://www.nehnutelnosti.sk/slovensko/predaj/?p[categories][ids]=1.2&p[order]=1&p[page]="

# scrape the number of pages
number_of_pages <- read_html(paste0(site, 1)) %>%
  html_nodes(xpath = '//*[@class="component-pagination__items d-flex align-items-center"]') %>%
  html_elements("li") %>%
  html_text(trim = TRUE) %>%
  as.numeric() %>%
  discard(is.na) %>%
  max()

# list of additional info we want to retrieve later on

info_names <- c("Stav", "Úžit. plocha", "Zast. plocha", "Plocha pozemku", "Provízia zahrnutá v cene")

# create a cluster of worker processes (cores)
plan(multisession, workers = 6)

advertisements <- future_map_dfr(1:number_of_pages, function(i) {
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
  tibble(price = price, type_of_real_estate = type_of_real_estate, address = address, link = link)
})

# Additional info from web

additional_info_df <- map_dfr(advertisements$link, function(i) {
  temp <- read_html(GET(i, timeout(60)))

  info_details <- temp %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "col-md-4", " " ))]//div') %>%
    html_text2()

  info <- as.data.frame(info_details) %>%
    separate(info_details, sep = ": ", c("info", "status")) %>%
    filter(info %in% info_names) %>%
    pivot_wider(names_from = "info", values_from = "status")

  temp_col_names <- colnames(info)

  condition <- ifelse("Stav" %in% temp_col_names, info$Stav, NA)
  usable_area <- ifelse("Úžit. plocha" %in% temp_col_names, info$"Úžit. plocha", NA)
  built_up_area <- ifelse("Zast. plocha" %in% temp_col_names, info$"Zast. plocha", NA)
  land_area <- ifelse("Plocha pozemku" %in% temp_col_names, info$"Plocha pozemku", NA)
  commission_in_price <- ifelse("Provízia zahrnutá v cene" %in% temp_col_names, info$"Provízia zahrnutá v cene", NA)

  tibble(condition = condition, usable_area = usable_area, built_up_area = built_up_area, land_area = land_area, commission_in_price = commission_in_price)
})






text_long <- map_dfr(advertisements$link, function(i) {
  temp <- read_html(GET(i, timeout(60)))
  
  info_text <- temp %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "text-inner", " " ))]') %>%
    html_text2() %>%
    as.character() %>%
    str_trim() %>% 
    str_squish()
  
  tibble(url = i, info_text = info_text)
})







# bind ads and additional info dataframes
advertisements <- cbind(advertisements, additional_info_df)

# clean data
advertisements_cleaned <- advertisements %>%
  separate(type_of_real_estate, c("type", "area"), sep = " • ") %>%
  separate(address, c("a", "b", "c"), sep = ", ", remove = TRUE) %>%
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
  unite("address", c(5, 4), sep = ", ", na.rm = TRUE, remove = FALSE) %>% # new address for geocoding
  unite("address0", c(6, 5, 4), sep = ", ", na.rm = TRUE, remove = TRUE) %>% # reordering to keep all districts in first column
  separate(address0, c("district", "municipality", "street"), sep = ", ") %>%
  filter(str_detect(district, "okres")) %>%
  mutate(price = as.numeric(str_replace_all(price, " ",""))) %>% 
  select(-area)


# save the old file to histo folder for further use in predictive analyses

if (file.exists("data/advertisements.rds")) {
   histo_rds <- import("data/advertisements.rds") %>% 
  mutate(timestamp = as.Date(file.info("data/advertisements.rds")$ctime))
  histo_date <- file.info("data/advertisements.rds")$ctime %>% as.Date() %>% as.character() %>% str_replace_all("-", "_")
  saveRDS(histo_rds, paste0("data/histo/advertisements", histo_date, ".rds"))
}

# save scraped data
# write.csv2(text_long, "data/texts.csv") 
saveRDS(text_long, file = "data/texts.rds") # instead of csv due to size reduction

# write.csv2(advertisements_cleaned, "data/advertisements.csv")
saveRDS(advertisements_cleaned, file = "data/advertisements.rds") # instead of csv due to size reduction


remote_driver <- remoteDriver(remoteServerAddr = "localhost", port = 4445, browserName = "chrome")
remote_driver$open()
remote_driver$navigate("https://www.nehnutelnosti.sk/5000493/4-izbovy-rodinny-dom-brezova-ulica-stupava/")
remote_driver$findElement(using = "xpath", '/html/body/div[5]/div[1]/section/div[1]/div[2]/div/div[1]/div[5]/div[1]/div[1]/div/div[2]/div[1]/div/div/p[1]/span')


remDr <-  remoteDriver$new()
remDr$open()
remDr$navigate("https://www.nehnutelnosti.sk/5000493/4-izbovy-rodinny-dom-brezova-ulica-stupava/")
remDr$navigate()
  