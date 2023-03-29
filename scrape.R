library(pacman)

p_load(rio, tidyverse, rvest, httr, doParallel, furrr, RSelenium, netstat)

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
  Sys.sleep(3) # wait 3 seconds
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

# Define a wrapper function that handles the errors
safe_find_element <- possibly(function(page, xpath) {
  page$findElement(using = "xpath", xpath)
}, NA_character_)


# Additional info from web

# start the server
rs_driver_object <- rsDriver(
  browser = "chrome",
  chromever = "111.0.5563.41",
  verbose = FALSE,
  port = free_port(random = TRUE)
)

# create a client object
remDr <- rs_driver_object$client
# open a browser
remDr$open()
remDr$navigate("https://www.nehnutelnosti.sk/")
Sys.sleep(3) # wait for 5 seconds
# deal with cookies
remDr$switchToFrame(remDr$findElement(using = "xpath", '//*[@id="sp_message_iframe_710573"]'))
remDr$findElement(using = "xpath", '//*[@id="notice"]/div[5]/div[2]/button')$clickElement()

# wait for another pop up window
Sys.sleep(20)

height <- as.numeric(remDr$executeScript("return document.documentElement.scrollHeight")) / 2
remDr$executeScript(paste("window.scrollTo(0, ", height, ");")) # scroll to middle of the page

# decline option
remDr$findElement(using = "xpath", '//*[@id="onesignal-slidedown-cancel-button"]')$clickElement()

advertisements[8510, ] %>% select(link)

additional_info_df <- map_dfr(advertisements$link, function(i) {
  page_check <- possibly(
    remDr$navigate(i) %>%
      Sys.sleep(5),
    NA_character_
  )

  if (is.na(page_check)) {
    info_text <- NA_character_
    condition <- NA_character_
    usable_area <- NA_character_
    built_up_area <- NA_character_
    land_area <- NA_character_
    commission_in_price <- NA_character_
    index <- NA_character_
    additional_chars <- NA_character_

    tibble(info_text = info_text, additional_characteristics = additional_chars, index_of_living = index, condition = condition, usable_area = usable_area, built_up_area = built_up_area, land_area = land_area, commission_in_price = commission_in_price)
  } else {
    remDr$navigate(i)
    Sys.sleep(5) # wait for 5 seconds

    height <- as.numeric(remDr$executeScript("return document.documentElement.scrollHeight")) - 2400 # 2400 pixels = lenght from botton which does not change
    remDr$executeScript(paste("window.scrollTo(0, ", height, ");")) # scroll to living index

    Sys.sleep(3)

    # tryCatch({
    #   suppressMessages(page <- remDr$findElement(using = 'xpath', '//*[@id="map-filter-container"]'))
    # }, error = function(e) {
    #   page <- NA_character_
    # })

    page <- safe_find_element(remDr, '//*[@id="map-filter-container"]')

    if (is.na(page)) {
      info_text <- NA_character_
      condition <- NA_character_
      usable_area <- NA_character_
      built_up_area <- NA_character_
      land_area <- NA_character_
      commission_in_price <- NA_character_
      index <- NA_character_
      additional_chars <- NA_character_

      tibble(info_text = info_text, additional_characteristics = additional_chars, index_of_living = index, condition = condition, usable_area = usable_area, built_up_area = built_up_area, land_area = land_area, commission_in_price = commission_in_price)
    } else {
      page_html <- page$getElementAttribute("outerHTML")
      page_html <- read_html(page_html[[1]])

      info_text <- page_html %>%
        html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "text-inner", " " ))]') %>%
        html_text2() %>%
        as.character() %>%
        str_trim() %>%
        str_squish()

      info_details <- page_html %>%
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


      tryCatch(
        {
          index <- page_html %>%
            html_nodes(xpath = '//*[@id="totalCityperformerWrapper"]/div/p[1]/span') %>%
            html_text2() # %>%
          # str_extract("[:digit:]+") %>%
          # as.numeric()
        },
        error = function(e) {
          index <- NA
        }
      )

      tryCatch(
        {
          additional_chars <- page_html %>%
            html_nodes(xpath = '//*[@id="additional-features-modal-button"]/ul') %>%
            html_text() %>%
            str_squish()
        },
        error = function(e) {
          additional_chars <- NA
        }
      )
      tibble(info_text = info_text, additional_characteristics = additional_chars, index_of_living = index, condition = condition, usable_area = usable_area, built_up_area = built_up_area, land_area = land_area, commission_in_price = commission_in_price)
    }
  }
})

rs_driver_object$client$close()
rs_driver_object$server$stop()
rm(rs_driver_object, remDr)
gc()

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
  mutate(price = as.numeric(str_replace_all(price, " ", ""))) %>%
  select(-area)


# save the old file to histo folder for further use in predictive analyses

if (file.exists("data/advertisements.rds")) {
  histo_rds <- import("data/advertisements.rds") %>%
    mutate(timestamp = as.Date(file.info("data/advertisements.rds")$ctime))
  histo_date <- file.info("data/advertisements.rds")$ctime %>%
    as.Date() %>%
    as.character() %>%
    str_replace_all("-", "_")
  saveRDS(histo_rds, paste0("data/histo/advertisements", histo_date, ".rds"))
}

# save scraped data
# write.csv2(text_long, "data/texts.csv")
saveRDS(text_long, file = "data/texts.rds") # instead of csv due to size reduction

# write.csv2(advertisements_cleaned, "data/advertisements.csv")
saveRDS(advertisements_cleaned, file = "data/advertisements.rds") # instead of csv due to size reduction




#
# # Start a Docker container running the Selenium Chrome browser image
# system("docker run -d -p 4445:4444 selenium/standalone-chrome")
# # Check that the container is running
# system("docker ps")
#
# remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
#                                  port = 4445L,
#                                  browserName = "chrome")
# # open a browser
# remDr$open()
# remDr$navigate("https://www.nehnutelnosti.sk/4968526/arvin-benet-moderny-nadcasovy-4i-rodinny-dom")
# webElem <- remDr$findElement("css", "body")
# webElem$sendKeysToElement(list(key = "end"))
# pagesource <- remDr$findElement(using = 'xpath', '//*[contains(concat( " ", @class, " " ), concat( " ", "align-self-stretch", " " ))]')
# page_html <- pagesource$getPageSource()[[1]] %>% xml2::read_html()
# page_html %>% html_nodes(xpath = '//*[@id="totalCityperformerWrapper"]/div/p[1]/span') %>% html_text2()
#
#
# # Stop Docker container
# system(paste0("docker stop ", container_id))
#
# # Remove Docker container
# system(paste0("docker rm ", container_id))
