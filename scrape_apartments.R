library(pacman)

p_load(rio, tidyverse, rvest, httr, doParallel, furrr, RSelenium, netstat)

# apartments page
site <- "https://www.nehnutelnosti.sk/slovensko/byty/predaj/?p[page]="

# scrape the number of pages
number_of_pages <- read_html(paste0(site, 1)) %>% # read_html(paste0(site, 1))
  html_nodes(xpath = '//*[@id="content"]/div[8]/div/div/div[1]/div[17]/div/div/ul/li[5]') %>%
  html_elements("a") %>%
  html_text(trim = TRUE) %>%
  as.numeric()

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

# Delete all cookies from the last 24 hours
clearCookies <- function(remDr) {
  remDr$deleteAllCookies()
}

# Define a function that handles the errors in page load
navigate_with_retry <- function(link, remDr) {
  success <- FALSE
  while (!success) {
    tryCatch(
      {
        remDr$navigate(link)
        Sys.sleep(5)
        success <- TRUE
      },
      error = function(e) {
        cat("Failed to navigate to", link, "- Retrying in 10 seconds...\n")
        clearCookies(remDr)
        Sys.sleep(10)
      }
    )
  }
}

# Define a wrapper function that handles the errors in element search
safe_find_element <- possibly(function(page, xpath) {
  page$findElement(using = "xpath", xpath)
}, NA)

# function to get text or return NA if not found
get_text_or_na <- function(nodes) {
  tryCatch(
    {
      text <- nodes %>%
        html_text2() %>%
        as.character() #%>%
        #str_trim() %>%
        #str_squish()
      if (text == "") NA else text
    },
    error = function(e) {
      NA
    }
  )
}

# Additional info from web

################################################################################################
# number of splits
num_splits <- 10
split_size <- ceiling(nrow(advertisements) / num_splits)

# split the data frame into subsets
advertisments_list <- split(advertisements, rep(1:num_splits, each = split_size, length.out = nrow(advertisements)))

for (i in seq_along(advertisments_list)) {
  assign(paste0("advertisements_", i), advertisments_list[[i]])
}


################################################################################################
# create empty dataframeoutside of the loop to hold additional info
additional_info_df <- tibble(link = character(),
  info_text = character(),
  additional_characteristics = character(),
  index_of_living = character(),
  info_details = character(),
  stringsAsFactors = FALSE
)

start_time <- Sys.time()

for (i in 1:10) {
  # get the current dataframe
  current_df <- get(paste0("advertisements_", i))

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

  # navigate to a website
  remDr$navigate("https://www.nehnutelnosti.sk/")
  Sys.sleep(5) # wait for 5 seconds

  # accept cookies
  remDr$switchToFrame(remDr$findElement(using = "xpath", '//*[@id="sp_message_iframe_710573"]'))
  remDr$findElement(using = "xpath", '//*[@id="notice"]/div[5]/div[2]/button')$clickElement()
  
# ##########################################################################################################################################################
#        delete if code works 
# ##########################################################################################################################################################  
#   #scroll
#   height <- as.numeric(remDr$executeScript("return document.documentElement.scrollHeight"))/2 # 3000 pixels = lenght from botton which does not change
#   remDr$executeScript(paste("window.scrollTo(0, ", height, ");"))
# 
#   # wait for pop up window
#   Sys.sleep(15)
#   
#   # decline option
#   tryCatch(
#            {
#              remDr$findElement(using = "xpath", '//*[@id="onesignal-slidedown-cancel-button"]')$clickElement()
#   },
#   error = function(e) {
#     decline <- NA
#   }
#   )
# ##########################################################################################################################################################
  
  # loop through each link in the current dataframe
  for (link in current_df$link) {

    info_text = NA
    additional_characteristics = NA
    index_of_living = NA
    info_details = NA
    
    # additional_info_df <- map_dfr(df$link, function(j) {
    navigate_with_retry(link, remDr)

    height <- as.numeric(remDr$executeScript("return document.documentElement.scrollHeight")) - 3200 # Scroll to load index of living
    remDr$executeScript(paste("window.scrollTo(0, ", height, ");")) # scroll to living index

    Sys.sleep(1)

    page <- safe_find_element(remDr, '//*[@id="map-filter-container"]')

    if (is.na(page)) {
      new_row <- tibble(link = link,
                            info_text = NA,
                            additional_characteristics = NA,
                            index_of_living = NA,
                            info_details = NA)
                              
      # bind new row to additional info dataframe
      additional_info_df <- rbind(additional_info_df, new_row)
      
    } else {
      page_html <- page$getElementAttribute("outerHTML")
      page_html <- read_html(page_html[[1]])

      info_text <- page_html %>%
        html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "text-inner", " " ))]') %>%
        get_text_or_na()

      info_details <- page_html %>%
        html_nodes(xpath = '//*[@id="map-filter-container"]/div[2]/div/div[1]/div[2]/div[5]/ul') %>% 
        html_text2()

      tryCatch(
        {
          index_of_living <- page_html %>%
            html_nodes(xpath = '//*[@id="totalCityperformerWrapper"]/div/p[1]/span') %>%
            get_text_or_na()
        },
        error = function(e) {
          index_of_living <- NA
        }
      )

      tryCatch(
        {
          additional_characteristics <- page_html %>%
            html_nodes(xpath = '//*[@id="additional-features-modal-button"]/ul') %>%
            html_text2()# %>%
            #str_squish()
        },
        error = function(e) {
          additional_characteristics <- NA
        }
      )
      
      new_row <- tibble(link = link, info_text = info_text,
                 additional_characteristics = additional_characteristics,
                 index_of_living = index_of_living,
                 info_details = info_details)
      
      # bind new row to additional info dataframe
      additional_info_df <- rbind(additional_info_df, new_row)
    }
  }

  # close remote driver
  rs_driver_object$client$close()
  rs_driver_object$server$stop()
  rm(rs_driver_object, remDr)
  gc()
}


end_time <- Sys.time()
elapsed_time <- end_time - start_time
etime <- elapsed_time

saveRDS(additional_info_df, "additional_info_df.RDS")
saveRDS(advertisements, "advertisements.RDS")

additional_info_df <- read_rds("additional_info_df.RDS")
advertisements <- read_rds("advertisements.RDS")

# list of characteristics, not all will be used
characteristics <- c("Vlastníctvo", 
                     "Počet izieb/miestností", 
                     "Orientácia", 
                     "Rok výstavby", 
                     "Rok poslednej rekonštrukcie", 
                     "Rok kolaudácie",
                     "Energetický certifikát",
                     "Počet nadzemných podlaží",
                     "Podlažie",
                     "Počet podzemných podlaži",
                     "Umiestnenie",
                     "Typ konštrukcie",
                     "Telekomunikáčné a dátové siete",
                     "Počet balkónov",
                     "Výťah",
                     "Počet lodžií",
                     "Kúrenie",
                     "Pivnica",
                     "Verejné parkovanie",
                     "Vonkajšie parkovacie miesto", # combine under Verejné parkovanie
                     "Plocha predzáhradky",
                     "Plochy pivníc",
                     "Garáž")
patterns <- list(
  Vlastníctvo = "(?<=Vlastníctvo: ).*?(?=\\s|$)",
  `Počet izieb/miestností` = "(?<=Počet izieb/miestností: ).*?(?=\\s|$)",
  Orientácia = "(?<=Orientácia: ).*?(?=\\s|$)",
  `Rok výstavby` = "(?<=Rok výstavby: ).*?(?=\\s|$)",
  `Rok poslednej rekonštrukcie` = "(?<=Rok poslednej rekonštrukcie: ).*?(?=\\s|$)",
  `Rok kolaudácie` = "(?<=Rok kolaudácie: ).*?(?=\\s|$)",
  `Energetický certifikát` = "(?<=Energetický certifikát: ).*?(?=\\s|$)",
  `Počet nadzemných podlaží` = "(?<=Počet nadzemných podlaží: ).*?(?=\\s|$)",
  Podlažie = "(?<=Podlažie: ).*?(?=\\s|$)",
  `Počet podzemných podlaži` = "(?<=Počet podzemných podlaži: ).*?(?=\\s|$)",
  Umiestnenie = "(?<=Umiestnenie: ).*?(?=\\s|$)",
  `Typ konštrukcie` = "(?<=Typ konštrukcie: ).*?(?=\\s|$)",
  `Telekomunikáčné a dátové siete` = "(?<=Telekomunikáčné a dátové siete: ).*?(?=\\s|$)",
  `Počet balkónov` = "(?<=Počet balkónov: ).*?(?=\\s|$)",
  Výťah = "(?<=Výťah: ).*?(?=\\s|$)",
  `Počet lodžií` = "(?<=Počet lodžií: ).*?(?=\\s|$)",
  Kúrenie = "(?<=Kúrenie: ).*?(?=\\s|$)",
  Pivnica = "(?<=Pivnica: ).*?(?=\\s|$)",
  `Verejné parkovanie` = "(?<=Verejné parkovanie: ).*?(?=\\s|$)|(?<=Vonkajšie parkovacie miesto: ).



# info <- as.data.frame(info_details) %>%
#   separate(info_details, sep = ": ", c("info", "status")) %>%
#   filter(info %in% info_names) %>%
#   pivot_wider(names_from = "info", values_from = "status")
# 
# temp_col_names <- colnames(info)
# 
# condition <- ifelse("Stav" %in% temp_col_names, info$Stav, NA)
# usable_area <- ifelse("Úžit. plocha" %in% temp_col_names, info$"Úžit. plocha", NA)
# built_up_area <- ifelse("Zast. plocha" %in% temp_col_names, info$"Zast. plocha", NA)
# land_area <- ifelse("Plocha pozemku" %in% temp_col_names, info$"Plocha pozemku", NA)
# commission_in_price <- ifelse("Provízia zahrnutá v cene" %in% temp_col_names, info$"Provízia zahrnutá v cene", NA)


# use this logic to get data from above:
# split_string <- strsplit(info_details, "\n")
# x <- split_string %>% unlist() %>% as.data.frame()
# separate(x, sep = ": ", c("info", "status")) this needs to be fixed
# then select only relevant cols

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
