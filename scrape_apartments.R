if (!require("pacman")) {
  install.packages("pacman")
}


pacman::p_load(
  rio,
  tidyverse,
  rvest,
  httr,
  doParallel,
  furrr,
  RSelenium,
  netstat
)

# apartments page
site <- "https://www.nehnutelnosti.sk/slovensko/byty/predaj/?p[param1][from]=1000&p[param1][to]=&p[page]="

# scrape the number of pages
number_of_pages <- read_html(paste0(site, 1)) %>%
  html_nodes(xpath = '//*[@id="content"]/div[7]/div/div/div[1]/div[17]/div/div/ul/li[5]') %>%
  html_elements("a") %>%
  html_text(trim = TRUE) %>%
  as.numeric()

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
        as.character() # %>%
      # str_trim() %>%
      # str_squish()
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
# create empty dataframe outside of the loop to hold additional info
additional_info_df <- tibble(
  link = character(),
  info_text = character(),
  additional_characteristics = character(),
  index_of_living = character(),
  environment = character(),
  quanlity_of_living = character(),
  safety = character(),
  transport = character(),
  services = character(),
  relax = character(),
  info_details = character(),
  stringsAsFactors = FALSE
)

# starting from this part, the code had been inside of fir i 1:10 loop

# start the server
rs_driver_object <- rsDriver(
  browser = "chrome",
  chromever = "119.0.6045.105",
  verbose = FALSE,
  port = free_port(random = TRUE)
)

# create a client object
remDr <- rs_driver_object$client

# open a browser
remDr$open()
remDr$maxWindowSize()

# navigate to a website
remDr$navigate("https://www.nehnutelnosti.sk/")
Sys.sleep(5) # wait for 5 seconds

# accept cookies
# switch to cookie iframe
remDr$switchToFrame(remDr$findElement(using = "xpath", '//*[@id="sp_message_iframe_920334"]'))
remDr$findElement(using = "xpath", '//*[@id="notice"]/div[2]/button')$clickElement()

# switch back to default frame
remDr$switchToFrame(NA)

# manually log in with azet acc

# code originally inside for loop ends here

for (i in 1:10) { #10, if ok, go to 2:10
  # get the current dataframe
  current_df <- get(paste0("advertisements_", i))

  # loop through each link in the current dataframe
  # testing link
  #link <- 'https://www.nehnutelnosti.sk/5335404/slnecny-3-izbovy-byt-na-prenajom-volny-od-01-02-2024-tn-zlatovce/'
  
  for (link in current_df$link) {
    info_text <- NA
    additional_characteristics <- NA
    index_of_living <- NA
    environment <- NA
    quanlity_of_living <- NA
    safety <- NA
    transport <- NA
    services <- NA
    relax <- NA
    info_details <- NA

    navigate_with_retry(link, remDr)
    #remDr$executeScript("document.body.style.zoom = '50%';")
    height <- as.numeric(remDr$executeScript("return document.documentElement.scrollHeight"))/10*4.2 # Scroll to load index of living
    remDr$executeScript(paste("window.scrollTo(0, ", height, ");")) # scroll to living index
    
    
    Sys.sleep(1)
    page <- safe_find_element(remDr, '//*[@id="map-filter-container"]')

    if (is.na(page)) {
      new_row <- tibble(
        link = link,
        info_text = NA,
        additional_characteristics = NA,
        index_of_living = NA,
        environment = NA,
        quanlity_of_living = NA,
        safety = NA,
        transport = NA,
        services = NA,
        relax = NA,
        info_details = NA
      )

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
          environment <- page_html %>%
            html_nodes(xpath = '//*[@id="map-filter-container"]/div[2]/div/div[1]/div[4]/div[1]/div[1]/div/div[2]/div[2]/div[1]/div[1]/div[2]/span[1]/span') %>%
            get_text_or_na()
        },
        error = function(e) {
          environment <- NA
        }
      )
      
      tryCatch(
        {
          quanlity_of_living <- page_html %>%
            html_nodes(xpath = '//*[@id="map-filter-container"]/div[2]/div/div[1]/div[4]/div[1]/div[1]/div/div[2]/div[2]/div[1]/div[2]/div[2]/span[1]/span') %>%
            get_text_or_na()
        },
        error = function(e) {
          quanlity_of_living <- NA
        }
      )
      
      tryCatch(
        {
          safety <- page_html %>%
            html_nodes(xpath = '//*[@id="map-filter-container"]/div[2]/div/div[1]/div[4]/div[1]/div[1]/div/div[2]/div[2]/div[1]/div[3]/div[2]/span[1]/span') %>%
            get_text_or_na()
        },
        error = function(e) {
          safety <- NA
        }
      )
      
      tryCatch(
        {
          transport <- page_html %>%
            html_nodes(xpath = '//*[@id="map-filter-container"]/div[2]/div/div[1]/div[4]/div[1]/div[1]/div/div[2]/div[2]/div[2]/div[1]/div[2]/span[1]/span') %>%
            get_text_or_na()
        },
        error = function(e) {
          transport <- NA
        }
      )
      
      tryCatch(
        {
          services <- page_html %>%
            html_nodes(xpath = '//*[@id="map-filter-container"]/div[2]/div/div[1]/div[4]/div[1]/div[1]/div/div[2]/div[2]/div[2]/div[2]/div[2]/span[1]/span') %>%
            get_text_or_na()
        },
        error = function(e) {
          services <- NA
        }
      )
      
      tryCatch(
        {
          relax <- page_html %>%
            html_nodes(xpath = '//*[@id="map-filter-container"]/div[2]/div/div[1]/div[4]/div[1]/div[1]/div/div[2]/div[2]/div[2]/div[3]/div[2]/span[1]/span') %>%
            get_text_or_na()
        },
        error = function(e) {
          relax <- NA
        }
      )
      
     tryCatch(
        {
          additional_characteristics <- page_html %>%
            html_nodes(xpath = '//*[@id="additional-features-modal-button"]/ul') %>%
            html_text2() # %>%
          # str_squish()
        },
        error = function(e) {
          additional_characteristics <- NA
        }
      )

      new_row <- tibble(
        link = link, info_text = info_text,
        additional_characteristics = additional_characteristics,
        index_of_living = index_of_living,
        environment = environment,
        quanlity_of_living = quanlity_of_living,
        safety = safety,
        transport = transport,
        services = services,
        relax = relax,
        info_details = info_details
      )

      # bind new row to additional info dataframe
      additional_info_df <- rbind(additional_info_df, new_row)
    }
  }

}

#code below was last before curly bracket

# close remote driver
rs_driver_object$client$close()
rs_driver_object$server$stop()
rm(rs_driver_object, remDr)
gc()

# until here

advertisements_round_2 <- additional_info_df %>% 
  filter(!is.na(index_of_living) & is.na(relax)) %>% 
  select(link)

additional_info_df_round_2 <- tibble(
  link = character(),
  info_text = character(),
  additional_characteristics = character(),
  index_of_living = character(),
  environment = character(),
  quanlity_of_living = character(),
  safety = character(),
  transport = character(),
  services = character(),
  relax = character(),
  info_details = character(),
  stringsAsFactors = FALSE
)

# start the server
rs_driver_object <- rsDriver(
  browser = "chrome",
  chromever = "119.0.6045.105",
  verbose = FALSE,
  port = free_port(random = TRUE)
)

# create a client object
remDr <- rs_driver_object$client

# open a browser
remDr$open()
remDr$maxWindowSize()

# navigate to a website
remDr$navigate("https://www.nehnutelnosti.sk/")
Sys.sleep(5) # wait for 5 seconds

# accept cookies
# switch to cookie iframe
remDr$switchToFrame(remDr$findElement(using = "xpath", '//*[@id="sp_message_iframe_920334"]'))
remDr$findElement(using = "xpath", '//*[@id="notice"]/div[2]/button')$clickElement()

# switch back to default frame
remDr$switchToFrame(NA)

# manually log in with azet acc


for (link in advertisements_round_2$link) {
  info_text <- NA
  additional_characteristics <- NA
  index_of_living <- NA
  environment <- NA
  quanlity_of_living <- NA
  safety <- NA
  transport <- NA
  services <- NA
  relax <- NA
  info_details <- NA
  
  navigate_with_retry(link, remDr)
  #remDr$executeScript("document.body.style.zoom = '50%';")
  height <- as.numeric(remDr$executeScript("return document.documentElement.scrollHeight"))/10*4.2 # Scroll to load index of living
  remDr$executeScript(paste("window.scrollTo(0, ", height, ");")) # scroll to living index
  
  
  Sys.sleep(1)
  page <- safe_find_element(remDr, '//*[@id="map-filter-container"]')
  
  if (is.na(page)) {
    new_row <- tibble(
      link = link,
      info_text = NA,
      additional_characteristics = NA,
      index_of_living = NA,
      environment = NA,
      quanlity_of_living = NA,
      safety = NA,
      transport = NA,
      services = NA,
      relax = NA,
      info_details = NA
    )
    
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
        environment <- page_html %>%
          html_nodes(xpath = '//*[@id="map-filter-container"]/div[2]/div/div[1]/div[5]/div[1]/div[1]/div/div[2]/div[2]/div[1]/div[1]/div[2]/span[1]/span') %>%
          get_text_or_na()
      },
      error = function(e) {
        environment <- NA
      }
    )
    
    tryCatch(
      {
        quanlity_of_living <- page_html %>%
          html_nodes(xpath = '//*[@id="map-filter-container"]/div[2]/div/div[1]/div[5]/div[1]/div[1]/div/div[2]/div[2]/div[1]/div[2]/div[2]/span[1]/span') %>%
          get_text_or_na()
      },
      error = function(e) {
        quanlity_of_living <- NA
      }
    )
    
    tryCatch(
      {
        safety <- page_html %>%
          html_nodes(xpath = '//*[@id="map-filter-container"]/div[2]/div/div[1]/div[5]/div[1]/div[1]/div/div[2]/div[2]/div[1]/div[3]/div[2]/span[1]/span') %>%
          get_text_or_na()
      },
      error = function(e) {
        safety <- NA
      }
    )
    
    tryCatch(
      {
        transport <- page_html %>%
          html_nodes(xpath = '//*[@id="map-filter-container"]/div[2]/div/div[1]/div[5]/div[1]/div[1]/div/div[2]/div[2]/div[2]/div[1]/div[2]/span[1]/span') %>% 
          get_text_or_na()
      },
      error = function(e) {
        transport <- NA
      }
    )
    
    tryCatch(
      {
        services <- page_html %>%
          html_nodes(xpath = '//*[@id="map-filter-container"]/div[2]/div/div[1]/div[5]/div[1]/div[1]/div/div[2]/div[2]/div[2]/div[2]/div[2]/span[1]/span') %>%
          get_text_or_na()
      },
      error = function(e) {
        services <- NA
      }
    )
    
    tryCatch(
      {
        relax <- page_html %>%
          html_nodes(xpath = '//*[@id="map-filter-container"]/div[2]/div/div[1]/div[5]/div[1]/div[1]/div/div[2]/div[2]/div[2]/div[3]/div[2]/span[1]/span') %>%
          get_text_or_na()
      },
      error = function(e) {
        relax <- NA
      }
    )
    
    tryCatch(
      {
        additional_characteristics <- page_html %>%
          html_nodes(xpath = '//*[@id="additional-features-modal-button"]/ul') %>%
          html_text2() # %>%
        # str_squish()
      },
      error = function(e) {
        additional_characteristics <- NA
      }
    )
    
    new_row <- tibble(
      link = link, info_text = info_text,
      additional_characteristics = additional_characteristics,
      index_of_living = index_of_living,
      environment = environment,
      quanlity_of_living = quanlity_of_living,
      safety = safety,
      transport = transport,
      services = services,
      relax = relax,
      info_details = info_details
    )
    
    # bind new row to additional info dataframe
    additional_info_df_round_2 <- rbind(additional_info_df_round_2, new_row)
  }}


saveRDS(additional_info_df, "data/additional_info_df.RDS")
saveRDS(advertisements, "data/advertisements.RDS")

additional_info_df <- read_rds("data/additional_info_df.RDS")
advertisements <- read_rds("data/advertisements.RDS")

# clean advertisements data

advertisements_cleaned <- advertisements %>%
  separate(type_of_real_estate, c("type", "area"), sep = " • ", remove = TRUE) %>% 
  separate(address, c("a", "b", "c"), sep = ", ", remove = TRUE) %>%
  unite("address", c(5, 4, 3), sep = ", ", na.rm = TRUE, remove = TRUE) %>% # reordering to keep all districts in first column
  mutate(
    price = str_replace_all(str_replace_all(price, " €", ""), " ", "") %>%
      as.integer(),
    address0 = address
  ) %>%
  separate(address0, c("district", "municipality", "street"), sep = ", ") %>%
  select(-street)

# get additional information from scraped data
# First list of additional info details
characteristics1 <- c(
  "Stav",
  "Úžit. plocha",
  "Energie",
  "Provízia zahrnutá v cene"
)

characteristics1_df <- data.frame(characteristics1, value = NA)
# Second list of additional info details
characteristics2 <- c(
  "Počet izieb/miestností",
  "Orientácia",
  "Rok výstavby",
  "Rok poslednej rekonštrukcie",
  "Energetický certifikát",
  "Počet nadzemných podlaží",
  "Podlažie",
  "Výťah",
  "Typ konštrukcie",
  "Počet balkónov",
  "Počet lodžií",
  "Pivnica"
)

characteristics2_df <- data.frame(characteristics2, value = NA)

characteristics_wrangler <- additional_info_df %>%
  mutate(
    chars1_list = str_split(info_details, "\n"),
    chars2_list = str_split(additional_characteristics, "\n")
  ) %>%
  select(-additional_characteristics, -info_details)

get_characteristics1 <- function(x) {
  temp_df <- x %>%
    unlist() %>%
    as.data.frame()
  temp_df <- rename(temp_df, chars = .)
  temp_df <- temp_df %>%
    separate_wider_delim(chars,
      delim = ": ",
      names = c(
        "info",
        "status"
      )
    ) %>%
    filter(info %in% characteristics1) %>%
    full_join(characteristics1_df, join_by("info" == "characteristics1"), keep = FALSE) %>%
    select(-value) %>%
    pivot_wider(names_from = info, values_from = status)
  return(temp_df)
}

get_characteristics2 <- function(x) {
  temp_df <- x %>%
    unlist() %>%
    as.data.frame()
  temp_df <- rename(temp_df, chars = .)
  temp_df <- temp_df %>%
    separate_wider_delim(chars,
      delim = ": ",
      names = c(
        "info",
        "status"
      )
    ) %>%
    filter(info %in% characteristics2) %>%
    full_join(characteristics2_df, join_by("info" == "characteristics2"), keep = FALSE) %>%
    select(-value) %>%
    pivot_wider(names_from = info, values_from = status)
  return(temp_df)
}

# Apply get_characteristics1() and get_characteristics2() to each row in additional_info_df and combine the results
output_df_characteristics1 <- map_dfr(characteristics_wrangler$chars1_list, get_characteristics1)
output_df_characteristics2 <- map_dfr(characteristics_wrangler$chars2_list, get_characteristics2)

# Add the new columns to additional_info_df
additional_info_df_complete <- cbind(
  additional_info_df %>%
    mutate(index_of_living = str_replace_all(index_of_living, " /", "")) %>%
    select(c(link, 
             info_text, 
             index_of_living,
             environment,
             quanlity_of_living,
             safety,
             transport,
             services,
             relax)) %>% 
    mutate(flag = "x"), 
  output_df_characteristics1,
  output_df_characteristics2
)
advertisements_complete <- advertisements_cleaned %>%
  left_join(additional_info_df_complete, by = "link", multiple = "first") %>%
  filter(!is.na(flag)) %>% 
  select(-flag, -c, -type)

# save the old file advertisements_complete to histo folder for further use in predictive analyses
if (file.exists("data/advertisements_complete.rds")) {
  histo_rds <- import("data/advertisements_complete.rds") %>%
    mutate(timestamp = as.Date(file.info("data/advertisements_complete.rds")$mtime))
  histo_date <- file.info("data/advertisements_complete.rds")$mtime %>%
    as.Date() %>%
    as.character() %>%
    str_replace_all("-", "_")
  saveRDS(histo_rds, paste0("data/histo/advertisements_complete", histo_date, ".rds"))
}

# create separate df for text analyses
text_long <- advertisements_complete$info_text

# save the old file text_long to histo folder for further use in predictive analyses
if (file.exists("data/text_long.rds")) {
  histo_rds <- import("data/text_long.rds") %>%
    as.data.frame() %>% 
    mutate(timestamp = as.Date(file.info("data/text_long.rds")$mtime))
  histo_date <- file.info("data/text_long.rds")$mtime %>%
    as.Date() %>%
    as.character() %>%
    str_replace_all("-", "_")
  saveRDS(histo_rds, paste0("data/histo/text_long", histo_date, ".rds"))
}
saveRDS(text_long, file = "data/text_long.rds") # instead of csv due to size reduction
saveRDS(advertisements_complete, file = "data/advertisements_complete.rds") # instead of csv due to size reduction

advertisements_complete <- read_rds("data/advertisements_complete.rds")

print(colnames(advertisements_complete))

advertisements_complete <- !is.na(advertisements_complete$`Počet izieb/miestností`)

advertisements_complete <- filter(!is.na(advertisements_complete$`Počet izieb/miestností`))

