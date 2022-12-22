library(tidyverse)
library(rvest)

library(usethis)
usethis::use_git_config(user.name = "Arnold-Kakas", user.email = "kakasarnold@gmail.com")

install.packages("gitcreds")
library(gitcreds)

gitcreds::gitcreds_set()


site <- "https://www.nehnutelnosti.sk/predaj/?p[categories][ids]=1.2&p[page]="

number_of_pages <- read_html(paste0(site, 1)) %>%
  html_nodes(xpath = '//*[@class="component-pagination__items d-flex align-items-center"]') %>%
  html_elements("li") %>%
  html_text(trim = TRUE) %>%
  as.numeric() %>%
  discard(is.na) %>%
  max()


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
  temp <- read_html(link) %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "col-md-4", " " ))]//div') %>%
    html_text2()
  info <- as.data.frame(temp) %>% separate(temp, sep = ': ', c("info", "status")) %>% filter(info %in% c("Stav", "Úžit. plocha", "Zast. plocha", "Plocha pozemku", "Provízia zahrnutá v cene")) %>% pivot_wider(names_from = "info", values_from = "status")
  
  advertisements <- rbind(advertisements, data.frame(price, type_of_real_estate, address, info, stringsAsFactors = FALSE))
}

advertisements_cleaned <- advertisements %>%
  separate(type_of_real_estate, c("type", "area"), sep = " • ") %>%
  separate(address, c("a", "b", "c"), sep = ", ") %>%
  mutate(
    rooms = case_when(
      str_detect(type, "byt") ~ substr(type, 1, 1),
      str_detect(type, "Garsónka") ~ "1,5",
      str_detect(type, "Dvojgarsónka") ~ "2,5"
    ),
    price = str_replace(price, " €", ""),
    area = str_replace(area, " m²", ""),
    type = case_when(
      !str_detect(type, "byt") ~ type,
      str_detect(type, "byt") ~ "Byt"
    )
  ) %>%
  unite("address", c(6, 5, 4), sep = ", ", na.rm = TRUE, remove = TRUE) %>%
  separate(address, c("district", "municipality", "street"), sep = ", ") %>%
  filter(price != "Cena dohodou", str_detect(district, "okres"))

# 
# test <- read_html("https://www.nehnutelnosti.sk/4902733/bungalov-v-malinove-na-predaj/") %>%
#   html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "col-md-4", " " ))]//div') %>%
#   html_text2()
# 
# tolist <- as.data.frame(test) %>% separate(test, sep = ': ', c("info", "status")) %>% filter(info %in% c("Stav", "Úžit. plocha", "Zast. plocha", "Plocha pozemku", "Provízia zahrnutá v cene")) %>% pivot_wider(names_from = "info", values_from = "status")
