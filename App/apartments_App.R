if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(shiny, 
               shinydashboard, 
               tidyverse, 
               tidymodels,
               openxlsx)

setwd("~/R/Projects/real_estate_appraisal/real_estate_appraisal")

model <- read_rds("data/model.RDS")

data <- read_rds("data/apartments_app_analysis_data.RDS")

mun_in_model <- read_rds("data/app_obce_filter_list.rds")

municipality_list <- read.xlsx("data/geospatial_data/obce_zoznam.xlsx") %>% 
  left_join(mun_in_model, join_by("obec" == "name_nsi"), keep = TRUE) %>% 
  filter(!is.na(name_nsi)) %>% 
  select(-name_nsi)


districts <- 
  read_rds("data/index_districts.RDS") %>% 
  mutate(district = as.character(district))

conditions <- read_rds("data/conditions.RDS") %>% 
  arrange(condition) %>% 
  drop_na()

certificate <- read_rds("data/certificate.RDS") %>% 
  arrange(certificate) %>% 
  drop_na()

type <- read_rds("data/type.RDS") %>% 
  arrange(type) %>% 
  drop_na()

df <- data.frame()

ui <- dashboardPage(
  dashboardHeader(title = "Apartments Valuation Dashboard"),
  
  dashboardSidebar(
    selectInput("v_municipality", 
                "Obec", 
                choices = c("",unique(municipality_list$obec)),
                selected = pluck(municipality_list[1,1])),
    
    selectInput("v_type", 
                "Typ", 
                choices = c("",unique(type$type)),
                selected = pluck(type[1,1])),     
    
    selectInput("v_condition", 
                "Stav", 
                choices = c("",unique(conditions$condition)),
                selected = pluck(conditions[1,1])), 
    
    numericInput("v_area",
                 "Rozloha",
                 value = 100,
                 min = 1,
                 max = 200,
                 step = 1),
    
    selectInput("v_provision",
                "Provízia v cene",
                choices = c("", "Áno", "Nie"),
                selected = "Nie"),
    
    selectInput("v_certificate", 
                "Energetický certifikát", 
                choices = c("",unique(certificate$certificate)),
                selected = "nemá"),
    
    actionButton("evaluate", "Odhadni cenu")
  ),
  dashboardBody(
    
    fluidRow(box(valueBoxOutput("predicted_value")))
    
))

server <- function(input, output) {
  df <- observeEvent(input$evaluate, {
    district_selected <- municipality_list %>%
      filter(obec == input$v_municipality) %>%
      select(okres) %>%
      pluck(1)
    
    index_mean_district_selected <- districts %>%
      filter(district == district_selected) %>%
      select(index_mean_district) %>%
      pluck(1)
    
    data.frame(
      name_nsi = as_factor(input$v_municipality),
      district = as_factor(district_selected),
      index = 8.3,
      condition = as_factor(input$v_condition),
      area = input$v_area,
      provision = as_factor(input$v_provision),
      certificate = as_factor(input$v_certificate),
      type = as_factor(input$v_type),
      rooms = case_when(
        input$v_type == "1 izbový byt" ~ 1,
        input$v_type == "2 izbový byt" ~ 2,
        input$v_type == "3 izbový byt" ~ 3,
        input$v_type == "4 izbový byt" ~ 4,
        input$v_type == "5 a viac izbový byt" ~ 5,
        input$v_type == "Garsónka" ~ 1,
        input$v_type == "Dvojgarsónka" ~ 2
      ),
      index_mean_district = index_mean_district_selected
    )
    
    value <- predict(model, df) %>%
      transmute(pred = format(round(.pred, -3), big.mark = " ")) %>%
      pluck(1)
  })
  
  output$predicted_value <- if (length(df) = 0) {NULL}
  else {renderValueBox({
    valueBox(
      value = paste0(value, "€", sep = " "),
      subtitle = "Odhadovaná cena",
      color = "olive"
    )
  })
  }
}



shinyApp(ui, server)
