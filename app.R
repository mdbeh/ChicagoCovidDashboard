library(DT)
library(htmlwidgets)
library(leaflet)
library(leaflet.mapboxgl)
library(RSocrata)
library(shiny)
library(shinydashboard)
library(sf)
library(tidyverse)

options(mapbox.accessToken = "pk.eyJ1IjoibWRiZWgiLCJhIjoiY2swNGJmcTRwMDNtdTNjazBhamozZnQ5ZiJ9.r_wk_xYk7peilpNqvMnzzA")

if (as.numeric(Sys.time() - file.info("Covid.rds")$mtime) > 2) {
  Covid <- read.socrata("https://data.cityofchicago.org/d/yhhz-zm2v")
  write_rds(Covid, "Covid.rds")} else {
    Covid <- read_rds("Covid.rds")}

zipcode_geo <- st_read("zipcode/geo_export_a149a704-979a-4b78-9a3c-29cf2ee70dd9.shp")

suppressWarnings(st_crs(zipcode_geo) <- st_crs("+proj=longlat +datum=WGS84"))

CovidTotals <- Covid %>% group_by(zip_code) %>% summarize(total_cases = sum(cases_weekly, na.rm = TRUE))

CovidZipWeekly <- Covid %>%
  semi_join(CovidTotals %>% filter(total_cases >= 20), by = "zip_code") %>%
  filter(zip_code != "Unknown") %>%
  transmute(zip_code, week_end,
            deaths_weekly = replace_na(deaths_weekly, 0),
            cases_weekly = replace_na(cases_weekly, 0),
            cases_per_100000 = round(cases_weekly / population * 100000),
            deaths_per_100000 = round(deaths_weekly / population * 100000))

ui <- dashboardPage(
  dashboardHeader(title = "Chicago Covid-19 Dashboard"),
  
  dashboardSidebar(dateRangeInput("week", "Date Range", "2020-03-01", Sys.Date()),
                   radioButtons("measure", "Select Measure", 
                                choices = c("Total Cases", "Total Deaths", "Cases Per 100,000", "Deaths Per 100,000"))),
  dashboardBody(fluidRow(column(leafletOutput("map"), width = 4),
                         column(plotOutput("zipWeekly"), width = 8)),
                htmlOutput("fileAge"))
  )

server <- function(input, output, session) {
  
  CovidTotals <- reactive({
    Covid %>% filter(zip_code != "Unknown",
                     week_start >= input$week[[1]], week_end <= input$week[[2]]) %>%
      group_by(zip_code) %>%
      summarize(total_cases = sum(cases_weekly, na.rm = TRUE),
                total_deaths = sum(deaths_weekly, na.rm = TRUE),
                population = median(population, na.rm = TRUE))
  })
  
  CovidGeo <- reactive({
    zipcode_geo %>%
      inner_join(CovidTotals(), by = c("zip" = "zip_code")) %>%
      mutate(cases_per_100000 = total_cases / population * 100000,
             deaths_per_100000 = total_deaths / population * 100000,
             blurb = paste0("<b>Zip Code ", zip, "</b><br>",
                            "Cases Per 100,000 People: ", round(cases_per_100000)))
  })

  output$map <- renderLeaflet({
     if (input$measure == "Total Cases") {
      pal <- reactive({colorNumeric(palette = "Blues", domain = CovidGeo()$total_cases)})
  
      leaflet(CovidGeo()) %>%
        addMapboxGL(style = "mapbox://styles/mapbox/light-v9") %>%
        addPolygons(fillColor = ~pal()(total_cases), weight = 1, color = "darkgrey", fillOpacity = 0.8,
                    popup = ~blurb)

    } else if (input$measure == "Total Deaths") {
      pal <- reactive({colorNumeric(palette = "Blues", domain = CovidGeo()$total_deaths)})

      leaflet(CovidGeo()) %>%
        addMapboxGL(style = "mapbox://styles/mapbox/light-v9") %>%
        addPolygons(fillColor = ~pal()(total_deaths), weight = 1, color = "darkgrey", fillOpacity = 0.8,
                    popup = ~blurb)


    } else if (input$measure == "Cases Per 100,000") {
      pal <- reactive({colorNumeric(palette = "Blues", domain = CovidGeo()$cases_per_100000)})

      leaflet(CovidGeo()) %>%
        addMapboxGL(style = "mapbox://styles/mapbox/light-v9") %>%
        addPolygons(fillColor = ~pal()(cases_per_100000), weight = 1, color = "darkgrey", fillOpacity = 0.8,
                    popup = ~blurb)


    } else if (input$measure == "Deaths Per 100,000") {
      pal <- reactive({colorNumeric(palette = "Blues", domain = CovidGeo()$deaths_per_100000)})

      leaflet(CovidGeo()) %>%
        addMapboxGL(style = "mapbox://styles/mapbox/light-v9") %>%
        addPolygons(fillColor = ~pal()(deaths_per_100000), weight = 1, color = "darkgrey", fillOpacity = 0.8,
                    popup = ~blurb)}
  })
      
      
  output$fileAge <- renderText({paste("Last Updated", file.info("Covid.rds")$mtime)})
  
  output$covid <- renderDataTable({CovidTotals()})
  
  output$zipWeekly <- renderPlot({
    if (input$measure == "Total Cases") {
      ggplot(CovidZipWeekly) + geom_line(aes(week_end, cases_weekly)) + 
        facet_wrap(~zip_code) +
        theme_light() +
        theme(axis.title = element_blank())
  
  } else if (input$measure == "Total Deaths") {
    ggplot(CovidZipWeekly) + geom_line(aes(week_end, deaths_weekly)) + 
      facet_wrap(~zip_code) +
      theme_light() +
      theme(axis.title = element_blank())
  
  } else if (input$measure == "Cases Per 100,000") {
    ggplot(CovidZipWeekly) + geom_line(aes(week_end, cases_per_100000)) + 
      facet_wrap(~zip_code) +
      theme_light() +
      theme(axis.title = element_blank())
  
  } else if (input$measure == "Deaths Per 100,000") {
    ggplot(CovidZipWeekly) + geom_line(aes(week_end, deaths_per_100000)) + 
      facet_wrap(~zip_code) +
      theme_light() +
      theme(axis.title = element_blank())}})
  
  
}

shinyApp(ui, server)