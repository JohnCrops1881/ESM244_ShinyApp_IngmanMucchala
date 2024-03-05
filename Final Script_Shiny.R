library(shiny)
library(shinydashboard)
library(cowplot)
library(leaflet)
library(tidyverse)
library(here)
library(sf)
library(tmap)
library(ggplot2)

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Community Solar App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Intro", tabName = "intro"),
      menuItem("Output 1", tabName = "output1"),
      menuItem("Output 2", tabName = "output2"),
      menuItem("Output 3", tabName = "output3"),
      menuItem("Output 4", tabName = "output4"),
      menuItem("Recommendations", tabName = "recommendations"),
      menuItem("Citations", tabName = "citations")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
                box(
                  title = "Introduction",
                  "Who is our audience?",
                  "Anyone working with community solar - developers, community organizations, implementers, advocates",
                  "Policy - researchers, policy advocates",
                  "Purpose of app, background of problem"
                )
              )
      ),
      tabItem(tabName = "output1",
              fluidRow(
                box(
                  title = "Output 1",
                  plotOutput("output1_plot"),
                  textOutput("output1_text"),
                  width = 12,  # Adjust width as needed
                  style = "width: 1200px;" 
                )
              )
      ),
      tabItem(tabName = "output2",
              fluidPage(
                titlePanel("Solar Capacity Comparison"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("policy", "Select Policy:",
                                choices = c("Low-Income Policy", "NEM Policy"),
                                selected = "Low-Income Policy"),
                    uiOutput("county_selection")
                  ),
                  mainPanel(
                    plotOutput("capacity_plot")
                  )
                )
              )
              ),
      tabItem(tabName = "output3",
              fluidPage(
                titlePanel("Community Solar Analysis"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("plotType", "Select Plot Type:",
                                choices = c("tmap", "ggplot2"),
                                selected = "tmap")
                  ),
                  mainPanel(
                    plotOutput("community_plot")
                )
              )
              )
      ),
      tabItem(tabName = "output4",
              fluidRow(
                box(
                  title = "Output 4",
                  "Header: Here, we score and map priority California counties for community solar projects based on energy burden, sunlight, household income, and qualified households.",
                  "Map: Our Energy Justice Score draws attention to counties that meet multiple priority factors. Scores range from 0 to 4 with 4 identifying counties that meet all cut-offs: 1) energy burden (% of income) >/= 3%; 2) Annual median sunlight >/= 20,000 kWh; 3) household income >= California's median household income of $91,905; 4) percent of households qualified for solar >= 85%. Central California counties stand out with the highest EJ scores, with some Northern and a Southern County also scoring a 4. These are Imperial, Kern, Tulare, Kings, Fresno, Madera, Merced, Stanislaus, San Joaquin, Sutter, and Yuba Counties. Many coastal counties scored zeros due to lower qualification for solar."
                )
              )
      ),
      tabItem(tabName = "recommendations",
              fluidRow(
                box(
                  title = "Recommendations",
                  "Increase capacities of utilities serving high energy-burdened counties",
                  "Prioritize solar and seek opportunities in high energy-justice scoring counties."
                )
              )
      ),
      tabItem(tabName = "citations",
              fluidRow(
                box(
                  title = "Citations",
                  "Insert citations here"
                )
              )
      )
    )
  )
)


# Defining the server logic
server <- function(input, output) {
  # Load and preprocess the data required for Output 1
  utility_consum_df <- read_csv(here('data/output 1/elec_by_utility_ca.csv')) %>% 
    janitor::clean_names()
  
  aggregated_utility_consum <- utility_consum_df %>%
    filter(utility_type == 'Investor owned utility') %>% 
    group_by(year) %>%
    summarize(total_residential = sum(residential))
  
  nem_capacity_df <- read_csv(here('data/output 1/nem-capacity-chart.csv')) %>% 
    janitor::clean_names() %>% 
    rename(year = category) %>% 
    mutate(year = as.integer(year))
  
  all_years <- data.frame(year = c(1990:2023))
  
  utility_nem_df <- merge(all_years, aggregated_utility_consum,
                          by = "year", all.x = TRUE) %>%
    left_join(nem_capacity_df, by = "year") 
  
  utility_nem_df$total_usage_mw <- utility_nem_df$total_residential * 1000
  
  # Output 1
  output$output1_plot <- renderPlot({
    # Plotting both time series with secondary axis
    plot_residential <- ggplot(utility_nem_df, aes(x = year, y = total_usage_mw)) +
      geom_line(color = "blue", size = 1) +
      labs(x = "Year", y = "Residential Consumption (MW)") +
      theme_minimal()
    
    plot_nem <- ggplot(utility_nem_df, aes(x = year, y = prior_years_capacity)) +
      geom_line(color = "red", size = 1) +
      labs(x = "Year", y = "NEM Capacity (MW)") +
      theme_minimal()
    
    # Combine plots into a grid
    plot_grid(plot_residential, plot_nem, nrow = 1)
    
  }, height = 300, width = 800)
  
  output$output1_text <- renderText({
    # Text output code for Output 1
    "Plots show the residential consumption (MW) and NEM Capacity (MW) over the years."
  })
  
 
  ## Intializing the data
  
  low_income_data <- read_csv(here('data/output 1/li-territory-and-location-chart.csv')) %>%
    janitor::clean_names() %>% 
    rename(capacity_kw = capacity_k_w) %>% 
    rename(county = category)
  
  nem_policy_data <- read_csv(here('data/output 1/nem-territory-and-location-chart.csv')) %>%
    janitor::clean_names() %>% 
    mutate(capacity_kw = capacity_mw*1000) %>% 
    rename(county = category)
  # Output 2
  
  selected_counties <- reactiveValues()
  
  # Update selected counties when policy changes
  observeEvent(input$policy, {
    selected_counties$prev <- isolate(selected_counties$curr)
    selected_counties$curr <- isolate(input$counties)
  })
  
  # Generating UI for selecting counties based on policy data
  output$county_selection <- renderUI({
    policy_data <- switch(input$policy,
                          "Low-Income Policy" = low_income_data,
                          "NEM Policy" = nem_policy_data)
    
    # Retain selected counties if available in the other policy data
    selected <- if (!is.null(selected_counties$curr)) {
      intersect(selected_counties$curr, unique(policy_data$county))
    } else {
      NULL
    }
    
    selectInput("counties", "Select Counties:",
                choices = unique(policy_data$county),
                multiple = TRUE,
                selected = selected)
  })
  
  # Generate plot based on selected policy and counties
  output$capacity_plot <- renderPlot({
    # Filter data based on selected policy
    policy_data <- switch(input$policy,
                          "Low-Income Policy" = low_income_data,
                          "NEM Policy" = nem_policy_data)
    
    # Filter data based on selected counties
    selected_county_data <- policy_data %>%
      filter(county %in% input$counties)
    
    # Plot for comparing selected counties
    ggplot(selected_county_data, aes(x = county, y = capacity_kw, fill = county)) +
      geom_bar(stat = "identity") +
      labs(title = "Solar Capacity by County",
           x = "County", y = "Solar Capacity (kW)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ## Initializzing data for output 3
  cities_sf <- read_sf(here("data", "CA_cities_pts2", "CA_cities_pts2.shp")) %>% 
    janitor::clean_names()
  
  comm_df <- read_csv(here("data", "Community_Solar_CA.csv")) %>% 
    janitor::clean_names()
  
  county_sf <- read_sf(here("data", "ca_counties", "CA_Counties_TIGER2016.shp")) %>% 
    janitor::clean_names() %>% 
    select(c(name, geometry))
  
  
  cities_sf %>% st_crs() #WGS 84 EPSG 4326 
  comm_df %>% st_crs()
  
  city_comm_sf <- full_join(cities_sf, comm_df, by=c("name"="city")) %>% 
    drop_na(project_name)
  
  city_comm_sf_short <- city_comm_sf %>% 
    select(project_name, name, lat.x, long, county_fips, utility, utility_type, system_size_k_w_ac, system_size_mw_ac, geometry, population.x)
  ## output 3
  
  output$community_plot <- renderPlot({
    if (input$plotType == "tmap") {
      tmap_mode("view")
      tm_shape(county_sf) + 
        tm_polygons() +
        tm_shape(city_comm_sf_short) +
        tm_dots(size = 0.02) +
        tm_view(view.legend.position = c("right", "top")) +
        tm_layout(title = 'System Size', title.position = c('right', 'top'))
    } else {
      ggplot(data = comm_df, aes(x = year_of_interconnection, 
                                 y = system_size_mw_ac,
                                 color = utility)) +
        geom_col() +
        labs(x = "Year of Interconnection",
             title = "California Community Projects Over Time",
             y = "System Size (kW-AC)", 
             color = "Utility") +
        theme_minimal() +
        scale_x_continuous(breaks = 2011:2021) #show all years
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
