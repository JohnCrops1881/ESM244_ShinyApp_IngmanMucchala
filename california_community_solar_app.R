library(shiny)
library(shinydashboard)
library(tidyverse)
library(here)
library(tsibble)
library(janitor)
library(sf)
library(tmap)
library(patchwork)
library(leaflet)
library(feasts)
library(fable)
library(terra)


#UI for app
ui <- fluidPage(
  titlePanel('California Community Solar Analysis'),
  mainPanel(
    tabsetPanel(
    type = 'tabs',
    tabPanel("Introduction",
             p('California leads the United States in terms of both solar potential and progressive environmental policy. However, not all Californians benefit equally from both the solar potential and the progressive policies. Through its solar policy over time, California has created a utility-driven energy-equity problem that increases energy burden on low-and-middle income households.'),
             p('This Shiny App explores the connections between utility energy consumption and adoption of rooftop solar through the lens of energy-equity. On Tab 1, we explore if reduced energy consumption and rising rooftop solar adoption are linked. On Tab 2, we explore the impacts of different solar incentive policies- Low Income and NEM- on California counties by comparing the capacity they have created. On Tab 3, we map California’s community solar projects, to ________. On Tab 4, we score and map California counties based on solar potential, income, solar qualification status, and assign them a priority rating.')
             
             ), #end introduction
    tabPanel('Deployment', plotOutput('output1_plot'),
             p("Plots show the residential consumption (MW) and NEM Capacity (MW) over the years.")
             ), #end tab 1
    tabPanel('Policy Comparison', plotOutput('capacity_plot'),
             selectInput("policy", "Select Policy:",
                         choices = c("Low-Income Policy", "NEM Policy"),
                         selected = "Low-Income Policy"),
             uiOutput("county_selection")
             ), #end tab 2
    tabPanel('Project Map', 
             tmapOutput('community_plot'),
             plotOutput('projects_timeline')
             ), #end tab 3
    tabPanel('EJ Score & Solar Suitability', 
             tmapOutput('ej_map'),
             p("County Energy Justice Score. Counties in gray do not have data for median sunlight or percent of households qualified, meaning we cannot assign a comparative EJ score. However, scores for energy burden and household income are available.")
             ), #end tab 4
    tabPanel('Recommendations',
             p("These are our recommendations:"),
             p('Recommendation 1'),
             p('Recommendation 2'),
             p('Recommendation 3')), #end tab 5
    tabPanel('Citations',
             p('Citation 1'),
             p('Citation 2'),
             p('Citation 3'),
             p('Citation 4'),
             ) #end tab 6    
    ) #end tabsetPanel  
        
  ) #end mainPanel
  
  
) #end UIfluidpage

#server for app
server <- function(input, output) {
  
#output 1
  
#output 1 data
  
  # All utilities data 
  
  utility_consum_df <- read_csv(here('data/output 1/elec_by_utility_ca.csv')) %>% 
    janitor::clean_names()
  
  ## These are the  investor owned utilities 
  
  aggregated_all_ious <- utility_consum_df %>% 
    filter(utility_type == 'Investor owned utility')  %>% 
    group_by(year) %>% 
    summarize(total_residential = sum(residential))
  
  ## These are all other utilities of california without the 3 top utilities for NEM implementation
  aggregated_other_utilities <- utility_consum_df %>% 
    filter(!utility_type == 'Invester owned utility') %>% 
    group_by(year) %>% 
    summarize(total_residential = sum(residential))
  
  
#output 1 UI display
  
  # Plotting for each scenario residential consumption
  
output$output1_plot <- renderPlot({
  plot_ious <- ggplot(utility_nem_ious, aes(x = year, y = total_ious_mw)) +
    geom_line(color = "blue", size = 1) +
    labs(x = "Year", y = "Residential Consumption (MW) across all SCE, SDGE and PGE") +
    theme_minimal()
  
  
  # Plotting NEM capacity
  plot_nem <- ggplot(utility_nem_ious, aes(x = year, y = prior_years_capacity)) +
    geom_line(color = "red", size = 1) +
    labs(x = "Year", y = "Rooftop Solar developed through NEM Policy (MW)") +
    theme_minimal()
  
  low_income_solar <- read_csv(here('data','output 1', 'li-capacity-chart.csv')) %>% 
    pivot_longer(cols = -Category, names_to = "policy", values_to = "values") 
  
  low_income_solar$Category <- as.character(low_income_solar$Category)
  
  plot_li <- ggplot(low_income_solar, aes(x = Category, y = values, fill = policy)) +
    geom_bar(stat = "identity", position = "stack") +  # Use position = "stack"
    labs(title = "Rooftop Solar Capacity by Policy and Year (Low-Income)",
         x = "Year",
         y = "Rooftop Solar Capacity through low-income policies (MW)",
         fill = "Policy") +
    theme_minimal() +
    scale_x_discrete() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  combined_plot <- plot_ious + plot_nem / plot_li
  
  combined_plot
  
  
  }, height = 300, width = 800)
  
  
#output 2
  
  #data

low_income_data <- read_csv(here('data/output 1/li-territory-and-location-chart.csv')) %>%
  janitor::clean_names() %>% 
  rename(capacity_kw = capacity_k_w) %>% 
  rename(county = category)

nem_policy_data <- read_csv(here('data/output 1/nem-territory-and-location-chart.csv')) %>%
  janitor::clean_names() %>% 
  mutate(capacity_kw = capacity_mw*1000) %>% 
  rename(county = category)
  
  #UI display

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
              selected = unique(policy_data$county))
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
  ggplot(selected_county_data, aes(x = county, y = capacity_kw, ful = "#48D937")) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    labs(title = "Solar Capacity by County",
         x = "County", y = "Solar Capacity (kW)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
})
  
#output 3
  
  #output 3 data

cities_sf <- read_sf(here("data", "CA_cities_pts2", "CA_cities_pts2.shp")) %>% 
  janitor::clean_names()

comm_df <- read_csv(here("data", "Community_Solar_CA.csv")) %>% 
  janitor::clean_names()

Counties <- read_sf(here("data", "ca_counties", "CA_Counties_TIGER2016.shp")) %>% 
  janitor::clean_names() %>% 
  select(c(name, geometry))

city_comm_sf <- full_join(cities_sf, comm_df, by=c("name"="city")) %>% 
  drop_na(project_name)

#View(city_comm_sf)
# lost Anza and Herlong, need better city data

Projects <- city_comm_sf %>% 
  select(project_name, name, county_fips, utility, utility_type, system_size_mw_ac, geometry, population.x, year_of_interconnection) %>% 
  mutate(year_of_interconnection = as.factor(year_of_interconnection))

Projects_Utility_Type <- city_comm_sf %>% 
  select(project_name, name, county_fips, utility, utility_type, system_size_mw_ac, geometry, population.x, year_of_interconnection) %>% 
  mutate(year_of_interconnection = as.factor(year_of_interconnection))

Projects_Year <- city_comm_sf %>% 
  select(project_name, name, county_fips, utility, utility_type, system_size_mw_ac, geometry, population.x, year_of_interconnection) %>% 
  mutate(year_of_interconnection = as.factor(year_of_interconnection))

# output 3 UI display 

output$projects_timeline <- renderPlot({

city_comm_sf %>%
  select(project_name, name, county_fips, utility, utility_type, system_size_mw_ac, geometry, population.x, year_of_interconnection) %>%
  mutate(year_of_interconnection = as.factor(year_of_interconnection))

Projects_Timeline <- ggplot(data = comm_df, aes(x = year_of_interconnection,
                                                y = system_size_mw_ac,
                                                fill=utility)) +
  geom_col(color="gray") +
  labs(x = "Year of Interconnection",
       title = "California Community Projects Over Time",
       y = "System Size (MW-AC)") +
  theme_minimal() +
  scale_fill_manual(values = c("darkgoldenrod2", "brown2", "royalblue3", "deepskyblue3", "springgreen4", "darkslateblue")) +
  theme(legend.position="bottom") +
  scale_x_continuous(breaks = 2011:2021) #show all years
}) #end output 3 ggplot
  


output$community_plot <- renderTmap({
  
  tmap_mode("view") 
  
  tm_shape(Counties) + 
    tm_polygons(alpha=0) + #transparent polygons like tm_borders, but gives names of countries
    tm_borders(alpha=0.4) +
    tm_shape(Projects) +
    tm_dots(size = 0.05, jitter = 0.2, col= "system_size_mw_ac", title = "System Size (MW/AC)", palette = "Greens") +
    tm_shape(Projects_Utility_Type) +  
    tm_dots(size = 0.05, jitter = 0.2, col= "utility_type", title = "Utility Type", palette = "Set2") +
    tm_shape(Projects_Year) +
    tm_dots(size = 0.05, jitter = 0.2, col= "year_of_interconnection", title = "Year of Interconnection", palette = "GnBu") +
    tm_view(view.legend.position = c("right", "top"))+
    tm_layout(title= 'System Size', 
              title.position = c('right', 'top'))
  
}) #end output 3 tMap


  
#output 4  
  
  #output 4 data

lead_df_raw <- read_csv(here("data", "LEAD_CA_County.csv")) %>% 
  janitor::clean_names()

lead_df <- lead_df_raw %>% 
  select(c(county, energy_burden_percent_income, avg_annual_energy_cost, total_households, household_income))

sunroof_df_raw <- read_csv(here("data", "project_sunroof_county_2019.csv")) %>% 
  janitor::clean_names() 

sunroof_df <- sunroof_df_raw %>% 
  select(c(region_name, lat_avg, lng_avg, yearly_sunlight_kwh_kw_threshold_avg, count_qualified, percent_covered, percent_qualified, number_of_panels_median, number_of_panels_total, yearly_sunlight_kwh_median, yearly_sunlight_kwh_total, existing_installs_count))

county_sf <- read_sf(here("data", "ca_counties", "CA_Counties_TIGER2016.shp")) %>% 
  janitor::clean_names() %>% 
  select(c(name, geometry))

# join the 3 datasets to county polygons
lead_sunroof_sf <- full_join(county_sf, lead_df, by=c("name"="county"))

county_sunroof_lead_sf <- full_join(lead_sunroof_sf, sunroof_df, by=c("name"="region_name"))

# scores
EJ_score <- county_sunroof_lead_sf %>% 
  mutate(E_burden_score = ifelse(energy_burden_percent_income>=3, 1, 0)) %>% # median (50th %ile)
  mutate(sunlight_score = ifelse(yearly_sunlight_kwh_median>=19205, 1, 0)) %>% # median
  mutate(income_score = ifelse(household_income<=86846, 1, 0)) %>% # median
  mutate(qualified_score = ifelse(percent_qualified>=92, 1, 0)) %>% #median is 92.35%
  mutate(EJ_score = E_burden_score + sunlight_score + income_score + qualified_score) %>% # Y/N, add all yes's so larger score = more burden (more median percentile thresholds crossed)
  select(name, geometry, E_burden_score, sunlight_score, income_score, qualified_score, EJ_score) %>% 
  filter(!st_is_empty(geometry)) #remove the 1 empty geometry

CSL_energy_burden <- county_sunroof_lead_sf %>% 
  select(c(geometry, energy_burden_percent_income)) %>% 
  filter(!st_is_empty(geometry))

CSL_sunlight <- county_sunroof_lead_sf %>% 
  select(c(geometry, yearly_sunlight_kwh_median)) %>% 
  filter(!st_is_empty(geometry))

CSL_income <- county_sunroof_lead_sf %>% 
  select(c(geometry, household_income)) %>% 
  filter(!st_is_empty(geometry))

CSL_qualified <- county_sunroof_lead_sf %>% 
  select(c(geometry, percent_qualified)) %>% 
  filter(!st_is_empty(geometry))

# EJ_score = placeholder thresholds (50th%ile "recs")

  
  #output 4 UI display

output$ej_map <- renderTmap({
  
  tmap_mode("view") 
  
  tm_shape(CSL_energy_burden) + 
    tm_polygons('energy_burden_percent_income', palette= c("YlGnBu"), 
                title='Energy Burden (% Income)',
                border.col='grey27', alpha=1) +
    tm_shape(CSL_sunlight) +
    tm_polygons('yearly_sunlight_kwh_median', palette= c("darkolivegreen", "goldenrod2"), 
                title='Median Annual Sunlight (kWh)',
                border.col='grey27', alpha=1) +
    tm_shape(CSL_income) +
    tm_polygons('household_income', palette= c("darkolivegreen3", "palegreen4", "lightskyblue4"), 
                title='Household Income ($)',
                border.col='grey27', alpha=1) +
    tm_shape(CSL_qualified) +
    tm_polygons('percent_qualified', palette= c("orange4", "burlywood4", "olivedrab3"), 
                title='Percent of Solar-Qualified Households',
                border.col='grey27', alpha=1) +
    tm_shape(EJ_score) +
    tm_polygons('EJ_score', palette= c("skyblue4", "seagreen", "lightgreen"), 
                title='EJ Score',
                border.col='grey27', alpha=1) +
    tmap_options(max.categories= 57) +
    tm_view(view.legend.position = c("right", "top"))+
    tm_layout(title= 'Solar Potential: Energy Justice Score', 
              title.position = c('right', 'top'))  
  
}) #end output 4 Tmap
    
  
} #end server

shinyApp(ui = ui, server = server)