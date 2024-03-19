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
library(shinythemes)


#UI for app
ui <- fluidPage(theme = shinytheme('flatly'),
  titlePanel('California Community Solar Analysis'),
  mainPanel(
    tabsetPanel(
    type = 'tabs',
    tabPanel("Introduction",
             p('California leads the United States in terms of both solar potential and progressive environmental policy. However, not all Californians benefit equally from both the solar potential and the progressive policies. Through its solar policy over time, California has created a utility-driven energy-equity problem that increases energy burden on low-and-middle income households.'),
             p('We aim to visualize the solar energy status quo and where opportunities for community solar lie. This app is intended for policy reachers and advocates along with anyone working with community solar projects (developers, community organizations, and advocates).'),
             p('This Shiny App explores the connections between utility energy consumption and adoption of rooftop solar through the lens of energy-equity. On Tab 1, we explore if reduced energy consumption and rising rooftop solar adoption are linked. On Tab 2, we explore the impacts of different solar incentive policies- Low Income and NEM- on California counties by comparing the capacity they have created. On Tab 3, we map California’s current community solar projects and show the timeline of project implementation by utility and system size. On Tab 4, we score and map California counties based on threshods for sunlight, household income, solar qualification status, and energy burden and assign them a priority rating.')
             
             ), #end introduction
    tabPanel('Deployment', 
             conditionalPanel(
               condition = "input.plot == 'Consumption vs Capacity Plot'",
               plotOutput('combined_plot'),
               p("Plots show the residential consumption (MW) and NEM Capacity (MW) over the years.")
             ),
             conditionalPanel(
               condition = "input.plot == 'Model Plot'",
               plotOutput('model_plot'),
               tableOutput('model_table'),
               p("Plots show the linear regression models across total energy consumption of three IOUs (SCE, SDGE and PGE) in connection to the year and rooftop solar deployment through NEM. The table indicates Model 3 with the largest effective size (0.99) and indicating the impact of rising NEM rooftop solar on energy consumption. The increasing deployment has led to reduced utility consumption with various other factors and leading to utility fixed costs shift on customers without solar. (Dave & Hausmann, 2022).")
             ),
             selectInput("plot", "Select Plot:",
                         choices = c("Consumption vs Capacity Plot", "Model Plot"),
                         selected = "Consumption vs Capacity Plot"),
             
    
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
             p('Recommendation 1: There is a need to prioritize solar development for low-income communities to manage the cost shift burden'),
             p('Recommendation 2: There needs to be further exploration in terms of what are the other factors beyond NEM Rooftop Solar development impacting the energy consumption of three IOUs'),
             p('Recommendation 3: Prioritize Community Solar in counties with the highest EJ scores. These are Imperial, Kern, Tulare, Kings, Fresno, Madera, Merced, Stanislaus, San Joaquin, Sutter, and Yuba Counties.')), #end tab 5
    tabPanel('Data Sources',
             p('Ma, Ookie. (2018). Low-Income Energy Affordability Data - LEAD Tool - 2018 Update. CA 2018 LEAD Data. Accessed 19 February, 2024. https://dx.doi.org/10.25984/1784729.'),
             p('Google. Project Sunroof Data Explorer (June 2019). Accessed 19 February, 2024. https://sunroof.withgoogle.com/data-explorer/place/ChIJPV4oX_65j4ARVW8IJ6IJUYs/'),
             p('“California - Historical Population Data”. Macrotrends. Accessed 19 February 2024. https://www.macrotrends.net/states/california/population'),
             p('Energy Consumption Database. (n.d.). Retrieved March 10, 2024, from https://ecdms.energy.ca.gov/'),
             p("CaliforniaDGStats. (n.d.). Retrieved March 10, 2024, from https://www.californiadgstats.ca.gov/charts/"),
             p("California Cities (May 2018). Accessed March 10, 2024, from https://hub.arcgis.com/datasets/06ce85b6fdf349d78c471d7c2ee8cb66_0/explore"))  #end tab 6    
    ) #end tabsetPanel  
        
  ) #end mainPanel
  
  
) #end UIfluidpage

#server for app
server <- function(input, output) {
  
#output 1
  
#output 1 data
  
  utility_consum_df <- read_csv(here('data/output 1/elec_by_utility_ca.csv')) %>% 
    janitor::clean_names()
  
  ## These are the  investor owned utilities 
  aggregated_all_ious <- utility_consum_df %>% 
    filter(utility_type == 'Investor owned utility')  %>% 
    group_by(year) %>% 
    summarize(total_residential = sum(residential))
  
  ## These are all other utilities of california without the 3 top utilities for NEM implementation
  aggregated_other_utilities <- utility_consum_df %>% 
    filter(!utility_type == 'Investor owned utility') %>% 
    group_by(year) %>% 
    summarize(total_residential = sum(residential))
  
  # Combine plots into a grid
  output$combined_plot <- renderPlot({
    plot_ious <- ggplot(aggregated_all_ious, aes(x = year, y = total_residential)) +
      geom_line(color = "blue", size = 1) +
      labs(x = "Year", y = "Residential Consumption (MW) across all SCE, SDGE and PGE") +
      theme_minimal()
    
    plot_nem <- ggplot(aggregated_other_utilities, aes(x = year, y = total_residential)) +
      geom_line(color = "red", size = 1) +
      labs(x = "Year", y = "Rooftop Solar developed through NEM Policy (MW)") +
      theme_minimal()
    
    low_income_solar <- read_csv(here('data/output 1/li-capacity-chart.csv')) %>% 
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
    
    cowplot::plot_grid(plot_ious, plot_nem, plot_li, ncols = 3, heights = c(2,2,2))
  })
  
  output$model_plot <- renderUI({
    plotOutput("model_plot")
  })
  
  output$model_plot <- renderPlot({
    # Loading the nem capacity dataset
    ## NEM capacity data
    nem_capacity_df <- read_csv(here('data/output 1/nem-capacity-chart.csv')) %>% 
      janitor::clean_names() %>% 
      rename(year = category) %>% 
      mutate(year = as.integer(year))
    
    all_years <- data.frame(year = c(1990:2023))
    
    # Merge datasets for 3 IOUs and NEM
    utility_nem_ious <- merge(all_years, aggregated_all_ious,
                              by = "year", all.x = TRUE) %>%
      left_join(nem_capacity_df, by = "year") 
    
    utility_nem_ious$total_ious_mw <- utility_nem_ious$total_residential * 1000
    
    aggregated_other_utilities$total_other_mw <- aggregated_other_utilities$total_residential * 1000
    
    missing_values <- is.na(utility_nem_ious$capacity_in_year) | !is.numeric(utility_nem_ious$capacity_in_year)
    
    # Remove rows with missing or non-numeric values
    utility_nem_ious <- utility_nem_ious[!missing_values, ]
    
    # Now try plotting the models again
    # Fit the models
    model1 <- lm(total_ious_mw ~ year + capacity_in_year, data = utility_nem_ious)
    model2 <- lm(total_ious_mw ~ year * capacity_in_year, data = utility_nem_ious)
    model3 <- lm(total_ious_mw ~ -1 + year + capacity_in_year, data = utility_nem_ious)
    
    # Create a dataframe for prediction
    pred_data <- expand.grid(year = seq(min(utility_nem_ious$year), max(utility_nem_ious$year), by = 1),
                             capacity_in_year = seq(min(utility_nem_ious$capacity_in_year), max(utility_nem_ious$capacity_in_year), length.out = 100))
    
    # Make predictions
    pred_model1 <- predict(model1, newdata = pred_data)
    pred_model2 <- predict(model2, newdata = pred_data)
    pred_model3 <- predict(model3, newdata = pred_data)
    
    # Plot the data and regression lines
    model_plot <- ggplot(utility_nem_ious, aes(x = capacity_in_year, y = total_ious_mw)) +
      geom_point() +
      geom_line(data = pred_data, aes(y = pred_model1, color = "Model 1")) +
      geom_line(data = pred_data, aes(y = pred_model2, color = "Model 2")) +
      geom_line(data = pred_data, aes(y = pred_model3, color = "Model 3")) +
      scale_color_manual(values = c("Model 1" = "orange2", "Model 2" = "lightblue", "Model 3" = "darkgreen")) +
      labs(x = "Rooftop Solar Capacity in Year (NEM Policy)", y = "Total three IOUs energy consumption (MW)", color = "Model") +
      theme_minimal()
    print(model_plot)
  })
  
  # Table display
  
  output$model_table <- renderTable({
    
    ## NEM capacity data
    nem_capacity_df <- read_csv(here('data/output 1/nem-capacity-chart.csv')) %>% 
      janitor::clean_names() %>% 
      rename(year = category) %>% 
      mutate(year = as.integer(year))
    
    all_years <- data.frame(year = c(1990:2023))
    
    # Merge datasets for 3 IOUs and NEM
    utility_nem_ious <- merge(all_years, aggregated_all_ious,
                              by = "year", all.x = TRUE) %>%
      left_join(nem_capacity_df, by = "year") 
    
    utility_nem_ious$total_ious_mw <- utility_nem_ious$total_residential * 1000
    
    aggregated_other_utilities$total_other_mw <- aggregated_other_utilities$total_residential * 1000
    
    missing_values <- is.na(utility_nem_ious$capacity_in_year) | !is.numeric(utility_nem_ious$capacity_in_year)
    
    # Remove rows with missing or non-numeric values
    utility_nem_ious <- utility_nem_ious[!missing_values, ]
    
    # Now try plotting the models again
    # Fit the models
    model1 <- lm(total_ious_mw ~ year + capacity_in_year, data = utility_nem_ious)
    model2 <- lm(total_ious_mw ~ year * capacity_in_year, data = utility_nem_ious)
    model3 <- lm(total_ious_mw ~ -1 + year + capacity_in_year, data = utility_nem_ious)
    
    effect_size_model1 <- summary(model1)$fstatistic[1] / (summary(model1)$fstatistic[1] + summary(model1)$fstatistic[2])
    effect_size_model2 <- summary(model2)$fstatistic[1] / (summary(model2)$fstatistic[1] + summary(model2)$fstatistic[2])
    effect_size_model3 <- summary(model3)$fstatistic[1] / (summary(model3)$fstatistic[1] + summary(model3)$fstatistic[2])
    
    # Create a data frame with BIC and effect size for each model
    model_data <- data.frame(
      Model = c("Model 1", "Model 2", "Model 3"),
      Effect_Size = c(effect_size_model1, effect_size_model2, effect_size_model3)
    )
    
    # Return the data frame
    model_data
  })
  
#}
  
  
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