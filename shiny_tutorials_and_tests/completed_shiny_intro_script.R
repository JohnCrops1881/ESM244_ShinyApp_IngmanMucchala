
### keeping this file for posterity- this is the completed shiny intro script tutorial.

library(shiny)
library(tidyverse)
library(palmerpenguins)



### Create the user interface:
ui <- fluidPage(
  titlePanel("Palmer's Penguins!"),       
  sidebarLayout(                            
    sidebarPanel("Put widgets here!",
      
                 radioButtons(
                   inputId = "penguin_species",
                   label = "Choose penguin species", 
                   choices = c("Adelie","Gentoo","Cool Chinstrap Penguins!" = "Chinstrap")
                 ),
    
                selectInput(
                  inputId = "pt_color", 
                  label = "Select point color", 
                  choices = c("Roses are red!"     = "red", 
                              "Violets are purple" = "purple", 
                              "Oranges are..."     = "orange")),

  ), ### end of sidebarPanel
      
      mainPanel("Penguin Graphs",
                  plotOutput(outputId = "penguin_plot"),
                  h3('Summary table'),
                  tableOutput(outputId = "penguin_table")
                
                ) ### end mainPanel
              
  ) ### end sidebayLayout
) ### end fluidPage

### Create the server function:
server <- function(input, output) {
  penguin_select <- reactive({
    penguins_df <- penguins %>%
      filter(species == input$penguin_species)
    
    return(penguins_df)
  }) ### end penguin_select
  
  output$penguin_plot <- renderPlot({               # <1>
    ggplot(data = penguin_select()) +               # <2>
      geom_point(aes(x = flipper_length_mm, y = body_mass_g),
                 color = input$pt_color)
  }) ### end penguin_plot
  
  penguin_sum_table <- reactive({
    penguin_summary_df <- penguins %>%
      filter(species == input$penguin_species) %>%
      group_by(sex) %>%
      summarize(mean_flip = mean(flipper_length_mm),
                mean_mass = mean(body_mass_g))
    
    return(penguin_summary_df)
  }) ### end penguin_sum_table reactive
  
  output$penguin_table <- renderTable({
    penguin_sum_table()
  })
  
} ### end server

### Combine them into an app:
shinyApp(ui = ui, server = server)






