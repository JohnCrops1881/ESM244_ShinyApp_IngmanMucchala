library(shiny)
library(plotly)

shinyApp(
  ui = fluidPage(
    tabsetPanel(
      tabPanel("Introduction", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(selectInput("Country", "Select Country", choices = "", selected = "")),
                 mainPanel(
                   htmlOutput("Attacks")
                 )
               )
      ),
      tabPanel("Output 1", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                 mainPanel(fluidRow(
                   column(7,  plotlyOutput("")),
                   column(5, plotlyOutput(""))   
                 )
                 )
               )
      ),
      tabPanel("Output 2", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                 mainPanel(fluidRow(
                   column(7,  plotlyOutput("")),
                   column(5, plotlyOutput(""))   
                 )
                 )
               )
      ),
      tabPanel("Output 3", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                 mainPanel(fluidRow(
                   column(7,  plotlyOutput("")),
                   column(5, plotlyOutput(""))   
                 )
                 )
               )
      ),
      tabPanel("Output 4", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                 mainPanel(fluidRow(
                   column(7,  plotlyOutput("")),
                   column(5, plotlyOutput(""))   
                 )
                 )
               )
      ),
      tabPanel("Recommendations", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                 mainPanel(fluidRow(
                   column(7,  plotlyOutput("")),
                   column(5, plotlyOutput(""))   
                 )
                 )
               )
      ),
    tabPanel("Citations", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
               mainPanel(fluidRow(
                 column(7,  plotlyOutput("")),
                 column(5, plotlyOutput(""))   
               )
               )
             )
    )
    )
  ), 
  server = function(input, output) {
    
  }
)