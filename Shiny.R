#loading libraries
library(shinyWidgets)
library(tidyverse)
library(shiny)
library(DT)

#using mtcars as dataset
df <- read.csv("aus_uv.csv")

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Please select a city to check UV index"),
  
  # Sidebar for input  filter
  sidebarLayout(
    sidebarPanel(
      selectInput("state","Select a state",unique(df$STATE)),
      selectizeInput("city","Select a city",choices = NULL),
      selectizeInput("month","Select a month",choices = NULL)
    ),
    
    # Show a table
    mainPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      h3(uiOutput("selected_var")),

      
    )
  )
)

# Define server logic required
server <- function(input, output,session) {

  #----reactive calculations
  sta_city <- reactive({
    req(input$state)
    df %>% filter(STATE == input$state)
  })
  
  
  observeEvent(sta_city(),{
    updateSelectizeInput(session,"city", choices = sta_city()$CITY)
    # })
    
    
    city_mat <- reactive({
      req(input$city)
      sta_city() %>% filter(CITY == input$city)
    })
    
    observeEvent(city_mat(),{
      updateSelectizeInput(session,"month",choices = colnames(city_mat() %>% dplyr:: select(-c("ID","STATE","CITY"))))
      
      output$selected_var <- renderUI({ 
        HTML(if((select(df,c("STATE","CITY",input$month))  %>% filter(STATE == input$state,CITY == input$city))[,3]<3){
          as.character(div(style="color: #8BC34A;", paste0("The average UV index of ", input$city,  ", ", input$state," is ", (select(df,c("STATE","CITY",input$month))  %>% filter(STATE == input$state,CITY == input$city))[,3], " in ", input$month)))
        }else if((select(df,c("STATE","CITY",input$month))  %>% filter(STATE == input$state,CITY == input$city))[,3]<6){
          as.character(div(style="color: #FDD835;", paste0("The average UV index of ", input$city,  ", ", input$state," is ", (select(df,c("STATE","CITY",input$month))  %>% filter(STATE == input$state,CITY == input$city))[,3], " in ", input$month)))
        } else if ((select(df,c("STATE","CITY",input$month))  %>% filter(STATE == input$state,CITY == input$city))[,3]<8){
          as.character(div(style="color: #FB8C00;", paste0("The average UV index of ", input$city,  ", ", input$state," is ", (select(df,c("STATE","CITY",input$month))  %>% filter(STATE == input$state,CITY == input$city))[,3], " in ", input$month)))
        }else if ((select(df,c("STATE","CITY",input$month))  %>% filter(STATE == input$state,CITY == input$city))[,3]<11){
          as.character(div(style="color: #D81B60;", paste0("The average UV index of ", input$city,  ", ", input$state," is ", (select(df,c("STATE","CITY",input$month))  %>% filter(STATE == input$state,CITY == input$city))[,3], " in ", input$month)))
        }else{
          as.character(div(style="color: #CC33FF;", paste0("The average UV index of ", input$city,  ", ", input$state," is ", (select(df,c("STATE","CITY",input$month))  %>% filter(STATE == input$state,CITY == input$city))[,3], " in ", input$month)))
        }
        )
      })
      
    })
  })
  
}

#> Run the application 
shinyApp(ui = ui, server = server)