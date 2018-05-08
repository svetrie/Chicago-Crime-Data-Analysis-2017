#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(leaflet)

library(dplyr)
library(stringr)


library(ggplot2)
library(plotly)

crime_dataset = read.csv("Crimes_-_2001_to_present (1).csv")

crime_dataset$Date = crime_dataset$Date %>% 
  str_extract(pattern = "[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}") %>%
  as.Date.character(format = "%m/%d/%y")  


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Chicago Crime Data Visualizations"),
   
   mainPanel(
     htmlOutput("map")
   ),
   
   selectInput(inputId = "crime_type",
               label = "Select Type of Crime",
               choices = as.character(unique(crime_dataset$Primary.Type)),
               selected = "THEFT"
   ),
   
   plotlyOutput("crime_rate_plot"),
   
   sliderInput(inputId = "district_slider",
               label = "Choose a District",
               min = 1,
               max = 25,
               value = 3),
   
   plotlyOutput("district_crime_plot")
   
#    # Sidebar with a slider input for number of bins 
#    sidebarLayout(
#       sidebarPanel(
#          sliderInput("bins",
#                      "Number of bins:",
#                      min = 1,
#                      max = 50,
#                      value = 30)
#       ),
#       
#       # Show a plot of the generated distribution
#       mainPanel(
#          plotOutput("distPlot")
#       )
#    )
 )

server <- function(input, output) {
  
  output$map = renderUI(includeHTML("fusion.html"))
  
  crime_rate = reactive(count(subset(crime_dataset,
                                     crime_dataset$Primary.Type == input$crime_type), Date))

  output$crime_rate_plot = renderPlotly(ggplotly(ggplot(crime_rate(), aes(x = Date, y = n)) +
                                                geom_line(colour = "turquoise", size = .8) +
                                                geom_point(aes(text = paste("Date:", Date, "Crimes:", n)), color = "blue", size = .5) +
                                                scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
                                                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                                theme(legend.position = "none") +
                                                labs(title = "Overall Crime Rate", subtitle = as.character(input$crime_type),x = "Date", y = "Number of Assaults"), tooltip = "text")
  
                                         )
  
  district_subset = reactive(subset(crime_dataset, crime_dataset$District == input$district_slider))
  
  output$district_crime_plot = renderPlotly(ggplotly(ggplot(district_subset(), aes(x = Primary.Type)) + 
                                                       geom_bar() + 
                                                       labs(title = "Crime Rates for Different Types of Crimes in District", x = "Types of Crimes", y = "Number of Occurrences") +
                                                       theme(axis.text.x = element_text(angle = 90, hjust = 1))
                                                     )
                                            )
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

