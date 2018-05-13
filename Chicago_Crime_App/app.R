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
   
   selectInput(inputId = "crime_type",
               label = "Select Type of Crime",
               choices = c("ALL", as.character(unique(crime_dataset$Primary.Type))),
               selected = "ALL"
   ),
  
   leafletOutput("crime_map"),
   
   plotlyOutput("crime_rate_plot"),
   
   sliderInput(inputId = "district_slider",
               label = "Choose a District",
               min = 1,
               max = 25,
               value = 3),
   
   plotlyOutput("district_crime_plot")
)

server <- function(input, output) {
  

  crime_type_subset = reactive({
    if(input$crime_type == "ALL") {
      crime_dataset
    } else {
      subset(crime_dataset, crime_dataset$Primary.Type == input$crime_type)
    }
  })
  
  crime_rate = reactive({
    count(crime_type_subset(), Date)
  })
  
  output$crime_map = renderLeaflet(leaflet() %>%
                                     addTiles() %>%
                                     addCircleMarkers(lng = crime_type_subset()$Longitude, lat = crime_type_subset()$Latitude,
                                                      popup = paste("Crime:", crime_type_subset()$Primary.Type, "<br>",
                                                                    "Date:", as.character.Date(crime_type_subset()$Date,
                                                                                               format = "%m/%d/%y"), "<br>",
                                                                    "Arrest: ", crime_type_subset()$Arrest),
                                                      radius = 1, fill = TRUE, color = "#f02ddc"))
  

  output$crime_rate_plot = renderPlotly(ggplotly(ggplot(crime_rate(), aes(x = Date, y = n)) +
                                                geom_line(colour = "turquoise", size = .8) +
                                                geom_point(aes(text = paste("Date:", Date, "Crimes:", n)), color = "blue", size = .5) +
                                                scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
                                                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                                theme(legend.position = "none") +
                                                labs(title = "Overall Crime Rate", x = "Date", y = "Number of Crime Incidents"),
                                                tooltip = "text")
  
                                         )
  
  district_subset = reactive(subset(crime_dataset, crime_dataset$District == input$district_slider))
  
  output$district_crime_plot = renderPlotly(ggplotly(ggplot(district_subset(), aes(x = Primary.Type)) + 
                                                       geom_bar(aes(fill = ..count..)) + 
                                                       labs(title = "Crime Rates for Different Types of Crimes in District",
                                                            x = "Types of Crimes", y = "Number of Occurrences") +
                                                       theme(axis.text.x = element_text(angle = 90, hjust = 1),
                                                             legend.key.height = unit(3, "inches"))  + 
                                                       scale_fill_gradient(low = "#ffe905", high = "#f51f00")
                                                     )
                                            )
}


# Run the application 
shinyApp(ui = ui, server = server)

