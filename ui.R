library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
  title = "Accidentologie in Paris (2012-2013)",
  titleWidth = 400
  
  
)

body <- dashboardBody(
  
  style="background-color: #DDDDDD;",
  
  fluidRow(
    box(width = NULL, 
        style="background-color: white",
        column(12,
               h3("Map of accidents"),
               
               p(
                 class = "text-muted",
                 paste("This graph represents the position of each accident on the map depending of the injuries of the victims and the range of hours where the accident took place."
                 )
               ),
               column(9,
                      
                      leafletOutput("map")
                      
               ),
               column(3,
                      
                      selectInput("selectInjury", label = "Accidents with at least one...", 
                                  choices = list("Indemn" = 1, "Light Injured" = 2, "Hospitalized" = 3, "Dead" = 4), 
                                  selected = 4),
                      sliderInput("sliderHour", label = "Select a  range of hours of accidents", min = 0, 
                                  max = 23, value = c(7, 19),step = 1, post = ":00"
                                  
                      ),
                      
                      p(
                        class = "text-muted",
                        paste("Note: As it's a big dataset, it can take severals seconds to display data."
                        )
                      )
                      
               )
        )
    )
  ),
  box(width = NULL, 
      style="background-color: white",
      
      column(12,
             h3("Average frequency of accidents per hour"),
             p(
               class = "text-muted",
               paste("This histogram shows the average number of accidents per hour"
                     
               )
             ),
             # Show a plot of the generated distribution
             verbatimTextOutput("info"),
             plotOutput("histDay", hover = "plot_hover")
             
             
      )
  ),
  box(width = NULL, 
      style="background-color: white",
      
      column(12,
             h3("Types of injury"),
             p(
               class = "text-muted",
               paste("This visualisation is composed of 1 to 4 histograms which represent the number of injured depending of the type of vehicle. The type of injuries can be selected in the checkbox."
               )
             ),
             column(9,
                    
                    plotOutput("histInjury")
                    
             ),
             column(3,
                    style=" border-radius: 5px; border: 1px solid; border-color:#CCCCCC; margin-top: 1%;",
                    class = "text-muted",
                    checkboxGroupInput("checkInjury", label = "Select injury to display", 
                                       choices = list("Unharmed", "Light injured", "Hospitalized", "Dead"),
                                       selected = c("Unharmed","Light injured"))
                    
                    
             )
             
      )
  )
)

dashboardPage(
  skin = "black",
  header,
  dashboardSidebar(disable = TRUE),
  body
)