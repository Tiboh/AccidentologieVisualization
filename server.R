library(shiny)
library(leaflet)
library(plyr)
library(ggplot2)
library(DataCombine)
library(grid)


data <- read.csv2("accidentologie.csv", na.strings=c("", "NA"))  # read csv file

# Init data to display in the interactive map
data.map = data[complete.cases(data[,29:30]),] # Remove NA values for lat and lng
data.map$Lat = as.numeric(as.character(data.map$Lat))
data.map$Lng = as.numeric(as.character(data.map$Lng))

color <- c("LimeGreen", "SlateBlue", "Tomato", "Black")
colorPaletteMap <- colorFactor(color, domain = c("1", "2", "3", "4"))

# Init data to display in the multiple columns barplot
newName <- c("VehiculeCAdmin","UsagerGrav")

# Init data to display in the multiple columns barplot

##subsampling dataframe Vehicule1CAdmin Usager1Lveh
sub1.map = subset(data, 
                  Vehicule1LVeh=="A" & Usager1Lveh=="A"| 
                    Vehicule1LVeh=="B" & Usager1Lveh=="B"|
                    Vehicule1LVeh=="Z" & Usager1Lveh=="Z"
                  , select = c(Vehicule1CAdmin,Usager1Grav))
colnames(sub1.map)<- newName

##subsampling dataframe Vehicule1CAdmin Usager2Lveh
sub2.map = subset(data, 
                  Vehicule1LVeh=="A" & Usager2Lveh=="A"| 
                    Vehicule1LVeh=="B" & Usager2Lveh=="B"|
                    Vehicule1LVeh=="Z" & Usager2Lveh=="Z"
                  , select = c(Vehicule1CAdmin,Usager2Grav ))
colnames(sub2.map)<- newName

##subsampling dataframe Vehicule1CAdmin Usager3Lveh
sub3.map = subset(data, 
                  Vehicule1LVeh=="A" & Usager3Lveh=="A"| 
                    Vehicule1LVeh=="B" & Usager3Lveh=="B"|
                    Vehicule1LVeh=="Z" & Usager3Lveh=="Z"
                  , select = c(Vehicule1CAdmin,Usager3Grav ))
colnames(sub3.map)<- newName

##subsampling dataframe Vehicule1CAdmin Usager4Lveh
sub4.map = subset(data, 
                  Vehicule1LVeh=="A" & Usager4Lveh=="A"| 
                    Vehicule1LVeh=="B" & Usager4Lveh=="B"|
                    Vehicule1LVeh=="Z" & Usager4Lveh=="Z"
                  , select = c(Vehicule1CAdmin,Usager4Grav ))
colnames(sub4.map)<- newName

##subsampling dataframe Vehicule2CAdmin Usager1Lveh
sub5.map = subset(data, 
                  Vehicule2LVeh=="B" & Usager1Lveh=="B"| 
                    Vehicule2LVeh=="W" & Usager1Lveh=="W"|
                    Vehicule2LVeh=="Z" & Usager1Lveh=="Z"
                  , select = c(Vehicule2CAdmin,Usager1Grav ))
colnames(sub5.map)<- newName

##subsampling dataframe Vehicule2CAdmin Usager2Lveh
sub6.map = subset(data, 
                  Vehicule2LVeh=="B" & Usager2Lveh=="B"| 
                    Vehicule2LVeh=="W" & Usager2Lveh=="W"|
                    Vehicule2LVeh=="Z" & Usager2Lveh=="Z"
                  , select = c(Vehicule2CAdmin,Usager2Grav ))
colnames(sub6.map)<- newName

##subsampling dataframe Vehicule2CAdmin Usager3Lveh
sub7.map = subset(data, 
                  Vehicule2LVeh=="B" & Usager3Lveh=="B"| 
                    Vehicule2LVeh=="W" & Usager3Lveh=="W"|
                    Vehicule2LVeh=="Z" & Usager3Lveh=="Z"
                  , select = c(Vehicule2CAdmin,Usager3Grav))
colnames(sub7.map)<- newName

##subsampling dataframe Vehicule2CAdmin Usager4Lveh
sub8.map = subset(data, 
                  Vehicule2LVeh=="B" & Usager4Lveh=="B"| 
                    Vehicule2LVeh=="W" & Usager4Lveh=="W"|
                    Vehicule2LVeh=="Z" & Usager4Lveh=="Z"
                  , select = c(Vehicule2CAdmin,Usager4Grav ))
colnames(sub8.map)<- newName

##Concatenation des sub1 -> sub8

vTotal <- rbind(sub1.map, sub2.map, sub3.map, sub4.map, sub5.map, sub6.map, sub7.map, sub8.map)

## Create replacements data frame
Replaces <- data.frame(from = c("Bicy", "Bus","Car","Cyclo","Engin","Moto>125","Moto50-125","PL<=7,5","PL>7,5","PLRem","Q<=50","Q>50","Scoo<=50","Scoo>125","Scoo50-125","TR","Tram","TRSem","VL","Voi","VU"),
                       to = c("Bike", "Public Transport","Public Transport","Motorcycle","Truck","Motorcycle","Motorcycle","Truck","Truck","Truck","Motorcycle","Motorcycle","Motorcycle","Motorcycle","Motorcycle","Truck","Public Transport","Truck","Car","Car","Car"))

## Replace patterns and return full data frame
vTotalReplaced <- FindReplace(data = vTotal, Var = "VehiculeCAdmin", replaceData = Replaces,
                              from = "from", to = "to",vector = FALSE)

## Create replacements injured
Replaces2 <- data.frame(from = c("BH","BL","Indem","Tue"),
                        to = c("Hospitalized","Light injured","Unharmed","Dead"))
vTotalReplaced2 <- FindReplace(data = vTotalReplaced, Var = "UsagerGrav", replaceData = Replaces2,
                               from = "from", to = "to")

freq <- count(vTotalReplaced2, vars=newName)


# Self-defined formatting function for times.
timeHMS_formatter <- function(x) {
  h <- floor(x/60)
  m <- floor(x %% 60)
  s <- round(60*(x %% 1))                   # Round to nearest second
  lab <- sprintf('%02d:%02d:%02d', h, m, s) # Format the strings as HH:MM:SS
  lab <- gsub('^00:', '', lab)              # Remove leading 00: if present
  lab <- gsub('^0', '', lab)                # Remove leading 0 if present
}

#HIST 2
accidentHourData <- count(as.numeric(substr(data$Heure,0,2)))
accidentHourData$freq <- accidentHourData$freq/365*2


shinyServer(
  
  function(input, output) {
    
    
    # Select data to display in the map
    dataMap <- reactive({
      dat <- switch(input$selectInjury, 
                    "1" = subset(data.map, Usager1Grav == 'Indem' | Usager2Grav == 'Indem' | Usager3Grav == 'Indem' | Usager4Grav == 'Indem'),
                    "2" = subset(data.map, Usager1Grav == 'BL' | Usager2Grav == 'BL' | Usager3Grav == 'BL' | Usager4Grav == 'BL'),
                    "3" = subset(data.map, Usager1Grav == 'BH' | Usager2Grav == 'BH' | Usager3Grav == 'BH' | Usager4Grav == 'BH'),
                    "4" = subset(data.map, Usager1Grav == 'Tue' | Usager2Grav == 'Tue' | Usager3Grav == 'Tue' | Usager4Grav == 'Tue')
      )
      subset(dat, as.numeric(substr(Heure,0,2)) >= input$sliderHour[1] & as.numeric(substr(Heure,0,2)) <= input$sliderHour[2])
      
    })
    
    # Display the map
    output$map <- renderLeaflet({
      dataMap = dataMap()
      
      date <- as.vector(t(subset(dataMap, select = Date)))
      hour <- as.vector(t(subset(dataMap, select = Heure)))
      
      for(i in 1:nrow(dataMap)){ dataMap$VehiculeInvolved[i] = sum(!is.na(dataMap[i,c("Vehicule1CAdmin","Vehicule2CAdmin","Vehicule3CAdmin")]))}
      nbVehicle <- as.vector(t(subset(dataMap, select = VehiculeInvolved)))
      
      popupDisplay <- paste0("<b>", date,"</b>", " ", hour,"<p style='color:green;'>", nbVehicle," vehicle(s) involved</p>")
      
      m <- leaflet(dataMap)
      m <- addProviderTiles(m, "Hydda.Full")
      content <- paste(sep = "<br/>", ~ Usager1Grav,~ Usager2Grav, ~ Usager3Grav, ~ Usager4Grav)
      m <- addCircleMarkers(m, lng = ~ Lng, lat = ~ Lat,
                            radius = 5,
                            color = ~ colorPaletteMap(input$selectInjury),
                            stroke = FALSE, fillOpacity = 0.5,
                            popup = popupDisplay
      )
      m
    })
    

    
    # Display the histogram of Accident by Hours
    output$histDay = renderPlot({
      
      ggplot(accidentHourData) + aes(x = x, y = freq, fill=freq ) + geom_histogram()+ geom_bar(stat="identity")+ scale_fill_gradient("Number of accident per hour", low = "#01FA4B", high = "#FF0000")+
        labs(x ="Hours", y = "Average number of accidents")+
        
        scale_x_continuous(label=timeHMS_formatter, breaks=seq(0, 23, 1))+
        scale_y_continuous(breaks=seq(0, 8, 1))+
        
        theme(axis.title.y = element_text( size=12),
              axis.title.x = element_text(size=13),
              legend.title = element_text(colour="#333333", size=12, face="bold"),
              legend.text = element_text(colour="#6F6F6F", size=12),
              legend.title.align = 1
              
        )
      
      
    })
    
    
    output$info <- renderText({
      hour <- round(as.numeric(input$plot_hover$x))  #round hour
      av <- subset(accidentHourData, accidentHourData$x == hour, select = freq)   #2 numbers after coma
      subset(freq, UsagerGrav %in% input$checkInjury)
      accidents <- round(av, digits = 2)
      
      
      if (length(hour)==0) {
        paste0("Move the mouse over the graph to show the values\n")
        
      }
      else
        paste0("At ", hour , ":00, the average number of accidents is : ", accidents, ".\n")
      
      
      
    })
    
    
    # Select data to display in the multiple columns barplot
    dataInjury <- reactive({
      subset(vTotalReplaced2, UsagerGrav %in% input$checkInjury)
    })
    
    # Display the multiple columns barplot
    output$histInjury <- renderPlot({
      histInjuryData <- dataInjury()
      histInjuryData <- histInjuryData[complete.cases(histInjuryData),] # Remove NA values
      
      ggplot(histInjuryData,aes(VehiculeCAdmin,fill=UsagerGrav))+
        geom_bar(position="dodge") +
        scale_fill_manual(values=rev(color),name="Types of injuries")+
        labs(x= "Types of injuries",y="Frequence")+
        theme(axis.text.x = element_text( angle=45,hjust = 1, size=10),
              axis.title.x  = element_blank(),
              axis.title.y  = element_text( vjust=0,size=13),
              legend.title = element_text(colour="#333333", size=12, face="bold"),
              legend.text = element_text(colour="#6F6F6F", size=12),                         
              legend.position = "bottom",
              legend.direction = "horizontal"
        )
    })
  })
