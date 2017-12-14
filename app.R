library(Rmisc)
library(ggplot2)
library(dplyr)
library(Metrics)
library(scales)
library(ggmap)
library(gdata)
library(shiny)
library(googleway)


# must setwd()
nycbuildingstable <- read.csv("nyc_disclosure.csv", header = TRUE, sep = ",")
sample_fulladdress <- read.csv("geocoded.csv", header = TRUE, sep = ",")
nycbuildingstable$Primary.Property.Type...Self.Selected <- as.character(nycbuildingstable$Primary.Property.Type...Self.Selected)
colnames(nycbuildingstable)[15] <- "property_type"
colnames(nycbuildingstable)[45] <- "grid_kBtu"
colnames(nycbuildingstable)[47] <- "allC02e"

boroughbyprop<- nycbuildingstable %>%
  na.omit() %>%
  group_by(Borough, property_type) %>%
  summarise(avg = mean(grid_kBtu)) %>%
  arrange(Borough)

propbyborough<- nycbuildingstable %>%
  na.omit() %>%
  group_by(property_type,Borough) %>%
  summarise(avg = mean(grid_kBtu)) %>%
  arrange(property_type)

boroughbypropco2<- nycbuildingstable %>%
  na.omit() %>%
  group_by(Borough, property_type) %>%
  summarise(avg = mean(allC02e)) %>%
  arrange(Borough)

propbyboroughco2<- nycbuildingstable %>%
  na.omit() %>%
  group_by(property_type,Borough) %>%
  summarise(avg = mean(allC02e)) %>%
  arrange(property_type)


#ggplot(data = boroughbyprop, aes(x=reorder(Borough, desc(avg)), y = avg)) + geom_histogram(fill="grey", stat = "identity", color="blue") + scale_x_discrete(name ="Property Type") + scale_y_continuous(name = "Total kBTU") + ggtitle("MultiFamily Avg kBTU by Borough") + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14, hjust=0)) + geom_text(aes(label = comma(round(avg)), sep=","), position = position_dodge(0.9), size=3, vjust=-0.25)

myboro  <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
mytypes <- unique(propbyborough$property_type)

newaddress <- sample_fulladdress[ which(sample_fulladdress$Borough=="Brooklyn"), ]
newaddress$lon<-as.numeric(newaddress$lon)
newaddress$lat<-as.numeric(newaddress$lat)
googlemapdf<- data.frame(newaddress$lat, newaddress$lon)
colnames(googlemapdf)[1] <- "lat"
colnames(googlemapdf)[2] <- "lon"
geokey <- "AIzaSyAhH_6014s0bMwIDVbpn36BekqtpCm2eNM"
google_map(key = geokey, data = googlemapdf, location = googlemapdf) %>% add_heatmap()




ui = fluidPage(
  
  titlePanel("All City Energy Dashboard"),
  
  selectInput(inputId = "boro", 
              label = "Choose Borough",
              choices = myboro,	selected = NULL, multiple = FALSE,
              selectize = TRUE, width = NULL, size = NULL),
  
  plotOutput("boro"),	
  
  
  selectInput(inputId = "type", 
              label = "Choose Property Type",
              choices = mytypes,	selected = NULL, multiple = FALSE,
              selectize = TRUE, width = NULL, size = NULL),
  
  plotOutput("type"),
  
  selectInput(inputId = "typeA", 
              label = "Choose Property Type",
              choices = mytypes,	selected = NULL, multiple = FALSE,
              selectize = TRUE, width = NULL, size = NULL),
  
  plotOutput("typeA"),
  
  selectInput(inputId = "myboro", 
              label = "Choose Borough (All City Default)",
              choices = myboro,	selected = NULL, multiple = TRUE,
              selectize = TRUE, width = NULL, size = NULL),
  
  google_mapOutput(outputId = "map")
)

server = function(input,output) {
  
  
  output$boro <-renderPlot({
  
  boroughbyprop<- nycbuildingstable %>%
      na.omit() %>%
      group_by(Borough, property_type) %>%
      summarise(avg = mean(grid_kBtu)) %>%
      arrange(Borough)
  
  boroughbyprop <- as.data.frame(boroughbyprop)
  boroughbyprop <- boroughbyprop[ which(boroughbyprop$Borough==input$boro), ]
   
  ggplot(data = boroughbyprop,aes(x = reorder(property_type,desc(avg)), y = avg)) + geom_histogram(fill="blue", stat = "identity") + scale_y_continuous(name = "Avg kBTU") + ggtitle("Avg kBTU by Borough (000)") + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14, hjust=0)) + geom_text(aes(label = comma(round(avg)), sep=","), position = position_dodge(0.9), size=5, color="black", vjust=-0.25)
      
  })
  
  output$type <-renderPlot({
    
    propbyborough<- nycbuildingstable %>%
      na.omit() %>%
      group_by(property_type,Borough) %>%
      summarise(avg = mean(grid_kBtu)) %>%
      arrange(property_type)
    
    propbyborough <- as.data.frame(propbyborough)
    propbyborough <- propbyborough[ which(propbyborough$property_type==input$type), ]
    
    ggplot(data = propbyborough,aes(x = reorder(Borough,desc(avg)), y = avg)) + geom_histogram(fill="green",stat = "identity") + scale_y_continuous(name = "Avg kBTU") + ggtitle("Avg kBTU by Property Type (000)") + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14, hjust=0)) + geom_text(aes(label = comma(round(avg)), sep=","), position = position_dodge(0.9), size=5, color="black", vjust=-0.25)
    
  })

  output$typeA <-renderPlot({
    
    propbyborough2<- nycbuildingstable %>%
      na.omit() %>%
      group_by(property_type,Borough) %>%
      summarise(avg = mean(allC02e)) %>%
      arrange(property_type)
    
    propbyborough2 <- as.data.frame(propbyborough2)
    propbyborough2 <- propbyborough2[ which(propbyborough2$property_type==input$typeA), ]
    
    ggplot(data = propbyborough2,aes(x = reorder(Borough,desc(avg)), y = avg)) + geom_histogram(fill="dark red",stat = "identity") + scale_y_continuous(name = "Avg C02e") + ggtitle("Avg C02e by Property Type (000)") + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14, hjust=0)) + geom_text(aes(label = comma(round(avg)), sep=","), position = position_dodge(0.9), size=5, color="black", vjust=-0.25)
    
  })
  
  geokey<- 'AIzaSyAhH_6014s0bMwIDVbpn36BekqtpCm2eNM'
  
  output$map <-renderGoogle_map({
    
    newaddress <- sample_fulladdress[ which(sample_fulladdress$Borough=="Brooklyn"), ]
    newaddress$lon<-as.numeric(newaddress$lon)
    newaddress$lat<-as.numeric(newaddress$lat)
    googlemapdf<- data.frame(newaddress$lat, newaddress$lon)
    colnames(googlemapdf)[1] <- "lat"
    colnames(googlemapdf)[2] <- "lon"
    google_map(key = geokey, data = googlemapdf, location = googlemapdf) %>% add_heatmap(data = googlemapdf, lat = "lat", lon = "lon",
                                                                                          option_radius = 0.003)
    
  })
  
  observeEvent(input$myboro, {
    
    newaddress <- sample_fulladdress[ which(sample_fulladdress$Borough==input$myboro), ]
    newaddress$lon<-as.numeric(newaddress$lon)
    newaddress$lat<-as.numeric(newaddress$lat)
    googlemapdf<- data.frame(newaddress$lat, newaddress$lon)
    colnames(googlemapdf)[1] <- "lat"
    colnames(googlemapdf)[2] <- "lon"
    
    google_map_update(map_id = "map") %>%
      update_heatmap(data = googlemapdf, lat = "lat", lon = "lon")
  }) 
  
}
shinyApp(ui = ui, server = server)