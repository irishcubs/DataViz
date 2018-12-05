#Paul Lewin
#Josh Lovejoy
#Bart Hawkins
#Tye Schriever

#Data Visualization Final Project

#https://github.com/irishcubs/DataViz

library(shiny)
library(rgdal)
library(leaflet)
library(DT)
library(markdown)
library(shinythemes)
library(ggthemes)
library(scales)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tigris)

library(stringr)
library(lattice)
library(htmltools)

library(ggmap)
library(rminer)

#setwd("C:/Users/LewinLappy/Documents/NotreDame/Classes/DataVisualization/Project/Data/DataVizFinal")

#------------------------------------------Start Paul's Preparation Section ------------------------------------------
#processing and loading
lights <- read.csv("Street_Lights.csv")
lights2 <- SpatialPointsDataFrame(coords = lights[,c("Lon","Lat")], data = lights,
                                  proj4string = CRS("+proj=longlat +datum=WGS84"))

school <- readOGR(dsn=".", "School_Boundaries", stringsAsFactors = F)

#------------------------------------------End Paul's Preparation Section --------------------------------------------


#------------------------------------------Start Josh's Preparation Section ------------------------------------------
#processing and loading 311 data
Call_Log <- read.csv("311_Phone_Call_Log_Mod.csv")
#Cases <- read.csv("311_Contact_Management_Cases.csv")
# fix name format
names(Call_Log)[1] <- "Call_DateTime"
#fix date time format
Call_Log$Call_DateTime <- Call_Log$Call_DateTime %>% as.Date(., tz = "UTC", "%Y-%m-%dT%H:%M:%OS")
# mutate for average duration
# Calculate agregated data per week
Call_Log_avg_wk=Call_Log %>% mutate(week = as.Date(cut(Call_DateTime, breaks = "week"))) %>%
  group_by(week, Department) %>% 
  summarise(average = mean(na.omit(duration_Seconds)))
# df for static duration display
Call_Log_avg_static=Call_Log %>% mutate(week = as.Date(cut(Call_DateTime, breaks = "week"))) %>%
  group_by(week) %>% 
  summarise(average = mean(na.omit(duration_Seconds))) 

#------------------------------------------End Josh's Preparation Section --------------------------------------------


#------------------------------------------Start Bart's Preparation Section ------------------------------------------

#BKH Processing and Handling Code Enforcement Cases
code <- read.csv("Code_Enforcement_Casesa.csv",header=TRUE)
as.data.frame(code)
code$Case_Year = as.integer(code$Case_Year)
code$Case_Year <- code$Case_Year+2000
code$Street_Address = as.character(code$Street_Address)
code$State_Code = as.character(code$State_Code)
code$Zip_Code = as.character(code$Zip_Code)
code$Case_Year  = as.character(code$Case_Year)
code$Case_Status_Code_Description = as.character(code$Case_Status_Code_Description)
#code$Address = as.character(code$Address)

code2 = code %>% 
  group_by_(.dots=c("Case_Year", "Case_Status_Code_Description", "Case_Type_Code_Description","Zip_Code")) %>% summarize(count = n())

code5 <- code %>% group_by(Zip_Code) %>% summarize(count=n())

#BKH Get Zip code Data as polygons for Indiana, specific to zip codes in South Bend
zip = zctas(cb = FALSE, year = 2010, state = "IN", starts_with = code2$Zip_Code)

zipper <- as.data.frame(zip@data)
code4 <- merge(zipper, code5, by.x="ZCTA5CE10", by.y="Zip_Code")

#code7 is a geocoded address file for a representative sample of code cases, 2468 of them, built via Google Maps
load("code7.RData")

#BKH Set Pallete

pal <- colorNumeric(palette = "RdBu", domain = code2$count)


#------------------------------------------End Bart's Preparation Section -------------------------------------------


#------------------------------------------Start Tye's Preparation Section ------------------------------------------

council_districts <- readOGR(dsn=".", layer = "City_Council_Districts", stringsAsFactors = F)
properties <- readOGR(dsn=".", layer = "Abandoned_Property_Parcels", stringsAsFactors = F)

#------------------------------------------End Tye's Preparation Section --------------------------------------------

# Define UI for application
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # Application title
                titlePanel("South Bend Data"),
                
                navbarPage("",
                           
                           
#------------------------------------------Start Paul's UI Section ------------------------------------------
                           tabPanel("Street Lights and Schools",
                                    
                                    headerPanel("Street Lights and Schools"),
                                    
                                    # Checkbox to show which type of school to display. 
                                    sidebarLayout(
                                      sidebarPanel(
                                        checkboxGroupInput(inputId = "selectschool", 
                                                           label = "Select School Type",
                                                           choices= unique(school$SchoolType),
                                                           selected = unique(school$SchoolType))
                                        
                                        
                                      ), #end sidebarPanel
                                      
                                     # Display the output in the main panel.
                                      mainPanel(
                                        tabsetPanel( 
                                          tabPanel("View Map", leafletOutput(outputId = "lightschool")),
                                          tabPanel("Street Lights", dataTableOutput("lights_table")),
                                          tabPanel("Schools", dataTableOutput("school_table"))
                                          
                                        ) # end tabsetPanel
                                        
                                      )# end mainPanel
                                    ) #endSidebarLayout
                                    
                           ), #end tabPanel
#------------------------------------------End Paul's UI Section --------------------------------------------
                           
                           
#------------------------------------------Start Josh's UI Section ------------------------------------------
                           tabPanel("311 Call Activity",
                                    
                                    headerPanel("311 Call Activity"),
                                    
                                    # Sidebar with a slider input for number of bins 
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons(inputId = "selection2",
                                                     label = "Select Department",
                                                     choices= unique(Call_Log$Department),
                                                     selected = "Water Works")
                                        #dateRangeInput(inputId = "dateRange_selection",
                                        #               label = h3("Date Range"),
                                        #               start = min(Call_Log$Call_DateTime),
                                        #               end = max(Call_Log$Call_DateTime))
                                        
                                      ), #end sidebarPanel
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                        tabsetPanel( 
                                          tabPanel("Call Activity", plotOutput(outputId = "areaPlot"), plotOutput(outputId = "areaPlot_static")),
                                          tabPanel("Average Call Duration", plotOutput(outputId = "linePlot"), plotOutput(outputId = "linePlot_static")),
                                          tabPanel("311 Call Logs", dataTableOutput("table3"))
                                          
                                        ) # end tabsetPanel
                                        
                                      )# end mainPanel
                                    ) #endSidebarLayout
                           ), #end tabPanel
#------------------------------------------End Josh's UI Section --------------------------------------------
                           
                           
#------------------------------------------Start Bart's UI Section ------------------------------------------
                           tabPanel("Code Compliance",
                                    
                                    headerPanel("Code Compliance"),
                                    
                                    # Sidebar check boxe for year 
                                    sidebarLayout(
                                      sidebarPanel(
                                        
                                        checkboxGroupInput(inputId = "selectYear", 
                                                           label = "Select Code Case Year",
                                                           choices= unique(code7$Case_Year),
                                                           selected = unique(code7$Case_Year))
                                        
                                        
                                      ), #end sidebarPanel
                                      
                                      # Panel Outputs
                                      mainPanel(
                                        tabsetPanel( 
                                          tabPanel("Map", leafletOutput(outputId = "mapBKH")),
                                          tabPanel("Code Violations Summary", dataTableOutput("tablecode1")),
                                          tabPanel("Code Violations Raw Data",dataTableOutput("tablecode2")),
                                          tabPanel("Violation Summary by Zip", dataTableOutput("tablecode3"))
                                          
                                        ) # end tabsetPanel
                                        
                                      )# end mainPanel
                                    ) #endSidebarLayout
                           ), #end tabPanel
#------------------------------------------End Bart's UI Section --------------------------------------------
                           
#------------------------------------------Start Tye's UI Section -------------------------------------------
                           tabPanel("City Council Districts",
                                    
                                    headerPanel("Abandoned Properties in City Council Districts"),
                                    
                                    # Sidebar for selection of council member district 
                                    sidebarLayout(
                                      sidebarPanel(
                                        checkboxGroupInput(inputId = "selection", 
                                                           label = "Select Council Member",
                                                           choices= unique(council_districts$Council_Me),
                                                           selected = unique(council_districts$Council_Me))
                                        
                                      ), #end sidebarPanel
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                        tabsetPanel( 
                                          tabPanel("Map", leafletOutput(outputId = "mymap")),
                                          tabPanel("City Council Districts", dataTableOutput("table1")),
                                          tabPanel("Abandoned Properties", dataTableOutput("table2"))
                                          
                                        ) # end tabsetPanel
                                        
                                      )# end mainPanel
                                    ) #endSidebarLayout
                           ) #end tabPanel
#------------------------------------------End Tye's UI Section ---------------------------------------------
                           
                           
                ) #end Navbar
                
)#end ui

# Define server logic
server <- function(input, output) {
  
  #------------------------------------------Start Paul's Server Section ------------------------------------------
  
  
  
  school$popup <- paste("<b>",school$School,"</b><br>",
                        "Type: ",school$SchoolType,"<br>",sep ="")
  
  set_lat = (mean(lights$Lat) + .015)
  set_long = (mean(lights$Lon) - .03)
  
  output$lightschool <- renderLeaflet(
    
    leaflet() %>%
      addTiles()%>%
      setView(lat=set_lat, lng=set_long, zoom=14) %>%
      addCircleMarkers(data=lights2, radius = 1, color = "blue")
  )
  
  #this is so the map does not zoom out whenever the checkbox is selected.
  observe(leafletProxy("lightschool") %>% 
            removeShape(layerId = school$School) %>% 
            addPolygons(layerId= school$School, data=school[school$SchoolType %in% (input$selectschool),],
                        popup = school$popup, weight=6, color="black", fillColor = "black", highlightOptions = highlightOptions(color = "purple", 
                                                                                                                                fillColor = "purple", weight = 6, opacity=.7, bringToFront = TRUE))
  )
  
  output$lights_table <- renderDataTable(
    datatable(lights)
  )
  
  output$school_table <- renderDataTable(
    datatable(school@data)
  )
  #------------------------------------------End Paul's Server Section --------------------------------------------
  
  
  #------------------------------------------Start Josh's Server Section ------------------------------------------
  selected_type2 <- reactive(input$selection2)
  #dateRangeInput <- reactive(input$dateRange_selection)
  #311 tabs
  #dynamic area plot of call logs over time
  output$areaPlot <- renderPlot(
    ggplot(Call_Log[Call_Log$Department %in% (input$selection2),], aes(Call_DateTime))+
      geom_area(stat="bin", fill=alpha('slateblue',0.2),colour="black", size=.2, aplha=.4)+
      xlab("Date of Call")+
      ylab("Number of Calls")+
      ggtitle(paste("Southbend",input$selection2, "Call Activity"))+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(labels = date_format("%b.%y"),
                   breaks = "1 month",
                   limits = as.Date(c('2016-09-29','2017-04-14'))) +
      theme(axis.text.x=element_text(angle=60, hjust=1))
  )
  
  
  #static area plot of call logs over time
  output$areaPlot_static <- renderPlot(
    ggplot(Call_Log, aes(Call_DateTime))+
      geom_area(stat="bin", fill=alpha('slateblue',0.2),colour="black", size=.2, aplha=.4)+
      ylim(low=0, high=3000)+
      xlab("Date of Call")+
      ylab("Number of Calls")+
      ggtitle("Southbend 311 Call Activity")+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(labels = date_format("%b.%y"),
                   breaks = "1 month",
                   limits = as.Date(c('2016-09-29','2017-04-14'))) +
      theme(axis.text.x=element_text(angle=60, hjust=1))
  )
  
  #dynamic plot for call duration
  output$linePlot <- renderPlot(
    ggplot(Call_Log_avg_wk[Call_Log_avg_wk$Department %in% (input$selection2),], aes(x=week, y=average)) +
      geom_line() + 
      #geom_point() +
      geom_area(fill=alpha('slateblue',0.2)) +
      xlab("Date of Call")+
      ylab("Average Call Duration (sec)")+
      ggtitle(paste("Southbend", input$selection2, "Average Call Duration"))+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(labels = date_format("%b.%y"),
                   breaks = "1 month",
                   limits = as.Date(c('2016-09-29','2017-04-14'))) +
      theme(axis.text.x=element_text(angle=60, hjust=1))
  )
  
  #static plot fro call duration
  output$linePlot_static <- renderPlot(
    ggplot(Call_Log_avg_static, aes(x=week, y=average)) +
      geom_line() + 
      #geom_point() +
      geom_area(fill=alpha('slateblue',0.2)) +
      xlab("Date of Call")+
      ylab("Average Call Duration (sec)")+
      ggtitle("Southbend 311 Average Call Duration")+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(labels = date_format("%b.%y"),
                   breaks = "1 month",
                   limits = as.Date(c('2016-09-29','2017-04-14'))) +
      theme(axis.text.x=element_text(angle=60, hjust=1))
  )
  #data table of call log data
  output$table3 <- renderDataTable(
    datatable(Call_Log)
  )
  
  #------------------------------------------End Josh's Server Section --------------------------------------------
  
  
  #------------------------------------------Start Bart's Server Section ------------------------------------------
  #BKH Code
  
  selected_type3 <- reactive(input$selectedYear)
  
  # create popup for Case Data
  code7$popupBKH <- paste("Status:    ",code7$Case_Status_Code_Description,"</b><br>",
                          "Type:      ",code7$Case_Type_Code_Description,"</b><br>",
                          "Year:      ",code7$Case_Year,"</b><br>",
                          "Address:   ",code7$Address, "</b><br>")
  
  
  #create palette for Case Types & Legend
  palb <- colorFactor(palette = 'RdYlBu', domain =code7$Case_Type_Code_Description)    
  
  #Build & present map data with two sets of overlays
  output$mapBKH <- renderLeaflet(
    
    leaflet() %>%
      
      addTiles(group = "Basic")  %>%
      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery")  %>%
      addPolygons(data=zip,color = "black", weight = 3, smoothFactor = 0.5, opacity = 1, fillOpacity = 0.01, 
                  label = paste("Zip: ",zip$ZCTA5CE10, "Cases:  ",code4$count),
                  highlightOptions = highlightOptions(color = "black", fillColor = "red", 
                                                      weight = 4, fillOpacity = 0.1, sendToBack = TRUE)) %>%
      addCircleMarkers(data=code7[code7$Case_Year %in% input$selectYear,], popup = ~popupBKH, radius = 3, 
                       color = ~palb(code7$Case_Type_Code_Description)) %>%
      addLegend("bottomleft", pal = palb, values = code7$Case_Type_Code_Description, title = "Case Types",
                labFormat = labelFormat()) %>%
      addLayersControl(
        baseGroups = c("Basic", "Terrain", "Imagery"),
        options = layersControlOptions(collapsed = FALSE))
    
  )
  #create table views of data - condensed, raw, zip codes
  output$tablecode1 <- renderDataTable(
    datatable(code2)
  )
  
  output$tablecode2 <- renderDataTable(
    datatable(code)
  )
  output$tablecode3 <- renderDataTable(
    datatable(code5)
  )
  
  #------------------------------------------End Bart's Server Section --------------------------------------------
  
  
  #------------------------------------------Start Tye's Server Section -------------------------------------------
  data2 <- reactive(
    council_districts[council_districts$Council_Me %in% (input$selection),])
  
  # create popup for abandoned properties data
  properties$popup <- paste("Status: ",properties$Outcome_St,"</b><br>",
                            "Structures: ",properties$Structures,"<br>",sep ="",
                            "Address: ", properties$Address_Nu, properties$Street_Nam, "</b><br>",
                            "County Tax: ", properties$County_Tax)
  
  #create palette
  pal_tye <- colorFactor(palette = 'Set1', domain =properties$Outcome_St) 
  
  output$mymap <- renderLeaflet(
    
    leaflet() %>%
      addTiles()%>%
      #add properties polygon
      addPolygons(data = properties, popup = ~popup, color = ~pal_tye(Outcome_St)) %>%
      #add legend for status of properties
      addLegend("bottomleft", pal = pal_tye, values = properties$Outcome_St, title = "Abandoned Property Status",
                labFormat = labelFormat())%>%
      addPolygons(layerId= data2()$OBJECTID, data=data2(), weight=3, color="black", fillColor = "gray", fillOpacity = 0.1,
                  label = (data2()$Council_Me),
                  highlightOptions = highlightOptions(color = "black", fillColor = "red", 
                                                      weight = 4, fillOpacity = 0.1, sendToBack = TRUE)
      ) #end of addPolygons function
  ) #end of renderLeaflet
  
  #this is so the map does not zoom out whenever the checkbox is selected.
  observe(leafletProxy("mymap") %>% 
            removeShape(layerId = data2()$OBJECTID) %>% 
            addPolygons(layerId= data2()$OBJECTID, data=data2(), weight=3, color="black", fillColor = "gray", fillOpacity = 0.1,
                        label = (data2()$Council_Me),
                        highlightOptions = highlightOptions(color = "black", fillColor = "red", 
                                                            weight = 4, fillOpacity = 0.1, sendToBack = TRUE)
            ) #end of addPolygons function
          
  ) #end of observe function
  
  output$table1 <- renderDataTable(
    datatable(council_districts@data)
  ) #end of output table1
  
  output$table2 <- renderDataTable(
    datatable(properties@data)
  ) #end of output table2
  
  #------------------------------------------End Tye's Server Section ---------------------------------------------
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

