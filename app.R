#Paul Lewin
#Josh Lovejoy
#Bart Hawkins
#Tye Schriever

#Data Visualization Final Project


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
library(ggmap)
library(stringr)
library(lattice)

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
code$Street_Address = as.character(code$Street_Address)
code$State_Code = as.character(code$State_Code)
code$Zip_Code = as.character(code$Zip_Code)
code$Case_Year  = as.character(code$Case_Year)
code2 = code %>% 
  group_by_(.dots=c("Case_Year", "Case_Status_Code_Description", "Case_Type_Code_Description","Zip_Code")) %>% summarize(count = n())

#BKH Get Zip code Data as polygons for Indiana, specific to zip codes in South Bend
zip = zctas(cb = FALSE, year = 2010, state = "IN", starts_with = code2$Zip_Code)

#BKH Set Pallete

pal_bart <- colorNumeric(palette = "Spectral", domain = code2$count)

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
                                          tabPanel("Map", leafletOutput(outputId = "lightschool")),
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
                                     
                                     # Sidebar check boxes for case type, year 
                                     sidebarLayout(
                                       sidebarPanel(
                                         checkboxGroupInput(inputId = "selectCase", 
                                                            label = "Select Code Case Type",
                                                            choices= unique(code2$Case_Status_Code_Description),
                                                            selected = unique(code2$Case_Status_Code_Description)),
                                         
                                         checkboxGroupInput(inputId = "selectYear", 
                                                            label = "Select Code Case Year",
                                                            choices= unique(code2$Case_Year),
                                                            selected = unique(code2$Case_Year))
                                         
                                         
                                       ), #end sidebarPanel
                                       
                                       # Panel Outputs
                                       mainPanel(
                                         tabsetPanel( 
                                           tabPanel("Map", leafletOutput(outputId = "mapBKH")),
                                           tabPanel("Code Violations", dataTableOutput("tablecode1")),
                                           tabPanel("Zip", dataTableOutput("tablecode2"))
                                           
                                         ) # end tabsetPanel
                                         
                                       )# end mainPanel
                                     ) #endSidebarLayout
                            ), #end tabPanel 
#------------------------------------------End Bart's UI Section --------------------------------------------


#------------------------------------------Start Tye's UI Section -------------------------------------------
                            tabPanel("City Council Districts",
                                     
                                     headerPanel("Abandoned Properties in City Council Districts"),
                                     
                                     # Sidebar with a slider input for number of bins 
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
                            )
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
    removeShape(layerId = school$OBJECTID) %>% 
      addPolygons(layerId= school$OBJECTID, data=school[school$SchoolType %in% (input$selectschool),],
                  popup = ~popup, weight=6, color="black", fillColor = "black", highlightOptions = highlightOptions(color = "purple", 
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
                   breaks = "1 month") +
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
                   breaks = "1 month") +
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
                   breaks = "1 month") +
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
                   breaks = "1 month") +
      theme(axis.text.x=element_text(angle=60, hjust=1))
  )
  #data table of call log data
  output$table3 <- renderDataTable(
    datatable(Call_Log)
  )
  
#------------------------------------------End Josh's Server Section --------------------------------------------
  

#------------------------------------------Start Bart's Server Section ------------------------------------------
  selected_type3 <- reactive(input$selectCase)
  selected_type4 <- reactive(input$selectedYear)
  
  output$mapBKH <- renderLeaflet(
    
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data=zip,color = ~pal_bart(code2$count*250), weight = 1, smoothFactor = 0.5,
                  opacity = .25, fillOpacity = 0.1)
  )
  
  output$tablecode1 <- renderDataTable(
    datatable(code2)
  )
  
  output$tablecode2 <- renderDataTable(
    datatable(zip@data)
  )
#------------------------------------------End Bart's Server Section --------------------------------------------
  
  
#------------------------------------------Start Tye's Server Section -------------------------------------------
  selected_type <- reactive(input$selection)
  
  properties$popup <- paste("Status: ",properties$Outcome_St,"</b><br>",
                            "Structures: ",properties$Structures,"<br>",sep ="",
                            "Address: ", properties$Address_Nu, properties$Street_Nam, "</b><br>",
                            "County Tax: ", properties$County_Tax)
  
  pal_tye <- colorFactor(palette = 'Set1', domain =properties$Outcome_St)                        
  
  output$mymap <- renderLeaflet(
    
    
    
    leaflet() %>%
      addTiles()%>%
      addPolygons(data = properties, popup = ~popup, color = ~pal_tye(Outcome_St)) %>%
      addLegend("bottomleft", pal = pal_tye, values = properties$Outcome_St, title = "Abandoned Property Status",
                labFormat = labelFormat())
  )
  
  #this is so the map does not zoom out whenever the checkbox is selected.
  observe(leafletProxy("mymap") %>% removeShape(layerId = council_districts$OBJECTID) %>% 
            addPolygons(layerId= council_districts$OBJECTID, data=council_districts[council_districts$Council_Me %in% (input$selection),], weight=3, color="black", fillColor = "gray", 
                        highlightOptions = highlightOptions(color = "black", fillColor = "red", weight = 4,
                                                            bringToFront = TRUE))
          
  )
  
  output$table1 <- renderDataTable(
    datatable(council_districts@data)
  )
  
  output$table2 <- renderDataTable(
    datatable(properties@data)
  )
  
#------------------------------------------End Tye's Server Section ---------------------------------------------
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

