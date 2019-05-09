# When You Run the code after output Select year and Gender. It will plot the Accident Severity based on the gender. and Trends of all time is by default. To see the Severity Please Zoom in the Map.

# Please choose different weather condition for variety of Effect
#Also open the app in browser for better view of the results.

# Red : Towards 7 (More Severe )
# Blue : Towrads 1 (less Severe)

# Code Below
#setwd("Set directory to the file location")
#getwd()

# Import Library

library(tidyverse)
library(leaflet)
library(shiny)
library(RColorBrewer)

library(dplyr)
library(lubridate)
library(tidyverse)
library(janitor)

df <-  read.csv('CambridgeCrashData.csv',stringsAsFactors = F )
#df <- clean_names(df)
#help(clean_names)
df <- dplyr::rename(df,street_name = street_name)

df$date_time <- as.Date(df$date_time, "%m/%d/%Y")

df$date_time <- ymd(df$date_time)
df <- df %>% 
    mutate(month = month(date_time),
           month_name = month.abb[month],
           day = day(date_time),
           Weekday = weekdays(date_time),
           dow = lubridate::wday(date_time),
           Week = week(date_time))

# create Season
df<- df %>% mutate(Season = ifelse(month %in% c(6,7,8), "Summer",
                                   ifelse(month %in% c(9,10,11), "Fall",
                                          ifelse(month %in% c(12,1,2), "Winter",
                                                 "Spring"))))

# Cross_street
#df_cross_street <- df %>% group_by(cross_street) %>% dplyr::summarise(n= n()) %>% arrange(desc(n))
#df_cross_street_top_10 <- df_cross_street[1:10, ]

# df_street_name_top_10
df_street_name <- df %>% group_by(street_name) %>% dplyr::summarise(n= n()) %>% arrange(desc(n))
df_street_name_top_10 <- df_street_name[1:10, ]


# df_year
df_year <- df %>% group_by(year) %>% dplyr::summarise(n= n())

# df_week
df_Week <- df %>% group_by(Week) %>% dplyr::summarise(n= n())%>% arrange(desc(n))
df_Week <- df_Week [1:10, ]

# Day_of_week
df_dow <- df %>% group_by(day_of_week) %>% dplyr::summarise(n= n())

# df_month_name
df_month_name <- df %>% group_by(month_name) %>% dplyr::summarise(n= n())%>% arrange(desc(n))

#create seasons
df_Season <- df %>% group_by(Season) %>% dplyr::summarise(n= n())

# df_weather
df_weather <- df %>% group_by(weather) %>% dplyr::summarise(n= n())

# df_gender
df_gender <- df %>% group_by(gender) %>% dplyr::summarise(n= n())

# df_hospital
df_hospital <- df %>% group_by(hospitals) %>% dplyr::summarise(n= n())

# df_obj_1_2_top_10
df_obj_1_2 <- df %>% group_by(object_1, object_2) %>% dplyr::summarise(n= n()) %>% arrange(desc(n))
df_obj_1_2_top_10 <- df_obj_1_2 [1:10, ]


# Clean datasets
df$street_number <- NULL
df$cross_street <- NULL

df$day_of_week <- as.factor(df$day_of_week)
df$object_1 <- as.factor(df$object_1)
df$object_2 <- as.factor(df$object_2)
df$weather <- as.factor(df$weather)
df$hospitals <- as.factor(df$hospitals)
df$gender <- as.factor(df$gender)
df$Season <- as.factor(df$Season)
df$month_name <- as.factor(df$month_name)
df$Weekday <- as.factor(df$Weekday)
write.csv(df, file = "ShinyAppDS.csv")


data <- read_csv("ShinyAppDS.csv")

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body { width: 100%; height: 100%"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(h3("Object_1 and Object_2 Accidents Basen on Gender and Year Selected."),
                  id = "controls", class = "panel panel-default", top = 10, right = 10,
                  fixed = TRUE, draggable = FALSE, width = 330, height = "auto",
                  selectInput("range", "year:",
                              c("All years","2010", "2011", "2012", "2013", "2014", "2015", "2016")),
                  
                  # Type filter
                  selectInput("gender", "Choose type: ", choices = NULL),
                  
                  # Histogram 
                  plotOutput("histCentile", height = 200),
                  plotOutput("lineTrend", height = 140),
                  
                  # Conditions filter 
                  selectInput("conditions", "Conditions:", choices = NULL)
                  
                  
    )
)

server <- function(input, output, session) {
    
    conditions_list <- data$weather
    names(conditions_list) <- conditions_list
    updateSelectInput(session, "conditions", choices = conditions_list)
    
    gender_list <- data$gender
    names(gender_list) <- gender_list
    updateSelectInput(session, "gender", choices = gender_list)
    
    palette_rev <- rev(brewer.pal(5, "RdYlBu"))
    
    colorpal <- reactive({
        colorNumeric(palette_rev, data$hospitals)
    })
    
    filteredData <- reactive({
        if ("All Years" %in% input$range) {
            data  
        } else {
            data %>% filter(year == input$range,
                                  gender == input$gender,
                                  weather == input$conditions)
        }
    })
    
    output$map <- renderLeaflet({
                        leaflet(data) %>% 
                        addProviderTiles(providers$CartoDB.Positron) %>% 
            fitBounds(-71.16031,-71.10973,42.37362,42.40275)
            #fitBounds(~ min(longitude), ~ min(latitude), ~ max(longitude), ~ max(latitude))
    })
    
    filteredSeverity <- reactive({
        if ("All Years" %in% input$range) {
            data
        } else {
            data %>% filter(year == input$range)
        }
    })
    
    observe({
        pal <- colorpal()
        
        leafletProxy("map", data = filteredData()) %>% 
            clearMarkers() %>% 
            clearControls() %>% 
            addCircleMarkers(radius = 6,
                             stroke = FALSE,
                             fillColor = ~pal(hospitals),
                             fillOpacity = 0.7,
                             popup = ~paste("Hospitals: ", hospitals, 
                                            "<br/>",
                                            "Object_2: ", object_2,
                                            "<br/>",
                                            "Object_1: ", object_1,
                                            "<br/>",
                                            "Gender: ", gender,
                                            "<br/>",
                                            "Weather: ", weather,
                                            "<br/>",
                                            "Season: ",Season)
            ) %>% 
            addLegend("bottomleft", pal = pal, values = ~hospitals,
                      title = "hospitals",
                      opacity = 1)
    })
    
    output$histCentile <- renderPlot({
        ggplot(filteredSeverity(), aes(x = hospitals)) +
            geom_bar(stat = "count", aes(fill = gender)) +
            theme_minimal() +
            labs(title = paste("Accident Severity in", input$range)) +
            xlab("Accident Severity (1 = most severe)") +
            ylab("No. of Accidents")
    })
    
    output$lineTrend <- renderPlot({
        ggplot(data, aes(x = year, color = gender)) +
            geom_line(stat = "count") +
            theme(legend.title = element_blank()) +
            labs(title = "Trend for All Years") +
            geom_vline(xintercept=as.numeric(input$range), linetype = 1)
    })
    
}

shinyApp(ui, server)