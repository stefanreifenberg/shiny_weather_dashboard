library(shinydashboard)
library(tidyverse)
library(patchwork)
library(owmr)
library(zoo)
library(lubridate)
library(emojifont)

ui <- dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(collapsed = TRUE,
                     sidebarMenu()),
    dashboardBody(
        tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }
                                '))
        ),
        
        fluidRow(
            valueBoxOutput("tempBox",width = 2),
            valueBoxOutput("feelTempBox",width = 2),
            valueBoxOutput("pressureBox",width = 2),
            valueBoxOutput("humidityBox",width = 2),
            valueBoxOutput("weatherMainBox",width = 2),
            valueBoxOutput("windBox",width = 2)
        ),
        fluidRow(
            column(6, fluidRow(plotOutput("pt1"))),
            column(6, fluidRow(plotOutput("pt2")))
        ),
        fluidRow(
            column(12, fluidRow(plotOutput("pt3")))
        )
    )
)

server <- function(input, output, session) {
    
    owmr_settings("")
    
    autoInvalidate <- reactiveTimer(600000)
    
    observe({
        autoInvalidate()
    })
    
    output$dateText <- renderText({
        autoInvalidate()
        paste("last update @", Sys.time())
    })
    
    data_current <- reactivePoll(
        intervalMillis = 600000, 
        session, 
        checkFunc = function(){
            Sys.time()
        }, 
        valueFunc = function(){
            get_current(lon=yourLon,lat=yourLat, units = "metric")
        }
    )
    
    data_forecast <- reactivePoll(
        intervalMillis = 600000, 
        session, 
        checkFunc = function(){
            Sys.time()
        }, 
        valueFunc = function(){
            get_forecast(lon=yourLon,lat=yourLat, units = "metric")
        }
    )
    
    output$tempBox <- renderValueBox({
        weather <- data_current()
        valueBox(
            paste0(format(weather$main$temp, digits = 2), " °C"), "temperature",
            color = "purple", icon = icon("thermometer-half")
        )
    })
    output$feelTempBox <- renderValueBox({
        weather <- data_current()
        valueBox(
            paste0(format(weather$main$feels_like, digits = 2), " °C"), "feels like",
            color = "purple", icon = icon("thermometer-half")
        )
    })
    
    output$pressureBox <- renderValueBox({
        weather <- data_current()
        valueBox(
            paste0(weather$main$pressure, " hpa"), "air pressure",
            color = "purple", icon = icon("area-chart")
        )
    })
    
    output$humidityBox <- renderValueBox({
        weather <- data_current()
        valueBox(
            paste0(weather$main$humidity, " %"), "humidity",
            color = "purple", icon = icon("tint")
        )
    })
    
    output$weatherMainBox <- renderValueBox({
        weather <- data_current()
        weather_tibble <- as_tibble(weather$weather)
       
        weather_tibble <- weather_tibble %>% 
            mutate(description = case_when(
                description == "overcast clouds" ~ "overcast",
                description == "broken clouds" ~ "broken",
                TRUE ~ description 
            ))
        valueBox(
            paste0(weather_tibble$description), "cloudiness",
            color = "purple", icon = icon("cloud")
        )
    })
    output$windBox <- renderValueBox({
        weather <- data_current()
        valueBox(
            paste0(weather$wind$speed, " km/h"), "wind speed",
            color = "purple", icon = icon("tachometer-alt")
        )
    })
    
    output$pt1 <- renderPlot({
        weather_forecast_data <- data_forecast()
        weather_forecast <- as_tibble(weather_forecast_data$list)
        weather_forecast$new_date <- ymd_hms(weather_forecast$dt_txt)
        
        ggplot(weather_forecast) +
            geom_line(aes(x=new_date,y=main.temp), size = 1.5) +
            geom_point(aes(x=new_date,y=main.temp), size = 5, color ='#605BA8') +
            geom_hline(yintercept = 0, linetype = "dashed") +
            scale_x_datetime(date_breaks = "12 hour", date_labels = "%a-%d\n%H:%M") +
            labs(x = "", y = "", title = "Temperature (°C) forecast") +
            theme_classic(base_size = 15)}, height = 400)
    
    output$pt2 <- renderPlot({ 
        weather_forecast_data <- data_forecast()
        weather_forecast <- as_tibble(weather_forecast_data$list)
        weather_forecast$new_date <- ymd_hms(weather_forecast$dt_txt)
        
        ggplot(weather_forecast, aes(x = new_date)) +
            geom_line(aes(y = main.humidity), size = 1.5) +
            geom_fontawesome(y = weather_forecast$clouds.all,
                             x=weather_forecast$new_date,
                             "fa-cloud",
                             color='#605BA8',
                             size=5) +
            labs(x = "", y = "", title = "Humidity and cloudiness (%) forecast") +
            scale_x_datetime(date_breaks = "12 hour", date_labels = "%a-%d\n%H:%M") +
            scale_y_continuous(limits = c(1,100)) + 
            theme_classic(base_size = 15)}, height = 400)
    
    output$pt3 <- renderPlot({ 
        weather_forecast_data <- data_forecast()
        weather_forecast <- as_tibble(weather_forecast_data$list)
        weather_forecast$new_date <- ymd_hms(weather_forecast$dt_txt)
        
        weather_forecast <- weather_forecast %>% 
            select(new_date,rain.3h) %>% 
            replace_na(list(rain.3h = 0))
        
        ggplot(weather_forecast, aes(x = new_date)) +
            geom_col(aes(y = rain.3h), fill='#605BA8') +
            scale_x_datetime(date_breaks = "12 hour", date_labels = "%a-%d\n%H:%M") +
            scale_y_continuous(expand = c(0,0)) +
            labs(x = "", y = "", title = "Rainfall (mm) forecast")+
            theme_classic(base_size = 15)}, height = 250)
}

shinyApp(ui, server)

