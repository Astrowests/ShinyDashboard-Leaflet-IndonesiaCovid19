library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(tidyverse)
library(lubridate)
library(glue)
library(leaflet)
library(plotly)

data <- read_csv("data_input/covid_19_indonesia_time_series_all.csv")
data <- data %>% mutate(Date=mdy(Date),
                        `Case Fatality Rate`= as.double(str_replace(`Case Fatality Rate`,"%","")),
                        `Case Recovered Rate`=as.double(str_replace(`Case Recovered Rate`,"%",""))) %>% 
    mutate_at(vars(`Location ISO Code`,Location,`Location Level`,`City or Regency`,Province,Country,Continent,Island,`Special Status`,`Total Regencies`,`Total Cities`,`Total Districts`,`Total Districts`,`Total Urban Villages`,`Total Rural Villages`,Population,`Population Density`,Latitude,Longitude),as.factor) %>% 
    mutate_at(vars(`Case Fatality Rate`,`Case Recovered Rate`),as.double)
data <- data %>% filter(Location != "Indonesia") 
unique(data$Date)

dashboardPage(
    dashboardHeader(title = "Covid19 ON INDONESIA",titleWidth = 10),
    dashboardSidebar(disable = T),
    dashboardBody(
        fluidPage(
            fluidRow(
                column(HTML("Data Terakhir Diambil : 19 Desember, Source :"),
                       HTML("<a href='https://www.kaggle.com/hendratno/covid19-indonesia'>source dataset</a>"),
                    width = 6,
                       sliderInput(inputId = 'sliderInput',label = "Tanggal :",min = min(unique(data$Date)),max = max(unique(data$Date)),value = seq(to = max(unique(data$Date)),from =  min(unique(data$Date)),by="day")),
                       selectizeInput(inputId = "provinceInput",label = "Provinsi :",choices=factor(c(as.vector(data$Province),"Semua")),selected ="Semua"),
                       dataTableOutput("dataKeseluruhan")
                       
                       
                       
                ),
                column(tags$h1("Kumulatif terhadap semua provinsi"),
                    width = 6,
                    infoBoxOutput(width = 6,"totalCases"),
                    infoBoxOutput(width = 6,"totalactiveCases"),
                    infoBoxOutput(width = 6,"totalDeaths"),
                    infoBoxOutput(width = 6,"totalRecovery")
                    
                )
                
                ),
            fluidRow(
               tags$h1("Pengelompokan berdasarkan Provinsi & Harian"),
                leafletOutput("map"),
               tags$h1("Pengelompokan berdasarkan  Harian"),
                plotlyOutput("plot")
            
                ),
            fluidRow(
                
            )
        )
    )
)
                    
                    
                 
    
