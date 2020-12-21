


server <- function(input,output){
    dataku <- reactive({
        
        tanggal_pertama <- input$sliderInput[1]
        tanggal_kedua <- input$sliderInput[2]
        
        total_Summary <- data %>% 
            group_by(Province) %>% 
            filter(Date>=tanggal_pertama, Date<=tanggal_kedua) %>% 
            filter(if (input$provinceInput == "Semua") Province != input$provinceInput else Province == input$provinceInput) %>% 
            arrange(desc(`Total Cases`)) %>% 
            filter(`Total Cases` == max(`Total Cases`)) %>% 
            distinct() %>% 
            ungroup() %>% 
            mutate(n_death = sum(`Total Deaths`),n_Cases=sum(`Total Cases`),n_ActiveCases=sum(`Total Active Cases`),n_Total_Recovered=sum(`Total Recovered`))
        
    })
    output$totalCases <- renderInfoBox({
        total_Summary <- dataku()
        infoBox(title = "Total Cases",value =total_Summary$n_Cases[1],subtitle = ,
                icon = icon("virus"))
        })
    output$totalDeaths <- renderInfoBox({
        total_Summary <- dataku()
        infoBox(title = "Total Death",color = 'red',value =total_Summary$n_death[1],subtitle = ,
                icon = icon("skull-crossbones"))
    })
    output$totalactiveCases <- renderInfoBox({
        total_Summary <- dataku()
        infoBox(title = "Total Active Cases",color = 'yellow',value =total_Summary$n_ActiveCases[1],subtitle = "subtitel",
                icon = icon("procedures"))
    })
    output$totalRecovery <- renderInfoBox({
        total_Summary <- dataku()
        infoBox(title = "Total Recovered",color = 'green',value =total_Summary$n_Total_Recovered[1],subtitle = "subtitel",
                icon = icon("virus-slash"))
    })
    
    output$map <- renderLeaflet({
        total_Summary <- dataku()
        popup <- glue('Lokasi : {total_Summary$Location}<br>
        Total Cases : {total_Summary$`Total Cases`}<br>
                      Total Death : {total_Summary$`Total Deaths`}<br>
                      Total Recovered : {total_Summary$`Total Recovered`}<br>
                      Total Active Cases : {total_Summary$`Total Active Cases`}')

      
        
        
        
        
            leaflet() %>% 
            addTiles() %>% 
            addMarkers(lng = total_Summary$Longitude,lat =total_Summary$Latitude,markerClusterOptions(),popup = popup )
                })
   
    
    output$plot <- renderPlotly(
        {
            total_summary <- dataku()
            plot<- total_summary %>% 
                group_by(Location,`Total Cases`) %>% 
                arrange(desc(`Total Cases`)) %>% 
                ggplot(aes(x=Date,y = `New Cases`)) +
                geom_jitter(aes(size=`Total Cases`,col=Location),show.legend = F) 
            
            ggplotly(plot)
            
            
        }
    )
}