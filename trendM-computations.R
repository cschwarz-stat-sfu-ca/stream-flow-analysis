### Trend computation for multiple stations 
# This will take the various input, select the data from the HYDAT database, compute trends
# and plot everything

  ###################################################################################
  
  # Extract discharge and water level data from HYDAT
  # Pull out the daily data, define the "water year" based on starting month and # of months
  # and the compute annual values as needed
  
  
trendMData <- reactive({
    if(myOptions$debug){
       cat("trendMData1", unlist(input$trendMstation),"\n")
    }
    station.data <- plyr::ldply(input$trendMstation, function(station_number){
        hy_daily(station_number=station_number)
    })
    
    trend.annual <- calc_annual_stats_rev(station.data, water_year_start=input$trendMAnnualMonthStart,
                                                        months_in_year  =input$trendMAnnualNumMonth)
    trend.annual$Parameter <- car::recode(trend.annual$Parameter,
                                          " 'Water Level'='Level' ")
    # restrict to years of interest
    trend.annual <- trend.annual %>% filter(Year >= input$trendMYears[1] & Year <= input$trendMYears[2])
    
    # rename some of the summary statistics
    trend.annual <- plyr::rename(trend.annual, c("Mean"="MEAN", "Maximum"="MAX", "Minimum"="MIN","Median"="P50"))
    
    # extract the min/max dates of the flow
    trend.min.max <- trend.annual[,c("Year","Parameter","STATION_NUMBER","when.min","when.max")]
    trend.min.max.long <- reshape2::melt(trend.min.max,
                                         id.var=c("Parameter","STATION_NUMBER","Year"),
                                         variable.name="Sum_stat",
                                         value.name="Date")
    trend.min.max.long$Sum_stat <- car::recode(trend.min.max.long$Sum_stat,
                                              " 'when.min'='MAX'; 'when.max'='MIN' ")
    trend.annual$when.min <- NULL
    trend.annual$when.max <- NULL
    
    # convert from wide to long format
    trend.annual.long <- reshape2::melt(trend.annual, 
                                        id.var=c("Parameter","STATION_NUMBER","Year"),
                                        variable.name="Sum_stat",
                                        value.name="Value")
    # fill in missing years
    master <- expand.grid( Parameter=unique(trend.annual$Parameter), 
                           STATION_NUMBER=unique(trend.annual$STATION_NUMBER),
                           Sum_stat=unique(trend.annual.long$Sum_stat),
                           Year=input$trendMYears[1]:input$trendMYears[2], stringsAsFactors=FALSE)
    trend.annual.long <- merge(trend.annual.long, master, all.y=TRUE )
    
    # add date of min max
    trend.annual.long <- merge(trend.annual.long, trend.min.max.long, all.x=TRUE)
    
    # add symbol - not sure where to get this from
    trend.annual.long$Symbol <- " "
    if(myOptions$debug){
       cat("trendMData \n")
       print(head(trend.annual.long))
    }
    trend.annual.long 
  }) # end of trendData

# The fitted values
trendMFits <- reactive({
    plotdata <- trendMData() %>% filter(Parameter==input$trendMParam)
    plotdata <- plotdata    %>% filter(Sum_stat %in% input$trendMStat)
    #plotdata
    fits <- plyr::ddply(plotdata,c("STATION_NUMBER","Parameter","Sum_stat"), function(x){
       res <- trend_stats(x[,c("Year","Value")], take.logs=input$trendMlog)
    })
    fits
})
  
  
  # Extract monthly instanteous peaks of discharge and water level data from HYDAT
  #annInstData <- reactive({
  #  # Extract and format columns for displaying information
  #  annual.instant <- hy_annual_instant_peaks(station_number=values$station)
  #  
  #  annual.instant <- annual.instant %>% 
  #    mutate(Date=as.Date(paste(YEAR,MONTH,DAY,sep="-"),format="%Y-%m-%d"),
  #           Time=paste0(HOUR,":",ifelse(nchar(MINUTE)>1,paste(MINUTE),paste0(0,MINUTE))," ",TIME_ZONE),
  #           DateTime=paste0(Date," at ",Time),
  #           Symbol=replace(Symbol, is.na(Symbol), ""),
  #           Parameter=replace(Parameter, Parameter=="Flow", "Flow"),
  #           Parameter=replace(Parameter, Parameter=="Water Level", "Level"),
  #           Value=round(Value,3)) %>% 
  #    filter(YEAR >= input$trendMYears[1] & YEAR <= input$trendMYears[2])
  #  annual.instant
  #})
  
  # Select the parameter to display
output$trendMParam <- renderUI({
    selectInput("trendMParam",
                label = "Display parameter:",
                choices = as.list(unique(trendMData()$Parameter)))
})
  
  # Select the annual statistics to display
output$trendMStat <- renderUI({
    selectizeInput("trendMStat", 
                   label = "Display annual statistic(s):",
                   choices = c("Mean"="MEAN","Maximum"="MAX","Minimum"="MIN","P90"="P90","P10"="P10","P50"="P50"),
                   selected = c("P50"="P50"),
                   multiple =FALSE)
})
  
output$trendMlog <- renderText({
    input$trendMlog
})
  
  
  # Select stations to analyze
output$trendMAddStation <- renderUI({
    selectizeInput("trendMstation", 
                   label = "Add/remove by station:",
                   choices = stations.list,
                   selected = values$stationM,
                   options = list(placeholder ="type station ID number", maxOptions = 2420 ),
                   multiple =TRUE)
  })

output$trendMAddStationByDistance1 <- renderUI({
  radioButtons( "trendMstationDistanceAction",
              label =paste0("Add/remove by distance from ",input$station),
              c("Wait"   = "trendMstationDistanceWait",
                "Add"    = "trendMstationDistanceAdd",
                "Remove" = "trendMstationDistanceRemove"),
              inline=TRUE)
})
output$trendMAddStationByDistance2 <- renderUI({
  # find the distance of each station to the base station (input$station)
  # and bin them into distance classes
  base.station   <-  tidyhydat::hy_stations(input$station)
  all.bc.station <- tidyhydat::hy_stations(prov_terr_state_loc="BC")
  all.bc.station$distance <- geosphere::distHaversine(base.station  [,c("LONGITUDE","LATITUDE")],
                                                      all.bc.station[,c("LONGITUDE","LATITUDE")])/1000 # distance in km
  all.bc.station$distanceClass <- cut(all.bc.station$distance,
                                      breaks=c(0, 10, 25, 50, 100, Inf))
  n.stations.by.distanceClass <- table(all.bc.station$distanceClass)
  #browser()
  radioButtons( "trendMstationDistanceClass",
              label ="Distance (km] (# stations)",
              choiceNames =paste(c("None",as.character(levels(all.bc.station$distanceClass))),c(0,n.stations.by.distanceClass)),
              choiceValues=c("None",as.character(levels(all.bc.station$distanceClass))))
  #browser()
})

# if you add/delete based on distance we want to update the station list in stationM
observeEvent(input$trendMstationDistanceAction, {
  if(myOptions$debug){
      cat("observeEvent(input$trendMstationDistanceAction)", unlist(input$trendMstationDistanceAction), unlist(input$trendMstationDistanceClass), "\n")
  }
  if(input$trendMstationDistanceClass!="None"){   # do nothing if none is still set}
      isolate({
        base.station   <-  tidyhydat::hy_stations(input$station)
        all.bc.station <- tidyhydat::hy_stations(prov_terr_state_loc="BC")
        all.bc.station$distance <- geosphere::distHaversine(base.station  [,c("LONGITUDE","LATITUDE")],
                                                      all.bc.station[,c("LONGITUDE","LATITUDE")])/1000 # distance in km
        all.bc.station$distanceClass <- cut(all.bc.station$distance,
                                      breaks=c(0, 10, 25, 50, 100, Inf))
        new.station.list <- all.bc.station$STATION_NUMBER[ all.bc.station$distanceClass==input$trendMstationDistanceClass]
        new.station.list <- na.omit(new.station.list)  # remove any stations where distance is not known
        if(myOptions$debug){
           cat("station to add/remove ", unlist(new.station.list), "\n")
        }
        if(input$trendMstationDistanceAction == "trendMstationDistanceAdd"){  # add these stations to the list
           values$stationM <- unique(c(values$stationM, new.station.list))
        }
        if(input$trendMstationDistanceAction == "trendMstationDistanceRemove"){  # add these stations to the list
           values$stationM <- setdiff(values$stationM, new.station.list)
        }
        if(myOptions$debug){
           cat("stationM after add/remove ", unlist(values$stationM), "\n")
        }
      })
  }
  isolate({  # reset the radio buttons
    updateRadioButtons(session, "trendMstationDistanceAction", selected="trendMstationDistanceWait") # reset the radio button
    updateRadioButtons(session, "trendMstationDistanceClass",  selected="None") # reset the radio button
  })
  })

output$trendMmap <- renderLeaflet({
    plot.stations <- stations[ stations$STATION_NUMBER %in% values$stationM,]
    if(myOptions$debug){
      cat("output$trendMap", unlist(values$stationM), nrow(plot.stations), "\n")
      #browser()
    }
    # add the slopes using a diverging color pallate
    # see https://medium.com/inside-machine-learning/center-diverging-colors-on-leaflet-map-515e69d7f81f
    slopes <- trendMFits()
    plot.stations <- merge(plot.stations, slopes[ slopes$method =="LS",])
    mymap <- leaflet(plot.stations) %>% addTiles()%>%
       addCircleMarkers(data= plot.stations[!is.na(plot.stations$slope),], lng = ~LONGITUDE, lat = ~LATITUDE, layerId = ~STATION_NUMBER, 
                       color = ~colorBin('RdBu',slope)(slope), radius = 20, fillOpacity = .9, stroke = FALSE,
                       label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS)) %>%
       addCircleMarkers(data= plot.stations[is.na(plot.stations$slope),], lng = ~LONGITUDE, lat = ~LATITUDE, layerId = ~STATION_NUMBER, 
                       color = "black", radius=1,
                       label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS)) %>%
      addLegend("topright", colorBin('RdBu', plot.stations$slope), values = plot.stations$slope, opacity = 0.9, 
                         title=paste("LS slope",ifelse(input$trendMlog,"\nlog() scale","\noriginal scale"))) %>%
      addCircleMarkers(layerId = "selected",
                       data = plot.stations[plot.stations$STATION_NUMBER %in% values$station,],
                       lng = ~LONGITUDE, lat = ~LATITUDE, 
                       color = "black", radius = 4)
   mymap
})
  
# check that trendMstation always contains at least one station
# never did get this to work properl
#observeEvent(input$trendMstation, {
#   cat("trendM station list ", unlist(input$trendMstation), "\n")
#   if(length(input$trendMstation)==0){
#      input$trendMstationM <- input$stationM)
#   }
#})  
  
  # Select the annual instantaneous peaks to display
  #output$annInstStat <- renderUI({
  #  selectizeInput("annInstStat", 
  #                 label = "Display annual instantaneous peak values:",
  #                 choices = c("Minimum"="MIN","Maximum"="MAX"),
  #                 multiple =TRUE)
  #})
  
  
  
  # Create annual plot title
  output$trendMPlot.title <- renderText({
    paste0("Trend Summary Statistics - ",metaData()[2,2]," (",metaData()[1,2],")")
  })
  
  # Plot the primary y-axis on log-scale if checked
  trendMPlot.y <- reactive({
    if (input$trendMlog) {list(title=ifelse(input$trendMParam== "Flow","Discharge (cms)","Water Level (m)"),type= "log")}
    else {               list(title=ifelse(input$trendMParam== "Flow","Discharge (cms)","Water Level (m)"))}
  })
  
  # Create the trend plot
  output$trendMPlot <- renderPlotly({
    if(myOptions$debug){
      cat("output$trendMPlot", input$trendMParam, "\n")
      print(head(trendMData()))
    }
    plot.data <- trendMData() %>% filter(Parameter==input$trendMParam) %>% mutate(Value=round(Value,3))
    plot.data <- plot.data    %>% filter(Sum_stat ==input$trendMStat)
    if(myOptions$debug){
      print(head(plot.data))
    }
    plot <- plot_ly() %>% 
      layout(xaxis=list(title="Year"),
             yaxis=trendMPlot.y(),
             showlegend = TRUE)
    
    # plot statistics for each station
    plyr::d_ply(plot.data, c("STATION_NUMBER","Parameter","Sum_stat"), function(x){
       plot <<- plot %>% add_trace(data=x,x= ~Year,y=~Value,name=~STATION_NUMBER, mode = 'lines+markers',
                                  hoverinfo= 'text',text=~paste(STATION_NUMBER,": ",Value," ",Symbol),
                                  line=list(color='grey'))
    })

    # add the fitted ls lines if requested
    if(input$trendMShowFits){
       fits <- trendMFits()
       fits <- fits[ fits$method=="LS",]
       if(myOptions$debug){
          print(head(fits))
       }
       plotdata2 <- merge(plot.data, fits, all.x=TRUE)
       plotdata2$fitted <- plotdata2$intercept + plotdata2$slope*plotdata2$Year
       if(input$trendMlog)plotdata2$fitted <- exp(plotdata2$fitted)
       if(myOptions$debug){
         print(head(plotdata2))
       }
       plyr::d_ply(plotdata2, c("STATION_NUMBER","Parameter","Sum_stat"), function(x){
          plot <<- plot %>% add_trace(data=x, x = ~Year, y = ~fitted, mode = "lines", showlegend=FALSE, line=list(color='grey'))
       })
    }
    plot
  })
  
  
  
output$trendMSummaryFit = DT::renderDataTable({
   fits <- trendMFits()
   fits$intercept <- NULL
   fits$slope    <- round(fits$slope,4)
   fits$slope.se <- round(fits$slope.se, 4)
   fits$slope.p.value  <- format.p(fits$slope.p.value)
   #fits$log <- input$trendMlog
   fits <- datatable(fits, colnames=c("Station","Parameter","Statistic","Trend\nmethod","Est\nslope","(SE)","p.value"))
   fits
})
  
  
  #Create and render table output - NOT tested
  output$trendMTable <- DT::renderDataTable(
    trendMData() %>%  select(-STATION_NUMBER) %>%  mutate(Value=round(Value,3),Time=NA) %>% 
      select(Year,Parameter,Sum_stat,Value,Date,Time,Symbol) %>% 
      bind_rows(annInstData() %>% select(YEAR,Parameter,PEAK_CODE,Value,Date,Time,Symbol) %>% 
                  mutate(PEAK_CODE=replace(PEAK_CODE, PEAK_CODE=="MAX", "INST. MAX"),
                         PEAK_CODE=replace(PEAK_CODE, PEAK_CODE=="MIN", "INST. MIN"),
                         Parameter=replace(Parameter, Parameter=="Flow", "Flow"),
                         Parameter=replace(Parameter, Parameter=="Water Level", "Level"),
                         Value=round(Value,3)) %>% 
                  rename("Year"=YEAR,"Sum_stat"=PEAK_CODE)),
    rownames=FALSE,
    selection=list(mode="single"),
    filter = 'top',
    colnames = c('Year', 'Parameter', 'Summary Statistic','Value','On Date','Time','Symbol'),
    extensions = c("Scroller","ColReorder","Buttons"),
    options = list(scrollX = TRUE,
                   scrollY=450,deferRender = TRUE,scroller = TRUE,
                   dom = 'Bfrtip', 
                   colReorder = TRUE,
                   buttons= list(list(extend='colvis',columns=c(0:6))),
                   columnDefs = list(list(className = 'dt-center', targets = 0:4)))
    
  )
  
  
  #Download data buttons - NOT tested
  output$download.trendMData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - trend summary.csv")},
    content = function(file) {
      write.csv(trendMData() %>% select(Station_Number=STATION_NUMBER,Year,Summary_Stat=Sum_stat,
                                        Parameter,Value,Date,Symbol),
                file, row.names = FALSE, na="")
    })  
  # not tested
  output$download.trendMPeakData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - trendM instantaneous peaks.csv")},
    content = function(file) {
      write.csv(annInstData() %>% select(Station_Number=STATION_NUMBER,Year=YEAR,Peak_Code=PEAK_CODE,
                                         Precision_Code=PRECISION_CODE,Month=MONTH,Day=DAY,Hour=HOUR,
                                         Minute=MINUTE,Time_Zone=TIME_ZONE,Parameter,Value,Symbol),
                file, row.names = FALSE, na="")
    })  
  
 