### Trend computation 
# This will take the various input, select the data from the HYDAT database, compute trends
# and plot everything


  ###################################################################################
  
  # Extract discharge and water level data from HYDAT
  # Pull out the daily data, define the "water year" based on starting month and # of months
  # and the compute annual values as needed
  
  
trendData <- reactive({
    
    station.data <- hy_daily(station_number=values$station)
    
    trend.annual <- calc_annual_stats_rev(station.data, water_year_start=input$trendAnnualMonthStart,
                                                        months_in_year  =input$trendAnnualNumMonth)
    trend.annual$Parameter <- car::recode(trend.annual$Parameter,
                                          " 'Water Level'='Level' ")
    # restrict to years of interest
    trend.annual <- trend.annual %>% filter(Year >= input$trendYears[1] & Year <= input$trendYears[2])
    
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
                           Year=input$trendYears[1]:input$trendYears[2], stringsAsFactors=FALSE)
    trend.annual.long <- merge(trend.annual.long, master, all.y=TRUE )
    
    # add date of min max
    trend.annual.long <- merge(trend.annual.long, trend.min.max.long, all.x=TRUE)
    
    # add symbol - not sure where to get this from
    trend.annual.long$Symbol <- " "
    if(myOptions$debug){
       cat("trendData \n")
       print(head(trend.annual.long))
    }
    trend.annual.long 
  }) # end of trendData

# The fitted values
trendFits <- reactive({
    plotdata <- trendData() %>% filter(Parameter==input$trendParam)
    plotdata <- plotdata    %>% filter(Sum_stat %in% input$trendStat)
    #plotdata
    fits <- plyr::ddply(plotdata,c("STATION_NUMBER","Parameter","Sum_stat"), function(x){
       res <- trend_stats(x[,c("Year","Value")], take.logs=input$trendlog)
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
  #    filter(YEAR >= input$trendYears[1] & YEAR <= input$trendYears[2])
  #  annual.instant
  #})
  
  # Select the parameter to display
  output$trendParam <- renderUI({
    selectInput("trendParam",
                label = "Display parameter:",
                choices = as.list(unique(trendData()$Parameter)))
  })
  
  # Select the annual statistics to display
  output$trendStat <- renderUI({
    selectizeInput("trendStat", 
                   label = "Display annual statistic(s):",
                   choices = c("Mean"="MEAN","Maximum"="MAX","Minimum"="MIN","P90"="P90","P10"="P10","P50"="P50"),
                   selected = c("Mean"="MEAN","P90"="P90","P10"="P10"),
                   multiple =TRUE)
  })
  
  output$trendlog <- renderText({
    input$trendlog
  })
  
  
  
  
  
  # Select the annual instantaneous peaks to display
  #output$annInstStat <- renderUI({
  #  selectizeInput("annInstStat", 
  #                 label = "Display annual instantaneous peak values:",
  #                 choices = c("Minimum"="MIN","Maximum"="MAX"),
  #                 multiple =TRUE)
  #})
  
  
  
  # Create annual plot title
  output$trendPlot.title <- renderText({
    paste0("Trend Summary Statistics - ",metaData()[2,2]," (",metaData()[1,2],")")
  })
  
  # Plot the primary y-axis on log-scale if checked
  trendPlot.y <- reactive({
    if (input$trendlog) {list(title=ifelse(input$trendParam== "Flow","Discharge (cms)","Water Level (m)"),type= "log")}
    else {               list(title=ifelse(input$trendParam== "Flow","Discharge (cms)","Water Level (m)"))}
  })
  
  # Create the trend plot
  output$trendPlot <- renderPlotly({
    if(myOptions$debug){
      cat("output$trendPlot", input$trendParam, "\n")
      print(head(trendData()))
    }
    plot.data <- trendData() %>% filter(Parameter==input$trendParam) %>% mutate(Value=round(Value,3))
    if(myOptions$debug){
       print(head(plot.data))
    }
    plot <- plot_ly() %>% 
      layout(xaxis=list(title="Year"),
             yaxis=trendPlot.y(),
             showlegend = TRUE)
    
    # Add each annual statistic if selected
    if ("MAX" %in% input$trendStat){
      plot <- plot %>%  add_trace(data=plot.data %>% filter(Sum_stat=="MAX"),
                                  x= ~Year,y=~Value,name="Daily Maximum",mode = 'lines+markers',
                                  hoverinfo= 'text',text=~paste("Maximum: ",Value,
                                                                "\nOn",Date," ",Symbol),
                                  line=list(color='rgba(1,102,94, 1)'))}
    if ("MEAN" %in% input$trendStat){
      plot <- plot %>%  add_trace(data=plot.data %>% filter(Sum_stat=="MEAN"),
                                  x= ~Year,y=~Value,name="Daily Mean", mode = 'lines+markers',
                                  hoverinfo= 'text',text=~paste("Mean: ",Value),
                                  line=list(color='rgba(61,151,53, 1)'))}
    if ("MIN" %in% input$trendStat){
      plot <- plot %>%  add_trace(data=plot.data %>% filter(Sum_stat=="MIN"),
                                  x= ~Year,y=~Value,name="Daily Minimum",
                                  mode = 'lines+markers',
                                  hoverinfo= 'text',text=~paste("Minimum: ",Value,
                                                                "\nOn",Date," ",Symbol),
                                  line=list(color='rgba(140,81,10, 1)'))}
    if ("P90" %in% input$trendStat){
      plot <- plot %>%  add_trace(data=plot.data %>% filter(Sum_stat=="P90"),
                                  x= ~Year,y=~Value,name="Daily P90",mode = 'lines+markers',
                                  hoverinfo= 'text',text=~paste("P90: ",Value),
                                  line=list(color='rgba(1,102,94, 1)'))}
    if ("P10" %in% input$trendStat){
      plot <- plot %>%  add_trace(data=plot.data %>% filter(Sum_stat=="P10"),
                                  x= ~Year,y=~Value,name="Daily P10",
                                  mode = 'lines+markers',
                                  hoverinfo= 'text',text=~paste("P10: ",Value),
                                  line=list(color='rgba(140,81,10, 1)'))}
    if ("P50" %in% input$trendStat){
      plot <- plot %>%  add_trace(data=plot.data %>% filter(Sum_stat=="P50"),
                                  x= ~Year,y=~Value,name="Daily P50", mode = 'lines+markers',
                                  hoverinfo= 'text',text=~paste("P50: ",Value),
                                  line=list(color='rgba(61,151,53, 1)'))}
    
    # add the fitted ls lines if requested
    if(input$trendShowFits){
       fits <- trendFits()
       fits <- fits[ fits$method=="LS",]
       if(myOptions$debug){
          print(head(fits))
       }
       plotdata2 <- merge(plot.data, fits, all.x=TRUE)
       plotdata2$fitted <- plotdata2$intercept + plotdata2$slope*plotdata2$Year
       if(input$trendlog)plotdata2$fitted <- exp(plotdata2$fitted)
       if(myOptions$debug){
          print(head(plotdata2))
       }
       plyr::d_ply(plotdata2, c("STATION_NUMBER","Parameter","Sum_stat"), function(x){
          plot <<- plot %>% add_trace(data=x, x = ~Year, y = ~fitted, mode = "lines", showlegend=FALSE, line=list(color='grey'))
       })
    }
    plot
  })
  
  
  
output$trendSummaryFit = DT::renderDataTable({
   fits <- trendFits()
   fits$intercept <- NULL
   fits$slope    <- round(fits$slope,4)
   fits$slope.se <- round(fits$slope.se, 4)
   fits$slope.p.value  <- format.p(fits$slope.p.value)
   #fits$log <- input$trendlog
   fits <- datatable(fits, colnames=c("Station","Parameter","Statistic","Trend\nmethod","Est\nslope","(SE)","p.value"))
   fits
})
  
  
  #Create and render table output - NOT tested
  output$trendTable <- DT::renderDataTable(
    trendData() %>%  select(-STATION_NUMBER) %>%  mutate(Value=round(Value,3),Time=NA) %>% 
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
  output$download.trendData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - trend summary.csv")},
    content = function(file) {
      write.csv(trendData() %>% select(Station_Number=STATION_NUMBER,Year,Summary_Stat=Sum_stat,
                                        Parameter,Value,Date,Symbol),
                file, row.names = FALSE, na="")
    })  
  # not tested
  output$download.trendPeakData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - trend instantaneous peaks.csv")},
    content = function(file) {
      write.csv(annInstData() %>% select(Station_Number=STATION_NUMBER,Year=YEAR,Peak_Code=PEAK_CODE,
                                         Precision_Code=PRECISION_CODE,Month=MONTH,Day=DAY,Hour=HOUR,
                                         Minute=MINUTE,Time_Zone=TIME_ZONE,Parameter,Value,Symbol),
                file, row.names = FALSE, na="")
    })  
  
 