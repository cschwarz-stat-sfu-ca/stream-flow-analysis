# various computation functions for this project

# define a simple formatter for p.values
# see https://stackoverflow.com/questions/8442322/the-use-of-format-pval-in-r-and-with-sexpr-in-sweave
format.p <- function(p, precision=0.001) {
  if(is.null(p))return(" ")
  digits <- -log(precision, base=10)
  p <- ifelse(is.na(p),"NA",formatC(p, format='f', digits=digits))
  p[p == formatC(0, format='f', digits=digits)] <- paste0('<', precision)
  p
}


calc_annual_stats_rev <- function(data, water_year_start=1, months_in_year=12){
    water_year_start <- as.numeric(water_year_start)
    months_in_year   <- as.numeric(months_in_year)
    #if(is.null(data))return(NULL)
    #if(nrow(data))return(NULL)
    if(myOptions$debug){
      cat("calc_annual_stats_rev ", nrow(data), water_year_start,months_in_year, " \n")
      print(head(data))
    }
    # a temporary fix until the fsstr package is updated
    data$Month <- lubridate::month(data$Date)
    good.months <- (water_year_start + (0:(months_in_year-1))) 
    good.months <- ifelse(good.months>12, good.months-12, good.months)
    data <- data[ data$Month %in% good.months,]
    data <- data [ !is.na(data$Value),]
    #data <- data[ data$Parameter=="Flow",]
    #browser()
    res <- plyr::ddply(data, c("STATION_NUMBER","Parameter"), function(x){
       fasstr::calc_annual_stats(x, water_year_start=water_year_start, ignore_missing=TRUE)
    })
    
    # find the date of the min/max parameter values in each water year
    data$water.year <- lubridate::year(data$Date)
    #browser()
    data$water.year <- data$water.year - ifelse(data$Month < water_year_start, 1, 0)
    when.min.max <- plyr::ddply(data, c("Parameter","water.year"), plyr::summarize,
                                when.min = Date[which.min(Value)],
                                when.max = Date[which.max(Value)])
    res <- merge(res, when.min.max, by.x=c("Year","Parameter"), by.y=c("water.year","Parameter"))
    res
}

#station.id <- "08NM116"
#
#daily <- hy_daily(station_number=station.id)
#head(daily)
#
#res1 <-calc_annual_stats_rev(daily, water_year_start=1, months_in_year=12)
#tail(res1[,1:10])
#res1 <-calc_annual_stats_rev(daily, water_year_start=10, months_in_year=4)
#tail(res1)


trend_stats<- function(data, take.logs=TRUE){
  # find trend statistics for each parameter in data
  # data should be a data.frame in long format with columns
  #     Year (could be fractional if data is collected at a finer scale)
  #     Value
  # if take.logs then log(Value) is trended to give % change/year
  #
  if(sum(is.finite(data$Value))<myOptions$trendM.min.n){
     if(myOptions$debug){
       cat("Minimum sample size for trend not here",sum(!is.na(data$Value)), "\n" )
       print(head(data))
     }
     res <-data.frame(method=c("LS","MK"), 
             intercept=NA, 
             slope=NA, 
             slope.se=NA,
             slope.p.value=NA)
     return(res)
  }
  offset <- min(data$Value[ data$Value > 0 & is.finite(data$Value)], na.rm=TRUE)*0.5
  if(!is.finite(offset))offset<- 0
  if(take.logs)data$Value <- log(data$Value+offset)
  data <- data[ complete.cases(data[,c("Year","Value")]),]
  fit <- lm(Value ~ Year, data=data)
  
  # do the MannKendall test. We need to sort the data and drop any missing values.
  data <- data[ order(data$Year),]
  mktest <- trend::sens.slope(na.omit(data$Value))
  #browser()
  data.frame(method=c("LS","MK"), 
             intercept=c(coef(fit)[1],NA), 
             slope=c(coef(fit)[2],mktest$estimates), 
             slope.se=c(sqrt(diag(vcov(fit)))[2],sum(mktest$conf.int*c(-1,1))/qnorm(.975)/2),
             slope.p.value=c(summary(fit)$coefficients[2,4], mk.p.value=mktest$p.value)
  )
  
}

station.id <- "08NM116"
yearly <- hy_annual_stats(station_number=station.id)
#trend_stats( yearly[yearly$Parameter=="Flow" & yearly$Sum_stat=="MEAN",c("Year","Value")])
#trend_stats( yearly[yearly$Parameter=="Flow" & yearly$Sum_stat=="MEAN",c("Year","Value")], take.logs=FALSE)



# This function doesn't work because it get upset in trying to compute monthly values outside of period - bummer.
calc_all_annual_stats_rev <- function(data, water_year_start=1, months_in_year=12){
    if(is.null(data))return(NULL)
    # a temporary fix until the fsstr package is updated
    data$Month <- lubridate::month(data$Date)
    good.months <- (water_year_start + (0:(months_in_year-1))) 
    good.months <- ifelse(good.months>12, good.months-12, good.months)
    data <- data[ data$Month %in% good.months,]
    data <- data[ data$Parameter=="Flow",]
    data <- data [ !is.na(data$Value),]
    #browser()
    res <- fasstr::calc_all_annual_stats(data, water_year_start=water_year_start, monthly_percentiles=NA, ignore_missing=TRUE)
}

#station.id <- "08NM116"
#
#daily <- hy_daily(station_number=station.id)
#head(daily)
#
#res1 <-calc_all_annual_stats_rev(daily, water_year_start=1, months_in_year=12)
#tail(res1[,1:10])
#res1 <-calc_all_annual_stats_rev(daily, water_year_start=10, months_in_year=4)
#tail(res1[,1:10])

