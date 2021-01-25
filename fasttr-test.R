

# Test out fasstr packages with water year start and end
library(tidyhydat)
library(fasstr)
library(plyr)


station.id <- "08NM116"

station.info <- hy_stations(station.id)
station.info


# get the annual values directly
annual_hy <- hy_annual_stats(station_number=station.id)
tail(annual_hy, n=10)



daily <- hy_daily(station_number=station.id)
daily$Year <- lubridate::year(daily$Date)
head(daily)
annual_from_daily <- plyr::ddply(daily, c("Year","Parameter"), plyr::summarize,
                                mean=mean(Value, na.rm=TRUE),
                                min =min (Value, na.rm=TRUE),
                                max =max (Value, na.rm=TRUE))
tail(annual_from_daily)


flows <- fasstr::calc_annual_stats(station_number=station.id, ignore_missing=TRUE)
tail(flows)


flows <- fasstr::calc_all_annual_stats(station_number=station.id)
tail(flows[,1:10])

flows2 <- fasstr::calc_annual_stats(daily, ignore_missing=TRUE)
tail(flows)


flows3 <- fasstr::calc_annual_stats(daily)
tail(flows)



### See how water year paramerer works
flows <- fasstr::calc_annual_stats(station_number=station.id, water_year_start=4,ignore_missing=TRUE)
tail(flows)
