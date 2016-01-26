setwd("/home/onno/open-fp/")
library("readr")
library("haven")
library("dplyr")
library("foreach")
library("RCurl")
library("readxl")
library("tidyr")
library("foreign")
#library("doParallel")

source("update_SPF_ECB.R")

SPFUSMutateTargetYear <- function(issued.year, issued.quarter, quarters.ahead) {
  new.quarter <- issued.quarter + quarters.ahead
  if((new.quarter)  <= 0) {
    return(issued.year - 1)
  }
  if((new.quarter)  <= 4) {
    return(issued.year)
  }
  if((new.quarter <= 8 && new.quarter >4)) {
    return(issued.year + 1)
  }
}

for (i in c(1:5)) {
  download.file(paste0("https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/historical-data/micro",i,".xls"),
                method = "curl",
                extra = "--insecure",
                destfile = paste0("/home/onno/open-fp/Submissions/SPF-US/micro",i,".xls"))
}

x <- foreach(i = 1:5) %do% {
  
  SPFdataUS <- read_excel(path = paste0("/home/onno/open-fp/Submissions/SPF-US/micro",i,".xls"), 
                          sheet = "RGDP", 
                          col_types = rep("numeric", times = 14), 
                          col_names = TRUE) %>%
    rename(panel.id = ID, issued.year = YEAR, issued.quarter = QUARTER)
  
  tidy.SPFdataUS.fixed.event.forecasts <- SPFdataUS %>%
    select(issued.year, issued.quarter, panel.id, RGDPA, RGDPB) %>%
    gather(key = years.ahead, value = point.forecast, RGDPA, RGDPB, convert = TRUE) %>%
    mutate(years.ahead = replace(years.ahead, years.ahead == "RGDPA", 0)) %>%
    mutate(years.ahead = replace(years.ahead, years.ahead == "RGDPB", 1))
  
  tidy.SPFdataUS.fixed.event.forecasts <- tidy.SPFdataUS.fixed.event.forecasts %>%
    mutate(years.ahead = as.numeric(tidy.SPFdataUS.fixed.event.forecasts$years.ahead)) %>%
    cbind(data_frame(fixed.event.or.horizon = "event",
                     variable = "RGDP",
                     panel = "SPF-US",
                     region = "US"),
          row.names = NULL) %>%
    mutate(target.year = issued.year + years.ahead)
  
  tidy.SPFdataUS.fixed.horizon.forecasts <- SPFdataUS %>%
    select(issued.year, issued.quarter, panel.id, RGDP1, RGDP2, RGDP3, RGDP4, RGDP5, RGDP6) %>%
    gather(key = quarters.ahead, value = point.forecast, RGDP1, RGDP2, RGDP3, RGDP4, RGDP5, RGDP6, convert = TRUE) %>%
    mutate(quarters.ahead = replace(quarters.ahead, quarters.ahead == "RGDP1", -1)) %>%
    mutate(quarters.ahead = replace(quarters.ahead, quarters.ahead == "RGDP2", 0)) %>%
    mutate(quarters.ahead = replace(quarters.ahead, quarters.ahead == "RGDP3", 1)) %>%
    mutate(quarters.ahead = replace(quarters.ahead, quarters.ahead == "RGDP4", 2)) %>%
    mutate(quarters.ahead = replace(quarters.ahead, quarters.ahead == "RGDP5", 3)) %>%
    mutate(quarters.ahead = replace(quarters.ahead, quarters.ahead == "RGDP6", 4))
  
  tidy.SPFdataUS.fixed.horizon.forecasts <- tidy.SPFdataUS.fixed.horizon.forecasts %>%
    mutate(quarters.ahead = as.numeric(tidy.SPFdataUS.fixed.horizon.forecasts$quarters.ahead)) %>%
    cbind(data_frame(fixed.event.or.horizon = "horizon",
                     variable = "RGDP",
                     panel = "SPF-US",
                     region = "US"),
          row.names = NULL) %>%
    mutate(target.quarter = (issued.quarter + quarters.ahead-1)%%4+1) %>%
    filter(quarters.ahead != -1)
  
  tidy.SPFdataUS.fixed.horizon.forecasts <- tidy.SPFdataUS.fixed.horizon.forecasts %>%
    mutate(target.year = mapply(FUN = SPFUSMutateTargetYear,
                                tidy.SPFdataUS.fixed.horizon.forecasts$issued.year,
                                     tidy.SPFdataUS.fixed.horizon.forecasts$issued.quarter,
                                     tidy.SPFdataUS.fixed.horizon.forecasts$quarters.ahead)
                                )
  
  
  SPF.data.US.joined <- full_join(tidy.SPFdataUS.fixed.horizon.forecasts, 
                                  tidy.SPFdataUS.fixed.event.forecasts)
}

forecast.panel.SPF.US <- Reduce(full_join, x) %>% filter(!is.na(panel.id))

all.joined <- full_join( forecast.panel.SPF.ECB, forecast.panel.SPF.US)

all.joined.reduced <- all.joined %>% select( panel, panel.id, variable, region, point.forecast, fixed.event.or.horizon, 
                       issued.year, issued.quarter, years.ahead, quarters.ahead, target.year, target.quarter)

forecast.panel.frontpage <- all.joined.reduced %>%
  select(issued.year, issued.quarter, fixed.event.or.horizon, target.year, point.forecast, region, variable) %>%
  filter(fixed.event.or.horizon == "event") %>%
  filter(issued.year == 2016) %>%
  filter(issued.quarter == 1) %>%
  filter(target.year == 2015 | target.year == 2016 | target.year == 2017 | target.year == 2020) %>%
  filter(is.na(point.forecast) == FALSE)

write_rds(forecast.panel.frontpage, path = "/home/onno/open-fp/forecast_panel_frontpage.rds")

forecast.panel <- all.joined.reduced

write_rds(forecast.panel, path = "/home/onno/open-fp/forecast.panel.rds")

library(readr)
library(foreign)

forecast.panel <- read_rds(path = "/home/onno/open-fp/forecast.panel.rds")

# Create various data formats
write_csv(forecast.panel, path = "/var/www/html/data/forecast.panel.csv")
write_rds(forecast.panel, path = "/var/www/html/data/forecast.panel.rds")
write.dta(forecast.panel, file = "/var/www/html/data/forecast.panel.dta")

write_csv(forecast.panel, path = "/home/onno/open-fp/open-fp/data/forecast.panel.csv")
write_rds(forecast.panel, path = "/home/onno/open-fp/open-fp/data/forecast.panel.rds")
write.dta(forecast.panel, file = "/home/onno/open-fp/open-fp/data/forecast.panel.dta")
