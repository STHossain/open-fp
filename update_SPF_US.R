
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

for (i in c(5)) {
  download.file(paste0("https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/historical-data/micro",i,".xls"),
                method = "curl",
                extra = "--insecure",
                destfile = paste0("Submissions/SPF-US/micro",i,".xls"))
}

x <- foreach(i = 1:5) %do% {
  
  SPF.df <- foreach(variable = c("RGDP", "PGDP", "UNEMP","HOUSING","CPI")) %do% {
    SPFdataUS <- read_excel(path = paste0("Submissions/SPF-US/micro",i,".xls"), 
                            sheet = paste0(variable),
                            col_names = TRUE) %>%
      rename(panel.id = ID, issued.year = YEAR, issued.quarter = QUARTER)
    
    
    
    tidy.SPFdataUS.fixed.event.forecasts <- SPFdataUS %>%
      select(issued.year, issued.quarter, panel.id, matches(paste0(variable,"A")), matches(paste0(variable,"B"))) %>%
      gather(key = years.ahead, value = point.forecast, matches(paste0(variable,"A")), matches(paste0(variable,"B")), convert = TRUE) %>%
      mutate(years.ahead = replace(years.ahead, years.ahead == paste0(variable,"A"), 0)) %>%
      mutate(years.ahead = replace(years.ahead, years.ahead == paste0(variable,"B"), 1)) %>%
      mutate(point.forecast = as.numeric(point.forecast))
    
    tidy.SPFdataUS.fixed.horizon.forecasts <- SPFdataUS %>%
      select(issued.year, issued.quarter, panel.id, 
             matches(paste0(variable,"1")), matches(paste0(variable,"2")), matches(paste0(variable,"3")), 
             matches(paste0(variable,"4")), matches(paste0(variable,"5")), matches(paste0(variable,"6"))) %>%
      gather(key = quarters.ahead, 
             value = point.forecast, 
             matches(paste0(variable,"1")), matches(paste0(variable,"2")), matches(paste0(variable,"3")), 
             matches(paste0(variable,"4")), matches(paste0(variable,"5")), matches(paste0(variable,"6")), 
             convert = TRUE) %>%
      mutate(quarters.ahead = replace(quarters.ahead, quarters.ahead == paste0(variable,"1"), -1)) %>%
      mutate(quarters.ahead = replace(quarters.ahead, quarters.ahead == paste0(variable,"2"), 0)) %>%
      mutate(quarters.ahead = replace(quarters.ahead, quarters.ahead == paste0(variable,"3"), 1)) %>%
      mutate(quarters.ahead = replace(quarters.ahead, quarters.ahead == paste0(variable,"4"), 2)) %>%
      mutate(quarters.ahead = replace(quarters.ahead, quarters.ahead == paste0(variable,"5"), 3)) %>%
      mutate(quarters.ahead = replace(quarters.ahead, quarters.ahead == paste0(variable,"6"), 4))
    
    # rename variables
    
    if (variable == "RGDP") {
      
    }
    
    if (variable == "UNEMP") {
      variable <- "Unemployment"
    }
    
    if (variable == "HOUSING") {
      variable <- "housing.starts"
    }
    
    if (variable == "CPI") {
      variable <- "Inflation"
    }
    
    tidy.SPFdataUS.fixed.event.forecasts <- tidy.SPFdataUS.fixed.event.forecasts %>%
      mutate(years.ahead = as.numeric(tidy.SPFdataUS.fixed.event.forecasts$years.ahead)) %>%
      cbind(data_frame(fixed.event.or.horizon = "event",
                       variable = variable,
                       panel = "SPF-US",
                       region = "US"),
            row.names = NULL) %>%
      mutate(target.year = issued.year + years.ahead)
    
    tidy.SPFdataUS.fixed.horizon.forecasts <- tidy.SPFdataUS.fixed.horizon.forecasts %>%
      mutate(quarters.ahead = as.numeric(tidy.SPFdataUS.fixed.horizon.forecasts$quarters.ahead)) %>%
      cbind(data_frame(fixed.event.or.horizon = "horizon",
                       variable = variable,
                       panel = "SPF-US",
                       region = "US"),
            row.names = NULL) %>%
      mutate(target.quarter = (issued.quarter + quarters.ahead-1)%%4+1) %>%
      filter(quarters.ahead != -1) %>%
      mutate(point.forecast = as.numeric(point.forecast))
    
    tidy.SPFdataUS.fixed.horizon.forecasts <- tidy.SPFdataUS.fixed.horizon.forecasts %>%
      mutate(target.year = mapply(FUN = SPFUSMutateTargetYear,
                                  tidy.SPFdataUS.fixed.horizon.forecasts$issued.year,
                                  tidy.SPFdataUS.fixed.horizon.forecasts$issued.quarter,
                                  tidy.SPFdataUS.fixed.horizon.forecasts$quarters.ahead)
      )
    
    
    SPF.data.US.joined <- full_join(tidy.SPFdataUS.fixed.horizon.forecasts, 
                                    tidy.SPFdataUS.fixed.event.forecasts)
  }
  Reduce(full_join, SPF.df)
}

rm(tidy.SPFdataUS.fixed.event.forecasts, tidy.SPFdataUS.fixed.horizon.forecasts, SPFdataUS, SPF.data.US.joined, variable, i, SPF.df)
forecast.panel.SPF.US <- Reduce(full_join, x) %>% 
  filter(!is.na(panel.id)) %>% 
  filter(!is.na(point.forecast)) %>%
  mutate(region = "US")
rm(x)

forecast.panel.SPF.US <- forecast.panel.SPF.US %>%
  mutate(issued.period = ifelse(
    is.na(forecast.panel.SPF.US$target.year),
    NA,
    ifelse(is.na(forecast.panel.SPF.US$issued.quarter),
           forecast.panel.SPF.US$issued.year,
           paste0(forecast.panel.SPF.US$issued.year,
                  "Q",
                  forecast.panel.SPF.US$issued.quarter))))  


forecast.panel.SPF.US <- forecast.panel.SPF.US %>%
  mutate(target.period = ifelse(
    is.na(forecast.panel.SPF.US$target.year),
    NA,
    ifelse(is.na(forecast.panel.SPF.US$target.quarter),
           forecast.panel.SPF.US$target.year,
           paste0(forecast.panel.SPF.US$target.year,
                  "Q",
                  forecast.panel.SPF.US$target.quarter))))