
SPFMutateFixedEventOrHorizon <- function(target.period.SPF) {
  if(nchar(as.character(target.period.SPF)) == 6) {
    return("horizon")
  } else {
    if(nchar(as.character(target.period.SPF)) == 4) {
      return("event")
    } else {
      if(nchar(as.character(target.period.SPF)) == 7) {
        return("horizon")
      } else {
        return(NA)
      }
    }
  }
}

SPFMutateTargetYear <- function(target.period.SPF) {
  
  if(is.na(target.period.SPF == TRUE)) {
    return(NA)
  } else {
    return(substr(target.period.SPF,1,4))
  }
}

SPFMutateTargetQuarter <- function(target.period.SPF) {
  
  if(is.na(target.period.SPF == TRUE)) {
    return(NA)
  } else {
    
    if(substr(target.period.SPF, 5,6) == "Q1" | substr(target.period.SPF, 5,7) == "Mar" | substr(target.period.SPF, 5,7) == "Feb") {
      return(1)
    } else {
      if(substr(target.period.SPF, 5,6) == "Q2" | substr(target.period.SPF, 5,7) == "Jun" | substr(target.period.SPF, 5,7) == "May") {
        return(2)
      } else {
        if(substr(target.period.SPF, 5,6) == "Q3" | substr(target.period.SPF, 5,7) == "Sep" | substr(target.period.SPF, 5,7) == "Aug") {
          return(3)
        } else {
          if(substr(target.period.SPF, 5,6) == "Q4" | substr(target.period.SPF, 5,7) == "Dec"| substr(target.period.SPF, 5,7) == "Nov") {
            return(4)
          } else {
            return(NA)
          }
        }
      }
    }
  }
}

SPFECBMutateQuartersAhead <- function(issued.year, issued.quarter, target.year, target.quarter) {
  if(is.na(target.quarter == TRUE)) {
    return(NA)
  } else {
    if (issued.year == target.year) {
      return(target.quarter - issued.quarter)
    } else {
      if (issued.year < target.year) {
        return((target.year-issued.year)*4-((issued.quarter-target.quarter)%%4))
      } else {
        return(NA)
      }
    }
  }
}

download.file("http://www.ecb.europa.eu/stats/prices/indic/forecast/shared/files/SPF_individual_forecasts_up_to_2015Q4.zip",
              destfile = "/home/onno/open-fp/Submissions/SPF.zip")

unzip(zipfile = "/home/onno/open-fp/Submissions/SPF.zip",
      exdir = "/home/onno/open-fp/Submissions/SPF-ECB/")

do.call(file.remove, list(list.files("/home/onno/open-fp/temp/", full.names = TRUE)))

for(year in c(1999:2018)) {
  for (quarter in c(1:4)) {
    if (file.exists(paste0("/home/onno/open-fp/Submissions/SPF-ECB/",year,"Q", quarter,".csv")) == FALSE) {
      
    } else {
      
      dataSPF <- read_csv(file = paste0("/home/onno/open-fp/Submissions/SPF-ECB/",year,"Q", quarter,".csv"), col_names = FALSE)
      empty.rows <- c(1)
      for (i in 1:as.integer(count(dataSPF))) {
        if(dataSPF[i,] %>% is.na() %>% sum() == length(dataSPF)) {
          empty.rows <- c(empty.rows,i)
        }
      }
      empty.rows <- c(empty.rows, as.integer(count(dataSPF)))
      
      for (i in 1:(as.integer(count(dataSPF))-1)) {
        # when there is no forecaster, there may be a new variable in the data set
        if (is.na(dataSPF[i,2]) == TRUE) {
          
          data.variable <- dataSPF[i:empty.rows[min(which(empty.rows > i))],]
          
          if(grepl(pattern = "INFLATION EXPECTATIONS; YEAR-ON-YEAR CHANGE IN HICP", x = data.variable[1,1]) == TRUE) {
            
            temp.path <- paste0("/home/onno/open-fp/temp/Inflation", year,"Q",quarter, ".csv")
            write_csv(data.variable[-1,], path = temp.path, col_names = FALSE, append = FALSE)
            clean.csv <- cbind(data_frame(variable = "Inflation"), 
                               read_csv(file = temp.path, col_names = TRUE),
                               row.names = NULL)
            clean.csv <- clean.csv[ , ! apply( clean.csv , 2 , function(x) all(is.na(x)) ) ]
            
            clean.csv <- cbind(clean.csv,
                               data_frame(region = "Euro", 
                                          issued.year = year, 
                                          issued.quarter = quarter,
                                          panel = "SPF-ECB"),
                               row.names = NULL)
            
            
            write_csv(clean.csv, path = temp.path, col_names = TRUE, append = FALSE)
          } 
          
          if(grepl(pattern = "GROWTH EXPECTATIONS; YEAR-ON-YEAR CHANGE IN REAL GDP", x = data.variable[1,1]) ) {
            
            temp.path <- paste0("/home/onno/open-fp/temp/GDP", year,"Q",quarter, ".csv")
            write_csv(data.variable[-1,], path = temp.path, col_names = FALSE, append = FALSE)
            clean.csv <- cbind(data_frame(variable = "GDP growth"), 
                               read_csv(file = temp.path, col_names = TRUE),
                               row.names = NULL)
            clean.csv <- clean.csv[ , ! apply( clean.csv , 2 , function(x) all(is.na(x)) ) ]
            
            clean.csv <- cbind(clean.csv,
                               data_frame(region = "Euro", 
                                          issued.year = year, 
                                          issued.quarter = quarter,
                                          panel = "SPF-ECB"),
                               row.names = NULL)
            
            
            write_csv(clean.csv, path = temp.path, col_names = TRUE, append = FALSE)
          }
          
          if(grepl(pattern = "EXPECTED UNEMPLOYMENT RATE; PERCENTAGE OF ", x = data.variable[1,1]) ) {
            
            temp.path <- paste0("/home/onno/open-fp/temp/Unemployment", year,"Q",quarter, ".csv")
            write_csv(data.variable[-1,], temp.path, col_names = FALSE, append = FALSE)
            clean.csv <- cbind(data_frame(variable = "Unemployment"), 
                               read_csv(file = temp.path, col_names = TRUE),
                               row.names = NULL)
            clean.csv <- clean.csv[ , ! apply( clean.csv , 2 , function(x) all(is.na(x)) ) ]
            
            clean.csv <- cbind(clean.csv,
                               data_frame(region = "Euro", 
                                          issued.year = year, 
                                          issued.quarter = quarter,
                                          panel = "SPF-ECB"),
                               row.names = NULL)
            
            
            
            write_csv(clean.csv, path = temp.path, col_names = TRUE, append = FALSE)
          }
        }
      }
    }
    
  }
}
rm(data.variable, dataSPF, clean.csv, quarter, year, temp.path,i, empty.rows)

files <- list.files(file.path("/home/onno/open-fp/temp/"), full.names = TRUE)
forecast.panel.SPF.ECB <- lapply(files, read_csv, col_names = TRUE)
rm(files)

forecast.panel.SPF.ECB <- Reduce(full_join, forecast.panel.SPF.ECB)

forecast.panel.SPF.ECB <- rename(forecast.panel.SPF.ECB, panel.id = FCT_SOURCE) %>%
  filter(!is.na(panel.id)) %>%
  rename(point.forecast = POINT) %>%
  rename(target.period = TARGET_PERIOD)

forecast.panel.SPF.ECB <- forecast.panel.SPF.ECB %>%
  mutate(fixed.event.or.horizon = sapply(forecast.panel.SPF.ECB$target.period,
                                         SPFMutateFixedEventOrHorizon)) %>%
  mutate(target.quarter = as.numeric(sapply(forecast.panel.SPF.ECB$target.period,
                                            SPFMutateTargetQuarter))) %>%
  mutate(target.year = as.numeric(sapply(forecast.panel.SPF.ECB$target.period,
                                         SPFMutateTargetYear)))

forecast.panel.SPF.ECB <- forecast.panel.SPF.ECB %>%
  mutate(quarters.ahead = mapply(FUN = SPFECBMutateQuartersAhead,
                                 forecast.panel.SPF.ECB$issued.year, 
                                 forecast.panel.SPF.ECB$issued.quarter, 
                                 forecast.panel.SPF.ECB$target.year,
                                 forecast.panel.SPF.ECB$target.quarter))

forecast.panel.SPF.ECB <- forecast.panel.SPF.ECB %>%
  mutate(target.period = ifelse(
    is.na(forecast.panel.SPF.ECB$target.year),
    NA,
    ifelse(is.na(forecast.panel.SPF.ECB$target.quarter),
           forecast.panel.SPF.ECB$target.year,
           paste0(forecast.panel.SPF.ECB$target.year,
                  "Q",
                  forecast.panel.SPF.ECB$target.quarter)))) %>%
  select(panel, panel.id, variable, region, point.forecast, fixed.event.or.horizon,
         issued.year, issued.quarter, target.period, quarters.ahead, target.year, target.quarter)

write_csv(forecast.panel.SPF.ECB, path = "/home/onno/open-fp/Submissions/SPF-ECB.csv", col_names = TRUE, append = FALSE)
