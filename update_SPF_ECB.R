
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

SPFECBRenameBins <- function(panel) {
  
  # fix bins strictly positive number to infinity 
  foreach( i = 1:20, .errorhandling='remove') %do% {
    renamed.columns.expression <- paste0("panel <- dplyr::rename(panel, F", i,"_0T", i, "_4 = F",i,"_0)")
    eval(parse(text = renamed.columns.expression))
  }
  foreach( i = 1:20, .errorhandling='remove') %do% {
    renamed.columns.expression <- paste0("panel <- dplyr::rename(panel, F", i,"_5T", i, "_9 = F",i,"_5)")
    eval(parse(text = renamed.columns.expression))
  }
  
  
  # fix bins negative infinity to strictly positive number
  foreach( i = 1:20, .errorhandling='remove') %do% {
    renamed.columns.expression <- paste0("panel <- dplyr::rename(panel, F", i-1,"_5T", i-1, "_9 = T",i,"_0)")
    eval(parse(text = renamed.columns.expression))
  }
  foreach( i = 1:20, .errorhandling='remove') %do% {
    renamed.columns.expression <- paste0("panel <- dplyr::rename(panel, F", i,"_0T", i, "_4 = T",i,"_5)")
    eval(parse(text = renamed.columns.expression))
  }
  
  # foreach loop just for error handling
  foreach( i = 1, .errorhandling='remove') %do% {
    # fix negative infinity to 0 
    renamed.columns.expression <- paste0("panel <- dplyr::rename(panel, FN0_5TN0_1 = T0_0)")
    eval(parse(text = renamed.columns.expression))
  }
  
  foreach( i = 1, .errorhandling = 'remove') %do% {
    # fix 0 to positive infinity
    renamed.columns.expression <- paste0("panel <- dplyr::rename(panel, F0_0T0_5 = F0_0)")
    eval(parse(text = renamed.columns.expression))
  }
  
  foreach( i = 1, .errorhandling = 'remove') %do% {
    # fix 0.5 to positive infinity
    renamed.columns.expression <- paste0("panel <- dplyr::rename(panel, F0_5T0_9 = F0_5)")
    eval(parse(text = renamed.columns.expression))
  }
  
  foreach( i = 1, .errorhandling = 'remove') %do% {
    # fix negative infinity to 0.5
    renamed.columns.expression <- paste0("panel <- dplyr::rename(panel, F0_0T0_4 = T0_5)")
    eval(parse(text = renamed.columns.expression))
  }
  
  foreach( i = 1, .errorhandling = 'remove') %do% {
    # fix -0.5 to positive infinity
    renamed.columns.expression <- paste0("panel <- dplyr::rename(panel, FN0_5TN0_1 = FN0_5)")
    eval(parse(text = renamed.columns.expression))
  }
  
  foreach( i = 1, .errorhandling = 'remove') %do% {
    # fix negative infinity to -0.5
    renamed.columns.expression <- paste0("panel <- dplyr::rename(panel, FN1_0TN0_6 = TN0_5)")
    eval(parse(text = renamed.columns.expression))
  }
  
  
  
  # fix bins negative infinity to strictly negative number
  foreach( i = 1:20, .errorhandling='remove') %do% {
    # (-inf,-4) -> (-4.5 , -4.1)
    renamed.columns.expression <- paste0("panel <- dplyr::rename(panel, FN", i,"_5TN", i, "_1 = TN",i,"_0)")
    eval(parse(text = renamed.columns.expression))
  }
  
  foreach( i = 1:20, .errorhandling='remove') %do% {
    # (-inf,-4.5) -> (-5.0 , -4.6)
    renamed.columns.expression <- paste0("panel <- dplyr::rename(panel, FN", i+1,"_0TN", i, "_6 = TN",i,"_5)")
    eval(parse(text = renamed.columns.expression))
  }
  
  # fix bins strictly negative number to infinity 
  foreach( i = 1:20, .errorhandling='remove') %do% {
    # (-4,inf) -> (-4,-3.6)
    renamed.columns.expression <- paste0("panel <- dplyr::rename(panel, FN", i,"_0Tn", i-1, "_4 = FN",i,"_0)")
    eval(parse(text = renamed.columns.expression))
  }
  
  foreach( i = 1:20, .errorhandling='remove') %do% {
    # (-4.5,inf) -> (-4.5,-4.1)
    renamed.columns.expression <- paste0("panel <- dplyr::rename(panel, F", i,"_5T", i, "_1 = FN",i,"_5)")
    eval(parse(text = renamed.columns.expression))
  }
  
  return(panel)
}

#x <- SPFECBRenameBins(panel = clean.csv)

download.file("http://www.ecb.europa.eu/stats/prices/indic/forecast/shared/files/SPF_individual_forecasts.zip",
              destfile = "Submissions/SPF.zip")

unzip(zipfile = "Submissions/SPF.zip",
      exdir = "Submissions/SPF-ECB/")

do.call(file.remove, list(list.files("temp/", full.names = TRUE)))

# random comment here

for(year in c(1999:2018)) {
  for (quarter in c(1:4)) {
    if (file.exists(paste0("Submissions/SPF-ECB/",year,"Q", quarter,".csv")) == FALSE) {
      
    } else {
      
      dataSPF <- read_csv(file = paste0("Submissions/SPF-ECB/",year,"Q", quarter,".csv"), col_names = FALSE)
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
            
            temp.path <- paste0("temp/Inflation", year,"Q",quarter, ".csv")
            write_csv(data.variable[-1,], path = temp.path, col_names = FALSE, append = FALSE)
            clean.csv <- cbind(data_frame(variable = "Inflation"), 
                               read_csv(file = temp.path, col_names = TRUE),
                               row.names = NULL)
            clean.csv <- clean.csv[ , ! apply( clean.csv , 2 , function(x) all(is.na(x)) ) ]
            
            clean.csv <- cbind(clean.csv,
                               data_frame(region = "Euro", 
                                          issued.year = year, 
                                          issued.quarter = quarter,
                                          panel = "SPF-ECB",
                                          issued.period = paste0(year,"Q",quarter)),
                               row.names = NULL)
            clean.csv <- SPFECBRenameBins(panel = clean.csv)
            
            write_csv(clean.csv, path = temp.path, col_names = TRUE, append = FALSE)
          } 
          
          if(grepl(pattern = "GROWTH EXPECTATIONS; YEAR-ON-YEAR CHANGE IN REAL GDP", x = data.variable[1,1]) ) {
            
            temp.path <- paste0("temp/GDP", year,"Q",quarter, ".csv")
            write_csv(data.variable[-1,], path = temp.path, col_names = FALSE, append = FALSE)
            clean.csv <- cbind(data_frame(variable = "GDP growth"), 
                               read_csv(file = temp.path, col_names = TRUE),
                               row.names = NULL)
            clean.csv <- clean.csv[ , ! apply( clean.csv , 2 , function(x) all(is.na(x)) ) ]
            
            clean.csv <- cbind(clean.csv,
                               data_frame(region = "Euro", 
                                          issued.year = year, 
                                          issued.quarter = quarter,
                                          panel = "SPF-ECB",
                                          issued.period = paste0(year,"Q",quarter)),
                               row.names = NULL)
            clean.csv <- SPFECBRenameBins(panel = clean.csv)
            
            write_csv(clean.csv, path = temp.path, col_names = TRUE, append = FALSE)
          }
          
          if(grepl(pattern = "EXPECTED UNEMPLOYMENT RATE; PERCENTAGE OF ", x = data.variable[1,1]) ) {
            
            temp.path <- paste0("temp/Unemployment", year,"Q",quarter, ".csv")
            write_csv(data.variable[-1,], temp.path, col_names = FALSE, append = FALSE)
            clean.csv <- cbind(data_frame(variable = "Unemployment"), 
                               read_csv(file = temp.path, col_names = TRUE),
                               row.names = NULL)
            clean.csv <- clean.csv[ , ! apply( clean.csv , 2 , function(x) all(is.na(x)) ) ]
            
            clean.csv <- cbind(clean.csv,
                               data_frame(region = "Euro", 
                                          issued.year = year, 
                                          issued.quarter = quarter,
                                          panel = "SPF-ECB",
                                          issued.period = paste0(year,"Q",quarter)),
                               row.names = NULL)
            
            clean.csv <- SPFECBRenameBins(panel = clean.csv)
            
            write_csv(clean.csv, path = temp.path, col_names = TRUE, append = FALSE)
          }
        }
      }
    }
    
  }
}

rm(data.variable, dataSPF, clean.csv, quarter, year, temp.path,i, empty.rows)

files <- list.files(file.path("temp/"), full.names = TRUE)
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
                  forecast.panel.SPF.ECB$target.quarter))))  

forecast.panel.SPF.ECB <- forecast.panel.SPF.ECB %>%
  mutate(quarters.ahead.ECB = ifelse(
    quarters.ahead <= 4,
    4,
    ifelse( quarters.ahead <= 8,
           8,
           ifelse( quarters.ahead == 15,
                   16,
                   NA )
           )
  )
  ) 

forecast.panel.SPF.ECB2 <- forecast.panel.SPF.ECB %>%
  select(panel, panel.id, variable, region, point.forecast, fixed.event.or.horizon,issued.period,
         issued.year, issued.quarter, target.period, quarters.ahead, quarters.ahead.ECB, target.year, target.quarter,
         FN6_5TN6_1, FN6_0TN5_6, FN5_5TN5_1, FN5_0TN4_6, FN4_5TN4_1, FN4_0TN3_6, FN3_5TN3_1, FN3_0TN2_6, 
         FN2_5TN2_1, FN2_0TN1_6, FN1_5TN1_1, FN1_0TN0_6, FN0_5TN0_1, F0_0T0_4, F0_5T0_9, F1_0T1_4, F1_5T1_9, 
         F2_0T2_4, F2_5T2_9, F3_0T3_4, F3_5T3_9, F4_0T4_4, F4_5T4_9, F5_0T5_4, F5_5T5_9, F6_0T6_4, F6_5T6_9, 
         F7_0T7_4, F7_5T7_9, F8_0T8_4, F8_5T8_9, F9_0T9_4, F9_5T9_9, F10_0T10_4, F10_5T10_9, F11_0T11_4, F11_5T11_9,
         F12_0T12_4, F12_5T12_9, 
         F13_0T13_4, F13_5T13_9, 
         F14_0T14_4, F14_5T14_9,
         F15_0T15_4)

write_csv(forecast.panel.SPF.ECB2, path = "Submissions/SPF-ECB.csv", col_names = TRUE, append = FALSE)
