setwd("~/ShinyApps/frontpage/data/")
library(dplyr)
library(readr)
x <- read_csv("x,y\n 2010-Q1,2\n 2011-Q4,4")

forecast.panel <- data_frame(forecaster = c("Onno"), 
                             variable = c("GDP growth","GDP growth","Inflation","Inflation"),
                             region = c("Euro","US","Euro","Euro"),
                             issued = c(as.Date(c("30/12/2015"), "%d/%m/%Y")),
                             type = c("quarterly"),
                             q.ahead = c(3,4,3,4),
                             target.date = c(as.Date(c("30/09/2016"), "%d/%m/%Y"),
                                             as.Date(c("31/12/2016"), "%d/%m/%Y"),
                                             as.Date(c("30/09/2016"), "%d/%m/%Y"),
                                             as.Date(c("31/12/2016"), "%d/%m/%Y")),
                             target.year = c(2015),
                             target.quarter = c(3,4,3,4),
                             target = c("2015 Q2","2015 Q3","2015 Q2","2015 Q3"),
                             mean.forecast = c(2,3,3,4)) 



forecast.panel
write_csv(forecast.panel, path = "/home/onno/Submissions/test.csv")
saveRDS(forecast.panel, file = "forecast.panel.rds")
