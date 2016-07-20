setwd("/home/onno/open-fp/")
setwd("~/Git/open-fp/")
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
source("update_SPF_US.R")


all.joined <- full_join( forecast.panel.SPF.ECB, forecast.panel.SPF.US)

all.joined.reduced <- all.joined %>% select(panel, panel.id, variable, region, point.forecast, fixed.event.or.horizon,
                                            issued.year, issued.quarter, years.ahead, quarters.ahead, target.year,
                                            target.quarter)

# forecast subpanel for frontpage
forecast.panel.frontpage <- all.joined.reduced %>%
  select(issued.year, issued.quarter, fixed.event.or.horizon, target.year, point.forecast, region, variable) %>%
  filter(fixed.event.or.horizon == "event") %>%
  dplyr::filter(issued.year > 2014) %>%
  #filter(issued.quarter == 1) %>%
  filter(target.year == 2015 | target.year == 2016 | target.year == 2017 | target.year == 2020) %>%
  filter(is.na(point.forecast) == FALSE)

rm(all.joined.reduced)
write_rds(forecast.panel.frontpage, path = "ShinyApps/frontpage/data/forecast_panel_frontpage.rds")
rm(forecast.panel.frontpage, forecast.panel.SPF.US, forecast.panel.SPF.ECB)

write_rds(all.joined, path = "forecast.panel.rds")

#
# code for saving into the right directories
#

library(dplyr)
library(readr)
library(foreign)

forecast.panel <- read_rds(path = "~/open-fp/forecast.panel.rds") %>% filter(panel == "SPF-US")

# Create various data formats
write_csv(forecast.panel, path = "/var/www/open-fp/data/forecast.panel.csv")
write_rds(forecast.panel, path = "/var/www/open-fp/data/forecast.panel.rds")
write.dta(forecast.panel, file = "/var/www/open-fp/data/forecast.panel.dta")

write_csv(forecast.panel, path = "/home/onno/open-fp/open-fp/data/forecast.panel.csv")
write_rds(forecast.panel, path = "/home/onno/open-fp/open-fp/data/forecast.panel.rds")
write.dta(forecast.panel, file = "/home/onno/open-fp/open-fp/data/forecast.panel.dta")
