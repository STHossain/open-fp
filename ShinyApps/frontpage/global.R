library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

current.year <- Sys.Date() %>% format("%Y") %>% as.numeric()