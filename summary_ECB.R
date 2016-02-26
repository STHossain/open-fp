panel <- read_rds(path = "/home/onno/open-fp/forecast.panel.rds") %>% filter(panel == "SPF-ECB")

panel %>% 
  group_by(issued.period, target.period) %>%
  mutate(mean.point.forecast = mean(point.forecast, na.rm = TRUE)) %>%
  mutate(cross.var.point.forecast = var(point.forecast, na.rm = TRUE))-> panel

  
mutate(panel, x = mean()) %>% select(x)

fit_beta <- function() {
  
}