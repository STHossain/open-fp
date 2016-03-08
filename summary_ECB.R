#panel <- read_rds(path = "/home/onno/open-fp/forecast.panel.rds") %>% filter(panel == "SPF-ECB")

library(readr)
library(optimx)
library(dplyr)
library(ggplot2)

panel <- read_rds(path = "forecast.panel.rds") %>% filter(panel == "SPF-ECB")


n <- dim(panel)[1]

panel %>% 
  filter(!is.na(panel.id)) %>%
  group_by(variable, issued.period, target.period) %>%
  mutate(avg.point.forecast = mean(point.forecast, na.rm = TRUE)) %>%
  mutate(var.point.forecast = var(point.forecast, na.rm = TRUE)) %>%
  ungroup() -> panel

triangular.distribution <- function(l, r, c) {
  list(mean = (l + r + c)/3, 
       var  = (l^2 + r^2 + c^2 - l * r - l * c - r * c)/18)
}


beta.parameters <- function(n) {
  beta.dist.function <- function(t, l, r, a, b) {
    
    if(t <= l) {
      return(0)
    } else {
      if(t >= r) {
        return(1) 
      } else {
        int <- function(x) {
          (x-l)^(a-1) * (r-x)^(b-1) / (r-l)^(a+b-1)
        }
        integrate(int, lower = l, upper = t)$value/beta(a, b)
      }
    }
  }
  
  distribution.panel <- data_frame(panel.id = rep(NA, times = n),
                                         issued.period = rep(NA, times = n),
                                         target.period = rep(NA, times = n),
                                         variable = rep(NA, times = n),
                                         fit.distr = rep(NA, times = n),
                                         a = rep(NA, times = n),
                                         b = rep(NA, times = n),
                                         l = rep(NA, times = n),
                                         r = rep(NA, times = n),
                                         mean.fitted.distr = rep(NA, times = n),
                                         var.fitted.distr = rep(NA, times = n),
                                         mean.empirical.distr = rep(NA, times = n),
                                         var.empirical.distr = rep(NA, times = n),  
                                         l.new = rep(NA, times = n),
                                         r.new = rep(NA, times = n))
  for(i in 1:n) {
    dist.panel <- panel[i,15:58]
    dist.panel[dist.panel == 0] <- NA
    
    distribution.panel$panel.id[i]      = panel$panel.id[i] 
    distribution.panel$issued.period[i] = panel$issued.period[i]
    distribution.panel$target.period[i] = panel$target.period[i]
    distribution.panel$variable[i]      = panel$variable[i]
    
    if(sum(is.na(panel[i,15:58]), na.rm = TRUE) == 44) {
      # no empirical distribution is calculated
    } else {
      distribution.panel$mean.empirical.distr[i] <- sum(as.numeric(panel[i,15:58]/100 * seq(-6.25 ,15.25, by = 0.5)), na.rm = TRUE)
      distribution.panel$var.empirical.distr[i]  <- sum((seq(-6.25 ,15.25, by = 0.5) - as.numeric(distribution.panel$mean.empirical.distr[i]))^2 * as.numeric(panel[i,15:58]/100), na.rm = TRUE)
    }
    
    if (length(dist.panel[!is.na(dist.panel)]) == 0) {
      # nothing happens
    } 
    
    dist.panel <- panel[i,15:58]
    dist.panel[dist.panel == 0] <- NA
    
    if (length(dist.panel[!is.na(dist.panel)]) == 1) {
      # find l
      dist.panel <- panel[i,15:58] %>% setNames(as.character(seq(-6.5, 15, by = 0.5)))
      dist.panel[dist.panel == 0] <- NA
      l <- as.numeric(colnames(dist.panel)[min(which(!is.na(dist.panel[1,])))]) - 0.05
      # find r
      dist.panel <- panel[i,15:58] %>% setNames(seq(-6.1, 15.4, by = 0.5))
      dist.panel[dist.panel == 0] <- NA
      r <- as.numeric(colnames(dist.panel)[max(which(!is.na(dist.panel[1,])))]) + 0.05
      
      distribution.panel$fit.distr[i] = "triangle" 
      distribution.panel$l[i]         = l
      distribution.panel$r[i]         = r
      distribution.panel$mean.fitted.distr[i] = triangular.distribution(l, r, (l+r)/2)$mean
      distribution.panel$var.fitted.distr[i]  = triangular.distribution(l, r, (l+r)/2)$var
    }
    
    dist.panel <- panel[i,15:58]
    dist.panel[dist.panel == 0] <- NA
    
    if (length(dist.panel[!is.na(dist.panel)]) == 2) {
      # find l
      dist.panel <- panel[i,15:58] %>% setNames(as.character(seq(-6.5, 15, by = 0.5)))
      dist.panel[dist.panel == 0] <- NA
      l <- as.numeric(colnames(dist.panel)[min(which(!is.na(dist.panel[1,])))]) -0.05
      # find r
      dist.panel <- panel[i,15:58] %>% setNames(seq(-6.1, 15.4, by = 0.5))
      dist.panel[dist.panel == 0] <- NA
      r <- as.numeric(colnames(dist.panel)[max(which(!is.na(dist.panel[1,])))]) + 0.05
      
      prob.left.bin <- as.numeric(dist.panel[!is.na(dist.panel)][1])/100
      prob.right.bin <- as.numeric(dist.panel[!is.na(dist.panel)][2])/100
      
      # equally weighted bins
      if(prob.left.bin == prob.right.bin) {
        l.new <- l
        r.new <- r
        mean.triangle <- triangular.distribution(l, r, (l+r)/2)$mean
        var.triangle  <- triangular.distribution(l, r, (l+r)/2)$var
      }
      
      # left bin has less probability mass than right bin
      if(prob.left.bin < prob.right.bin) {
        
        l.new <- as.numeric(r - 0.5 - 0.5*sqrt(prob.left.bin/2)/(1-sqrt(prob.left.bin/2)) )
        r.new <- as.numeric(r)
        c.new <- (r.new + l.new)/2
        
        mean.triangle <- triangular.distribution(l.new, r.new, c.new)$mean
        var.triangle  <- triangular.distribution(l.new, r.new, c.new)$var
      }
      
      # left bin has more probability mass than right bin
      if(prob.left.bin > prob.right.bin) {
        
        l.new <- as.numeric(l)
        r.new <- as.numeric(l + 0.5 + 0.5 * sqrt(prob.right.bin/2)/(1-sqrt(prob.right.bin/2)))
        c.new <- (r.new + l.new)/2
        
        mean.triangle <- triangular.distribution(l.new, r.new, c.new)$mean 
        var.triangle  <- triangular.distribution(l.new, r.new, c.new)$var
      }
      
      
      distribution.panel$fit.distr[i]  = "triangle"
      distribution.panel$l[i]          = l
      distribution.panel$r[i]          = r
      distribution.panel$l.new[i]      = l.new
      distribution.panel$r.new[i]      = r.new
      distribution.panel$mean.fitted.distr[i] = mean.triangle
      distribution.panel$var.fitted.distr[i]  = var.triangle
      
      rm(mean.triangle, var.triangle)
    }
    
    dist.panel <- panel[i,15:58]
    dist.panel[dist.panel == 0] <- NA
    
    if (length(dist.panel[!is.na(dist.panel)]) >= 3) {
      # find l
      dist.panel <- panel[i,15:58] %>% setNames(as.character(seq(-6.5, 15, by = 0.5)))
      dist.panel[dist.panel == 0] <- NA
      l <- as.numeric(colnames(dist.panel)[min(which(!is.na(dist.panel[1,])))]) - 0.05
      # find r
      dist.panel <- panel[i,15:58] %>% setNames(seq(-6.1, 15.4, by = 0.5)) 
      dist.panel[dist.panel == 0] <- NA
      r <- as.numeric(colnames(dist.panel)[max(which(!is.na(dist.panel[1,])))]) + 0.05
      
      t.grid <- seq(as.numeric(colnames(dist.panel)[min(which(!is.na(dist.panel[1,])))]),
                    as.numeric(colnames(dist.panel)[max(which(!is.na(dist.panel[1,])))]),
                    by = 0.5)
      
      
      y <- as.numeric(dist.panel[1,min(which(!is.na(dist.panel[1,]))):max(which(!is.na(dist.panel[1,])))], na.rm = TRUE)/sum(dist.panel[1,], na.rm = TRUE)
      y[is.na(y) == TRUE] <- 0
      f.t.grid <- cumsum(y)
      rm(y)
      
      sum.sq.diff <- function(optim.parameters) {
        a <- optim.parameters[1]
        b <- optim.parameters[2]
        sum((sapply(t.grid, beta.dist.function, a = a, b = b, l = l, r = r) - f.t.grid)^2)
      }
      
      optimum <- optimx(par = c(2, 2), 
                        method = "L-BFGS-B",
                        fn = sum.sq.diff,
                        lower = c(1.001,1.001))
      
      # if(optimum$p1 == 0.001) {
      #   optimum <- optimx(par = c(3, 3), 
      #                     method = "L-BFGS-B",
      #                     fn = sum.sq.diff,
      #                     lower = c(1.001,1.001))
      # }
      a <- optimum$p1
      
      b <- optimum$p2
      
      
      distribution.panel$fit.distr[i] = "beta"
      distribution.panel$a[i] = a 
      distribution.panel$b[i] = b 
      distribution.panel$l[i] = l 
      distribution.panel$r[i] = r
      distribution.panel$mean.fitted.distr[i] = a/(a+b) * (r-l) + l
      distribution.panel$var.fitted.distr[i] = a * b/((a+b+1)*(a+b)^2)*(r-l)^2
      rm(a,b,l,r,t.grid,f.t.grid)
      
    }
    #if (i %% 100 == 0) {
    #  print(i)
    #}
    print(i)
  }
  distribution.panel
}

x <- beta.parameters(4)

n <- dim(panel)[1]


zz <- file("all.Rout.txt")
sink(zz, append = TRUE)
sink(zz, append = TRUE, type = "message")

system.time(
distribution.panel <- beta.parameters(n)
)


write_rds(distribution.panel, path = "distribution_panel.rds")


distribution.panel <- read_rds("distribution_panel.rds")

panel.with.beta.distributions <- full_join(panel, distribution.panel) %>%
  group_by(variable, issued.period, target.period) %>%
  mutate(avg.fitted.distr.mean = mean(mean.fitted.distr, na.rm = TRUE)) %>%
  mutate(avg.fitted.distr.variance = var(var.fitted.distr, na.rm = TRUE)) %>%
  mutate(avg.empirical.distr.mean = mean(mean.empirical.distr, na.rm = TRUE)) %>%
  mutate(avg.empirical.distr.variance = var(var.empirical.distr, na.rm = TRUE)) %>%
  ungroup()

panel.christian.matthias <- panel.with.beta.distributions %>% 
  select(panel.id, variable, issued.period, target.period.ECB,
         fixed.event.or.horizon,
         quarters.ahead.ECB, point.forecast,
         mean.fitted.distr,
         var.fitted.distr,
         mean.empirical.distr,
         var.empirical.distr,
         avg.point.forecast, var.point.forecast,
         avg.fitted.distr.mean,
         avg.fitted.distr.variance,
         avg.empirical.distr.mean,
         avg.empirical.distr.variance
         )

panel.christian.matthias$variable[panel.christian.matthias$variable == "GDP growth"] <- "gdpgrowth"
panel.christian.matthias$variable[panel.christian.matthias$variable == "Inflation"] <- "infl"
panel.christian.matthias$variable[panel.christian.matthias$variable == "Unemployment"] <- "unempl"

write.dta(panel.christian.matthias, file = "ecb_spf.dta")
write_csv(panel.christian.matthias, path = "ecb_spf.csv")

plot.data <- panel.with.beta.distributions %>% 
  #filter(issued.year == 2009) %>% 
  #filter(a != 1.001 || is.na(a) == TRUE || b == 1.001) %>%
  group_by(variable) %>% 
  filter(variable == "Inflation") %>%
  select(-l.new, - r.new)

qplot(mean.fitted.distr, point.forecast, data = panel.with.beta.distributions, color = "variable")

ggplot(dat, aes(x=xvar, y=yvar, color=cond)) +
  geom_point(shape=1) +

plot.data <- panel.with.beta.distributions %>% 
  #filter(issued.year == 2009) %>% 
  #filter(a != 1.001 || is.na(a) == TRUE || b == 1.001) %>%
  #group_by(variable) %>% 
  filter(variable == "Inflation")# %>%
  #filter(fit.distr == "beta")

ggplot(plot.data, aes(y=point.forecast, x=mean.fitted.distr)) + 
  geom_point(aes(group = variable, color = variable), size = 1) +
  xlim(-3,5) +
  ylim(-3,5)

ggplot(panel.with.beta.distributions, aes(y=point.forecast, x=mean.fitted.distr)) + 
  geom_point(aes(group = variable, color = variable), size = 1) +
  xlim(-5,15) +
  ylim(-5,15)

ggplot(panel.with.beta.distributions, aes(y=avg.fitted.distr.variance, x=avg.empirical.distr.variance)) + 
  geom_point(aes(group = variable, color = variable), size = 0.5) +
  xlim(0,2) +
  ylim(0,2)



ggplot(panel.with.beta.distributions, aes(y=point.forecast, x=mean.empirical.distr)) + 
  geom_point(aes(group = variable, color = variable), size = 1) +
  xlim(-5,15) +
  ylim(-5,15)

ggplot(panel.with.beta.distributions, aes(y=mean.empirical.distr, x=mean.fitted.distr)) + 
  geom_point(aes(group = variable, color = variable), size = 1) +
  xlim(-5,15) +
  ylim(-5,15)

ggplot(panel.with.beta.distributions, aes(y=avg.point.forecast, x=avg.fitted.distr.point.forecast)) + 
  geom_point(aes(group = variable, color = variable), size = 1) +
  xlim(-5,15) +
ylim(-5,15)



beta.dist.function <- function(t, l, r, a, b) {
  
  if(t <= l) {
    return(0)
  } else {
    if(t >= r) {
      return(1) 
    } else {
      int <- function(x) {
        (x-l)^(a-1) * (r-x)^(b-1) / (r-l)^(a+b-1)
      }
      integrate(int, lower = l, upper = t)$value/beta(a, b)
    }
  }
}

beta.dist.density <- function(t,a,b,l,r) {
  beta.dist.function(t = t, a = a, b = b, l = l, r = r) - beta.dist.function(t = t-0.001, a = a, b = b, l = l, r = r)
}


i = 73
support <- seq(plot.data$l[i]-1, plot.data$r[i]+1, by = 0.001)
plot(support,
     sapply(support,
            beta.dist.density, 
            a = plot.data$a[i], 
            b = plot.data$b[i], 
            l = plot.data$l[i], 
            r = plot.data$r[i]),
     type = "l")

plot(support,
     sapply(support,
            beta.dist.function, 
            a = plot.data$a[i], 
            b = plot.data$b[i], 
            l = plot.data$l[i], 
            r = plot.data$r[i]),
     type = "l")









