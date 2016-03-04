#panel <- read_rds(path = "/home/onno/open-fp/forecast.panel.rds") %>% filter(panel == "SPF-ECB")
panel <- read_rds(path = "forecast.panel.rds") %>% filter(panel == "SPF-ECB")


n <- dim(panel)[1]

#investigate.spf <- panel %>% filter(fixed.event.or.horizon == "horizon", issued.year == 1999, target.year == 2003)

panel %>% 
  filter(!is.na(panel.id)) %>%
  group_by(issued.period, target.period) %>%
  mutate(avg.point.forecast = mean(point.forecast, na.rm = TRUE)) %>%
  mutate(avg.var.point.forecast = var(point.forecast, na.rm = TRUE)) -> panel

triangular.distribution <- function(l, r, c) {
  list(mean = (l + r + c)/3, 
       var  = (l^2 + r^2 + c^2 - l * r - l * c - r * c)/18)
}


beta.parameters <- function(n) {
  empty.distribution.panel <- data_frame(panel.id = rep(NA, times = n),
                                         issued.period = rep(NA, times = n),
                                         target.period = rep(NA, times = n),
                                         variable = rep(NA, times = n),
                                         fit.distr = rep(NA, times = n),
                                         a = rep(NA, times = n),
                                         b = rep(NA, times = n),
                                         l = rep(NA, times = n),
                                         r = rep(NA, times = n),
                                         mean.distr = rep(NA, times = n),
                                         var.distr = rep(NA, times = n),
                                         l.new = rep(NA, times = n),
                                         r.new = rep(NA, times = n))
  for(i in 1:n) {
    dist.panel <- panel[i,15:51]
    dist.panel[dist.panel == 0] <- NA
    
    empty.distribution.panel$panel.id[i]      = panel$panel.id[i] 
    empty.distribution.panel$issued.period[i] = panel$issued.period[i]
    empty.distribution.panel$target.period[i] = panel$target.period[i]
    empty.distribution.panel$variable[i]      = panel$variable[i]
    
    if (length(dist.panel[!is.na(dist.panel)]) == 0) {
      # nothing happens
    } 
    
    dist.panel <- panel[i,15:51]
    dist.panel[dist.panel == 0] <- NA
    
    if (length(dist.panel[!is.na(dist.panel)]) == 1) {
      # find l
      dist.panel <- panel[i,15:51] %>% setNames(as.character(seq(-6.5,11.5, by = 0.5)))
      dist.panel[dist.panel == 0] <- NA
      l <- as.numeric(colnames(dist.panel)[min(which(!is.na(dist.panel[1,])))]) - 0.05
      # find r
      dist.panel <- panel[i,15:51] %>% setNames(seq(-6.1, 11.9, by = 0.5))
      dist.panel[dist.panel == 0] <- NA
      r <- as.numeric(colnames(dist.panel)[max(which(!is.na(dist.panel[1,])))]) + 0.05
      
      empty.distribution.panel$fit.distr[i] = "triangle" 
      empty.distribution.panel$l[i]         = l
      empty.distribution.panel$r[i]         = r
      empty.distribution.panel$mean.distr   = triangular.distribution(l, r, (l+r)/2)$mean
      var.distr                             = triangular.distribution(l, r, (l+r)/2)$var
    }
    
    dist.panel <- panel[i,15:51]
    dist.panel[dist.panel == 0] <- NA
    
    if (length(dist.panel[!is.na(dist.panel)]) == 2) {
      # find l
      dist.panel <- panel[i,15:51] %>% setNames(as.character(seq(-6.5,11.5, by = 0.5)))
      dist.panel[dist.panel == 0] <- NA
      l <- as.numeric(colnames(dist.panel)[min(which(!is.na(dist.panel[1,])))]) -0.05
      # find r
      dist.panel <- panel[i,15:51] %>% setNames(seq(-6.1, 11.9, by = 0.5))
      dist.panel[dist.panel == 0] <- NA
      r <- as.numeric(colnames(dist.panel)[max(which(!is.na(dist.panel[1,])))]) + 0.05
      
      prob.left.bin <- as.numeric(dist.panel[!is.na(dist.panel)][1])/100
      prob.right.bin <- as.numeric(dist.panel[!is.na(dist.panel)][2])/100
      
      # equally weighted bins
      if(prob.left.bin == prob.right.bin) {
        l.new <- l
        r.new <- r
        mean.distr <- triangular.distribution(l, r, (l+r)/2)$mean
        var.distr  <- triangular.distribution(l, r, (l+r)/2)$var
      }
      
      # left bin has less probability mass than right bin
      if(prob.left.bin < prob.right.bin) {
        
        l.new <- as.numeric(r - 0.5 - 0.5*sqrt(prob.left.bin/2)/(1-sqrt(prob.left.bin/2)) )
        r.new <- as.numeric(r)
        c.new <- (r.new + l.new)/2
        
        mean.distr <- triangular.distribution(l.new, r.new, c.new)$mean
        var.distr  <- triangular.distribution(l.new, r.new, c.new)$var
      }
      
      # left bin has more probability mass than right bin
      if(prob.left.bin > prob.right.bin) {
        
        l.new <- as.numeric(l)
        r.new <- as.numeric(l + 0.5 + 0.5 * sqrt(prob.right.bin/2)/(1-sqrt(prob.right.bin/2)))
        c.new <- (r.new + l.new)/2
        
        mean.distr <- triangular.distribution(l.new, r.new, c.new)$mean 
        var.distr  <- triangular.distribution(l.new, r.new, c.new)$var
      }
      
      
      empty.distribution.panel$fit.distr[i]  = "triangle"
      empty.distribution.panel$l[i]          = l
      empty.distribution.panel$r[i]          = r
      empty.distribution.panel$l.new[i]      = l.new
      empty.distribution.panel$r.new[i]      = r.new
      empty.distribution.panel$mean.distr[i] = mean.distr
      empty.distribution.panel$var.distr[i]  = var.distr
    }
    
    dist.panel <- panel[i,15:51]
    dist.panel[dist.panel == 0] <- NA
    
    if (length(dist.panel[!is.na(dist.panel)]) >= 3) {
      # find l
      dist.panel <- panel[i,15:51] %>% setNames(as.character(seq(-6.5,11.5, by = 0.5)))
      dist.panel[dist.panel == 0] <- NA
      l <- as.numeric(colnames(dist.panel)[min(which(!is.na(dist.panel[1,])))]) - 0.05
      # find r
      dist.panel <- panel[i,15:51] %>% setNames(seq(-6.1, 11.9, by = 0.5)) 
      dist.panel[dist.panel == 0] <- NA
      r <- as.numeric(colnames(dist.panel)[max(which(!is.na(dist.panel[1,])))]) + 0.05
      
      t.grid <- seq(as.numeric(colnames(dist.panel)[min(which(!is.na(dist.panel[1,])))]),
                    as.numeric(colnames(dist.panel)[max(which(!is.na(dist.panel[1,])))]),
                    by = 0.5)
      
      
      dist.panel[1,] <- as.numeric(dist.panel[1,], na.rm = TRUE)/sum(dist.panel[1,], na.rm = TRUE)
      
      f.t.grid <- cumsum(as.numeric(dist.panel[!is.na(dist.panel)]))
      
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
      
      sum.sq.diff <- function(optim.parameters) {
        a <- optim.parameters[1]
        b <- optim.parameters[2]
        sum((sapply(t.grid, beta.dist.function, a = a, b = b, l = l, r = r) - f.t.grid)^2)
      }
      
      optimum <- optimx(par = c(1.5, 1.5), 
                        method = "L-BFGS-B",
                        fn = sum.sq.diff,
                        lower = c(1.001,1.001))
      
      if(optimum$p1 == 0.001) {
        optimum <- optimx(par = c(3, 3), 
                          method = "L-BFGS-B",
                          fn = sum.sq.diff,
                          lower = c(1.001,1.001))
      }
      a <- optimum$p1
      
      b <- optimum$p2
      
      
      empty.distribution.panel$fit.distr[i] = "beta"
      empty.distribution.panel$a[i] = a 
      empty.distribution.panel$b[i] = b 
      empty.distribution.panel$l[i] = l 
      empty.distribution.panel$r[i] = r
      empty.distribution.panel$mean.distr[i] = a/(a+b) * (r-l) + l
      empty.distribution.panel$var.distr[i] = a * b/((a+b+1)*(a+b)^2)*(r-l)^2
      
      
    }
    if (i %% 100 == 0) {
      print(i)
    }
  }
  empty.distribution.panel
}

x <- beta.parameters(4)

n <- dim(panel)[1]

system.time(
distribution.panel <- lapply(100, beta.parameters)
)


write_rds(distribution.panel, path = "distribution_panel.rds")
distribution.panel <- read_rds("distribution_panel.rds")

panel.with.beta.distributions <- left_join(panel, distribution.panel) %>%
  group_by(issued.period, target.period) %>%
  mutate(avg.distr.point.forecast = mean(mean.distr, na.rm = TRUE)) %>%
  mutate(avg.distr.uncertainty = var(var.distr, na.rm = TRUE))
  

infl.panel.with.beta.distributions <- panel.with.beta.distributions %>% 
  filter(issued.year == 2009) %>% 
  filter(a != 1.001 || is.na(a) == TRUE || b == 1.001) %>%
  group_by(variable) %>% 
  filter(variable == "Inflation") %>%
  select(-l.new, - r.new, -region)

  
write_rds(infl.panel.with.beta.distributions, path = "ecb_infl_panel_with_fitted_distribution.rds")
write.dta(infl.panel.with.beta.distributions, file = "ecb_infl_panel_with_fitted_distribution.dta")

plot.data <- panel.with.beta.distributions %>% 
  #filter(issued.year == 2009) %>% 
  #filter(a != 1.001 || is.na(a) == TRUE || b == 1.001) %>%
  group_by(variable) %>% 
  filter(variable == "Inflation") %>%
  select(-l.new, - r.new)

qplot(mean.distr, point.forecast, data = panel.with.beta.distributions, color = "variable")

ggplot(dat, aes(x=xvar, y=yvar, color=cond)) +
  geom_point(shape=1) +

plot.data <- panel.with.beta.distributions %>% 
  #filter(issued.year == 2009) %>% 
  #filter(a != 1.001 || is.na(a) == TRUE || b == 1.001) %>%
  group_by(variable) %>% 
  filter(variable == "Inflation")# %>%
  #filter(fit.distr == "beta")

ggplot(plot.data, aes(y=point.forecast, x=mean.distr)) + 
  geom_point(aes(group = variable, color = variable), size = 1) +
  xlim(-3,5) +
  ylim(-3,5)

ggplot(plot.data, aes(y=point.forecast, x=mean.distr)) + 
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









