#panel <- read_rds(path = "/home/onno/open-fp/forecast.panel.rds") %>% filter(panel == "SPF-ECB")
panel <- read_rds(path = "forecast.panel.rds") %>% filter(panel == "SPF-ECB")


#investigate.spf <- panel %>% filter(fixed.event.or.horizon == "horizon", issued.year == 1999, target.year == 2003)

panel %>% 
  filter(!is.na(panel.id)) %>%
  group_by(issued.period, target.period) %>%
  mutate(mean.point.forecast = mean(point.forecast, na.rm = TRUE)) %>%
  mutate(cross.var.point.forecast = var(point.forecast, na.rm = TRUE)) -> panel

triangular.distribution <- function(l, r, c) {
  list(mean = (l + r + c)/3, 
       var  = (l^2 + r^2 + c^2 - l * r - l * c - r * c)/18)
}

beta.parameters <- function(i) {
  dist.panel <- panel[i,15:51]
  dist.panel[dist.panel == 0] <- NA
  
  if (length(dist.panel[!is.na(dist.panel)]) == 0) {
    out = data_frame(panel.id = panel$panel.id[i], 
                     issued.period = panel$issued.period[i], 
                     target.period = panel$target.period[i], 
                     variable = panel$variable[i])
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
    
    
    out = data_frame(panel.id = panel$panel.id[i], issued.period = panel$issued.period[i], target.period = panel$target.period[i],
                     variable = panel$variable[i],
                     fit.distr = "triangle", l = l, r = r, 
                     mean.distr = triangular.distribution(l, r, (l+r)/2)$mean,
                     var.distr  = triangular.distribution(l, r, (l+r)/2)$var)
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
    
    
    out = data_frame(panel.id = panel$panel.id[i], issued.period = panel$issued.period[i], target.period = panel$target.period[i],
                     variable = panel$variable[i],
                     fit.distr = "triangle", l = l, r = r,
                     l.new = l.new,
                     r.new = r.new,
                     mean.distr = mean.distr,
                     var.distr  = var.distr)
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
      #l <- optim.parameters[3]
      #r <- optim.parameters[4]
      sum((sapply(t.grid, beta.dist.function, a = a, b = b, l = l, r = r) - f.t.grid)^2)
    }
#     
#     sum.abs.diff <- function(optim.parameters) {
#       a <- optim.parameters[1]
#       b <- optim.parameters[2]
#       #l <- optim.parameters[3]
#       #r <- optim.parameters[4]
#       sum(abs(sapply(t.grid, beta.dist.function, a = a, b = b, l = l, r = r) - f.t.grid))
#     }
    
#     max.abs.diff <- function(optim.parameters) {
#       a <- optim.parameters[1]
#       b <- optim.parameters[2]
#       #l <- optim.parameters[3]
#       #r <- optim.parameters[4]
#       max(abs(sapply(t.grid, beta.dist.function, a = a, b = b, l = l, r = r) - f.t.grid))
#     }
#     
    optimum <- optimx(par = c(1.5, 1.5), 
                      method = "L-BFGS-B",
                      fn = sum.sq.diff,
                      lower = c(1.001,1.001))
    
    #optimum.abs <- optimx(par = c(1.5, 1.5), 
    #                  method = "L-BFGS-B",
    #                  fn = sum.abs.diff,
    #                  lower = c(1.001,1,001))
    
    #optimum.max <- optimx(par = c(1.5, 1.5), 
    #                      method = "L-BFGS-B",
    #                      fn = max.abs.diff,
    #                      lower = c(1.001,1.001))
#     
#     optimum <- optimx(par = c(1.5, 1.5, l-0.01, r+0.01), 
#                       method = "L-BFGS-B",
#                       fn = sum.sq.diff,
#                       lower = c(1, 1, -10, r),
#                       upper = c(Inf, Inf, l, 30))
    a <- optimum$p1
      
    b <- optimum$p2
    
    out <- data_frame(panel.id = panel$panel.id[i], issued.period = panel$issued.period[i], target.period = panel$target.period[i],
                      fixed.event.or.horizon = panel$fixed.event.or.horizon[i],
                      variable = panel$variable[i],
                      fit.distr = "beta", a = a, b = b, l = l, r = r,
                      mean.distr = a/(a+b) * (r-l) + l,
                      var.distr = a * b/((a+b+1)*(a+b)^2)*(r-l)^2
                      #,
                      #a.abs = optimum.abs$p1, b.abs = optimum.abs$p2,
                      #a.max = optimum.max$p1, b.max = optimum.max$p2
                      )
    
  }
  if (i %% 1000 == 0) {
    print(i)
  }
  out
}

beta.parameters(3)


n <- dim(panel)[1]

n <- 50

system.time(
  distribution.panel <- Reduce(full_join, lapply(c(1:n), beta.parameters))
)






distribution.panel.stripped <- distribution.panel %>% select(-fixed.event.or.horizon)

write_rds(distribution.panel.stripped, path = "distribution_panel.rds")

panel.with.beta.distributions <- left_join(panel, distribution.panel.stripped)

write_rds(panel.with.beta.distributions, path = "ecb_panel_with_fitted_distribution.rds")
write.dta(panel.with.beta.distributions, file = "ecb_panel_with_fitted_distribution.dta")

qplot(mean.distr, point.forecast, data = panel.with.beta.distributions, color = "variable")

ggplot(dat, aes(x=xvar, y=yvar, color=cond)) +
  geom_point(shape=1) +

plot.data <- panel.with.beta.distributions %>% 
  #filter(target.year == 2005) %>% 
  group_by(variable) #%>% 
  #filter(variable == "Unemployment") %>%
  #filter(fit.distr == "beta")
ggplot(plot.data, aes(y=point.forecast, x=mean.distr)) + 
  geom_point(aes(group = variable, color = variable), size = 1) +
  xlim(-5,13.5) +
  ylim(-5,13.5)



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
  beta.dist.function(t = t, a = a, b = b, l = l, r = r) - beta.dist.function(t = t-0.01, a = a, b = b, l = l, r = r)
}


i = 89
support <- seq(distribution.panel$l[i], distribution.panel$r[i], by = 0.01)
plot(support,
     sapply(support,
            beta.dist.density, 
            a = distribution.panel$a[i], 
            b = distribution.panel$b[i], 
            l = distribution.panel$l[i], 
            r = distribution.panel$r[i]),
     type = "l")

plot(support,
     sapply(support,
            beta.dist.function, 
            a = distribution.panel$a[i], 
            b = distribution.panel$b[i], 
            l = distribution.panel$l[i], 
            r = distribution.panel$r[i]),
     type = "l")









