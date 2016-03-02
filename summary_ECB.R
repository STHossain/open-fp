panel <- read_rds(path = "/home/onno/open-fp/forecast.panel.rds") %>% filter(panel == "SPF-ECB")
panel <- read_rds(path = "forecast.panel.rds") %>% filter(panel == "SPF-ECB")


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
    out = data_frame(panel.id = panel$panel.id[i], issued.period = panel$issued.period[i], target.period = panel$target.period[i])
  } 
  
  dist.panel <- panel[i,15:51]
  dist.panel[dist.panel == 0] <- NA
  
  if (length(dist.panel[!is.na(dist.panel)]) == 1) {
    # find l
    dist.panel <- panel[i,15:51] %>% setNames(as.character(seq(-6.5,11.5, by = 0.5)))
    l <- as.numeric(colnames(dist.panel)[min(which(!is.na(dist.panel[1,])))])
    # find r
    dist.panel <- panel[i,15:51] %>% setNames(seq(-6.1, 11.9, by = 0.5)) 
    r <- as.numeric(colnames(dist.panel)[max(which(!is.na(dist.panel[1,])))])
    
    
    out = data_frame(panel.id = panel$panel.id[i], issued.period = panel$issued.period[i], target.period = panel$target.period[i],
                     fit.distr = "triangle", l = l, r = r, 
                     mean.distr = triangular.distribution(l, r, (l+r)/2)$mean,
                     var.distr  = triangular.distribution(l, r, (l+r)/2)$var)
  }
  
  dist.panel <- panel[i,15:51]
  dist.panel[dist.panel == 0] <- NA
  
  if (length(dist.panel[!is.na(dist.panel)]) == 2) {
    # find l
    dist.panel <- panel[i,15:51] %>% setNames(as.character(seq(-6.5,11.5, by = 0.5)))
    l <- as.numeric(colnames(dist.panel)[min(which(!is.na(dist.panel[1,])))])
    # find r
    dist.panel <- panel[i,15:51] %>% setNames(seq(-6.1, 11.9, by = 0.5)) 
    r <- as.numeric(colnames(dist.panel)[max(which(!is.na(dist.panel[1,])))])
    
    if(dist.panel[!is.na(dist.panel)][1] == dist.panel[!is.na(dist.panel)][2]) {
      mean.distr <- triangular.distribution(l, r, (l+r)/2)$mean
      var.distr  <- triangular.distribution(l, r, (l+r)/2)$var
    }
    
    if(dist.panel[!is.na(dist.panel)][1] < dist.panel[!is.na(dist.panel)][2]) {
      
      alpha <- dist.panel[!is.na(dist.panel)][1] %>% as.numeric()
      t <- sqrt(alpha/2)/(1-sqrt(alpha/2))/2
      
      mean.distr <- triangular.distribution(l + 0.5 - t, r, (l + 1.5 - t)/2)$mean
      var.distr  <- triangular.distribution(l + 0.5 - t, r, (l + 1.5 - t)/2)$var
      
      
    }
    
    
    if(dist.panel[!is.na(dist.panel)][1] > dist.panel[!is.na(dist.panel)][2]) {
      mean.distr <- (l+r)/2
      var.distr  <- triangular.distribution(l, r, (l+r)/2)$var
    }
    
    
    out = data_frame(panel.id = panel$panel.id[i], issued.period = panel$issued.period[i], target.period = panel$target.period[i],
                     fit.distr = "triangle", l = l, r = r,
                     mean.distr = mean.distr,
                     var.distr  = var.distr)
  }
  
  dist.panel <- panel[i,15:51]
  dist.panel[dist.panel == 0] <- NA
  
  if (length(dist.panel[!is.na(dist.panel)]) >= 3) {
    # find l
    dist.panel <- panel[i,15:51] %>% setNames(as.character(seq(-6.5,11.5, by = 0.5)))
    dist.panel[dist.panel == 0] <- NA
    l <- as.numeric(colnames(dist.panel)[min(which(!is.na(dist.panel[1,])))])
    # find r
    dist.panel <- panel[i,15:51] %>% setNames(seq(-6.1, 11.9, by = 0.5)) 
    dist.panel[dist.panel == 0] <- NA
    r <- as.numeric(colnames(dist.panel)[max(which(!is.na(dist.panel[1,])))])
  
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
                      fit.distr = "beta", a = a, b = b, l = l, r = r,
                      mean.distr = a/(a+b) * (r-l) + l,
                      var.distr = a * b/((a+b+1)*(a+b)^2)*(r-l)^2
                      #,
                      #a.abs = optimum.abs$p1, b.abs = optimum.abs$p2,
                      #a.max = optimum.max$p1, b.max = optimum.max$p2
                      )
    
  }
  
  out
}

beta.parameters(5)

# comment here

system.time(
distribution.panel <- Reduce(full_join, lapply(c(1:333), beta.parameters))
)
i = 88

beta.dist.density <- function(t,a,b,l,r) {
  beta.dist.function(t = t, a = a, b = b, l = l, r = r) - beta.dist.function(t = t-0.01, a = a, b = b, l = l, r = r)
}

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









