
# Function to compute percentile bootstrapped CI --------------------------
ci <- function(x){quantile(x, c(0.025, 0.975))}  

#xx <- x<- rnorm(100); yy<-y<-rnorm(100)
#ModDeming(xx, yy, vr, w=rep(1, length(xx)), wform=1) 

#mcreg(x, y,method.reg="Deming", method.ci = "bootstrap")@para
#par.ests(x,y, vr=1)

#c2 <- read.csv("C2R.csv")
#attach(c2)
#names(c2)
#insit <- In.situ.Chl.a
#rs <- chl_conc_mean

#lm1 <- lm(rs~insit)
#lm2 <- lm(insit ~ rs)
#er <- var(resid(lm1))/var(resid(lm1))
#plot(x, y)
#m1 <- mcreg(insit, rs, ,method.reg="WDeming", error.ratio=1,  method.ci = "bootstrap")@para
#rsquared(insit, rs, a=m1[1,1],b=m1[2,1])


ci.line <- function(x,y,a,b){
  xgrid <- seq(min(x, na.rm=T), max(x, na.rm=T), length.out=200)
  n <- length(x)
  sxx <- sum((x-mean(x))^2); syy <- sum((y-mean(y))^2)
  sxy <- sum(x*y) - n*mean(x)*mean(y)
  sigmasq <- (1/(n-2))*(syy-((sxy^2)/sxx))  
  p1 <- (1/n)+((xgrid-mean(xgrid))^2/sxx)
  cc <- sqrt(sigmasq*p1)
  fitted <- a + b*xgrid
  ucl <- fitted+qt(0.975, n)*cc
  lcl <- fitted-qt(0.975, n)*cc
  list(xgrid=xgrid, ucl=ucl, lcl=lcl)
}


rsquared <- function(x,y,a,b){
  fitted <- a + (b*x)
  ss.reg = sum(  (fitted - mean(y))^2  )
  ss.tot = sum( (y -  mean(y))^2   )
  Rsq <- ss.reg/ss.tot  
  print(Rsq)
}



