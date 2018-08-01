library(boot)

deming <- function(xx, yy, vr) 
{
  n <- length(xx)
  xbar <- mean(xx) # mean of xx
  ybar <- mean(yy) # mean of yy
  
  lSSDy <- sum(vr*(yy-ybar)^2)
  SSDy <- sum((yy-ybar)^2)
  lSPDxy <-  sum(vr*(xx-xbar)*(yy-ybar))
  SPDxy <- sum((xx-xbar)*(yy-ybar))
  lSPDxy.2 <- lSPDxy*SPDxy
  SSDxy.2 <- SPDxy*SPDxy
  SSDx <- sum((xx-xbar)^2)
  
  ## slope of deming regression
  beta <- ((lSSDy -  SSDx) 
           + sqrt((SSDx - lSSDy)^2 + 
                    (4 * lSPDxy.2)))/(2 * lSPDxy)
  ## intercept of deming regression
  alpha <- ybar - xbar * beta
  list(alpha=alpha, beta=beta)

}
 



wdeming <- function(xx, yy, vr, iter.max = 30, threshold = 1e-06) {
  X <- xx
  Y <- yy
  n <- length(X)
  mX <- mean(X)
  mY <- mean(Y)
  u <- sum((X - mX)^2)
  q <- sum((Y - mY)^2)
  p <- sum((X - mX) * (Y - mY))
  lq <- sum(vr*((Y - mY)^2))
  lp <- sum(vr* (X - mX) * (Y - mY))
  
  b1 <- ((lq - u) + sqrt((u - lq)^2 + 4 * lp*p))/(2*lp)
  b0 <- mean(Y) - b1 * mean(X)
  
  i <- 0
  warn <- "no warnings"
  repeat {
    if (i >= iter.max) {
      warning(paste("no konvergence after", iter.max, 
                    "iterations"))
      break
    }
    i <- i + 1
    d <- Y - (b0 + b1 * X)
    XHAT <- X + (vr * b1 * d/(1 + vr * 
                                b1^2))
    YHAT <- Y - (d/(1 + vr * b1^2))
    W <- ((XHAT + vr * YHAT)/(1 + vr))^(-2)
    XW <- sum(W * X)/sum(W)
    YW <- sum(W * Y)/sum(W)
    U <- sum(W * ((X - XW)^2))
    Q <- sum(W * ((Y - YW)^2))
    P <- sum(W * (X - XW) * (Y - YW))
    LQ <- sum(vr* W * ((Y - YW)^2))
    LP <- sum(vr*W * (X - XW) * (Y - YW))
    
    B1 <- (LQ - U + sqrt((U - LQ)^2 + 4 * LP*P))/(2*LP)
    B0 <- YW - B1 * XW
    if (abs(b1 - B1) < threshold & abs(b0 - B0) < threshold) 
      break
    b1 <- B1
    b0 <- B0
  }
  return(list(alpha = B0, beta = B1))#, iter = i, xw = XW, weight = W))
}




# Function for bootstrapping  ---------------------------------------------


newpar <- function(dfr,indices, vr){
d <- dfr[indices,]
xx <- d[,1]; yy <- d[,2]
r1 <- deming(xx,yy, vr=vr)
alpha<-r1$alpha; beta<-r1$beta
rse=sd(yy-c(alpha+beta*xx))
return(c(alpha=alpha, beta=beta, rse=rse))
}

newpar2 <- function(dfr,indices, vr){
  d <- dfr[indices,]
  xx <- d[,1]; yy <- d[,2]
  r1 <- wdeming(xx,yy, vr=vr)
  alpha<-r1$alpha; beta<-r1$beta
  rse=sd(yy-c(alpha+beta*xx))
  return(c(alpha=alpha, beta=beta, rse=rse))
}



# Modified Deming Regression Function -------------------------------------


ModDeming <- function(xx, yy, vr, boots = FALSE, keep.boot = FALSE, 
                    alfa = 0.05, w=rep(1, length(xx))) 
  
{

  n <- length(xx)
  if (length(vr)==1) vr <- rep(vr, n)
  vn <- c(deparse(substitute(xx)), deparse(substitute(yy))) #vector with variable names
  pn <- c("Intercept", "Slope") #vector with parameter names
  alfa <- alfa ## significance level
  dfr <- data.frame(xx = xx, yy = yy) # bind xx and yy data as data frame
  dfr <- dfr[complete.cases(dfr), ] # only include complete cases of the data
  xx <- dfr$xx #name xx data 
  yy <- dfr$yy # name yy data

  if (all(w==1)){  ## if all weights equal 1
    res0 <- deming(xx, yy, vr) 
  }
    
  if (any(w!=1)){  ## if any weights are different from 1
    res0 <- wdeming(xx, yy, vr) 
  }   
    
  alpha <- res0$alpha
  beta <-  res0$beta
  
  if (!boots) {  res <- c(alpha, beta);  names(res) <- pn; return(res)}   
   
  if(boots){
      if (is.numeric(boots)) {N <- boots} else {N <- 1000}  
      if (all(w==1))    {   results <- boot(data=dfr, statistic=newpar, R=N, vr=vr) }
      if (!(all(w==1))) {   results <- boot(data=dfr, statistic=newpar2, R=N, vr=vr) }
       
      
      
    ci <- function(a) quantile(a,c(0.025, 0.975))
    md2 <- cbind(c(results$t0), t(apply(results$t, 2, ci)))
    colnames(md2) <- c("Estimate","LCI","UCI")
    rownames(md2) <- c("Intercept", "Slope","rse")
    
    if (keep.boot) {
      results$t
      invisible(res)
    }
    return(md2)
  }
}

# Function to compute percentile bootstrapped CI --------------------------
ci <- function(x){quantile(x, c(0.025, 0.975))}  

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



