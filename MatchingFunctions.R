#### MODELS 1 & 2


# Function to compute percentile bootstrapped CI --------------------------
ci <- function(x){quantile(x, c(0.025, 0.975))}  



# Function for bootstrapping linear regression parameters -----------------
m1boot <- function(formula, dat, indices, ww) {
  d <- dat[indices,]    
  fit <- lm(formula, data=d, weights=ww)
  rmse <- mean(residuals(fit)^2)
  out <- cbind(coefficients(fit)[1], coefficients(fit)[2], rmse)
  return(out)
}



# Function to fit OLS and weighted OLS ------------------------------------
model1 <- function(xx,yy, ww){  
  if (missing(ww)) ww=rep(1, length(xx)) else {ww <- 1/(xx^2)}
      dat <- as.data.frame(cbind(xx,yy,ww))
      results <-  boot(dat=dat, statistic=m1boot,ww=ww,
                   R=1000, formula=yy~xx)
  
md2 <- cbind(c(results$t0), t(apply(results$t, 2, ci)))
colnames(md2) <- c("Estimate","LCI","UCI")
rownames(md2) <- c("Intercept", "Slope","rmse")

return(md2)
}




# Function to compute GMR bootstrap intervals -----------------------------
bet <- function(data, indices) {
  d <- data[indices,]  
  xx <- d[,1]; yy <- d[,2]
  moda <- lm(yy~xx, data=d) ; aa <- moda$coeff[2]
  modb <- lm(xx~yy, data=d) ; bb <- modb$coeff[2]
  beta <- sqrt(aa*bb)
  alpha <- mean(yy)-(beta*mean(xx))

  #par(mfrow=c(1,2));plot(x,y);abline(moda);abline(alpha, beta, col=2);abline(0,1,col=3)
  #plot(y,x);abline(modb); abline(alpha, beta, col=2);  abline(0,1,col=3)
  
  rmse <- sqrt(mean((yy-(alpha+beta*xx))^2))
  return(c(alpha=alpha, beta=beta, rmse=rmse))
}  

# Function to compute GMR estimates ---------------------------------------
GMR <- function(xx, yy, nboot=1000, ylims=range(yy, na.rm=T), legplace="bottomright", plotting=FALSE){
data <- as.data.frame(cbind(xx=xx,yy=yy))
if (any(is.na(xx)|is.na(yy))) stop("There are NA values in x and/or y")

##geometric mean regression
moda <- lm(yy~xx) ; aa <- moda$coeff[2]
modb <- lm(xx~yy) ; bb <- modb$coeff[2]
beta <- sqrt(aa*bb)

if (plotting==TRUE){

plot(xx, yy, pch=19, xlab="In Situ",
     ylab="Remote Sensing", ylim=ylims)
abline(v=seq(10,30, by=10), col="grey80", lty=2)
abline(h=seq(0,35,by=5), col="grey80", lty=2)
points(xx, yy, pch=19)
}

alpha <- mean(yy)-(beta*mean(xx))
preds <- alpha + beta*xx

dat <- as.data.frame(cbind(xx,yy))


# bootstrapping with 1000 replications
results <-  boot(data=dat, statistic=bet,R=1000)

md2 <- cbind(c(results$t0), t(apply(results$t, 2, ci)))
colnames(md2) <- c("Estimate","LCI","UCI")
rownames(md2) <- c("Intercept", "Slope","rmse")
   

  
  return(md2)
}


## Function to compute PB bootstrap intervals ------------------------------
PB.boot = function(dat, indices) {
  alpha=.05
  ## (1) calculate all pairwise slopes
  d <- dat[indices,]; 
  yy <- d[,2]; xx <- d[,1]
  n <- length(xx)
  
  S <- array(NA,dim=rep(n,2))
  for(i in 1:(n-1)){
    for(j in (i+1):n) {
      if(i != j) {
        S[i,j] <- (yy[i] - yy[j])/(xx[i] - xx[j])
      }
    }
  }
  S <- sort(na.exclude(as.vector(S)))
  K <- sum(S <= -1) - .5 * sum(S == -1)
  N <- length(S)
  b <- ifelse(N%%2,S[(N+1)/2+K],mean(S[N/2+K+0:1]))
  a <- median(yy - b*xx)
  rmse <-sqrt(mean((yy-(a+b*xx))^2))
  
  
  return(c(a=a, b=b, rmse=rmse))
  
}


# Function to compute PB estimates ----------------------------------------


PB.reg = function(xx,yy) {
  alpha=.05
  ## (1) calculate all pairwise slopes
  dat <- cbind(xx,yy)
  n <- length(xx)
  
  S <- array(NA,dim=rep(n,2))
  for(i in 1:(n-1)){
    for(j in (i+1):n) {
      if(i != j) {
        S[i,j] <- (yy[i] - yy[j])/(xx[i] - xx[j])
      }
    }
  }
  S <- sort(na.exclude(as.vector(S)))
  K <- sum(S <= -1) - .5 * sum(S == -1)
  N <- length(S)
  b <- ifelse(N%%2,S[(N+1)/2+K],mean(S[N/2+K+0:1]))
  a <- median(yy - b*xx)
  rmse <- sqrt(mean((yy-(a+b*xx))^2))
  
  # bootstrapping with 1000 replications
  results <-  boot(data=dat, statistic=PB.boot,R=1000)
  
  md2 <- cbind(c(results$t0), t(apply(results$t, 2, ci)))
  colnames(md2) <- c("Estimate","LCI","UCI")
  rownames(md2) <- c("Intercept", "Slope","rmse")
  return(md2)
}

