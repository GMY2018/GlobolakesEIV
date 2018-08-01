#install packages; shiny, boot, markdown, mcr
#set wd to relevant folder

setwd("R://")
list.files()
setwd("R://ShinyApps//GlobolakesEIV")
shiny::runApp()


cs <- read.csv("C2R.csv")
attach(cs)
names(cs)
insit <- In.situ.Chl.a; rs <- chl_conc_mean
v1 <- insit; v2 <- rs

6.03316
v1 <- log(v1); v2 <- log(v2)
pars <- mcreg(v1, v2, method.reg="WDeming", error.ratio=6)@para

fitted <- pars[1,1] + pars[2,1]*v1
ssreg = sum((fitted-mean(v2))^2)
sstot <- sum((v2-mean(v2))^2)   
abline(h=mean(v2))
ssres = sum((fitted-v2)^2)
Rsqq <- 1-(ssres/ssto)
(corr(cbind(fitted, v2)))^2

plot(v1, v2)
abline(pars[1,1], pars[2,1])
