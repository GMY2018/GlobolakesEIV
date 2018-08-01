shinyServer(function(input, output) {

## get data
  mydat <- reactive({
       inFile <- input$file1
       if (is.null(inFile))  return(NULL)
       read.csv(inFile$datapath, header=TRUE)#input$header)
  })


output$contents <- renderTable({
  mydat()
})

## get variables
output$var1 = renderUI({
        vnames = names(mydat())
        selectInput('var1', 'Variable 1 (reference method)', c(Choose='',  vnames ), selectize=FALSE)
          })

output$var2 = renderUI({
  vnames = names(mydat())
  selectInput('var2', 'Variable 2 (test method)', c(Choose='',  vnames ), selectize=FALSE)

  })


## get data corresponding to the variables selected
myvars <- reactive({
  inFile <- input$file1;  if (is.null(inFile))  return(NULL)
  mdat <- mydat()
  var1 <- l1 <- as.character(input$var1);var2 <- l2 <- as.character(input$var2)
  id1 <- which(names(mdat)==var1);  id2 <- which(names(mdat)==var2)
  groupid <- as.character(input$groupid);
  idg1 <- which(names(mdat)==groupid);  
  g11 <- c(mdat[,idg1]); 
  b1 <- c(mdat[,id1]); b2 <- c(mdat[,id2])
  list(v1 = b1, v2 = b2, g1=g11)
})

mygroups <- reactive({
  inFile <- input$file1;  if (is.null(inFile))  return(NULL)
  if (is.null(input$method))  return(NULL)
  mdat <- mydat()
  groupid <- as.character(input$groupid);
  idg1 <- which(names(mdat)==groupid);  
  g11 <- c(mdat[,idg1]); 
  list(g1=g11)
})






##function to remove missing values and print details
#  ------------------------------------------------------------------------
output$groupid = renderUI({
  um <- input$method
  if(um==4||um==3){
    vnames = names(mydat())
    selectInput('groupid', label=list( h4("Error Variance Ratio"),h6('Select grouped error variance ratio column. This column should contain reference method/test method error variance ratios corresponding to each observation.')), c(Choose='',  vnames ), selectize=FALSE)
  }
    })

output$evr = renderUI({
  um <- input$method
  if(um==1||um==2){
    radioButtons("evr", label = h4("Error Variance Ratio"),
                 choices = list("Orthogonal (1:1)" = 1,
                                "Estimated" = 2,
                                "User Input" = 3),selected = 1)
  }
})


output$userevr1 = renderUI({
  uevr <- input$evr
  um <- input$method
  if(!is.null(uevr)&uevr==3&(um==1||um==2)){
    numericInput("userevr1", 
                 label = h6("Enter estimated error variance of Variable 1 (reference method)"), 
                 value = 1, min=1, max=5)}
  })

output$userevr2 = renderUI({
  uevr <- input$evr
  um <- input$method
  if(!is.null(uevr)&uevr==3&(um==1||um==2)){
    numericInput("userevr2", 
                 label = h6("Enter estimated error variance of Variable 2 (test method)"), 
                 value = 1, min=1, max=5)}
})




#  ------------------------------------------------------------------------


#  ------------------------------------------------------------------------
#  ------------------------------------------------------------------------



#reference/test
#reference =1, test=2
getparams <- reactive({

  v1 <- myvars()$v1; v2 <-myvars()$v2; g1 <-mygroups()$g1
  if (input$lg==TRUE) {v1 <- log(v1); v2 <- log(v2)}

  if (input$evr == 1) evar <- 1
  if (input$evr == 2) {
    mod1 <- lm(v1 ~ v2); bb1 <- var(residuals(mod1))
    mod2 <- lm(v2 ~ v1); bb2 <- var(residuals(mod2))
    evar <- bb1/bb2    
  }

  if (input$evr == 3) {
    evar <- as.numeric(input$userevr1/input$userevr2)
  }

  if (input$method==1) {mod1 <-  ModDeming(v1, v2, vr=evar, boots = TRUE, w=FALSE)  }
  
  if (input$method==2) {
  mod1 <-  ModDeming(v1, v2, vr=evar, boots = TRUE, w=TRUE)
  
  }
  if (input$method==3) {
    evv <- g1
    mod1 <- ModDeming(v1, v2, vr=g1, boots = TRUE, alfa=0.05, w=FALSE) 
}
  if (input$method==4) {
    evv <- g1
    mod1 <- ModDeming(v1, v2, vr=g1, boots = TRUE, alfa=0.05,  w=TRUE)
}
  
  
  list(mod1=mod1, evar=evar, v1=v1, v2=v2, g1=g1)
})

#  ------------------------------------------------------------------------


output$params <- renderPrint({
  validate(
  need(input$file1, 'Please upload a data file'),
  need(input$var1,  'Please choose variable 1'),
  need(input$var2,  'Please choose variable 2'), 
  if(input$method==3|input$method==4)  {need(input$groupid,  'Please choose group EVRs')}
  )
  a <- getparams()
  pars <- a$mod1; v1 <- a$v1; v2 <- a$v2
  fitted <- pars[1,1] + pars[2,1]*v1
  Rsqq <- (corr(cbind(fitted, v2)))^2
  MSE <- (sum((v2-fitted)^2))/length(v2)
  model=paste(input$var2, "=", round(pars[1,1],2), "+", round(pars[2,1],2), "x", input$var1 ) 
  if(input$method==1|input$method==2){evvv <- a$evar}
  if(input$method==3|input$method==4){evvv <- a$g1}
  
  print(list("model"=model, "parameters"=pars, "error variance ratio(s)"=evvv, "Mean Square Error"=MSE))

  
  })


output$regparams <- renderPrint({
  validate(
    need(input$file1, ""),
    need(input$var1, ""),
    need(input$var2, ""), 
    need(input$regular, "") 
    )
  
  a <- getparams()
 v1 <- a$v1; v2 <- a$v2
 mod2 <- lm(v2~v1)
 pars <- mod2$coefficients
 TAB <-  cbind(c(pars), confint(mod2))
 colnames(TAB) <- c("Estimate", "LCI", "UCI")
 rownames(TAB) <- c("Intercept", "Slope")
 
 MSE <- sum((residuals(mod2))^2)/length(v1)
 model=paste(input$var2, "=", round(TAB[1,1],2), "+", round(TAB[2,1],2), "x", input$var1 ) 
 
 
  if (input$regular)  {print(list("model"=model, "parameters"=TAB,  "Mean Square Error"=MSE))}

})


output$vplot <- renderPlot({
   validate(
      need(input$file1, 'Please upload a data file'),
      need(input$var1,  'Please choose variable 1'),
      need(input$var2,  'Please choose variable 2'), 
      if(input$method==3|input$method==4)  {need(input$groupid,  'Please choose group EVRs')}

    )
  
  a <- getparams();
  v1 <- a$v1; v2 <- a$v2; g1 <- a$g1
  if(is.null(g1)) {g1 <- rep(1, length(v1))}

  l1 <- input$var1; l2<- input$var2
  if (input$lg==TRUE) {l1 <- paste("log", l1); L2 <- paste("log", l2)}

 if(input$method==1|input$method==2){  plot(v1, v2, pch=19, xlab=l1, ylab=l2,  cex.lab=1.5, cex.axis=1.5)}

 if(input$method==3|input$method==4){  plot(v1, v2, pch=19, xlab=l1, ylab=l2,  cex.lab=1.5, cex.axis=1.5, col=as.factor(g1))}
  
  pars <- a$mod1
  if (input$xyline) abline(0, 1, lty=2, col="grey50", lwd=2)
  if (input$regular) {abline(lm(v2~v1), lty=2, col="orange", lwd=2)}
   
  if (input$addband) {

  if(input$method==2|input$method==4) {W <- TRUE} 
  if(input$method==1|input$method==3) {W <- FALSE} 
    if(input$method==1|input$method==2){evvv <- a$evar}
    if(input$method==3|input$method==4){evvv <- a$g1}
    
 cl <- cifn(x=v1,y=v2, vr=evvv, w=W)
 xgrid <- cl[,1];  ucl <- cl[,4]; lcl <- cl[,3]
 i.for <- order(xgrid); i.back <- order(xgrid, decreasing = TRUE )
 x.polygon <- c(xgrid[i.for], xgrid[i.back] )
 y.polygon <- c(ucl[i.for]  , lcl[i.back] )
 polygon(x.polygon, y.polygon , col = rgb(0,0,1, alpha=0.3) , border = NA )
  
  }
  if (input$addline) abline(pars[1:2,1], lty=1, col="blue", lwd=2)
})


output$resplot <- renderPlot({
  validate(
    need(input$file1, 'Please upload a data file'),
    need(input$var1,  'Please choose variable 1'),
    need(input$var2,  'Please choose variable 2'), 
    if(input$method==3|input$method==4)  {need(input$groupid,  'Please choose group EVRs')}
    
  )
  if(input$residplot){
  a <- getparams()
  g <- a$mod1
  v1 <- a$v1; v2 <- a$v2
  fitted <- g[1,1]+g[2,1]*v1
  resid <- v2-fitted
  plot(fitted, resid, pch=19,
       xlab="Fitted Values", ylab="Residuals", cex.lab=1.5, cex.axis=1.5)
  abline(h=0, col="grey80")
  }
})


output$downloadPlot <- downloadHandler(
  filename = function() { paste(input$vplot, '.png', sep='') },
  content = function(file) {
    png(file)
    dev.off()
  }
)


})