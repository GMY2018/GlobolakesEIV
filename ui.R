library(shiny)
source("MDR2.R")
library(boot)
library(markdown)
library(mcr)


shinyUI(fluidPage(

   tags$head(includeScript("googleanalytics.js")),


pageWithSidebar(


  headerPanel(title="GloboLakes: Errors In Variables App" ),
  
  sidebarPanel(

    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
  #  checkboxInput('header', 'Header', TRUE),
    tags$hr(),
    htmlOutput("var1"),
    htmlOutput("var2"),
    checkboxInput('lg', label=list(h4('Log Transform?'),h6("WARNING: Log transform should only be applied when unweighted regression is used.")), FALSE),
    
    wellPanel(
    
    radioButtons("method", label = h4("Method"),
                        choices = list("Deming Regression" = 1,
                                       "Weighted Deming Regression" = 2,
                                       "Modified Deming Regression" = 3,
                                       "Weighted Modified Deming Regression" = 4),selected = 1),
    checkboxInput('residplot', 'Show residual vs fit plot', FALSE),
    
    htmlOutput("evr"), 
    htmlOutput("groupid") , 
    htmlOutput("userevr1"), 
    htmlOutput("userevr2")
) #, 
#wellPanel(
# downloadButton("downloadPlot", label = "Download Data Plot", class = NULL) 
  
#)
  ),
  
  mainPanel(
    
    wellPanel(
      div(class='row',
          div(class="span4",      checkboxInput("addline", "Show fitted EIV regression line (blue)", TRUE )),
          div(class="span4",      checkboxInput("addband", "Show uncertainty associated with EIV regression estimate", FALSE)),
          div(class="span4",      checkboxInput("xyline", "Show x=y reference line (grey)", TRUE )),
          div(class="span4",      checkboxInput("regular", "Show standard regression line assuming no error on Variable 1 (orange)", FALSE )))),
    
    tabsetPanel(
      tabPanel("Main",    
               plotOutput('vplot',height = "600px"), 
               h4("Summary"),
               verbatimTextOutput("params"),
             
               conditionalPanel("input.regular == TRUE",
               verbatimTextOutput("regparams")),
               
               
               conditionalPanel("input.residplot == TRUE", plotOutput('resplot',height = "600px"))
               ),
      
      tabPanel("Data",      tableOutput('contents')),
      tabPanel("App Details",  includeMarkdown('EIVdetails.md')),
      tabPanel("About Globolakes",  includeMarkdown('APPTAB2.md'))
    ))

)))
