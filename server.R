options(shiny.maxRequestSize=100*1024^2) 
source("pooled.R")
source('unpooled.R')
source('two_sample_prop.R')
source('Paired_sample.R')
source('helper.R')
options("scipen"=100, "digits"=4)
library("pastecs")
library("psych")
library("car")
library("xtable")
library("perturb")
library("MASS")


function(input,output) {
  set.seed(123)
  histData = rnorm(100)
  output$mapCI = renderPlot({normal(input$meanCI,input$sdCI,input$lbCI,input$ubCI)})
  
  output$MenuItem = renderMenu({
    menuItem("Menu Item", icon = icon("calendar"))
    
  })
  output$ProgressBox = renderInfoBox({
    infoBox("Progress", paste0(25+input$Count,"%"),icon = icon("list"),color = "purple")
  })
  
  output$approvalBox = renderInfoBox({
    infoBox("Approval", "80%",icon = icon("thumbs-up",lib = "glyphicon"),color = "yellow")
  })
}
