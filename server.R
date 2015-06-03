options(shiny.maxRequestSize=100*1024^2) 
source("pooled.R")
source('unpooled.R')
source('two_sample_prop.R')
source('Paired_sample.R')
source('helper.R')
options("scipen"=100, "digits"=4)
library("shinydashboard")
library("pastecs")
library("psych")
library("car")
library("xtable")
library("perturb")
library("MASS")


function(input,output) {
  
  #=================Confidense Interval Starts=================#
    output$mapCI = renderPlot({normal(input$meanCI,input$sdCI,input$lbCI,input$ubCI)})
  #=================Confidense Interval Ends===================#
  
  #=================Hypothesis Testing Starts==================#
    output$mapHT = renderPlot({hypothesis_test(input$meanHT,input$SmeanHT,input$sdHT,input$sdchoice,input$type,input$alpha,input$n)})
    alt_hyp = reactive({
      if (input$type == '1'){
        return(paste("Alternate Hypothesis Ha: Mu != ", input$meanHT))
      }
      if (input$type == '2'){
        return(paste("Alternate Hypothesis Ha: Mu < ", input$meanHT))
      }
      if (input$type == '3'){
        return(paste("Alternate Hypothesis Ha: Mu > ", input$meanHT))
      }
    })
    
    output$H0 =  renderText({paste("Null Hypothesis H0: Mu = ", input$meanHT)})
    output$Ha =  renderText({alt_hyp()})
    output$result1 =  renderText({hyp_test_res(input$meanHT,input$SmeanHT,input$sdHT,input$sdchoice,input$type,input$alpha,input$n)[1]})
    output$result2 =  renderText({hyp_test_res(input$meanHT,input$SmeanHT,input$sdHT,input$sdchoice,input$type,input$alpha,input$n)[2]})
    output$result3 =  renderText({hyp_test_res(input$meanHT,input$SmeanHT,input$sdHT,input$sdchoice,input$type,input$alpha,input$n)[3]})
  #=================Hypothesis Testing Ends===================#
  
  
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
