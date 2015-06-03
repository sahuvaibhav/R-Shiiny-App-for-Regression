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
  
  #=================Hypothesis Testing for 2 populations starts===================#
  
  text_ind = reactive({
    if(input$VarType == "Pooled Variance"){
      return(pooled(input$x1,input$x2,input$du,input$s1,input$s2,input$n1,
                    input$n2,input$itype,input$alphaI))  
    }
    if(input$VarType == "Unpooled Variance"){
      return(unpooled(input$x1,input$x2,input$du,input$s1,input$s2,input$n1,
                      input$n2,input$itype,input$alphaI)) }
  })
  
  plot_ind = reactive({
    if(input$VarType == "Pooled Variance"){
      pooled(input$x1,input$x2,input$du,
             input$s1,input$s2,input$n1,input$n2,input$itype,input$alphaI)  }
    if(input$VarType == "Unpooled Variance"){
      unpooled(input$x1,input$x2,input$du,input$s1,input$s2,input$n1,
               input$n2,input$itype,input$alphaI) }
  })
  output$plotIndependent = renderPlot({plot_ind()})
  output$textIndependent1 = renderText({text_ind()[[1]]})
  output$textIndependent2 = renderText({text_ind()[[2]]})
  output$textIndependent3 = renderText({text_ind()[[3]]})
  
  text_Pr = reactive({
    return(two_sample_prop(input$p1,input$p2,input$dp,
                           input$n1Pr,input$n2Pr,input$prtype,input$alphaPr))  
    
  })
  
  output$plotProportions = renderPlot({two_sample_prop(input$p1,input$p2,input$dp,
                                                       input$n1Pr,input$n2Pr,input$prtype,input$alphaPr)})
  output$textProp1 = renderText({text_Pr()[[1]]})
  output$textProp2 = renderText({text_Pr()[[2]]})
  output$textProp3 = renderText({text_Pr()[[3]]})
  
  text_P = reactive({
    return(Paired_sample(input$dbar,input$D,input$sdbar,input$nP,input$ptype,input$alphaP))  
    
  })
  
  output$plotPaired = renderPlot({Paired_sample(input$dbar,input$D,
                                                input$sdbar,input$nP,input$ptype,input$alphaP)})
  output$txtPaired1 = renderText({text_P()[[1]]})
  output$txtPaired2 = renderText({text_P()[[2]]})
  output$txtPaired3 = renderText({text_P()[[3]]})
  
  
  #=================Hypothesis Testing for 2 populations ends===================#
  
  
  
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
