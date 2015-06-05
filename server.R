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
library("ggplot2")
library("shiny")

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
  
  
  #=================Anova Starts===================#
  data2 = reactive({
    data = data()
    var = input$variable
    return(data$var)
  })
  
  data = reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    else 
      data = read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    return(data)
  })
  filterCatVar = reactive({
    data = data()
    data.temp = data
    data.temp[colnames(data)] = lapply(data[colnames(data)],as.factor)
    levels.temp = lapply(data.temp[colnames(data.temp)],levels)
    length.levels = lapply(levels.temp,length)
    length.levels = length.levels[length.levels<30]
    return(names(length.levels))
  })
  
  
  
  output$filename = renderText({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    paste("File Name : ",inFile$name, "         Size:" , inFile$size/1000, "KB",sep = " ")
  })
  
  output$contents <- renderTable({
    if (is.null(data()))
      return(NULL)
    #data = read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    #stat.desc(data(),norm = T)
    describe(data())
    #describe(read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote))
  })
  
  output$variable = renderUI({
    #inFile <- input$file1
    if (is.null(data()))
      return(NULL)
    variables = colnames(data())
    checkboxGroupInput("variable", "Choose Variables for Plots", variables,selected = variables[[1]])
  })
  
  output$plots <- renderUI({
    if (is.null(data()))
      return(NULL)
    if (is.null(input$variable))
      return(NULL)
    plot_output_list_anova <- lapply(1:length(input$variable), function(i) {
      #plotname <- paste(input$variable[[i]], i, sep="")
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 350, width = 700)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list_anova)
    
  })
  
  
  observe({ for (i in 1:length(input$variable)) {
    
    local({
      my_i <- i
      plotname <- paste("plot",my_i, sep="")
      data = data() 
      output[[plotname]] <- renderPlot({
        par(oma = c( 2, 2, 2, 2 ) )
        nf<- layout( matrix( c( 1, 2 ), 1, 2, byrow = TRUE),c(2, 2), c( 5, 5 ), TRUE )
        layout.show(nf )
        x = input$variable[[my_i]]
        #print(x)
        ggplot(data= data,aes_string(x=input$variable[[my_i]]))+geom_histogram(aes(y=..density..),color="black",fill="white")+geom_density(alpha=0.2,fill="red")
#         par(mfrow = c(1,2))
#          hx <- dnorm(data[[input$variable[[my_i]]]],mean(data[[input$variable[[my_i]]]]),sd(data[[input$variable[[my_i]]]])/sqrt(length(data[[input$variable[[my_i]]]])))
#          boxplot(data[[input$variable[[my_i]]]],col = "red",xlab = NULL)
#          hist(data[[input$variable[[my_i]]]], col = "red",main = NULL,prob = T,xlab = NULL)
#          curve(dnorm(x,mean(data[[input$variable[[my_i]]]]),sd(data[[input$variable[[my_i]]]])),add = T, col = "green", lwd = 2)
# #         ##  Create an overall title.
#          mtext( paste(input$variable[[my_i]]), outer = TRUE,cex = 1.5,col = "Blue")
        #hist(data[[input$variable[[my_i]]]])
      })
    })
  }
  })
  
  output$varAnova = renderUI({
    if (is.null(data()))
      return(NULL)
    variables = colnames(data())
    catVar = filterCatVar()
    #checkboxGroupInput("variable", "Choose Variables", variables,selected = variables[[1]])
    switch(input$TypeAnova,
           "anova" = div(class = "row-fluid",
                         div(class="span5",selectInput("DepVar", "Dependent Variable",setdiff(variables,catVar))),
                         div(class="span5",checkboxGroupInput("IndepVar", "Independent Variables", catVar,selected = catVar[1]))
           ),
           #                "ancova" = div(class = "row-fluid",
           #                               div(class="span5",selectInput("DepVar", "Dependent Variable",variables)),
           #                               div(class="span5",checkboxGroupInput("IndepVar", "Independent Variables", catVar,selected = catVar[1]))
           #                ),
           "manova" =  div(class = "row-fluid",
                           div(class="span5",checkboxGroupInput("MDepVar", "Dependent Variables", setdiff(variables,catVar),selected = variables[[1]])),
                           div(class="span5",checkboxGroupInput("MIndepVar", "Independent Variables", catVar,selected = catVar[1]))
           ) )
    
  })

output$anovaPlot = renderPlot({
  data=data()
  if (is.null(input$IndepVar))
    return(NULL)
  if (is.null(data()))
    return(NULL)
  if (length(input$IndepVar)==1)
        return(qplot(data=data,x = factor(data[[input$IndepVar]]),y=data[[input$DepVar]],geom=c("boxplot","jitter"),xlab=input$IndepVar,ylab=input$DepVar,fill=factor(data[[input$IndepVar]]))
               +theme(axis.title=element_text(face="bold",size="12", color="brown"), legend.position="top")+scale_fill_discrete(name = input$IndepVar)) 
#   if (length(input$IndepVar)==2)
#         return(qplot(data=data,x = factor(data[[input$IndepVar[[1]]]]),y=data[[input$DepVar]],geom=c("boxplot","jitter"),xlab=input$IndepVar,ylab=input$DepVar,fill=factor(data[[input$IndepVar[1]]]))
#                 +theme(axis.title=element_text(face="bold",size="12", color="brown"),legend.position="top")+facet_wrap(.~factor(data[[input$IndepVar[[2]]]])))
})


output$anovaPlotText = renderText({
  if (is.null(input$IndepVar))
    return(NULL)
  if (is.null(data()))
    return(NULL)
  if(length(levels(factor(input$IndepVar)))>15)
    paste("Independent Variable is not Categorical or has more than 15 categories")
})
  
  output$textAnova1 = renderText({
    paste("Response Variable:",input$DepVar,sep = " ")
  })
  output$textAnova2 = renderText({
    paste("Independent Variables:",paste(input$IndepVar,collapse = ","),sep = " ")
  })
  
  anova.fit = reactive({
    if (is.null(input$IndepVar))
      return(NULL)
    data1 = data()
    data1[input$IndepVar] = lapply(data1[input$IndepVar],as.factor)
    data1[input$DepVar] = lapply(data1[input$DepVar],as.numeric)
    e1 = paste(input$DepVar,paste(input$IndepVar,collapse = "*"),sep = "~")
    fit = aov(formula(e1),data = data1)
    return(fit)
    
  }
  )
  output$SummaryAnova <- renderTable({
    if (is.null(data()))
      return(NULL)
    if (is.null(input$IndepVar))
      return(NULL)
    
    summary(anova.fit())
  })
  
  anova.plot = reactive({
    if (is.null(data()))
      return(NULL)
    if (is.null(input$IndepVar))
      return(NULL)
    layout(matrix(c(1,2,3,4),2,2))
    plot(anova.fit())
  })
  
  output$PlotAnova = renderPlot({
    anova.plot()
  })
  
  output$TukeyPlot <- renderPlot({
    if (is.null(data()))
      return(NULL)
    if (is.null(input$IndepVar))
      return(NULL)
    
    plot(TukeyHSD(anova.fit()))
  })
  
  output$Tukey <- renderTable({
    if (is.null(data()))
      return(NULL)
    if (is.null(input$IndepVar))
      return(NULL)
    
    data.frame(TukeyHSD(anova.fit())[1])
  })
  
  bartlett = reactive({
    if (is.null(input$IndepVar))
      return(NULL)
    data1 = data()
    data1[input$IndepVar] = lapply(data1[input$IndepVar],as.factor)
    data1[input$DepVar] = lapply(data1[input$DepVar],as.numeric)
    e1 = paste(input$DepVar,paste("interaction(",paste(input$IndepVar,collapse = ","),")",sep = ""),sep = "~")
    b.fit = bartlett.test(formula(e1),data = data1)
    data.bartlett = cbind(b.fit[1],b.fit[2],b.fit[3],b.fit[4],b.fit[5])
    colnames(data.bartlett) = c("Bartlett's K-squared","df","p.value","data.name","method")
    return(data.bartlett)
    
  })
  output$Bartlett <- renderTable({
    if (is.null(data()))
      return(NULL)
    if (is.null(input$IndepVar))
      return(NULL)
    
    bartlett()
  })
  
  lavene = reactive({
    if (is.null(input$IndepVar))
      return(NULL)
    data1 = data()
    data1[input$IndepVar] = lapply(data1[input$IndepVar],as.factor)
    data1[input$DepVar] = lapply(data1[input$DepVar],as.numeric)
    e1 = paste(input$DepVar,paste(input$IndepVar,collapse = "*"),sep = "~")
    l.fit = leveneTest(formula(e1),data = data1)
    data.lavene = cbind(l.fit[1],l.fit[2],l.fit[3])
    colnames(data.lavene) = c("df","F value","Pr(>F)")
    return(data.lavene)
  })
  
  output$Levene <- renderTable({
    if (is.null(data()))
      return(NULL)
    if (is.null(input$IndepVar))
      return(NULL)
    lavene()
  })
  
  summary_manova = reactive({
    if (is.null(data()))
      return(NULL)
    if (is.null(input$MIndepVar))
      return(NULL)
    if (is.null(input$MDepVar))
      return(NULL)
    data1 = data()
    data1[input$MIndepVar] = lapply(data1[input$MIndepVar],as.factor)
    data1[input$MDepVar] = lapply(data1[input$MDepVar],as.numeric)
    e1 = paste(paste("cbind(",paste(input$MDepVar,collapse = ","),")",sep = ""),paste(input$MIndepVar,collapse = "*"),sep = "~")
    fit = manova(formula(e1),data = data1)
    summ.manova= summary.aov(fit)
    tables <- list()
    for (i in 1:length(summ.manova)){
      tables[i] = paste(strong("Response Variable: "),input$MDepVar[i],br(),br(),print(xtable(summ.manova[1]),type = "html"),br(),sep = "" )
    }
    
    return(lapply(tables, paste))
    
    
  })
  output$SummaryManova= renderUI({
    if (is.null(data()))
      return(NULL)
    if (is.null(input$MIndepVar))
      return(NULL)
    if (is.null(input$MDepVar))
      return(NULL)
    #print(summary.aov(manova.fit()))
    #   s1 = print(xtable(summary.aov(manova.fit())[1]),type = "html")
    #   s3 = print("response Variable:")
    #   s2 = print(xtable(summary.aov(manova.fit())[2]),type = "html")
    out = summary_manova()
    return(div(HTML(out),class="shiny-html-output"))
    #HTML(print(xtable(summary.aov(manova.fit())[1]),type = "html"))
    #HTML(print(xtable(summary.aov(manova.fit())[2]),type = "html"))
    # HTML(print(xtable(manova.fit()),type = "html"))
  })
  
  # output$fitManova= renderTable({
  #   if (is.null(data()))
  #     return(NULL)
  #   manova.fit()
  #   
  # })
  
  manova.fit = reactive({
    if (is.null(data()))
      return(NULL)
    if (is.null(input$MIndepVar))
      return(NULL)
    if (is.null(input$MDepVar))
      return(NULL)
    data1 = data()
    data1[input$MIndepVar] = lapply(data1[input$MIndepVar],as.factor)
    data1[input$MDepVar] = lapply(data1[input$MDepVar],as.numeric)
    e1 = paste(paste("cbind(",paste(input$MDepVar,collapse = ","),")",sep = ""),paste(input$MIndepVar,collapse = "*"),sep = "~")
    fit = manova(formula(e1),data = data1)
    return(fit)
  })
  output$PillaiManova= renderTable({
    if (is.null(data()))
      return(NULL)
    if (is.null(input$MIndepVar))
      return(NULL)
    if (is.null(input$MDepVar))
      return(NULL)
    #   out = print(xtable(summary(manova.fit(),test = "Pillai")),type= "html")
    #   return(div(HTML(out),class="shiny-html-output"))
    data.frame(summary(manova.fit(),test = "Pillai")[4])
  })
  output$WilksManova= renderTable({
    if (is.null(data()))
      return(NULL)
    if (is.null(input$MIndepVar))
      return(NULL)
    if (is.null(input$MDepVar))
      return(NULL)
    data.frame(summary(manova.fit(),test = "Wilks")[4])
  })
  output$HotLawManova= renderTable({
    if (is.null(data()))
      return(NULL)
    if (is.null(input$MIndepVar))
      return(NULL)
    if (is.null(input$MDepVar))
      return(NULL)
    data.frame(summary(manova.fit(),test = "Hotelling-Lawley")[4])
  })
  output$RoyManova= renderTable({
    if (is.null(data()))
      return(NULL)
    if (is.null(input$MIndepVar))
      return(NULL)
    if (is.null(input$MDepVar))
      return(NULL)
    data.frame(summary(manova.fit(),test = "Roy")[4])
  })
  
  
  #=================Anova Ends===================#


#=================Regression Starts===================#
dataReg = reactive({
  inFile <- input$fileReg
  if (is.null(inFile))
    return(NULL)
  else 
    data1 = read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  return(data1)
})

output$Regfilename = renderText({
  inFile <- input$fileReg
  if (is.null(inFile))
    return(NULL)
  paste("File Name :",inFile$name, "         Size:" , inFile$size/1000, "KB",sep = " ")
})
output$Regcontents <- renderTable({
  if (is.null(dataReg()))
    return(NULL)
  #data = read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  # stat.desc(data(),norm = T)
  describe(dataReg())
  #describe(read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote))
})
output$Regvariable = renderUI({
  #inFile <- input$file1
  if (is.null(dataReg()))
    return(NULL)
  variables = colnames(dataReg())
  checkboxGroupInput("Regvariable", "Choose Variables to Plot", variables,selected = variables[[1]])
})
#     output$t = renderText(
#       paste(filterCatVar(),collapse = ",")
#       #paste(input$variable[1],length(input$variable))
#       )

output$Regplots <- renderUI({
  if (is.null(dataReg()))
    return(NULL)
  if (is.null(input$Regvariable))
    return(NULL)
  plot_output_list_reg <- lapply(1:length(input$Regvariable), function(i) {
    #plotname <- paste(input$variable[[i]], i, sep="")
    plotname <- paste("Regplots", i, sep="")
    plotOutput(plotname, height = 350, width = 700)
  })
  
  # Convert the list to a tagList - this is necessary for the list of items
  # to display properly.
  do.call(tagList, plot_output_list_reg)
  
})


observe({ for (i in 1:length(input$Regvariable)) {
  
  local({
    my_i <- i
    plotname <- paste("Regplots",my_i, sep="")
    data = dataReg() 
    output[[plotname]] <- renderPlot({
      par(oma = c( 2, 2, 2, 2 ) )
      nf<- layout( matrix( c( 1, 2 ), 1, 2, byrow = TRUE),c(2, 2), c( 5, 5 ), TRUE )
      layout.show(nf )
      ggplot(data= data,aes_string(x=input$Regvariable[[my_i]]))+geom_histogram(aes(y=..density..),color="black",fill="white")+geom_density(alpha=0.2,fill="red")
#       par(mfrow = c(1,2))
#       hx <- dnorm(data[[input$Regvariable[[my_i]]]],mean(data[[input$Regvariable[[my_i]]]]),sd(data[[input$Regvariable[[my_i]]]])/sqrt(length(data[[input$Regvariable[[my_i]]]])))
#       boxplot(data[[input$Regvariable[[my_i]]]],col = "red",xlab = NULL)
#       hist(data[[input$Regvariable[[my_i]]]], col = "red",main = NULL,prob = T,xlab = NULL)
#       curve(dnorm(x,mean(data[[input$Regvariable[[my_i]]]]),sd(data[[input$Regvariable[[my_i]]]])),add = T, col = "green", lwd = 2)
#       ##  Create an overall title.
#       mtext( paste(input$Regvariable[[my_i]]), outer = TRUE,cex = 1.5,col = "Blue")
#       #hist(data[[input$variable[[my_i]]]])
    })
  })
}
})



output$textReg = renderText({
  #paste("Response Variable:",input$DepVar,sep = " ")
  paste("Model(e1):", Reg.model(),sep = " ")
})
# output$textReg2 = renderText({
#   paste("Independent Variables:",paste(input$IndepVar,collapse = ","),sep = " ")
# })

output$RegPlot = renderPlot({
  data1 = dataReg()
  model = Reg.model()
  #modelsplit = strsplit(model, "\\~")
  
  #   depVarPlot = modelsplit[[1]][1]
  #   print(depVarPlot)
  #   indepVarPlot = strsplit(modelsplit[[1]][2],"\\+")
  #   print(indepVarPlot)
  if(length(input$IndepVar) == 1){
    plot(data1[,input$DepVar],data1[,input$IndepVar], 
         main = paste("Scatter Plot, Correlation:", eval(cor(data1[,input$DepVar],data1[,input$IndepVar]))),
         ylab = paste(input$DepVar), xlab= paste(input$IndepVar),
         col="red", col.main="Dodgerblue4", col.lab="Dodgerblue4",pch=20)
  }
  if (length(input$IndepVar) >= 2){
    #     pairs(cbind(data1[depVarPlot],data1[,indepVarPlot[[1]]]),data = data1,upper.panel=panel.cor,
    #           main = "Scatter Plot Matrix with Correlation Coefficients")
    pairs(cbind(data1[input$DepVar],data1[,input$IndepVar]),upper.panel=panel.cor,
          main = "Scatter Plot Matrix with Correlation Coefficients")
  }
  #   if (length(input$IndepVar) > 2){
  #     
  #     pairs(cbind(data1[input$DepVar],data1[,input$IndepVar]),upper.panel=panel.cor,
  #           main = "Scatter Plot Matrix with Correlation Coefficients")
  #     
  #   }
  
})


output$varReg = renderUI({
  if (is.null(dataReg()))
    return(NULL)
  variables = colnames(dataReg())
  div(class = "row-fluid",
      div(class="col-md-2",selectInput("DepVar", strong("Dependent Variable",style = "color:red"),variables)),
      div(class="col-md-2",checkboxGroupInput("Select", strong("Options",style = "color:blue"),c(BoxCox = "Boxcox","No Intercept Model" = "NoIntercept","Center Variables" = "center"))),
      #div(class="span1",checkboxInput("Nointercept", "No Intercept Model", FALSE)),
      div(class="col-md-1",radioButtons("Txdef", strong("Transform Dep Var",style = "color:green"),c(None = "dNone",Log= "dlog", Sqrt = "dsqrt",Square = "dsq",
                                                                                                  Cube = "dcube", Exponential = "dexp"))),
      div(class="col-md-1",checkboxGroupInput("IndepVar", strong("Indep Vars",style = "color:green"), variables,selected = variables[[2]])),
      div(class="col-md-1",checkboxGroupInput("CatVar", strong("Categorical Variables",style = "color:green"), variables)),
      div(class="col-md-1",checkboxGroupInput("Log", strong("Log",style = "color:green"), variables)),
      div(class="col-md-1",checkboxGroupInput("Sqrt", strong("Sqrt",style = "color:green"), variables)),
      div(class="col-md-1",checkboxGroupInput("Sq", strong("Square",style = "color:green"), variables)),
      div(class="col-md-1",checkboxGroupInput("Cube", strong("Cube",style = "color:green"), variables)),
      div(class="col-md-1",checkboxGroupInput("exp", strong("Exponential",style = "color:green"), variables)),
      br(),
      p("...")
      
  )
  
})

Reg.model <- reactive({
  print(input$Select)
  if (is.null(input$Txdef))
    return(NULL)
  switch(input$Txdef,
         dNone = { depvar = input$DepVar},
         dlog = { depvar = paste("log(",input$DepVar,")",sep = "")},
         dsqrt = { depvar = paste("sqrt(",input$DepVar,")",sep = "")},
         dsq = { depvar = paste(input$DepVar,"^2",sep = "")},
         dcube = { depvar = paste(input$DepVar,"^3",sep = "")},
         dexp = { depvar = paste("exp(",input$DepVar,")",sep = "")})
  indepvar = NULL
  if (!is.null(input$IndepVar)){
    indepvar = c(indepvar, input$IndepVar)
  }
  if (!is.null(input$CatVar)){
    indepvarcat = paste("factor(",input$CatVar,")",sep = "")
    indepvar = c(indepvar, indepvarcat)
  }
  if (!is.null(input$Log)){
    indepvarlog = paste("log(",input$Log,")",sep = "")
    indepvar = c(indepvar, indepvarlog)
  }
  if (!is.null(input$Sqrt)){
    indepvarsqrt = paste("sqrt(",input$Sqrt,")",sep = "")
    indepvar = c(indepvar, indepvarsqrt)
  }
  if (!is.null(input$Sq)){
    indepvarsq = paste("I(",input$Sq,"^2)",sep = "")
    indepvar = c(indepvar, indepvarsq)
  }
  if (!is.null(input$Cube)){
    indepvarcube = paste("I(",input$Cube,"^3)",sep = "")
    indepvar = c(indepvar, indepvarcube)
  }
  if (!is.null(input$exp)){
    indepvarexp = paste("exp(",input$exp,")",sep = "")
    indepvar = c(indepvar, indepvarexp)
  }
  
  if (!is.null(indepvar)){
    if (!is.null(input$Select) && any(input$Select == "NoIntercept")){
      model = paste(depvar,paste("-1+",paste(indepvar,collapse = "+"),sep = ""),sep="~")
    }
    else  {
      model = paste(depvar,paste(indepvar,collapse = "+"),sep = "~")
    }
    return(model)
  }
  else {
    return(NULL)
  }
})   


Reg.fit = reactive({
  if (is.null(input$IndepVar))
    return(NULL)
  
  data1 = dataReg()
  if (input$Txdef == "dlog"){
    if (nrow(data1[data1[input$DepVar] <= 0,]) > 0){
      data1[data1[input$DepVar] <=0,input$DepVar] = 1e-5}
  }
  #data1[input$IndepVar] = lapply(data1[input$IndepVar],as.factor)
  data1[input$DepVar] = lapply(data1[input$DepVar],as.numeric)
  #e1 = paste(input$DepVar,paste(input$IndepVar,collapse = "+"),sep = "~")
  e1 = Reg.model()
  
  fit = lm(formula(e1),data = data1)
  return(fit)
  
}
)  

output$boxcoxPlot <-  renderPlot({
  if (is.null(input$IndepVar))
    return(NULL)
  if(is.null(input$Select))
    return(NULL)
  if (input$Select != "Boxcox")
    return(NULL)
  
  data1 = dataReg()
  if (nrow(data1[data1[input$DepVar] <= 0,]) > 0){
    data1[data1[input$DepVar] <=0,input$DepVar]= 1e-5}
  
  #data1[input$IndepVar] = lapply(data1[input$IndepVar],as.factor)
  data1[input$DepVar] = lapply(data1[input$DepVar],as.numeric)
  #e1 = paste(input$DepVar,paste(input$IndepVar,collapse = "+"),sep = "~")
  e2 = Reg.model()
  
  boxCox(formula(e2),data = data1)
})   

output$SummaryReg <- renderPrint({
  if (is.null(dataReg()))
    return(NULL)
  if (is.null(input$IndepVar))
    return(NULL)
  summary(Reg.fit())
})

# output$Rsquared = renderText({
#     if (is.null(dataReg()))
#         return(NULL)
#     if (is.null(input$IndepVar))
#         return(NULL)
#     sumfit = summary(Reg.fit())
#     paste("R-Squared :",round(sumfit$r.squared,5),"Adj R-Squared: ",round(sumfit$adj.r.squared,5), sep = " ")
# })
# 
# output$AnovaReg = renderTable({
#   if (is.null(dataReg()))
#     return(NULL)
#   if (is.null(input$IndepVar))
#     return(NULL)
#   anova(Reg.fit())
#   
# })

Res.plot = reactive({
  if (is.null(dataReg()))
    return(NULL)
  if (is.null(input$IndepVar))
    return(NULL)
  layout(matrix(c(1,2,3,4),2,2))
  plot(Reg.fit())
})
output$ResPlot = renderPlot({
  Res.plot()
})

output$avPlot = renderPlot({
  if (is.null(dataReg()))
    return(NULL)
  if (is.null(input$IndepVar))
    return(NULL)
  a = strsplit(Reg.model(),"\\+")
  n = length(a[[1]])
  avPlots(Reg.fit(), id.n=2, id.cex=0.7,layout = c(round(sqrt(n+1)),ceiling((n+1)/round(sqrt(n+1)))))
  
})

output$ResidualPlot = renderPlot({
  if (is.null(dataReg()))
    return(NULL)
  if (is.null(input$IndepVar))
    return(NULL)
  
  a = strsplit(Reg.model(),"\\+")
  n = length(a[[1]])
  residualPlots(lm(Reg.model(),data = dataReg()),layout = c(round(sqrt(n+1)),ceiling((n+1)/round(sqrt(n+1))))) 
  
})

output$ResidualTable = renderTable({
  if (is.null(dataReg()))
    return(NULL)
  if (is.null(input$IndepVar))
    return(NULL)
  residualPlots(Reg.fit(),plot= F) 
})

output$VIF = renderTable({
  if (is.null(dataReg()))
    return(NULL)
  if (is.null(input$IndepVar))
    return(NULL)
  as.matrix(vif(Reg.fit()))
  
})

output$VarDecompProp = renderTable({
  if (is.null(dataReg()))
    return(NULL)
  if (is.null(input$IndepVar))
    return(NULL)
  if(!is.null(input$Select) && input$Select == "center"){
    print(colldiag(Reg.fit(),center = T))
  }
  else {
    print(colldiag(Reg.fit(),center = F))
  }
  
})

output$BestAIC = renderPrint({
  if (is.null(dataReg()))
    return(NULL)
  if (is.null(input$IndepVar))
    return(NULL)
  data1 = dataReg()
  if (nrow(data1[data1[input$DepVar] <= 0,]) > 0){
    data1[data1[input$DepVar] <=0,input$DepVar]= 1e-5}
  #        print(min(data1[input$DepVar]))
  #data1[input$IndepVar] = lapply(data1[input$IndepVar],as.factor)
  data1[input$DepVar] = lapply(data1[input$DepVar],as.numeric)
  #e1 = paste(input$DepVar,paste(input$IndepVar,collapse = "+"),sep = "~")
  e3 = Reg.model()
  stepAIC(lm(formula(e3),data=data1))
})


# output$BestAIC = renderTable({
#        if (is.null(dataReg()))
#            return(NULL)
#        if (is.null(input$IndepVar))
#            return(NULL)
#        print(stepAIC(Reg.fit())$anova)
# })


output$InfIndexPlot = renderPlot({
  if (is.null(dataReg()))
    return(NULL)
  if (is.null(input$IndepVar))
    return(NULL)
  influenceIndexPlot(Reg.fit(), id.n=3)
})

output$InfluencePlot = renderPlot({
  if (is.null(dataReg()))
    return(NULL)
  if (is.null(input$IndepVar))
    return(NULL)
  influencePlot(Reg.fit(), id.n=3)
})
#=================Regression Ends===================#
 
}
