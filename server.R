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
        print(x)
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
  
 
}
