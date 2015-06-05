header =   dashboardHeader(title = "Statistics")

sidebar = dashboardSidebar(
  sidebarMenu(
              menuItem("Confidence Interval", tabName = "tabCI",icon = icon("area-chart")),
              menuItem("Hypothesis Testing", tabName = "tabHT", icon = icon("area-chart"),badgeLabel = "New",badgeColor = "green"),
              menuItem("Hypothesis Testing for 2 Populations", tabName = "tabHT2",icon = icon("area-chart")),
              menuItem("Anova", tabName = "tabAnova",icon = icon("area-chart")),
              menuItem("Regression", tabName = "tabRegression",icon = icon("line-chart"))
              
              
              )
  )

body = dashboardBody(
  tabItems(
    
    #=================Confidense Interval Starts==================#
    tabItem(tabName = "tabCI",
            h2("Confidense Interval"),
            fluidRow(
              box(title = "Input Parameters",solidHeader = T,collapsible = F,width =4,
                  status = "primary",height= 550,
                  p("Provide mean, Standard Deviation,Upper Bound and Lower Bound to plot confidence Interval on Normal Plot"),
                  numericInput("meanCI", label = h4("Mean"), value = 0),
                  numericInput("sdCI", label = h4("Standard Deviation(not Variance)"), value = 1),
                  numericInput("lbCI", label = h4("Lower Bound"), value = -1),
                  numericInput("ubCI", label = h4("Upper Bound"), value = 1)
                  
                  ),
              box(
                title = "Normal",solidHeader = T, collapsible = F,status = "primary",width = 8,height = 550,
                plotOutput("mapCI")
                
              )
            )
    ),
    #=================Confidense Interval Ends==================#
    
    #=================Hypothesis Testing Starts==================#
    tabItem(tabName = "tabHT",
            h2("Hypothesis Testing"),
            fluidRow(
              box(title = "Parameter for Hypothesis Testing",solidHeader = T, collapsible =T, width=4,status = "primary",
                      h4("Hypothesis Testing for Population Mean"),
                      p("Select the type of your Hypothesis Test"),
                      radioButtons("type",label = "",choices = list("Two Tail" = 1, "Left Tail" = 2, "Right Tail" = 3),selected = 1),
                      numericInput("meanHT",label = h4("Population Mean(Mu)"),value = 0),
                      numericInput("SmeanHT",label = h4("Sample Mean(Xbar)"),value = 0),
                      radioButtons("sdchoice", label = h4("Standard Deviation"), choice = list("Population Standard Deviation(Sigma)" = 1, 
                                                                                      "Sample Standard Deviation(s)" = 2),selected = 1),
                      
                      numericInput("sdHT",label = "", value =1),
                      numericInput("alpha", label = h4("Alpha"), value = 0.05),
                      numericInput("n", label = h4("Sample Size(n)"), value = 100)
                      
        #               infoBox("New Orders", 10*2, fill =F),
        #               infoBoxOutput("ProgressBox"),
        #               infoBoxOutput("approvalBox"),
        #               box(width = 4, actionButton("Count","increment Progress"))
              ),
              box(title = "Hypothesis Testing", solidHeader = T, collapsible = T, status = "primary", width = 8,
                  #div(p("Z-Test"),align="center", style = "font-size:150%;color:blue;"),
                  p(strong(span("Z-Test:",style = "color:blue")), "We are applying Z-test for hypotheis testing 
                       when population standard deviation is known."),
                  p(strong(span("t-Test:",style = "color:blue")), "We are applying t-test for hypotheis testing 
                       when population standard deviation is nor known.",
                    span("Strictly assuming population is normally distribured.",
                         style = "color:blue")),
                  plotOutput("mapHT"),
                  div(h4("Hypotheses"),align= "center",style = "font-family: Times;font-size:150%;color:blue"),
                  textOutput("H0"),
                  textOutput("Ha"),
                  div(h4("Test Results"),align= "center",style = "font-size:150%;color:blue;"),
                  textOutput("result1"),
                  textOutput("result2"),
                  textOutput("result3")
                )
    )
  ),
  #=================Hypothesis Testing Ends====================#

#=================Hypothesis Testing for 2 populations Starts====================#
    tabItem(tabName="tabHT2",h2("Hypothesis Testing for 2 Populations"),
            fluidRow(
              tabBox(title="Input Parameters",id="ht2",height = 550,width=5,side = "right",selected= strong(p("Paired Sample")),
                     tabPanel(strong(p("Proportions")),h4("Proportions"),
                              radioButtons("prtype",h5("Tail Type"),c("Two Tail","Left Tail","Right Tail"),selected = "Two Tail"),
                              div(class="row-fluid",
                                  div(class="col-md-6",numericInput("p1", label = "p1", value = 0.2)),
                                  div(class="col-md-6",numericInput("p2", label = "p2", value = 0.3))
                              ),
                              div(class="row-fluid",
                                  div(class="col-md-6",numericInput("n1Pr", label = "n1", value = 100.0)),
                                  div(class="col-md-6",numericInput("n2Pr", label = "n2", value = 100.0))
                              ),
                              numericInput("dp", label = "dp", value = 0.0),
                              numericInput("alphaPr", label = "alpha", value = 0.05)
                     ),
                  tabPanel(strong(p("Independent Sample")),h4(p("Independent Sample")),
                           div(class = "row-fluid",
                               div(class="span4",radioButtons("itype",h5("Tail Type"),c("Two Tail","Left Tail","Right Tail"),selected = "Two Tail")),
                               div(class="span4 offset2",radioButtons("VarType",h5("Variance Type"),c("Pooled Variance","Unpooled Variance"),selected = "Pooled Variance"))
                           ),
                           #radioButtons("VarType",h5("Variance Type"),c("Pooled Variance","Unpooled Variance"),selected = "Pooled Variance"),
                           div(class="row-fluid",
                               div(class="col-md-6",numericInput("x1", label = "x1", value = 2.0)),
                               div(class="col-md-6",numericInput("x2", label = "x2", value = 1.0))
                           ),
                           div(class="row-fluid",
                               div(class="col-md-6",numericInput("s1", label = "s1", value = 1.0)),
                               div(class="col-md-6",numericInput("s2", label = "s2", value = 1.0))
                           ),
                           div(class="row-fluid",
                               div(class="col-md-6",numericInput("n1", label = "n1", value = 100.0)),
                               div(class="col-md-6",numericInput("n2", label = "n2", value = 100.0))
                           ),
                           numericInput("du", label = "du", value = 0.0),
                           numericInput("alphaI", label = "alpha", value = 0.05)
                         
                           ),
                  tabPanel(strong(p("Paired Sample")),h4(p("Paired Sample")),solidHeader=T,
                           radioButtons("ptype",h5("Tail Type"),c("Two Tail","Left Tail","Right Tail"),selected = "Two Tail"),
                           numericInput("dbar", label = "dbar", value = 0.0),
                           numericInput("D", label = "D", value = 0.0),
                           numericInput("sdbar", label = "sd", value = 1.0),
                           numericInput("nP", label = "n", value = 100.0),
                           numericInput("alphaP", label = "alpha", value = 0.05)
                  )
              ),
        tabBox(title = "Hypothesis Testing for 2 populations", width = 7,side= "right",selected = strong(p("Paired Sample")),
                 tabPanel(strong(p("Proportions")),h4("Proportions"),
                         plotOutput("plotProportions"),
                         textOutput("textProp1"),
                         textOutput("textProp2"),
                         textOutput("textProp3")),
                 tabPanel(strong(p("Independent Sample")),h4(p("Independent Sample")),
                          plotOutput("plotIndependent"),
                          textOutput("textIndependent1"),
                          textOutput("textIndependent2"),
                          textOutput("textIndependent3")),
                 tabPanel(strong(p("Paired Sample")),h4(p("Paired Sample")),solidHeader=T,
                          plotOutput("plotPaired"),
                          textOutput("txtPaired1"),
                          textOutput("txtPaired2"),
                          textOutput("txtPaired3"))
            )
      ) ),
#=================Hypothesis Testing for 2 populations Ends====================#

#=============================ANOVA Starts=====================================#
      tabItem(tabName= "tabAnova",h2("Analysis of Variance"),
              fixedRow(
                column(width= 3,
                  box(title = strong("Input Parameters"),solidHeader =T,collapsible =T,width = NULL, status = "primary",collapsed=F,
                       fileInput('file1',"",accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                       div(class = "row-fluid",
                                div(class="col-md-6",radioButtons("sep",h5("Separator"),c(Comma=',',Semicolon=';',Tab='\t'),',')),
                                div(class="col-md-6",radioButtons("quote",h5("Quote"),c(None='','Double Quote'='"','Single Quote'="'"),'"'))
                             ),
                      checkboxInput('header', 'Header', TRUE)),
                  box(title = strong("Input Variables for Anova"), solidHeader =T,collapsible =T,width = NULL, status = "primary",
                    uiOutput("variable"),
                          radioButtons('TypeAnova', 'Type of Anova',c(ANOVA='anova',MANOVA='manova'),'anova'),
                          strong(h6(span("*** Do not use SAME Dependent and Independent Variables",style = "color:red"))),
                          uiOutput("varAnova"))   
                      ),
                column(width=9,
                  box(title = strong(p("Data Summary & Plots")),solidHeader =T, collapsible =T,width = NULL, status = "warning",
                     tabBox(title = strong("Summary"),width = 14,side = "right",
                            tabPanel(strong("Summary"),
                     textOutput('filename'),
                     tableOutput('contents'),
                     strong(h5(div("Notes:",style = "color:red"))),
                     p("For more Info on Data Summary refer",a("Link.",href = "https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/R/stat.desc.R?revision=2&root=pastecs&pathrev=4"))
                     ),
                         tabPanel(strong("Exploratory Plots"),uiOutput('plots'))
                     )),
                 
              #),
#               ),
#                 
#               fluidRow(
                box(title = strong("Output"),solidHeader=T,status="warning",collapsible=T,width=NULL,collapsed=T,
                    tabBox(title = strong("Analysis Of Variance"),width = 14,side = "left",
                           tabPanel(strong("Anova"),
                                    fluidRow(
                                      column(width = 6,
                                        box(title=strong("Box Plot"),textOutput("anovaPlotText"),width=NULL,solidHeader=T,collapsible=T,status="primary",
                                            plotOutput("anovaPlot",width = "100%", height = "630px")),
                                        box(title = strong("Tukeyplot"),plotOutput('TukeyPlot'),width =NULL,status="primary",collapsible=T,solidHeader=T),
                                        box(title = strong(strong("Tukey HSD(Honestly Significant Difference)")),
                                            solidHeader=T,collapsible=T,width=NULL,status="primary",tableOutput('Tukey'))),
                                        column(width=6,
                                               box(title = strong("Summary"),width=NULL,solidHeader=T,collapsible=T,status="primary",
                                                   textOutput('textAnova1'),textOutput('textAnova2'),tableOutput('SummaryAnova')),
                                               box(title = "Residual Plots",width = NULL,solidHeader=T,collapsible=T,status="primary",
                                                    plotOutput('PlotAnova')),
                                                box(title = strong("Tests"),width=NULL,solidHeader=T,collapsible=T,status="primary",
                                                    strong(strong("Bartlett test of homogeneity of variances")),
                                                    h6("***There must be at least 2 observations in each group"),
                                                    tableOutput('Bartlett'),
                                                    strong(strong("Levene's Test for Homogeneity of Variance")),
                                                    h6("***Levene's test is not appropriate with quantitative explanatory variables"),
                                                    tableOutput('Levene'))
                                    )
                                      )
                                    ),
                           tabPanel(
                              strong("Manova"),
                              fluidRow(box(title=strong("Summany Manova"),collapsible=T,solidHeader=T,status="primary",
                                    uiOutput("SummaryManova")),
                                    box(title=strong("Pillai's Test"),collapsible=T,solidHeader=T,status="primary",
                                    tableOutput("PillaiManova")),
                                    box(title=strong("Wilks's Test"),collapsible=T,solidHeader=T,status="primary",
                                    tableOutput("WilksManova")),
                                    box(title=strong("Hotelling-Lawley's Test"),collapsible=T,solidHeader=T,status="primary",
                                    tableOutput("HotLawManova")),
                                    box(title=strong("Roy's Test"),collapsible=T,solidHeader=T,status="primary",
                                    tableOutput("RoyManova"))))
                           ))
                ))
      
              ),

#==============================ANOVA Ends======================================#

#=================Regression Starts====================#
      tabItem(tabName= "tabRegression",h2("Linear Regression Model") )



#==============================Regression Ends=================================#
))

dashboardPage(skin = "blue",header,sidebar,body)
