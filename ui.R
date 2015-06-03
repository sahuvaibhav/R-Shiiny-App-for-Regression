header =   dashboardHeader(title = "Stats",
                           dropdownMenu(type = "messages",
                                        messageItem(
                                          from = "sales-dept",message = "sales ready for this month"),
                                        messageItem(
                                          from ="new user", message = "Register", time = "12:45", icon = icon("question")),
                                        messageItem(
                                          from= "support", message = "how can i help u?", icon = icon("life-ring"), time = "13-12-2015")
                                        
                           ))


sidebar = dashboardSidebar(
  sidebarUserPanel("User Name", 
                   subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                   image = "C:/Users/Vaibhav/Desktop/Test/StatsDashBoard/fetchImage.jpg"),
  sidebarMenu(
    menuItem("Confidence Interval", tabName = "tabCI",icon = icon("area-chart")),
    menuItem("Hypothesis Testing", tabName = "tabHT", icon = icon("area-chart"),badgeLabel = "New",badgeColor = "green"),
    menuItem("Hypothesis Testing for 2 Populations", tabName = "tabHT2",icon = icon("area-chart")),
    menuItem("Anova", tabName = "tabAnova",icon = icon("area-chart")),
    menuItem("Regression", tabName = "tabRegression",icon = icon("line-chart")),
    menuItemOutput("MenuItem")
    ),
  sidebarSearchForm(textId = "searchText", label = "Search...", buttonId = "SearchButton")
  
  
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
              box(title = "Parameter for Hypothesis Testing",solidHeader = T, collapsible =T, width=4,height=800,status = "primary",
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
              box(title = "Hypothesis Testing", solidHeader = T, collapsible = T, status = "primary", width = 8, height = 800,
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
      )

#=================Hypothesis Testing for 2 populations Ends====================#
 )
))

dashboardPage(skin = "black",header,sidebar,body)
