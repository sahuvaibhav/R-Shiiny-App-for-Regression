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
    tabItem(tabName = "tabCI",
            h2("Confidense Interval"),
            fluidRow(
              box(title = "Input Parameters",solidHeader = T,collapsible = F,width =4,
                  status = "primary",
                  p("Provide mean, Standard Deviation,Upper Bound and Lower Bound to plot confidence Interval on Normal Plot"),
                  numericInput("meanCI", label = h4("Mean"), value = 0),
                  numericInput("sdCI", label = h4("Standard Deviation(not Variance)"), value = 1),
                  numericInput("lbCI", label = h4("Lower Bound"), value = -1),
                  numericInput("ubCI", label = h4("Upper Bound"), value = 1)
                  
                  ),
              box(
                title = "Normal",solidHeader = T, collapsible = F,status = "primary",width = 8,
                plotOutput("mapCI")
              )
            )
    ),
    tabItem(tabName = "widgets",
            h2("Info Box"),
            fluidRow(
              infoBox("New Orders", 10*2, fill =F),
              infoBoxOutput("ProgressBox"),
              infoBoxOutput("approvalBox"),
              box(width = 4, actionButton("Count","increment Progress"))
              )
            
    )
    
  )
)

dashboardPage(skin = "black",header,sidebar,body)
