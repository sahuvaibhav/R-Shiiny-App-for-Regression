header =   dashboardHeader(title = "Basic Header",
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
    menuItem("Dashboard", tabName = "dashBoard",icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("life-sing"),badgeLabel = "New",badgeColor = "green"),
    menuItem("Source Code", icon = icon("file-code-o"), href= "https://github.com/rstudio/shinydashboard/"),
    menuItemOutput("MenuItem")
    ),
  sidebarSearchForm(textId = "searchText", label = "Search...", buttonId = "SearchButton")
  
  
)

body = dashboardBody(
  tabItems(
    tabItem(tabName = "dashBoard",
            h2("MY Dashboard"),
            fluidRow(
              box(title = "Histogram",solidHeader = T,collapsible = T,
                  status = "primary",background = "red", plotOutput("plot1",height=250)),
              box(
                title = "controls",solidHeader = T, collapsible = T,status = "warning",background = "maroon",
                sliderInput("slider","Number of obs",1,100,50),
                textInput("Text","Text Input")
              )
            )
    ),
    tabItem(tabName = "widgets",
            h2("widget tab contents")
            
    )
    
  )
)

dashboardPage(header,sidebar,body)
