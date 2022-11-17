#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(DT)
library(shiny)
library(shinyjqui)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "GROUP 1"),
  dashboardSidebar(
    #minified = TRUE,
    sidebarMenu(
      menuItem(
        text = "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      menuItem(
        text = "Data Pretreatment",
        tabName = "pretreatment",
        icon = icon("gear")
      ),
      menuItem(
        text = "Rules Mining",
        tabName = "rules",
        icon = icon("exchange")
      ),
      menuItem(
        text = "Classification",
        tabName = "classification",
        icon = icon("list-ol")
      ),
      menuItem(
        text = "Clustering",
        tabName = "clustering",
        icon = icon("list")
      )
    )),
  dashboardBody(
    ## sidebare management for icon minimized preview
    tags$style(
      '
      @media (min-width: 768px){
      .sidebar-mini.sidebar-collapse .main-header .logo {
      width: 230px; 
      }
      .sidebar-mini.sidebar-collapse .main-header .navbar {
      margin-left: 230px;
      }
      }
      '
    ),
    
    tabItems(
      
      # 
      # pretreatment view
      # 
      # 
      
      tabItem(
        tabName = "pretreatment",
        fluidRow(
          tabBox(
            width = 12, 
            # Title can include an icon
            title = tagList(shiny::icon("gear"), "Data Pretreatment"),
            tabPanel(title = tagList(shiny::icon("upload"), "Data Loading"),
                     fileInput("file","upload the file",multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                     h5("Max file size to upload is 5 MB"),
                     radioButtons("sep","separator", choices = c(Comma = ',', Semicolon = ';', Period = '.', Tilde = '~', minus = '-', Tab = "\t")),
                     checkboxInput("header","header?")
            ),
            tabPanel(title = tagList(shiny::icon("gear"), "Data pretreatment"), "Tab content 2")
          )
        ),
        fluidRow(
          box(
            width=12,
            title = "data Visualisation", 
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            div(style = "overflow-x: scroll", DT::dataTableOutput("input_file"))
            #tableOutput("input_file")
          )
        ),
        fluidRow(
          box(
            width=12,
            title = "Summary", 
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE
            #tableOutput("input_file")
          )
        )
        
      ),
      
      # 
      # dashboard view
      # 
      # 
      
      tabItem(
        tabName = "dashboard",
        fluidRow(
          # A static infoBox
          infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
          # Dynamic infoBoxes
          infoBoxOutput("progressBox"),
          infoBoxOutput("approvalBox")
        ),
        
        # infoBoxes with fill=TRUE
        fluidRow(
          infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
          infoBoxOutput("progressBox2"),
          infoBoxOutput("approvalBox2")
        ),
        
        fluidRow(
          # Clicking this will increment the progress amount
          box(width = 4, actionButton("count", "Increment progress"))
        )
      ),
      tabItem(
        tabName = "other",
        fluidRow(
          box(
            width=12,
            title = "title", 
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30
            )
          ),
          box(
            width=12,
            title = "customize",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("distPlot")
          )
        )
      )
    )
    
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # _variables
  VecDF <- c()
  currentPT <- c()
  currentCLAS <- c()
  currentCLUS <- c()
  
  
  
  # dashboard
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  # Same as above, but with fill=TRUE
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  
  
  # plot
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  
  # file reader
  output$input_file <- renderDT({
    file_to_read = input$file
    if(is.null(file_to_read)){
      return ()
    }
    
    df <- read.csv(file_to_read$datapath, sep = input$sep, header = input$header)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

