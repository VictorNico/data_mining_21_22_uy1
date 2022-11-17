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
library(DMwR)
library(arules)
library(arulesViz)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
# Define UI for application that draws a histogram
shinyUI(
  dashboardPagePlus(
    dashboardHeaderPlus(title = tagList(shiny::icon("cogs"), "BDKit Tools DM"),
                        dropdownMenuOutput("messageMenu"),
                        dropdownMenuOutput("notificationMenu")
                        ),
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
                       checkboxInput("header","header?"),
                       actionBttn("loadDF","Load the dataset with the above parameters")
              ),
              tabPanel(title = tagList(shiny::icon("eraser"), "Data Selection"),
                       fluidRow(
                         box(width = 6,status = "primary",
                             uiOutput("pretSE"),
                             uiOutput("pretSEM"),
                             uiOutput("pretSEL"),
                             actionBttn("saveSE","save dataset reduiced to selected variables")
                         ),
                         box(
                           width=6,
                           title = "Selected data Visualisation", 
                           #status = "primary",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           div(style = "overflow-x: scroll", DT::dataTableOutput("SEDS"))
                           #tableOutput("input_file")
                         )
                         # box(width = 8,status = "primary",
                         #     actionBttn("saveSE","save dataset reduice to selected variables")
                         # )
                       )
               ),
              tabPanel(title = tagList(shiny::icon("eraser"), "Treatment of Missing Values"), 
                       fluidRow(
                         box(width = 6,status = "primary",
                             uiOutput("pretVM"),
                             sliderInput("precisionNa",
                                         "row is considered to have too many NA values (defaults to 0.2, i.e. 20% of the columns):",
                                         min = 0.1,
                                         max = 0.9,
                                         value = 0.2),
                             hr(
                               h2('details'),br(),
                               h3('filename:'),textOutput('filename'),br(),
                               h3("number of row:"),textOutput('nbROW'),br(),
                               h3("number of col:"),textOutput('nbCOL'),br(),
                               h3("number of NA lines:"),textOutput('nbNA'),br()
                               
                               ),
                             radioButtons(
                               inputId = "opt",
                               label = "Which type of NA Treatment will like you do?",
                               choiceNames = list(
                                 tagList(shiny::icon("trash"), HTML("<font color='red'>NA Deletion</font>")),
                                 tagList(shiny::icon("edit"), tags$span(style = "color:blue", "NA Replacement")),
                                 tagList(shiny::icon("backward"), tags$span(style = "color:green", "NA Regression"))
                                 
                               ),
                               choiceValues = c("deletion","replacement","regression")
                             ),
                             conditionalPanel(
                               condition = "input.opt == 'deletion'"
                               ),
                               conditionalPanel(
                                 condition = "input.opt == 'replacement'",
                                 radioButtons(
                                   inputId = "repl",
                                   label = "Which type of NA Replacement will like you do?",
                                   choiceNames = list(
                                     tagList(shiny::icon("edit"), HTML("<font color='blue'>mean</font>")),
                                     tagList(shiny::icon("edit"), tags$span(style = "color:blue", "median")),
                                     tagList(shiny::icon("edit"), tags$span(style = "color:blue", "correlation")),
                                     tagList(shiny::icon("edit"), tags$span(style = "color:blue", "mode")),
                                     tagList(shiny::icon("edit"), tags$span(style = "color:blue", "standard deviation")),
                                     tagList(shiny::icon("edit"), tags$span(style = "color:blue", "customize"))
                                     
                                   ),
                                   choiceValues = c("mean","median","cor","mode","sd","cust")
                                 )
                             ),
                             conditionalPanel(
                               condition = "input.repl == 'cust'",
                               textInput("replacer",label = "Replacer",placeholder = "hit replacer value")
                             ),
                             conditionalPanel(
                               condition = "input.opt == 'regression'",
                               radioButtons(
                                 inputId = "reg",
                                 label = "Which type of NA regression will like you do?",
                                 choiceNames = list(
                                   tagList(shiny::icon("backward"), HTML("<font color='green'>calcul of correlation coefficients</font>")),
                                   tagList(shiny::icon("backward"), tags$span(style = "color:green", "Using similarities"))
                                   
                                 ),
                                 choiceValues = c("cor","similarity")
                               )
                             ),
                             uiOutput("pretNA"),
                             checkboxInput("NAP", "will you like to preview the new DataSet that the above operation will produce?", FALSE),
                             actionBttn("saveNAR","save the new dataset")
                             
                             
                         ),
                         box(
                           width=6,
                           title = "NA data Visualisation", 
                           #status = "primary",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           div(style = "overflow-x: scroll", DT::dataTableOutput("NADS"))
                           #tableOutput("input_file")
                         )
                         # box(width = 8,status = "primary",
                         #     actionBttn("saveSE","save dataset reduice to selected variables")
                         # )
                       )
                       ),
              tabPanel(title = tagList(shiny::icon("cut"), "Data Transformation"), 
                fluidRow(
                  box(
                    width = 6,
                    title = "Discretization Paremeter's", 
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    uiOutput("discretizeDS"),
                    radioButtons("discretizeOpt","Which type of discretization will you like to do?", choices = c('Specific Numeric Modalities' = 's', 'All Numeric Modalities' = 'a')),
                    sliderInput("discretizeBreakOpt",
                                "which break indice support will you use?",
                                min = 2,
                                max = 10,
                                value = 2),
                    selectInput("discretizeMethodOpt","Select the discretization method",c("interval","frequency")),
                    conditionalPanel(
                      "input.discretizeOpt == 's'",
                      uiOutput("discretizeMOpt")
                    ),
                    checkboxInput("DiscretizePC", "will you like to preview the new DataSet that the above operation will produce?", FALSE),
                    actionBttn("saveDiscretizeDS","save the new dataset")
                  ),
                  box(
                    width = 6,
                    title = "Discretization's Preview", 
                    #status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    div(style = "overflow-x: scroll", DT::dataTableOutput("DDS"))
                  )
                ),
                fluidRow(
                  box(
                    width = 6,
                    title = "Binarization Paremeter's", 
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    uiOutput("binarizeDS"),
                    uiOutput("binarizeMOpt"),
                    checkboxInput("BinarizePC", "will you like to preview the new DataSet that the above operation will produce?", FALSE),
                    actionBttn("saveBinarizeDS","save the new dataset")
                    
                  ),
                  box(
                    width = 6,
                    title = "Binarization's Preview", 
                    #status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    div(style = "overflow-x: scroll", DT::dataTableOutput("BDS"))
                  )
                )   
               )
            )
          ),
          fluidRow(
            box(
              width=12,
              title = "data Visualisation", 
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              uiOutput("pretV"),
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
              collapsible = TRUE,
              uiOutput("pretSU"),
              verbatimTextOutput("pretsu")
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
        
        # 
        # rules mining view
        # 
        # 
        
        tabItem(
          tabName = "rules",
          fluidRow(
            box(
                title = "Parameter", 
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 4,
                sliderInput("minsupp",
                            "which indice support will you use?",
                            min = 0.01,
                            max = 1.00,
                            value = 0.60),
                sliderInput("minconf",
                            "which confiance will you accept?",
                            min = 0.01,
                            max = 1.00,
                            value = 0.80),
                uiOutput("pretRMSMR"),
                uiOutput("pretRMSML"),
                uiOutput("pretRMST")
            ),
            box(
              title = "data loader", 
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 8,
              uiOutput("pretRM"),
              div(style = "overflow-x: scroll", DT::dataTableOutput("RMDS"))
            )
            
          ),
          fluidRow(
            box(
              title = "result rules mining", 
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              div(style = "overflow-x: scroll", DT::dataTableOutput("RMDSresult")),
              div(style = "overflow-x: scroll; overflow-y: scroll", plotOutput("rules"))
            )
          )
          
        ),
        
        # 
        # Classification view
        # 
        # 
        
        tabItem(
          tabName = "classification",
          fluidRow(
            box(
              title = "Parameter", 
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 4,
              # uiOutput("CLASSM"),
              actionBttn("bt","build tree"),
              checkboxInput("showTr", "will you like to preview the train DataSet ?", FALSE),
              checkboxInput("showTe", "will you like to preview the test DataSet ?", FALSE),
              checkboxInput("showTR", "will you like to preview the decision tree build on train DataSet ?", FALSE),
              checkboxInput("showCM", "will you like to preview the confusion matrix of test DataSet ?", FALSE)
              # uiOutput("pretRMSML"),
              # uiOutput("pretRMST")
            ),
            box(
              title = "Classification data loader", 
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 8,
              uiOutput("pretCLA"),
              div(style = "overflow-x: scroll", DT::dataTableOutput("CLASS"))
            )
            
          ),
          fluidRow(
            conditionalPanel("input.showTe",
            box(
              title = "Test Dataset Visualization", 
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              # width = 6,
              div(style = "overflow-x: scroll", DT::dataTableOutput("TEST"))
            )),
            conditionalPanel("input.showTr",
            box(
              title = "Train Dataset Visualization", 
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              # width = 6,
              div(style = "overflow-x: scroll", DT::dataTableOutput("TRAIN"))
            ))
          ),
          fluidRow(
            conditionalPanel("input.showTR",
            box(
              title = "Tree Visualization", 
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              # div(style = "overflow-x: scroll", DT::dataTableOutput("RMDSresult")),
              div(style = "overflow-x: scroll; overflow-y:scroll", plotOutput("DecTree"))
            ))
          ),
          fluidRow(
            conditionalPanel("input.showCM",
                             box(
                               title = "Confusion Matrix Visualization", 
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               width = 12
                               # div(style = "overflow-x: scroll", DT::dataTableOutput("RMDSresult")),
                               # div(style = "overflow-x: scroll; overflow-y: scroll", plotOutput("rules"))
                             ))
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
                          value = 30)
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
)
