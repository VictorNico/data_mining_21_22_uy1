library(shiny)
library(shinydashboard)
library(shinydashboard)
library(dplyr)
library(plyr)
library(shiny)
 library(tidyverse)
library(tm)
library(tokenizers)
anyLib::anyLib(c("shiny", "shinydashboard", "shinyWidgets", "DT", "plotly", 
                 "ggplot2", "googleVis", "colourpicker"))
library(funModeling)
library(data.table)

library(skimr)


# ui
ui <- shinyUI(
  dashboardPage(
    # header
    dashboardHeader(title="TP GROUPE 3"),
    
    # sidebar
    dashboardSidebar(
      fileInput("file","Upload CSV files",multiple=TRUE,accept=("text/comma")
      ),
      # sidebar menu
      sidebarMenu(                  
        # option
        menuItem(text = "Data", tabName="Data", icon=icon("table"), startExpanded = TRUE
        ),
                  
        # option
        menuItem("Descriptive Analysis", tabName="Feedback Analysis",
                           
          # input
          selectInput(inputId = "Question",
            label = h2("Valider Pour avoir le résumé"),
            choices="",
            selected=""
          ), 
            # button
            actionButton(inputId = "Go",label = "Go")
        )
                 
      )
    ),
    
    # body
    dashboardBody(
      # tab 1: data
      tabItem(tabName="Data",
        tabsetPanel(id="Data",
          # affichage de jeux de données 
          tabPanel("Data Set",  column(12, dataTableOutput('data.frame'))
          ),
          # affichage de résumé de données
          #tabPanel("summary", tableOutput("sum")
          #),
          # Etat des données : Nombre des NA par colonne, Nombre total de ligne, Nombre Total de ligne sans NA
          tabPanel("summary",
            fluidRow(
              #Nombre total de lignes
              #verbatimTextOutput("Number_row"),
              #Nombre des NA par colonnes
              verbatimTextOutput("Summary"),
              #Nombre des lignes sans NA
              verbatimTextOutput("Name")
            )
          ),
          tabPanel("Selection_Reduction", 
            fluidRow(
              column(12,
                dataTableOutput('Selection_Reduction')
              )
            )
          ),
          tabPanel("Discretize", column(12, dataTableOutput('Discretize'))
         )
        )
    
      )
    )
  )
)



#partie serveur
server <- shinyServer(function(input,output,session){

  # read file
  values <<- reactive({ read.csv(input$file$datapath, na.strings=c("?", "NA")) 
  })
  
  # observeEvent for input$file
  observeEvent(input$file, {
    
    # render tables at file upload
    output$data.frame <- DT::renderDataTable(values(), options = list(scrollX = TRUE)
    )
    # update selectInput#Question
    updateSelectInput(
      session, 
      inputId = "Question",
      choices=names(values()),    
      selected=names(values())[1]
    )
    
  })

    # valider pour afficher le résumé de données
    observeEvent(input$Go, {
        do = values()
        #Nombre de ligne de jeux de données
        #output$Summary <- renderPrint({
          #skim(do)
          #nrow(do)
        #})

        #Nombre de NA par Variable
        output$Summary <- renderPrint({
          #colSums(is.na(do))
          skim(do)
        })

        #Nombre de ligne sans NA
        output$Name <- renderPrint({
          #nrow(na.omit(do))
          names(do)
        })
 
        #data_new = do[,!sapply(do, function(x) mean(is.na(x)))>0.5]
        # supprimer les colonnnes qui ont plus de 50% des NA et les lignes qui ont plus de 40% des NA
        # data_new contient les données néttoyées, sélectionnées et réduite
        data_new = do[which(rowMeans(!is.na(do)) > 0.42), which(colMeans(!is.na(do)) > 0.5)]
    
        # Les opérations de Nettoyage, Sélection et de Réduction
        output$Selection_Reduction <- renderDataTable(data_new,
          options = list(
            pageLength = 5,
            scrollX = TRUE,
            initComplete = I("function(settings, json) {alert('Done.');}")
          )
        )
    
        
        
        # la fonction qui renvoit le mode d'un vecteur
        getmode <- function(v){
          v=v[nchar(as.character(v))>0]
          uniqv <- unique(v)
          uniqv[which.max(tabulate(match(v, uniqv)))]
        }

        #la boucle qui parcours le dataFrame et remplace soit par la moyenne ou par le mode
         for (cols in colnames(data_new)) {
            if (cols %in% names(data_new[,sapply(data_new, is.numeric)])) {
              data_new<-data_new%>%mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), mean(!!rlang::sym(cols), na.rm=TRUE)))
            }
            else 
            {
              data_new<-data_new%>%mutate(!!cols := replace(!!rlang::sym(cols), !!rlang::sym(cols)=="NA", getmode(!!rlang::sym(cols))))
            }     
          }  
    })

   
      #output$Discretize <- renderTable({
        #Discretize_Data = do %>% mutate_if(is.numeric, function(x) if(sum(x)>0){x/sum(x)}else{0})
        #})
        #Discretize_Data = do %>% mutate_if(is.numeric, function(x) if(sum(x)>0) x/sum(x))
        #Discretize_Data %>% select_if(is.numeric) %>% colSums()
        #do %>% 
         #Discretize_Data = mutate_if(~ is.numeric(.) && sum(.) != 0, ~ ./sum(.))
        min_max_norm <- function(x) {
           (x - min(x)) / (max(x) - min(x))
          }
      
        output$Discretize <- renderDataTable(
          #do %>%
          #mutate_if(~ is.numeric(.) && sum(.) != 0, ~ ./sum(.))
          #do %>% mutate_if(is.numeric, function(x) if(sum(x)>0){x/sum(x)}else{0})
          min_max_norm(do[10:12])
           #normalize(do)
         
        )

})

shinyApp(ui, server)