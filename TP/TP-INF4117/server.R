# Define server logic required to draw a histogram
source("storage.R")
source("helpers.R")

server <- function(input, output) {
  
  ## Loading
  df <- matrix(c("GROUP 1","WELCOME TO BDKit Tools"),ncol=2, byrow=T)
  #print(df)
  colnames(df) <- c("from", "message")
  save_messages_flatfile(df,"first")
  df <- matrix(c("Please make sure to load your dataset first ","assistive-listening-systems","primary"),ncol=3, byrow=T)
  #print(df)
  colnames(df) <- c("text", "icon","status")
  save_notifications_flatfile(df,"first")
  
  
  # pretreatment
  # _variables
  rv <- reactiveValues()
  rv$sel_c <- c(load_filenames_flatfile())
  
  # _observe
  observeEvent(input$loadDF, {
    file_to_read = input$file
    req(file_to_read)
    df <- read.csv(file_to_read$datapath, sep = input$sep, header = input$header)
    if(!(file_to_read$name %in% rv$sel_c)){
      # MatrixC <- cbind(MatrixA, c(10, 11, 12))
      
      rv$sel_c <- c(rv$sel_c,save_data_flatfile(df,file_to_read$name))
    }
  })
  
  observeEvent(input$saveSE, {
    modals = input$pretSEM
    lignes = input$pretSEL
    if(!is.null(modals) | !is.null(lignes) )rv$sel_c <- c(rv$sel_c,save_data_flatfile(custSelectDS(),paste0(input$pretSE,"selection")))
  })
  
  observeEvent(input$saveNAR, {
    if(isTRUE(input$NAP)){
      if(!is.null(NADSNAT())){
        rv$sel_c <- c(rv$sel_c,save_data_flatfile(NADSNAT(),paste0(input$pretVM,"na")))
      }else{
        showNotification("Can't save empty data set.",type = "warning")
      }
    }else{
      showNotification("Please Preview the new data set before saving.",type = "warning")
    }
  })
  
  observeEvent(input$saveDiscretizeDS, {
    if(isTRUE(input$DiscretizePC)){
      if(!is.null(dataDisc())){
        rv$sel_c <- c(rv$sel_c,save_data_flatfile(dataDisc(),paste0(input$discretizeDSselect,"discretize")))
      }else{
        showNotification("Can't save empty data set.",type = "warning")
      }
    }else{
      showNotification("Please Preview the new data set before saving.",type = "warning")
    }
  })
  
  observeEvent(input$saveBinarizeDS, {
    if(isTRUE(input$BinarizePC)){
      if(!is.null(dataBin())){
        rv$sel_c <- c(rv$sel_c,save_data_flatfile(dataBin(),paste0(input$binarizeDSselect,"binarize")))
      }else{
        showNotification("Can't save empty data set.",type = "warning")
      }
    }else{
      showNotification("Please Preview the new data set before saving.",type = "warning")
    }
  })
  
  # _reactive
  
  messages <- reactive({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    messageData <- load_messages_flatfile()
    if(!is.null(messageData)){
      
      msgs <- apply(messageData, 1, function(row) {
        messageItem(from = row[["from"]], message = row[["message"]])
      })
      
      # This is equivalent to calling:
      #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
      dropdownMenu(type = "messages", .list = msgs)
    }
  })
  
  notifications <- reactive({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    notificationData <- load_notification_flatfile()
    if(!is.null(notificationData)){
      
      msgs <- apply(notificationData, 1, function(row) {
        notificationItem(text = row[["text"]], icon(row[["icon"]]), status=row[["status"]])
      })
      
      # This is equivalent to calling:
      #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
      dropdownMenu(type = "notifications", .list = msgs)
    }
  })
  
  datasetInput0 <- reactive({
    if (!is.character(input$pretV)) {
      print(input$pretV)
      #print(rv$sel_c)
      return ()
    }
    else{
      print(!identical(input$pretV,character(1)))
      print(input$pretV)
      if(!identical(input$pretV,character(1))) load_data_flatfile(input$pretV)else{return ()}
      
    }
  }) 
  datasetInput1 <- reactive({
      if (!is.character(input$pretSU)) {
        print(input$pretSU)
        #print(rv$sel_c)
        return ()
      }
      else{
        print(!identical(input$pretSU,character(1)))
        print(input$pretSU)
        if(!identical(input$pretSU,character(1))) load_data_flatfile(input$pretSU)else{return ()}
        
      }
    
    
  })
  
  selectDS <- reactive({
    if (!is.character(input$pretSE)) {
      print(input$pretSE)
      #print(rv$sel_c)
      return ()
    }
    else{
      print(!identical(input$pretSE,character(1)))
      print(input$pretSE)
      if(!identical(input$pretSE,character(1))) load_data_flatfile(input$pretSE)else{return ()}
      
    }
    
    
  })
  
  custSelectDS <- reactive({
    modals = input$pretSEM
    ligne = input$pretSEL
    print(ligne)
    if(!is.null(modals)){
      if(!is.null(ligne)){
        print(typeof(modals))
        print(selectDS()[modals])
        selectDS()[-strtoi(ligne),modals]
      }else{
        print(typeof(modals))
        print(selectDS()[modals])
        selectDS()[modals]
      }
    }else{
      if(!is.null(ligne)){
        print(typeof(modals))
        print(selectDS()[modals])
        selectDS()[-strtoi(ligne),]
      }else{
        print(typeof(modals))
        print(selectDS()[modals])
        selectDS()
      }
    }
    
    
    
    
  })
    
  modalities <- reactive({
    if (!is.character(input$pretSE)) {
      print(input$pretSE)
      #print(rv$sel_c)
      return ()
    }
    else{
      print(!identical(input$pretSE,character(1)))
      print(input$pretSE)
      if(!identical(input$pretSE,character(1))) colnames(load_data_flatfile(input$pretSE))else{return ()}
      
    }
    
    
  })
  
  NADSrows <- reactive({
    if (!is.character(input$pretVM)) {
      print(input$pretVM)
      #print(rv$sel_c)
      return ()
    }
    else{
      print(!identical(input$pretVM,character(1)))
      print(input$pretVM)
      if(!identical(input$pretVM,character(1))) nrow(load_data_flatfile(input$pretVM))else{return ()}
      
    }
    
    
  })
  
  NADS <- reactive({
    if (!is.character(input$pretVM)) {
      print(input$pretVM)
      #print(rv$sel_c)
      return ()
    }
    else{
      print(!identical(input$pretVM,character(1)))
      print(input$pretVM)
      if(!identical(input$pretVM,character(1))) load_data_flatfile(input$pretVM)else{return ()}
      
    }
    
    
  })
  
  DiscretizeDS <- reactive({
    if (!is.character(input$discretizeDSselect)) {
      print(input$discretizeDSselect)
      #print(rv$sel_c)
      return ()
    }
    else{
      print(!identical(input$discretizeDSselect,character(1)))
      print(input$discretizeDSselect)
      if(!identical(input$discretizeDSselect,character(1))) load_data_flatfile(input$discretizeDSselect)else{return ()}
      
    }
    
    
  })
  
  BinarizeDS <- reactive({
    if (!is.character(input$binarizeDSselect)) {
      print(input$binarizeDSselect)
      #print(rv$sel_c)
      return ()
    }
    else{
      print(!identical(input$binarizeDSselect,character(1)))
      print(input$binarizeDSselect)
      if(!identical(input$binarizeDSselect,character(1))) load_data_flatfile(input$binarizeDSselect)else{return ()}
      
    }
    
    
  })
  
  BinarizeDS <- reactive({
    if (!is.character(input$binarizeDSselect)) {
      print(input$binarizeDSselect)
      #print(rv$sel_c)
      return ()
    }
    else{
      print(!identical(input$binarizeDSselect,character(1)))
      print(input$binarizeDSselect)
      if(!identical(input$binarizeDSselect,character(1))) load_data_flatfile(input$binarizeDSselect)else{return ()}
      
    }
    
    
  })
  
  filename <- reactive({
    if (!is.character(input$pretVM)) {
      print(input$pretVM)
      #print(rv$sel_c)
      return ()
    }
    else{
      print(!identical(input$pretVM,character(1)))
      print(input$pretVM)
      if(!identical(input$pretVM,character(1))) input$pretVM else{return ()}
      
    }
    
    
  })
  
  NADScols <- reactive({
    if (!is.character(input$pretVM)) {
      print(input$pretVM)
      #print(rv$sel_c)
      return ()
    }
    else{
      print(!identical(input$pretVM,character(1)))
      print(input$pretVM)
      if(!identical(input$pretVM,character(1))) ncol(load_data_flatfile(input$pretVM))else{return ()}
      
    }
    
    
  })
  
  NADSNAnb <- reactive({
    if (!is.character(input$pretVM)) {
      print(input$pretVM)
      #print(rv$sel_c)
      return ()
    }
    else{
      print(!identical(input$pretVM,character(1)))
      print(input$pretVM)
      if(!identical(input$pretVM,character(1))){
        #print(load_data_flatfile(input$pretVM)[manyNAs(load_data_flatfile(input$pretVM), nORp = 0.5),]) 
        #print(manyNAs(load_data_flatfile(input$pretVM), nORp = 0.5))
        nrow(load_data_flatfile(input$pretVM)[manyNAs(load_data_flatfile(input$pretVM), nORp = input$precisionNa),])
      }
      else{return ()}
      
    }
    
    
  })
  
    NADSNA <- reactive({
      if (!is.character(input$pretVM)) {
        print(input$pretVM)
        #print(rv$sel_c)
        return ()
      }
      else{
        print(!identical(input$pretVM,character(1)))
        print(input$pretVM)
        if(!identical(input$pretVM,character(1))){
          #print(load_data_flatfile(input$pretVM)[manyNAs(load_data_flatfile(input$pretVM), nORp = 0.5),]) 
          #print(manyNAs(load_data_flatfile(input$pretVM), nORp = 0.5))
          load_data_flatfile(input$pretVM)[manyNAs(load_data_flatfile(input$pretVM), nORp = input$precisionNa),]
        }
        else{return ()}
        
      }
    
    
  })
    
  dataDisc <- reactive({
    if(isTRUE(input$DiscretizePC)){ 
      if(identical(input$discretizeOpt,"a")){
        discretizeDF(DiscretizeDS(),default = list(method=input$discretizeMethodOpt, labels= paste("level",1:input$discretizeBreakOpt,sep=" "), breaks=input$discretizeBreakOpt))
      }else{
        if(!is.null(input$discretizeModalitiesOpt)){
          data <- DiscretizeDS()
          data[input$discretizeModalitiesOpt] <- sapply(data[input$discretizeModalitiesOpt], as.numeric)
          for(elt in input$discretizeModalitiesOpt){
            print(mode(unlist(data[elt])))
            data[elt] <- discretize(unlist(data[elt]),method=input$discretizeMethodOpt, labels = paste("level",1:input$discretizeBreakOpt,sep=" "),breaks=input$discretizeBreakOpt)
          }
          data
        }
      }
      
    }
  })
  
  dataBin <- reactive({
    if(isTRUE(input$BinarizePC)){ 
        if(!is.null(input$binarizeModalitiesOpt)){
          data <- BinarizeDS()
          for(elt in input$binarizeModalitiesOpt){
            for (item in unique(unlist(data[elt]))){
              data <- cbind(data, unlist(data[elt]) == item)
              names(data)[ncol(data)] = paste(elt,item,sep=" ")
            }
            
          }
          data
        }
      }
      
  })
    
    NADSNAT <- reactive({

      if(isTRUE(input$NAP)){ 
        #print(input$NAP)
        if(identical(input$opt,"deletion")){
          na.omit(NADS())
        }else if(identical(input$opt,"replacement")){
          if(identical(input$repl,"mean")){
            data <- NADS()
            if (is.null(input$pretNA)){
              return (matrix(c("Error: Please select variables to replace")))
            }else{
              for(elt in input$pretNA){
                # print(elt)
                # print(typeof(data[elt]))
                # print(data[elt])
                # print(colMeans(data[elt], na.rm = TRUE))
                data[is.na(data[elt]),elt] <- colMeans(data[elt], na.rm = TRUE)
              }
              data
              
            }
            
          }else if(identical(input$repl,"median")){
            data <- NADS()
            if (is.null(input$pretNA)){
              return (matrix(c("Error: Please select variables to replace")))
            }else{
              for(elt in input$pretNA){
                # print(elt)
                # print(typeof(data[elt]))
                #print(str(data[is.na(data[elt]),elt]))
                print(unlist(na.omit(data[elt])))
                # print(colMeans(data[elt], na.rm = TRUE))
                data[is.na(data[elt]),elt] <- median(as.numeric(unlist(na.omit(data[elt]))))
              }
              data
              
            }
          }else if(identical(input$repl,"sd")){
            data <- NADS()
            if (is.null(input$pretNA)){
              return (matrix(c("Error: Please select variables to replace")))
            }else{
              for(elt in input$pretNA){
                # print(elt)
                # print(typeof(data[elt]))
                # print(data[elt])
                # print(colMeans(data[elt], na.rm = TRUE))
                data[is.na(data[elt]),elt] <- sd(as.numeric(unlist(na.omit(data[elt]))))
              }
              data
              
            }
          }else if(identical(input$repl,"mode")){
            data <- NADS()
            if (is.null(input$pretNA)){
              return (matrix(c("Error: Please select variables to replace")))
            }else{
              for(elt in input$pretNA){
                # print(elt)
                # print(typeof(data[elt]))
                # print(data[elt])
                # print(colMeans(data[elt], na.rm = TRUE))
                data[is.na(data[elt]),elt] <- getmode(data[-is.na(data[elt]),elt])
              }
              data
              
            }
          }else if(identical(input$repl,"cust")){
            data <- NADS()
            if (is.null(input$pretNA) | is.null(input$replacer)){
              return (matrix(c("Error: Please select variables to replace")))
            }else{
              for(elt in input$pretNA){
                # print(elt)
                # print(typeof(data[elt]))
                # print(data[elt])
                # print(colMeans(data[elt], na.rm = TRUE))
                data[is.na(data[elt]),elt] <- input$replacer
              }
              data
              
            }
          }else{
            data <- NADS()
            if (is.null(input$pretNA)){
              return (matrix(c("Error: Please select variables to replace")))
            }else{
              for(elt in input$pretNA){
                # print(elt)
                # print(typeof(data[elt]))
                print(str(data[elt]))
                # print(colMeans(data[elt], na.rm = TRUE))
                data[is.na(data[elt]),elt] <- cor(data[,input$pretNA],use = "complete.obs")
              }
              data
              
            }
          }
        }else{
          if(identical(input$reg,"cor")){
            data <- NADS()
            if (is.null(input$pretNA)){
              return (matrix(c("Error: Please select variables to replace")))
            }else{
              for(elt in input$pretNA){
                # print(elt)
                # print(typeof(data[elt]))
                # print(data[elt])
                # print(colMeans(data[elt], na.rm = TRUE))
                data[is.na(data[elt]),elt] <- sd(as.numeric(unlist(na.omit(data[elt]))))
              }
              data
              
            }
          }else{
            data <- NADS()
            if (is.null(input$pretNA)){
              return (matrix(c("Error: Please select variables to replace")))
            }else{
              for(elt in input$pretNA){
                # print(elt)
                # print(typeof(data[elt]))
                # print(data[elt])
                # print(colMeans(data[elt], na.rm = TRUE))
                data[is.na(data[elt]),elt] <- knnImputation(data[elt], k=10)
              }
              data
              
            }
          }
        }
      }
      
    })
  
  # _rendering
  output$nbROW <- renderText({
    NADSrows()
  })
  output$nbCOL <- renderText({
    NADScols()
  })
  output$nbNA <- renderText({
    NADSNAnb()
  })
  output$filename <- renderText({
    filename()
  })
  output$pretV <- renderUI({
    selectInput(
      "pretV", "Select your data frame to visualize", rv$sel_c, selected = rev(rv$sel_c)[1]
    )
  })
  output$pretSU <- renderUI({
    selectInput(
      "pretSU", "Select your data frame to summarize", rv$sel_c, selected = rev(rv$sel_c)[1]
    )
  })
  output$pretSE <- renderUI({
      selectInput(
        "pretSE", "Select your data frame to selectize", rv$sel_c, selected = rev(rv$sel_c)[1]
      )
  })
  output$discretizeDS <- renderUI({
    selectInput(
      "discretizeDSselect", "Select your data frame to discretize", rv$sel_c, selected = rev(rv$sel_c)[1]
    )
  })
  output$binarizeDS <- renderUI({
    selectInput(
      "binarizeDSselect", "Select your data frame to binarize", rv$sel_c, selected = rev(rv$sel_c)[1]
    )
  })
  output$pretSEM <- renderUI({
    selectInput(
      "pretSEM", "Select modalities or variables to save", modalities(),multiple = TRUE
    )
  })
  output$pretNA <- renderUI({
    selectInput(
      "pretNA", "Select modalities or variables", colnames(NADS()),multiple = TRUE
    )
  })
  output$discretizeMOpt <- renderUI({
    selectInput(
      "discretizeModalitiesOpt", "Select specific modalities or variables", colnames(DiscretizeDS()),multiple = TRUE
    )
  })
  output$binarizeMOpt <- renderUI({
    selectInput(
      "binarizeModalitiesOpt", "Select specific modalities or variables", colnames(BinarizeDS()),multiple = TRUE
    )
  })
  output$pretSEL <- renderUI({
    selectInput(
      "pretSEL", "Select lines to loss save", 1:nrow(selectDS()),multiple = TRUE
    )
  })
  output$pretVM <- renderUI({
    selectInput(
      "pretVM", "Select your data frame to replace NA value", rv$sel_c, selected = rev(rv$sel_c)[1]
    )
  })
  output$input_file <- renderDT({
    datasetInput0()
    #rv$currentDF
  })
  output$NADS <- renderDT({
    #print(input$NAP)
    if(isTRUE(input$NAP)){ 
      NADSNAT()
    }
    else{
      NADSNA()
    }
    
    #rv$currentDF
  })
  
  output$DDS <- renderDT({
    dataDisc()
  })
  
  output$BDS <- renderDT({
    dataBin()
  })
  
  output$SEDS <- renderDT({
    custSelectDS()
    #rv$currentDF
  })
  
  output$pretsu <- renderPrint({
    summary(datasetInput1())
    #rv$currentDF
  })
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


  
  # Header
  output$messageMenu <- renderMenu({
    messages()
  })
  
  output$notificationMenu <- renderMenu({
    notifications()
  })
  
  
  # Rules Mining
  # _variables
  # _observe
  # _reactive
  RMDS <- reactive({
    if (!is.character(input$pretRM)) {
      print(input$pretRM)
      #print(rv$sel_c)
      return ()
    }
    else{
      print(!identical(input$pretRM,character(1)))
      print(input$pretRM)
      if(!identical(input$pretRM,character(1))) load_data_flatfile(input$pretRM)else{return ()}
      
    }
    
    
  })
  
  custSelectDSRM <- reactive({
    rmodals = input$pretRMSR
    lmodals = input$pretRMSL
    # #print(data.matrix(RMDS()))
    # train <- sapply(RMDS(),as.factor)
    # train <- data.frame(train, check.names=FALSE)
    trans <- as(RMDS() ==1,"transactions")
    inspect(trans)
    #data <- as(data.matrix(RMDS()),"transactions")
    #print(data)
    if(!is.null(rmodals)){
      if(!is.null(lmodals)){
        rules <- apriori(trans,parameter = list(supp = input$minsupp, conf = input$minconf, target = input$selectRMST), appearance = list(rhs = rmodals, lhs=lmodals))
      } else{
        rules <- apriori(trans,parameter = list(supp = input$minsupp, conf = input$minconf, target = input$selectRMST), appearance = list(rhs = rmodals))
      }
      inspect(rules)

        
    }else if(!is.null(lmodals)){
      if(!is.null(rmodals)){
        rules <- apriori(trans,parameter = list(supp = input$minsupp, conf = input$minconf, target = input$selectRMST), appearance = list(rhs = rmodals, lhs=lmodals))
      } else{
        rules <- apriori(trans,parameter = list(supp = input$minsupp, conf = input$minconf, target = input$selectRMST), appearance = list(lhs = lmodals))
      }
      inspect(rules)
      
      
    }else if(is.null(lmodals) & is.null(rmodals) ){
      rules <- apriori(trans,parameter = list(supp = input$minsupp, conf = input$minconf, target = input$selectRMST))
      inspect(rules)
    }
    #extract <- apriori()
    #trans
    
    
  })
  # _rendering
  output$pretRM <- renderUI({
    selectInput(
      "pretRM", "Select your data frame to extract rules", rv$sel_c, selected = rev(rv$sel_c)[1]
    )
  })
  output$pretRMSMR <- renderUI({
    selectInput(
      "pretRMSR", "Select modalities in right part", colnames(RMDS()),multiple = TRUE
    )
  })
  output$pretRMSML <- renderUI({
    selectInput(
      "pretRMSL", "Select modalities in left part", colnames(RMDS()),multiple = TRUE
    )
  })
  output$pretRMST <- renderUI({
    selectInput(
      "selectRMST", "Select the target of your rules mining", c("rules","frequent itemsets","maximally frequent itemsets","closed frequent itemsets","hyperedgesets")
    )
  })
  output$RMDS <- renderDT({
    RMDS()
  })
  
  output$RMDSresult <- renderDT({
    custSelectDSRM()
  })
  
  output$rules <- renderPlot({
    plot(head(custSelectDSRM()[c("rhs","lhs")]))
  })
  
  
  
  
  # Classification
  # _variables
  # _observe
  observeEvent(input$bt, {
    data <- CLADS()
    V = sample(nrow(data), 2*nrow(data)/3)
    train = data[V,]
    test = data[-V,]
    output$DecTree <- renderPlot({
      print("inside plot")
      mytree <- rpart(Place.names ~ A+C+E+H+N+SP, data = train ,method="class")
      print(mytree)
      rpart.plot(mytree)
    })
    output$TEST <- renderDT({
      print("inside test")
      test
    })
    output$TRAIN <- renderDT({
      print("inside train")
      train
    })
  })
  # _reactive
  CLADS <- reactive({
    if (!is.character(input$pretCLA)) {
      print(input$pretCLA)
      #print(rv$sel_c)
      return ()
    }
    else{
      print(!identical(input$pretCLA,character(1)))
      print(input$pretCLA)
      if(!identical(input$pretCLA,character(1))) load_data_flatfile(input$pretCLA)else{return ()}
      
    }
    
    
  })
  # _rendering
  # output$CLASSM <- renderUI({
  #   selectInput(
  #     "CLASSN", "Select class modalities to predict", colnames(CLADS()),multiple = FALSE
  #   )
  # })
  output$pretCLA <- renderUI({
    selectInput(
      "pretCLA", "Select your data frame to classify", rv$sel_c, selected = rev(rv$sel_c)[1]
    )
  })
  output$CLASS <- renderDT({
    CLADS()
  })
  
  
  
  
  
  # plot
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  
  # Saving and loading data 
  # text file local storage
  # results_dir <- "responses"
  # ## save
  # save_data_flatfile <- function(data) {
  #   data <- t(data)
  #   file_name <- paste0(paste(get_time_human(), digest(data,algo = "md5"), sep ="_"), ".csv")
  #   write.csv(x = data, file = file.path(results_dir, file_name),row.names = FALSE, quote = TRUE)
  # }
  # ## load
  # load_data_flatfile <- function() {
  #   files <- list.files(file.path(results_dir), full.names = TRUE)
  #   data <- lapply(files, read.csv, stringsAsFactors = FALSE) %>% 
  #     do.call(rbind, .)
  #   data
  # }
  # sqlite local storage
  # mysql local and remote storage
  # mongodb local and remote storage
  # google sheet remote storage
  # dropbox remote storage 
  # aws remote storage
}