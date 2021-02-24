server <- function(input, output, session) {
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(readr)
  library(readxl)
  library(shiny)
  library(composr)
  library(stringr)
  library(shinyjs)
  library(rlang)
  library(writexl)
  shinyjs::disable("dwclog")
  shinyjs::disable("dwclean")
  shinyjs::disable("dwagg")
  shinyjs::disable("dwconv")
  shinyjs::disable("runchecks")
  shinyjs::disable("runclean")
  shinyjs::disable("runagg")
  shinyjs::disable("runconv")
  output$ui.db<-renderUI({
    if(input$operation%!in%c(NULL,"")){
      shiny::fileInput("data", "DATASET/ DONNEES (.csv or .xlsx)", accept = c(".csv",".xlsx",".xls"))
    }
  })
  output$ui.questionnaire<-renderUI({
    if(input$operation%!in%c(NULL,"")){
      shiny::fileInput("questionnaire", "Questionnaire EXCEL workbook", accept = c(".xlsx",".xls"))
    }
  })
  output$ui.clog<-renderUI({
    if(input$operation=="clean"){
      shiny::fileInput("clog", "Cleaning LOG (.csv or .xlsx)", accept = c(".csv",".xlsx",".xls"))
    }
  })
  output$ui.synthese<-renderUI({
    if(input$operation=="aggregation"){
      shiny::fileInput("synthese", "Champs synthese (.csv or .xlsx)", accept = c(".csv",".xlsx",".xls"))
    }
  })
  output$ui.slrecode<-renderUI({
    if(input$operation=="aggregation"){
      shiny::fileInput("slrecode", "Skip Logic list (.csv or .xlsx)", accept = c(".csv",".xlsx",".xls"))
    }
  })
  output$ui.admins<-renderUI({
    if(input$operation=="aggregation"){
      # req(input$data)
      req(input$questionnaire)
      selectInput("admins", "Selectionnez les variables d'agrégation dans l'ordre (admin)",choices = names(db()),multiple = TRUE)
    }
  })
  output$ui.agglabel<-renderUI({
    if(input$operation=="aggregation"){
      shiny::selectInput('agglabel', "Voulez vous labeliser les donnees agregees ?", choices = setNames(c("non","oui"),c("Non","Oui")))
    }

  })
  output$ui.pays<-renderUI({
    if(input$operation=="clog"){
      shiny::selectizeInput(
        'pays', "Pays", choices = setNames(c("niger","burkina_faso","mali"),c("Niger","Burkina Faso","Mali")),
        options = list(
          placeholder = 'Veuillez choisir une des options suivantes',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    }
  })
  output$ui.cloglabel<-renderUI({
    if(input$operation=="clog"){
      shiny::selectizeInput(
        'cloglabel', "Voulez vous labeliser les questions/reponses dans le cleaning log", choices = setNames(c("non","oui"),c("Non","Oui")),
        options = list(
          placeholder = 'Veuillez choisir une des options suivantes',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    }
  })
  db <- shiny::reactive({
    shiny::req(input$data)
    if(input$operation=="label_toxml"){
      load_file(input$data$name,input$data$datapath)%>% prepdata(.,F)
    } else {load_file(input$data$name,input$data$datapath) %>% prepdata(.,T)}
  })
  survey <- shiny::reactive({
    shiny::req(input$questionnaire)
    df<-readxl::read_excel(input$questionnaire$datapath,"survey",col_types = "text")
  })
  choices <- shiny::reactive({
    shiny::req(input$questionnaire)
    df<-readxl::read_excel(input$questionnaire$datapath,"choices",col_types = "text")
  })
  clog <- shiny::reactive({
    shiny::req(input$clog)
    load_file(input$clog$name,input$clog$datapath)
  })
  synthese <- shiny::reactive({
    shiny::req(input$synthese)
    load_file(input$synthese$name,input$synthese$datapath)
  })
  slrecode <- shiny::reactive({
    shiny::req(input$slrecode)
    load_file(input$slrecode$name,input$slrecode$datapath)
  })
  observe({
    stateoperation<-input$operation
    statedata<-input$data
    statequestionnaire<-input$questionnaire
    stateclog<-input$clog
    if(stateoperation=="clog"&!is.null(statedata)&!is.null(statequestionnaire)){enable("runchecks")} else{disable("runchecks")}
  })
  observe({
    stateoperation<-input$operation
    statedata<-input$data
    statequestionnaire<-input$questionnaire
    stateclog<-input$clog
    if(stateoperation=="clean"&!is.null(statedata)&!is.null(statequestionnaire)&!is.null(stateclog)){enable("runclean")} else{disable("runclean")}
  })
  observe({
    stateoperation<-input$operation
    statedata<-input$data
    statequestionnaire<-input$questionnaire
    statesynthse<-input$synthese
    stateslrecode<-input$slrecode
    statesadmins<-input$admins
    statesagglabel<-input$agglabel
    if(!is.null(statesadmins)&stateoperation=="aggregation"&!is.null(statedata)&!is.null(statequestionnaire)&!is.null(statesynthse)&!is.null(stateslrecode)){enable("runagg")} else{disable("runagg")}
  })
  observe({
    stateoperation<-input$operation
    statedata<-input$data
    statequestionnaire<-input$questionnaire
    if(stateoperation%in%c("label_toxml","xml_tolabel")&!is.null(statedata)&!is.null(statequestionnaire)){enable("runconv")} else{disable("runconv")}
  })
  check<-reactive({
    time_check<-survey_tonext_check(filter(db(),info_pays==input$pays))
    autre_check<-other_check(filter(db(),info_pays==input$pays),survey())
    logbook<-apply_checks(filter(db(),info_pays==input$pays),input$pays)
    clog<-bind_rows(logbook,autre_check,time_check)
    if(input$cloglabel=="oui"){clog<-label_clog(clog,survey(),choices())}
    shinyjs::enable("dwclog")
    if(input$cloglabel=="oui"){
      shinyjs::html("dwclog", "Download labeled Cleaning LOG")
    } else{shinyjs::html("dwclog", "Download Cleaning LOG")}
    clog
    
  })
  forout_clog <- reactiveValues()
  observeEvent(input$runchecks, {
    x<-check()
    forout_clog$x=x
  })
  clean<-reactive({
    db<-db()[which(!is.na(db()$uuid)),]
    db<- cleaning_data(db(),clog(),survey(),choices())
    shinyjs::enable("dwclean")
    shinyjs::html("dwclean", "Download Clean data")
    db
    
  })
  forout_clean <- reactiveValues()
  observeEvent(input$runclean, {
    x<-clean()
    forout_clean$x=x
  })
  agg<-reactive({
    db<- aggregation(db(),survey(),choices(),synthese(),input$admins,slrecode(),input$agglabel)
    shinyjs::enable("dwagg")
    shinyjs::html("dwagg", "Download Aggregated DATA")
    db
    
  })
  forout_agg <- reactiveValues()
  observeEvent(input$runagg, {
    x<-agg()
    forout_agg$x=x
  })
  conv<-reactive({
    if(input$operation=="xml_tolabel"){
      db<-from_xml_tolabel(db(),choices(),survey())
    } else {
      db<-from_label_toxml(db(),choices(),survey()) %>% sm_label_toxml(.,survey())
    }
    shinyjs::enable("dwconv")
    shinyjs::html("dwconv", "Download Converted DATA")
    db
    
  })
  forout_conv <- reactiveValues()
  observeEvent(input$runconv, {
    x<-conv()
    forout_conv$x=x
  })
  output$dwclog <- shiny::downloadHandler(
    filename = function() {
      if(input$cloglabel=="non"){
        paste0("cleaninglog-",humanTime(),".csv")
      }else {paste0("labeled-cleaninglog-",humanTime(),".csv")}
    },
    content = function(file) {
      xout<-forout_clog$x
      write.csv(xout, file, row.names = FALSE)
    }
  )
  output$dwclean <- shiny::downloadHandler(
    filename = function() {
      paste0("cleandata-",humanTime(),".csv")
    },
    content = function(file) {
      xout<-forout_clean$x
      write.csv(xout, file, row.names = FALSE)
    }
  )
  output$dwagg <- shiny::downloadHandler(
    filename = function() {
        paste0("Agg_REG_1903B_3F-",humanTime(),".csv")
    },
    content = function(file) {
      xout<-forout_agg$x
        write.csv(xout, file, row.names = FALSE)
    }
  )
  output$dwconv <- shiny::downloadHandler(
    filename = function() {
      paste0("Converted_data-",humanTime(),".xlsx")
    },
    content = function(file) {
      xout<-forout_conv$x
      write_xlsx(xout, file)
    }
  )
}