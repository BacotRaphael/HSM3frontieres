ui <- shiny::fluidPage(
    
    shinyjs::useShinyjs(),
    shiny::titlePanel("3 Frontieres (Burkina Faso / Mali / Niger)"),
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::selectizeInput(
                'operation', "Quelle type d'operation voulez vous effectuer:", choices = setNames(c("clog","clean"),c("Produire un Cleaning Log","Nettoyer des donnees")),
                options = list(
                    placeholder = 'Veuillez choisir une des options suivantes',
                    onInitialize = I('function() { this.setValue(""); }')
                )
            ),
            shiny::tags$hr(),
            shiny::uiOutput("ui.cloglabel"),
            shiny::tags$hr(),
            shiny::uiOutput("ui.db"),
            shiny::tags$hr(),
            shiny::uiOutput("ui.questionnaire"),
            shiny::tags$hr(),
            shiny::uiOutput("ui.clog"),
            shiny::tags$hr(),
            shiny::actionButton("runchecks","RUN checks"),
            shiny::actionButton("runclean","CLEAN data"),
            shiny::tags$hr(),
            shiny::downloadButton("dwclog"),
            shiny::downloadButton("dwclean"),
            shiny::helpText("Download will be available once the processing is completed.")
            )
        ,
        shiny::mainPanel(
            textOutput("state")
        )
    )
)

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
    shinyjs::disable("dwclog")
    shinyjs::disable("dwclean")
    shinyjs::disable("runchecks")
    shinyjs::disable("runclean")
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
            shiny::fileInput("clog", "Cleaning LOG", accept = c(".csv",".xlsx",".xls"))
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
        load_file(input$data$name,input$data$datapath) %>% prepdata()
    })
    survey <- shiny::reactive({
        shiny::req(input$questionnaire)
        df<-readxl::read_excel(input$questionnaire$datapath,1,col_types = "text")
    })
    choices <- shiny::reactive({
        shiny::req(input$questionnaire)
        df<-readxl::read_excel(input$questionnaire$datapath,2,col_types = "text")
    })
    clog <- shiny::reactive({
        shiny::req(input$clog)
        load_file(input$clog$name,input$clog$datapath)
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
    output$state<-renderText(paste(is.null(input$data)))
    check<-reactive({
        time_check<-survey_tonext_check(db())
        autre_check<-other_check(db(),survey())
        logbook<-apply_checks(db())
        clog<-bind_rows(logbook,time_check,autre_check)
        if(input$cloglabel=="oui"){clog<-label_clog(clog,survey(),choices())}
        shinyjs::enable("dwclog")
        shinyjs::html("dwclog", "Download Cleaning LOG")
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
}

H2R_HSM<-function(...){
    options(shiny.maxRequestSize =200 * 1024^2)
    shiny::shinyApp(ui, server,...)
}
