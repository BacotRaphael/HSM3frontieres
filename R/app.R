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
            shiny::downloadButton("dwoutput"),
            # shiny::downloadButton("dwclean"),
            shiny::helpText("Download will be available once the processing is completed.")
            )
        ,
        shiny::mainPanel(
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
    shinyjs::disable("dwoutput")
    # shinyjs::disable("dwclean")
    shinyjs::disable("runchecks")
    shinyjs::disable("runclean")
    output$ui.db<-renderUI({
        if(input$operation%!in%c(NULL,"")){
            shiny::fileInput("db", "DATASET/ DONNEES (.csv or .xlsx)", accept = c(".csv",".xlsx",".xls"))
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
        shiny::req(input$db)
        load_file(input$db$name,input$db$datapath) %>% prepdata()
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
        if(!is.null(db())&!is.null(survey())&!is.null(choices())&input$operation=="clog"&&input$cloglabel!=""){
            enable("runchecks")
        } else {disable("runchecks")}
    })
    observe({
        if(!is.null(db())&!is.null(survey())&!is.null(choices())&!is.null(clog())&input$operation=="clean"){
            enable("runclean")
        } else {disable("runclean")}
    })
    check<-reactive({
        shinyjs::disable("dwoutput")
        time_check<-survey_tonext_check(db())
        autre_check<-other_check(db(),survey())
        logbook<-apply_checks(db())
        clog<-bind_rows(logbook,time_check,autre_check)
        if(input$cloglabel=="oui"){clog<-label_clog(clog,survey(),choices())}
        shinyjs::enable("dwoutput")
        shinyjs::html("dwoutput", "Download Cleaning LOG")
        clog
        
    })
    forout_clog <- reactiveValues()
    observeEvent(input$runchecks, {
        x<-check()
        forout_clog$x=x
    })
    clean<-reactive({
        shinyjs::disable("dwoutput")
        db<-db()[which(!is.na(db()$uuid)),]
        db<- cleaning_data(db(),clog(),survey(),choices())
        shinyjs::enable("dwoutput")
        shinyjs::html("dwoutput", "Download Clean data")
        db

    })
    forout_clean <- reactiveValues()
    observeEvent(input$runclean, {
        x<-clean()
        forout_clean$x=x
    })
    output$dwoutput <- shiny::downloadHandler(
        filename = function() {
            if(input$operation=="clog"&input$cloglabel=="non"){
                paste0("cleaninglog-",humanTime(),".csv")
            }else if(input$operation=="clog"&input$cloglabel=="oui"){
                paste0("labeled-cleaninglog-",humanTime(),".csv")
            }else{paste0("cleandata-",humanTime(),".csv")}
        },
        content = function(file) {
            if(input$operation=="clog"){
                xout<-forout_clog$x
                write.csv(xout, file, row.names = FALSE)
            } else {
                xout<-forout_clean$x
                write.csv(xout, file, row.names = FALSE)
            }
        }
    )
    # output$dwclean <- shiny::downloadHandler(
    #     filename = function() {
    #         paste0("cleandata-",humanTime(),".csv")
    #     },
    #     content = function(file) {
    #         xout<-forout_clean$x
    #         write.csv(clean(), file, row.names = FALSE)
    #     }
    # )
}

H2R_HSM<-function(...){
    options(shiny.maxRequestSize =200 * 1024^2)
    shiny::shinyApp(ui, server,...)
}
