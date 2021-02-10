ui <- shiny::fluidPage(
    
    shinyjs::useShinyjs(),
    shiny::titlePanel("3 Frontieres (Burkina Faso / Mali / Niger)"),
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::selectizeInput(
                'operation', "Quelle type d'operation voulez vous effectuer:", choices = setNames(c("clog","clean"),c("Produire un Cleaning Log","Nettoyer les donnees")),
                options = list(
                    placeholder = 'Veuillez choisir une des options suivantes',
                    onInitialize = I('function() { this.setValue(""); }')
                )
            ),
            shiny::tags$hr(),
            shiny::uiOutput("ui.db"),
            shiny::tags$hr(),
            shiny::uiOutput("ui.questionnaire"),
            shiny::tags$hr(),
            shiny::uiOutput("ui.clog"),
            shiny::tags$hr(),
            shiny::uiOutput("ui.run.check"),
            shiny::tags$hr(),
            shiny::downloadButton("dwoutput"),
            shiny::helpText("Download will be available once the processing is completed.")
            )
        ,
        shiny::mainPanel(
            shiny::dataTableOutput("preview"))
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
    output$ui.run.check<-renderUI({
        if(!is.null(db())&!is.null(survey())&!is.null(choices())&input$operation=="clog") actionButton("runchecks","RUN checks")
    })
    check<-shiny::eventReactive(input$runchecks, {

        time_check<-survey_tonext_check(db())
        autre_check<-other_check(db(),survey())
        logbook<-apply_checks(db())

        clog<-bind_rows(logbook,time_check,autre_check)
        shinyjs::enable("dwoutput")
        shinyjs::html("dwoutput", "Download output")
        clog

    })
    output$preview <- shiny::renderDataTable({
        check()
    })
    output$dwoutput <- shiny::downloadHandler(
        filename = function() {
            paste0("cleaninglog-",humanTime(),".csv")
        },
        content = function(file) {
            write.csv(check(), file, row.names = FALSE)
        }
    )
}

H2R_HSM<-function(...){
    options(shiny.maxRequestSize = 60 * 1024^2)
    shiny::shinyApp(ui, server,...)
}
