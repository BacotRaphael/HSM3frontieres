H2R_HSM<-function(...){
    options(shiny.maxRequestSize =200 * 1024^2)
    shiny::shinyApp(ui, server,...)
}
