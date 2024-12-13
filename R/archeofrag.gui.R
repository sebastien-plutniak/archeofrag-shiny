
archeofrag.gui <- function(){
  shiny::addResourcePath('www', system.file('www', package = 'archeofrag.gui'))
  doParallel::registerDoParallel() # by default, half of parallel::detectCores()
  # foreach::getDoParWorkers() 
  shinyApp(ui = ui, server = server)
}


 