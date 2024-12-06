
archeofrag.gui <- archeoViz <- function(){
  doParallel::registerDoParallel() # by default, half of parallel::detectCores()
  # foreach::getDoParWorkers() 
  shinyApp(ui = ui, server = server)
}



