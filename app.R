library(shiny)
library(shinythemes)
library(archeofrag)
library(ggplot2)
library(foreach)
library(DT)
library(reshape2) # for acast()
library(dendextend)
library(igraph) # for 
library(tidyr) # for pivot_longer()
if( ! requireNamespace("RBGL")){
  if( ! requireNamespace("BiocManager")){
    install.packages("BiocManager")
  }
  BiocManager::install("RBGL")
}
library(RBGL)

library(doParallel)
registerDoParallel()
# getDoParWorkers()

ui <- shinyUI(fluidPage(  # UI ----
                          theme = shinytheme("cosmo"),  # slate  flatly
                          sidebarLayout(
                            sidebarPanel(                  
                              h3(div(HTML("<a href=https://github.com/sebastien-plutniak/archeofrag title='Go to the archeofrag page' target=_blank>archeofrag</a> v",  as.character(utils::packageVersion("archeofrag")) ))),
                              h3("Input data"),
                              selectInput("use_example", "Load example data", 
                                          choices = c("-", "Liang Abu", "Tai"), selected = "-"),
                              uiOutput("layers.selector"),
                              fileInput('inputEdges', 'Relations (CSV file):',
                                        width="70%",
                                        accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                              fileInput('inputNodes', 'Fragments (CSV file):',
                                        width="70%",
                                        accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                              radioButtons(inputId = 'sep', label = 'Separator:', 
                                           choices = c("," =',' , ";"=';'
                                                       ,"tab"='\t'), inline=T, selected = ','),
                              width=2), # end sidebarpanel
                            
                            mainPanel(
                              tabsetPanel(id="tabs",
                                          tabPanel("Introduction", # Introduction ----      
                                                   column(10, align="center",
                                                          tags$div(
                                                            HTML("<div style=width:40%;, align=left>
                <h1><i>archeofrag.gui</i></h1>
                <p>
                This application implements some features of the
                <i><a href=https://cran.r-project.org/web/packages/archeofrag/index.html target=_blank>archeofrag</a></i> R package for spatial analysis in archaeology from the study of refitting fragments of objects. Based on the TSAR method (Topological Study of Archaeological Refitting),  it includes functions to <b>evaluate and validate</b> the distinction between <b>archaeological spatial units</b> (e.g. layers), from the distribution and the topology of the refitting relationships between fragments contained in these units.
                </p>
                <h3>Input Data</h3>
                <p>
                  Use the menu on the left to upload your “relations” and “fragments” data as CSV files. 
                  <ul>
                    <li>The <b>relations</b> table must have a row for each refitting relationship and two columns containing the identifiers of each pair of refitting fragments, respectively;</li>
                  <li>the <b>fragments</b> table must have a row for each fragment, the first column contains its identifier and the second column contains the spatial unit it belongs to (name this column “layer”).</li>
                  </ul>
                  Alternatively, load one of the example datasets.
                </p>
                <h3>Measurements</h3>
                <p>In this tab, statistics are reported for all pairs of spatial units defined in the dataset: number of fragments and refitting relationships, etc. The <b>cohesion</b> and <b>admixture</b> values are calculated using the TSAR method. Tables and figures facilitate the exploration of the results</p>
                <h3>Comparison with simulated data</h3>
                <p>This tab includes functions for in-depth analysis of a specific pair of spatial units. This pair is compared to similar artificial data, simulated for two different formation hypothesis:
                <ul>
                    <li>H1, the archaeological material studied comes from a <b>single deposition event</b> (although archaeologists might have distinguished two spatial units / depositional events).</li>
                    <li>H2, the material was deposited during <b>two deposition events</b>.</li>
                </ul>                    
                <h1>References</h1>
                <h2>About <i>archeofrag</i></h2>
                <p>
                The code and more information are available on <a target=_blank, href=https://github.com/sebastien-plutniak/archeofrag/>github</a> and in the following publications:
                <ul>
                  <li>Plutniak, S. 2021. “<a href=https://hal.archives-ouvertes.fr/hal-03419952 target=_blank>The Strength of Parthood Ties. Modelling Spatial Units and Fragmented Objects with the TSAR Method – Topological Study of Archaeological Refitting</a>”, <i>Journal of Archaeological Science</i>, 136, p. 105501. DOI: <a href=https://doi.org/10.1016/j.jas.2021.105501 target=_blank>10.1016/j.jas.2021.105501</a>.</li>
                  <li>Plutniak, S. 2022. “Archeofrag: an R package for Refitting and Spatial Analysis in Archaeology”, <i>Journal of Open Source Software</i>, 7 (75), p. 4335. DOI: <a href=https://doi.org/10.21105/joss.04335 target=_blank>10.21105/joss.04335</a>.</li>
                  <li>Plutniak, S. 2022. “<a href=https://rzine.fr/ressources/20220811_archeofrag_joss/ target=_blank>Archeofrag: un package R pour les remontages et l'analyse spatiale en archéologie</a>”, <i>Rzine</i>.</li>
                  <li>Plutniak, S. 2022. “<a href=http://www.prehistoire.org/offres/doc_inline_src/515/0-BSPF_2022_1_2e_partie_Correspondance_PLUTNIAK.pdf target=_blank>L'analyse topologique des remontages archéologiques : la méthode TSAR et le package R archeofrag</a>”, <i>Bulletin de la Société préhistorique française</i>, 119 (1), p. 110–113.</li>
                  <li>Plutniak, S., J. Caro, C. Manen 2023. “<a href=https://hal.science/hal-04355706 target=_blank>Four Problems for Archaeological Fragmentation Studies. Discussion and Application to the Taï Cave’s Neolithic Pottery Material (France)</a>”, in A. Sörman, A. Noterman, M. Fjellström (eds.) <i>Broken Bodies, Places and Objects. New Perspectives on Fragmentation in Archaeology</i>, London: Routledge, p. 124–142. DOI: <a href=https://doi.org/10.4324/9781003350026-1 target=_blank>10.4324/9781003350026-11</a>.</li>
                </ul>
                </p>
                <h2>Datasets</h2> 
                <ul>
                  <li><b>Liang Abu</b>: Plutniak S. 2021. “Refitting Pottery Fragments from the Liang Abu Rockshelter, Borneo”. DOI: <a href=https://doi.org/10.5281/zenodo.4719577 target=_blank>10.5281/zenodo.4719577</a> </li>
                  <li><b>Taï</b>:  Caro J., Plutniak S. 2022. “Refitting and Matching Neolithic Pottery Fragments from the Taï site, France”. DOI:  <a href=https://doi.org/10.5281/zenodo.7408706 target=_blank>10.5281/zenodo.7408706</a> </li>
                </ul>
                <br>
                                                                 </div>"))
                                                   ) # end column
                                          ), #end tabPanel
                                          
                                          tabPanel("Measurements", # Measurements ----
                                                   br(),
                                                   h1("Stats by pair of spatial units"),
                                                   DT::dataTableOutput("resultsTab",  width="80%"), 
                                                   h1("Admixture between spatial units"),
                                                   tableOutput("admixTab"),
                                                   imageOutput("admix.plot",  width= "50%")
                                          ), #end tabPanel
                                          tabPanel("Visualisation", # Visualisation ----
                                                   br(),
                                                   imageOutput("visualisation.plot", height = "800px", width= "100%")
                                          ), #end tabPanel
                                          tabPanel("Simulation", # Simulation ---- 
                                                   fluidRow(
                                                     h1("Information"),
                                                   column(10, align="center",
                                                          tags$div(
                                                            HTML("<div style=width:40%;, align=left>
                    <h2>Instruction</h2>
                    <p>
                      <ul>
                      <li>Select the pair of spatial units to compare in the menu</li>
                      <li>The parameters of the simulation are automatically filled with the values measured on the graph corresponding to the two spatial units selected, but can be edited.</li>
                      <li> Set the number of simulated graphs to generate for each hypothesis, and click on the “Run” button. Using parallelization speeds up the computation (however, if it raises an error, untick the box, re-run the computation and be patient).</li>
                      </ul>
                    </p>
                    <h2>Procedure</h2>
                    <p>
                    Parameters (number of objects, fragment balance, etc.) are extracted from the input graph for the pair of spatial units under study, and used to generate a series of artificial graphs. Artificial graphs are generated for two deposition hypotheses:
                    <ol type='1'>
                     <li> the objects were buried in a single spatial unit and subsequently moved;</li>
                     <li> the objects were buried in two spatial units and subsequently moved.</li>
                    </ol>
                    </p>
                    <p>
                    <h2>Results</h2>
                    The table below summarises the results for each parameter, indicating 
                   <ul>
                     <li>whether the simulated values for H1 and H2 are significantly different (<a href=https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test target=_blank>Wilcoxon test</a>, “H1 != H2?” and “p.value” columns), and 
                     <li>whether the observed value is lower / within / higher the interquartile range of values simulated for H1 and H2, respectively (“Obs. value/H1” and “Obs. value/H2” columns).</li>
                     </ul>
                      Charts are generated for the parameters measured on the empirical and artificial fragmentation graphs: 
                      <ul>
                      <li>the value observed on the empirical graph is represented by a vertical bar, </li>
                      <li>the distribution of values for each hypotheses are represented by dark (H1) and light (H2) grey shades, respectively.</li>
                      </ul>
                      </p>
                    </p>
                 </div>") )
                                                   ) # end column
                                                   ), # end fluidrow
                                                   # fluidRow(
                                                   #   h2("Pair of spatial units"),
                                                   #   column(2, uiOutput("layers.selector")),
                                                   # ),
                                                   fluidRow(
                                                    h2("Model parameters set up"),
                                                    column(2, uiOutput("n.components")),
                                                    column(3, uiOutput("components.balance")),
                                                  ),
                                                  fluidRow(
                                                    column(2, uiOutput("planar")),
                                                    column(3, uiOutput("balance")),
                                                    column(3, uiOutput("aggreg.factor")),
                                                    column(2, uiOutput("asymmetric"))
                                                  ), #end fluidrow
                                                  fluidRow(
                                                    column(2, uiOutput("n.final.fragments")),
                                                    column(3, uiOutput("disturbance"))
                                                  ),
                                                  fluidRow(
                                                            h2("Computation set up"),
                                                            column(1, 
                                                                   numericInput("replications", "Replications",
                                                                                50, min=30, max=500, width = "100%")),
                                                            column(1, actionButton("goButton", "Run"), style="padding:27px;"),
                                                            column(2, checkboxInput("parallelize",
                                                                                    paste0("Parallelize (n workers: ",
                                                                                           foreach::getDoParWorkers(), ")"), value=T),
                                                                   style="padding:27px;")
                                                          ), #end fluidrow
                                                  fluidRow(
                                                        h2("Results"),
                                                        column(10, align="center",
                                                          tableOutput("summary.tab"), 
                                                          imageOutput("test.simul.edges.plot", height = "200px", width= "100%"),
                                                          imageOutput("test.simul.weightsum.plot", height = "200px", width= "100%"),
                                                          imageOutput("test.simul.disturbance.plot", height = "200px", width= "100%"),
                                                          imageOutput("test.simul.balance.plot", height = "200px", width= "100%"),
                                                          imageOutput("test.simul.admixture.plot", height = "200px", width= "100%"),
                                                          imageOutput("test.simul.cohesion.plot", height = "400px", width= "100%")
                                                   ) # end column
                                                   ) # end fluidrow
                                                   ), # end tabPanel
                                          tabPanel("R code", # R code ----
                                                   column(12, 
                                                          br(),
                                                          HTML("The following R code runs the simulation with the current settings and returns a series of values for each hypothesis:
                                                          <ul>
                                                            <li>admixture</li>
                                                            <li>cohesions value for the spatial units 1 and 2</li> 
                                                            <li>number of refitting relations</li>
                                                            <li>fragment balance</li>
                                                            <li>disturbance</li> 
                                                            <li>sum of relation weights</li>"),
                                                          br(),
                                                          HTML(paste("<div style=width:80%;, align=left>",
                                                                     "<br><div style=\"font-family:Courier; font-size: small; width:100%;\", align=left>",
                                                                     htmlOutput("reproducibility"),
                                                                     "</div>",
                                                                     "</div>"
                                                          ) # end paste
                                                          )  # end HTML
                                                   ) # end column
                                          ), #end tabPanel
                                          ), # end  tabsetPanel
                              width=10) # end mainPanel
                          ) #sidebarLayout
) #end fluidPage
) #end  shinyUI



# DEFINE SERVER  ----    
server <- function(input, output, session) { 
  
  # data input ----
  userNodes <- reactive({
    req(input$inputNodes)
    validate(
      need(input$inputNodes,
           message = "Choose a node file or use the example data."),
      need(unique(input$inputNodes[, 2]) > 2,
           message = "Error! Only two different layers are allowed.")
    )
    input$inputNodes
  })
  
  userEdges <- reactive({
    validate(
      need(input$inputEdges,
           message = "Choose an edge file or use the example data.")
    )
    input$inputEdges
  })
  
  graph.data <- reactive({
    
    if(input$use_example != "-") {
      if(input$use_example == "Liang Abu"){
        data(LiangAbu)
        edges.df <- df.cr
        objects.df <- fragments.info
      } else if(input$use_example == "Tai"){
        data(Tai)
        edges.df <- tai.connection
        objects.df <- tai.fragments
      }
    } else {
      query <- shiny::parseQueryString(session$clientData$url_search)
      
      if ( ! is.null(query[['objects']])) {
        objects.df <- utils::read.csv(url(as.character(query[['objects']])))
      } else{
        objects.df <- read.csv(userNodes()$datapath, header = T, sep=input$sep)
      }
      if ( ! is.null(query[['relations']])) {
        edges.df <- utils::read.csv(url(as.character(query[['relations']])))
      } else{
        edges.df <- read.csv(userEdges()$datapath, header = T, sep=input$sep)
      }
    }
    list("objects.df"=objects.df, "edges.df"=edges.df)
  })
  
  
  graph <- reactive({
    req(graph.data())
    
    graph.data <- graph.data()
    objects.df <- graph.data$objects.df
    edges.df <- graph.data$edges.df
    
    graph <- make_frag_object(edges.df, fragments = objects.df)
    graph <- make_cr_graph(graph)
    
    pairs <- combn(sort(unique(V(graph)$layer)), 2)
    
    g.list <- lapply(1:ncol(pairs), function(x){
      g <- frag.get.layers.pair(graph, "layer", pairs[, x]  )
      if(length(unique(V(g)$layer)) != 2){ return() }
      
      frag.edges.weighting(g, "layer")
    })
    names(g.list) <- sapply(1:ncol(pairs), function(x)
      paste(pairs[1, x], "/", pairs[2, x]))
    
    g.list[ ! sapply(g.list, is.null)]
  })
  
  # pair selector ----
  output$layers.selector <- renderUI({
    req(graph())
    
    g.list <- graph()
    
    choices.val <- 1:length(g.list)
    names(choices.val) <- names(g.list)
    
    selectInput("units.pair", "Spatial units",
                selected = choices.val[1],
                choices = choices.val, width= "90%")
  })
  
  
  input.graph.params <- reactive({ # input graph params ----
    req(graph.selected())
    frag.get.parameters(graph.selected(), "layer")
  })
  
  output$n.components <- renderUI({
    req(graph()) 
    numericInput("n.components", "Initial objects number", value = input.graph.params()$n.components, width = "100%")
  })
  
  output$components.balance <- renderUI({
    req(input.graph.params())
    sliderInput("components.balance", "Initial objects balance",
                min = 0, max= 1, step = .01, 
                value = input.graph.params()$components.balance, width = "100%")
  })
  
  output$balance <- renderUI({
    req(input.graph.params())
    sliderInput("balance", "Fragments balance",  min = 0, max= 1, step = .01,
                value = input.graph.params()$balance, width = "100%")
  })
  
  output$n.final.fragments <- renderUI({
    req(graph())
    numericInput("n.final.fragments", "Final fragments number", value = input.graph.params()$vertices, width = "100%")
  })
  
  output$disturbance <- renderUI({
    req(input.graph.params())
    sliderInput("disturbance", "Final disturbance", min = 0, max= .5, step = .01, 
                value = input.graph.params()$disturbance, width = "100%")
  })
  
  output$aggreg.factor <- renderUI({
    req(input.graph.params())
    sliderInput("aggreg.factor", "Aggregation factor", min = 0, max= 1, step = .01, 
                value = input.graph.params()$aggreg.factor, width = "100%")
  })
  
  output$asymmetric <- renderUI({
    req(input.graph.params())
    selectInput("asymmetric", "Asymmetric move from unit", choices = c("none", "1", "2"), selected = "none", width = "50%")
  })
  
  output$planar <- renderUI({
    req(input.graph.params())
    checkboxInput("planar", "Generate only planar graphs", value = input.graph.params()$planar)
  })
  

# MEASUREMENT-----
  
stats.table <- reactive({    # stats table ----
    req(graph())
    g.list <- graph()
    
    make.stat.table <- function(g){
      balance <- NA
      if(igraph::gorder(g) > 6){
        balance <- frag.get.parameters(g, "layer")$balance
      }
      cohesion <- frag.layers.cohesion(g, "layer")
      data.frame(
        "Pair of spatial units" = paste(sort(unique(V(g)$layer)), collapse=" / "),
        "Objects" =  as.integer(igraph::count_components(g)),
        "Fragments" = as.integer(igraph::gorder(g)),
        "Relations" = as.integer(igraph::gsize(g)),
        "Balance" = balance,
        "Cohesion 1st unit" = cohesion[1],
        "Cohesion 2nd unit" = cohesion[2],
        "Admixture" =  frag.layers.admixture(g, "layer")
      )
    }
    
    df <- lapply(g.list, make.stat.table)
    df <- do.call(rbind, df)
    colnames(df) <- gsub("\\.", " ", colnames(df))
    df
  })
  
  output$resultsTab <- DT::renderDataTable({ 
    DT::datatable(stats.table(), rownames=F,  escape=F, style = "default", selection = 'none',
                  options = list(dom = 'tp'))
  })
  
  admixTab <- reactive({  # admix table ----
    req(stats.table(), graph.data())
    stats.table <- stats.table()
    
    stats.table$unit1 <- gsub("(.*) / .*", "\\1", stats.table[,1])
    stats.table$unit2 <- gsub("^.* / (.*$)", "\\1", stats.table[,1])
    
    pairs <- combn(sort(unique(graph.data()$objects.df$layer)), 2)
    pairs <- t(as.data.frame(pairs))
    pairs <- rbind(pairs, pairs[, 2:1])
    
    colnames(pairs) <- c("unit1", "unit2")
    pairs <- merge(pairs,
                   stats.table[, c("Admixture", "unit1", "unit2")], 
                   by = c("unit1", "unit2"), all.x = T)
    
    
    reshape2::acast(pairs, unit2 ~ unit1, value.var = "Admixture", na.rm = F, drop = F)
  })
  
  output$admixTab <- renderTable({ 
    req(admixTab())
    admixTab()
  }, rownames = T, colnames = T, na = "-")
  
  
  output$admix.plot <- renderPlot({  # admix plot ----
    req(admixTab())
    admixTab <- 1 - admixTab()
    admixTab[is.na(admixTab)] <- 1
    
    admixTab <- as.dist(admixTab)
    
    dend.plot <- as.dendrogram(hclust(admixTab, method = "complete"))
    dend.plot <- sort(dend.plot, decreasing = T)
    plot(dend.plot, horiz = T, 
         xlab ="Dissimilarity: 1 – admixture. An alphanumerical ordering constraint is applied to the branches of the dendrogram") 
  })
  
  
  
  graph.selected <- reactive({
    req(graph(), input$units.pair)
    graph.list <- graph()
    
    graph.list[[as.numeric(input$units.pair)]]
  })
  
  
  # SIMULATION ####
  hypotheses <- eventReactive(input$goButton, { # run simulation ####
    
    req(input$replications)
    req(graph.selected())
    graph <- graph.selected()
     
    asymmetric <- input$asymmetric
    if(asymmetric == "none") asymmetric <- NULL
    
    params <- list("n.components" = input$n.components,
                   "n.final.fragments" = input$n.final.fragments,  
                   "balance" = input$balance,
                   "components.balance" = input$components.balance,
                   "disturbance" = input$disturbance,
                   "aggreg.factor" = input$aggreg.factor,
                   "planar" = input$planar)
    
    if(input$parallelize){
    hypothese1.res <- foreach(i=1:input$replications,  .combine = "rbind",
                              .errorhandling = "remove") %dopar%{
                                g <- frag.simul.process(initial.layers = 1,
                                                        n.components = params$n.components,
                                                        vertices = params$n.final.fragments,  
                                                        balance = params$balance,
                                                        components.balance = params$components.balance,
                                                        disturbance = params$disturbance,
                                                        aggreg.factor = params$aggreg.factor,
                                                        planar = params$planar,
                                                        asymmetric.transport.from = asymmetric)
                                g <- frag.edges.weighting(g, "layer")
                                inter.layer.e <- E(g)[V(g)[V(g)$layer == 1] %--% V(g)[V(g)$layer == 2]]
                                data.frame(
                                  "structural_admixture" = round(frag.layers.admixture(g, "layer"), 3),
                                  "cohes" = rbind(frag.layers.cohesion(g, "layer")),
                                  "v.obs" = igraph::gorder(g),
                                  "e.obs" = igraph::gsize(g),
                                  "bal.obs" = sort(table(V(g)$layer))[1] / sum(table(V(g)$layer)),
                                  "dist.obs" = length(inter.layer.e) / igraph::gsize(g),
                                  "weightsum" = sum(E(g)$weight)
                                )
                              }
    
    hypothese2.res <- foreach(i=1:input$replications,  .combine = "rbind",
                              .errorhandling = "remove") %dopar%{
                                g <- frag.simul.process(initial.layers = 2,
                                                        n.components = params$n.components,
                                                        vertices = params$n.final.fragments,  
                                                        balance = params$balance,
                                                        components.balance = params$components.balance,
                                                        disturbance = params$disturbance,
                                                        aggreg.factor = params$aggreg.factor,
                                                        planar = params$planar,
                                                        asymmetric.transport.from = asymmetric)
                                g <- frag.edges.weighting(g, "layer")
                                inter.layer.e <- E(g)[V(g)[V(g)$layer == 1] %--% V(g)[V(g)$layer == 2]]
                                data.frame(
                                  "structural_admixture" = round(frag.layers.admixture(g, "layer"), 3),
                                  "cohes" = rbind(frag.layers.cohesion(g, "layer")),
                                  "v.obs" = gorder(g),
                                  "e.obs" = gsize(g),
                                  "bal.obs" = sort(table(V(g)$layer))[1] / sum(table(V(g)$layer)),
                                  "dist.obs" = length(inter.layer.e) / gsize(g),
                                  "weightsum" = sum(E(g)$weight)
                                )
                              }
    
    } else {
      hypothese1.res <- foreach(i=1:input$replications,  .combine = "rbind",
                                .errorhandling = "remove") %do%{
                                  g <- frag.simul.process(initial.layers = 1,
                                                          n.components = params$n.components,
                                                          vertices = params$n.final.fragments,  
                                                          balance = params$balance,
                                                          components.balance = params$components.balance,
                                                          disturbance = params$disturbance,
                                                          aggreg.factor = params$aggreg.factor,
                                                          planar = params$planar,
                                                          asymmetric.transport.from = asymmetric)
                                  g <- frag.edges.weighting(g, "layer")
                                  inter.layer.e <- E(g)[V(g)[V(g)$layer == 1] %--% V(g)[V(g)$layer == 2]]
                                  data.frame(
                                    "structural_admixture" = round(frag.layers.admixture(g, "layer"), 3),
                                    "cohes" = rbind(frag.layers.cohesion(g, "layer")),
                                    "v.obs" = igraph::gorder(g),
                                    "e.obs" = igraph::gsize(g),
                                    "bal.obs" = sort(table(V(g)$layer))[1] / sum(table(V(g)$layer)),
                                    "dist.obs" = length(inter.layer.e) / igraph::gsize(g),
                                    "weightsum" = sum(E(g)$weight)
                                  )
                                }
      
      hypothese2.res <- foreach(i=1:input$replications,  .combine = "rbind",
                                .errorhandling = "remove") %do%{
                                  g <- frag.simul.process(initial.layers = 2,
                                                          n.components = params$n.components,
                                                          vertices = params$n.final.fragments,  
                                                          balance = params$balance,
                                                          components.balance = params$components.balance,
                                                          disturbance = params$disturbance,
                                                          aggreg.factor = params$aggreg.factor,
                                                          planar = params$planar,
                                                          asymmetric.transport.from = asymmetric)
                                  g <- frag.edges.weighting(g, "layer")
                                  inter.layer.e <- E(g)[V(g)[V(g)$layer == 1] %--% V(g)[V(g)$layer == 2]]
                                  data.frame(
                                    "structural_admixture" = round(frag.layers.admixture(g, "layer"), 3),
                                    "cohes" = rbind(frag.layers.cohesion(g, "layer")),
                                    "v.obs" = gorder(g),
                                    "e.obs" = gsize(g),
                                    "bal.obs" = sort(table(V(g)$layer))[1] / sum(table(V(g)$layer)),
                                    "dist.obs" = length(inter.layer.e) / gsize(g),
                                    "weightsum" = sum(E(g)$weight)
                                  )
                                }
    } # end else
    
    
    hypothese1.res$hypothesis <- "1"
    hypothese2.res$hypothesis <- "2"
    rbind(hypothese1.res, hypothese2.res)
  })
  
  summary.tab <- eventReactive(input$goButton, {# summary table  ----
    
    req(hypotheses())
    hypotheses.df <- hypotheses()
    
    hypotheses.df <- hypotheses.df[ , -4]
    colnames(hypotheses.df) <- c("admixture", "cohesion1", "cohesion2",  "edges", "balance", "disturbance", "weightsum","hypothesis")
    
    summary.df <- frag.simul.summarise(graph.selected(), 
                                       layer.attr = "layer", 
                                       res.h1 = hypotheses.df[hypotheses.df$hypothesis == "1", -8], 
                                       res.h2 = hypotheses.df[hypotheses.df$hypothesis == "2", -8], 
                                       cohesion1.attr="cohesion1", cohesion2.attr="cohesion2", 
                                       admixture.attr="admixture")
    colnames(summary.df)  <- c("H1 != H2?", "p.value", "Obs. value/H1", "Obs. value/H2")
    summary.df
  })
  
  output$summary.tab <- renderTable({summary.tab()}, rownames=T)
  
  
  test.simul.edges.plot <- eventReactive(input$goButton, {    # plot edge count ####
    req(hypotheses())
    
    hypotheses.df <- hypotheses()
    obs.graph <- graph.selected()
    
    ggplot(hypotheses.df, aes(x= e.obs, fill = hypothesis)) +
      theme_light(base_size = 12) +
      geom_density(alpha=.5, linewidth=.3) +
      scale_fill_grey(start = .4, end = .9) +
      geom_vline(xintercept = gsize(obs.graph))  + 
      xlab("Relations count") + ggtitle("Relations count")
  })
  
  output$test.simul.edges.plot <- renderPlot({test.simul.edges.plot()})
  
  
  test.simul.weightsum.plot <- eventReactive(input$goButton, { # plot weights ####
    hypotheses.df <- hypotheses()
    obs.graph <- graph.selected()
    
    ggplot(hypotheses.df, aes(x = weightsum,  fill= hypothesis)) +
      theme_light(base_size = 12) +
      geom_density(alpha=.5, linewidth=.3) +
      scale_fill_grey(start = .4, end = .9)  +
      geom_vline(xintercept = sum(E(obs.graph)$weight)) +
      xlab("Sum of relation weights") + ggtitle("Sum of relation weights")
  })
  
  output$test.simul.weightsum.plot <-  renderPlot({test.simul.weightsum.plot()})
  
  
  test.simul.disturbance.plot <-  eventReactive(input$goButton, {  # plot disturbance ####
    hypotheses.df <- hypotheses()
    obs.graph <- graph.selected()
    
    stats <- frag.get.parameters(obs.graph, "layer")
    
    ggplot(hypotheses.df, aes(x= dist.obs, fill= hypothesis)) +
      theme_light(base_size = 12) +
      geom_density(alpha=.5, linewidth=.3, adjust=1.5) +
      geom_vline(xintercept = stats$disturbance) +
      scale_fill_grey(start = .4, end = .9) +
      xlab("Disturbance") + ggtitle("Disturbance")
  })  
  
  output$test.simul.disturbance.plot <- renderPlot({   test.simul.disturbance.plot()})
  
  
  
  test.simul.balance.plot <-  eventReactive(input$goButton, {  # plot balance ####
    hypotheses.df <- hypotheses()
    obs.graph <- graph.selected()
    
    stats <- frag.get.parameters(obs.graph, "layer")
    
    ggplot(hypotheses.df, aes(x= bal.obs, fill = hypothesis)) +
      theme_light(base_size = 12) +
      geom_density(alpha=.5, linewidth=.3, bw=.01)  +
      geom_vline(xintercept = stats$balance) +
      scale_x_continuous("Balance", limits = c(.0, .5)) + #, breaks = seq(.2, .4, .02)) + #) +
      scale_fill_grey(start = .4, end = .9) + ggtitle("Balance")
  })
  output$test.simul.balance.plot <- renderPlot({test.simul.balance.plot()})
  
  
  test.simul.admixture.plot <- eventReactive(input$goButton, {   # plot admixture ####
    hypotheses.df <- hypotheses()
    obs.graph <- graph.selected() 
    
    ggplot(hypotheses.df, aes(x = structural_admixture, fill= hypothesis)) +
      theme_light(base_size = 12) +
      geom_density(alpha=.5, linewidth=.3) +
      scale_fill_grey("Hypothesis", start = .4, end = .9) +
      geom_vline(xintercept = round(frag.layers.admixture(obs.graph, "layer"), 3)) +
      xlab("Admixture") + ggtitle("Admixture")
  })
  
  output$test.simul.admixture.plot <- renderPlot({ test.simul.admixture.plot()  })
  
  
  test.simul.cohesion.plot <- eventReactive(input$goButton, {   # plot cohesion ####
    hypotheses.df <- hypotheses()
    obs.graph <- graph.selected()
    
    hypotheses.df2 <- pivot_longer(hypotheses.df, c("cohes.cohesion1", "cohes.cohesion2"),
                                   names_to = "Layer")
    hypotheses.df2$Layer <- factor(hypotheses.df2$Layer, labels = c("1", "2"))
    hypotheses.df2$hypothesis <- factor(hypotheses.df2$hypothesis, 
                                        labels=c("Hypothesis 1", "Hypothesis 2"))
    
    ggplot(hypotheses.df2, aes(x = value, fill=Layer, linetype=Layer)) +
      theme_light(base_size = 12) +
      geom_density(alpha=.5, linewidth=.3) +
      geom_boxplot(outlier.shape = 21) +
      scale_fill_grey(start = .9, end = .4) +
      geom_vline(xintercept = frag.layers.cohesion(obs.graph, "layer")[1], linetype=1) +
      geom_vline(xintercept = frag.layers.cohesion(obs.graph, "layer")[2], linetype=2) +
      facet_wrap(~hypothesis, ncol=1) +
      scale_x_continuous("Cohesion", limits=c(0,1)) + ggtitle("Cohesion")
  })
  output$test.simul.cohesion.plot <- renderPlot({test.simul.cohesion.plot()})
  
  
  
  output$visualisation.plot <- renderPlot({   # VISUALISATION ####
    req(graph.selected())
    frag.graph.plot(graph.selected(), layer.attr = "layer")
  })
  
  # SIMULATION CODE ----
  
  output$reproducibility <- reactive({
    req(input$n.components)
  
    asymmetric <- input$asymmetric
    if(asymmetric == "none") asymmetric <- "NULL"
    
    mode <- "%par%"
    if(input$parallelize) mode <- "%dopar%"
    
  generate.run.code <- function(n.layers){  
      paste0("<pre>",
      "h", n.layers, " <- foreach(i=1:", input$replications, ", .combine = 'rbind', .errorhandling = 'remove') ", mode," {<br>",
    "             g <- frag.simul.process(initial.layers = ", n.layers, ",\n",
    "                                     n.components = ", input$n.components, ",<br>",
    "                                     vertices = ", input$n.final.fragments, ",<br>",  
    "                                     balance = ", input$balance, ",<br>",
    "                                     components.balance = ", input$components.balance, ",<br>",
    "                                     disturbance = ", input$disturbance, ",<br>",
    "                                     aggreg.factor = ", input$aggreg.factor, ",<br>",
    "                                     planar = ", input$planar, ",<br>",
    "                                     asymmetric.transport.from = ", asymmetric, ")<br>",
    "              g <- frag.edges.weighting(g, 'layer')<br>", 
    "              inter.layer.e <- E(g)[V(g)[V(g)$layer == 1] %--% V(g)[V(g)$layer == 2]]<br>",
    "              data.frame(<br>",
    "                 'admixture'       = round(frag.layers.admixture(g, 'layer'), 3),<br>",
    "                 'cohesion'        = rbind(frag.layers.cohesion(g, 'layer')),<br>",
    "                 'relations'       = igraph::gsize(g),<br>",
    "                 'balance'         = sort(table(V(g)$layer))[1] / sum(table(V(g)$layer)),<br>",
    "                 'disturbance'     = length(inter.layer.e) / igraph::gsize(g),<br>",
    "                 'weights.sum'     = sum(E(g)$weight)<br>",
    "              )<br>",
    "       }", 
       "</pre>")
  }

  parallel.string <- ""
  if(input$parallelize) parallel.string <- "<br>library(doParallel)<br>registerDoParallel()"
  
  paste0("<pre>library(archeofrag) <br>library(foreach)", parallel.string, "</pre>",
         generate.run.code(1), 
         "<br><br>", 
         generate.run.code(2))
    
  }) # end reactive
  
}

# Run app:
shinyApp(ui = ui, server = server)


