library(shiny)
library(shinythemes)
library(archeofrag)
library(ggplot2)
library(dplyr)
library(foreach)
library(igraph)
library(tidyr) # for pivot_longer()
library(doParallel)
registerDoParallel()
data(LiangAbu)


ui <- shinyUI(fluidPage(  # UI ----
                          theme = shinytheme("slate"),  # slate  flatly
                          sidebarLayout(
                            sidebarPanel(                  
                                   h3(div(HTML("<a href=https://github.com/sebastien-plutniak/archeofrag title='Go to the archeofrag page' target=_blank>archeofrag</a>"))),
                                   uiOutput("version"),
                                   h3("Input"),
                                   checkboxInput("use_example", "use example data", value = F),
                                   fileInput('inputEdges', 'Edges (CSV file):',
                                             width="70%",
                                             accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                                   fileInput('inputNodes', 'Nodes (CSV file):',
                                             width="70%",
                                             accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                                   radioButtons(inputId = 'sep', label = 'Separator:', 
                                                choices = c("," =',' , ";"=';'
                                                            ,"tab"='\t'), inline=T, selected = ','),
                                   
                                   h3("Compare to simulated values"),
                                   uiOutput("layers.selector"),
                                   numericInput("replications", "Replications", 50, min=30, max=500, width="50%"),
                                 actionButton("goButton", "Run", width="50%"),
                            width=2), # end sidebarpanel
                            
                            mainPanel(
                              tabsetPanel(id="tabs",
                                      tabPanel("Introduction", # Introduction ----      
                            column(10, align="center",
                                   # conditionalPanel(condition = "typeof output.resultsTab == 'undefined'",    
                                                    tags$div(
                                                      HTML("<div style=width:400px;, align=left>
                <h2>Welcome to <i>archeofrag</i></h2>
                <p>
                This application demonstrates some features of the
                <i><a href=https://cran.r-project.org/web/packages/archeofrag/index.html target=_blank>archeofrag</a></i>
                R package for the spatial analysis of refitting objects 
                 in archaeology.
                </p>
                <p>
                It includes methods to measure the <b>cohesion</b> and <b>admixture</b> of 
                pairs of archaeological spatial units (e.g. layers),
                from the distribution and the topology of the 
                refitting relationships between the
                fragments contained in these units. The package also makes it possible to compare the measured values to <b>simulated</b> fragmentation graphs.
                </p>
                <h3>Input Data</h3>
                <p>
                  Either load the example data set (refitting data from the <a href=10.5281/zenodo.4719577 target=_blank>Liang Abu rock shelter</a>, Borneo) or upload  your data. Use the menu on the left to upload your edges and nodes data as CSV files. 
                  <ul>
                    <li>The edges table must have a row for each refitting relationship, with two columns containing the identifiers of the two fragments;</li>
                  <li>the nodes table must have a row for each fragment, the first column contains the fragments identifiers and the second column
                  contains their layer.</li>
                  </ul>
                </p>
                <h3>Comparison with simulated data</h3>
                <p>The observed data can be compared to similar simulated data for two formation hypothesis:
                <ul>
                    <li>H1, the archaeological material studied comes from a single deposition episode, within which archaeologists distinguished two subsets;</li>
                    <li>H2, the material was deposited during two deposition episodes, that archaeologists could not distinguish due to subsequent perturbations, admixture, and sampling resulting either from human or non-human action</li>
                </ul>                    
                Select the pair of spatial units to compare in the menu, set the number of simulated data sets to generate, and click on the “Run” button. Depending on the size of the data set, the computing time can be long. Charts are generated for various parameters measured on the fragmentation graphs: the value observed on the empirical graph is represented by a vertical bar, the distribution of values for each hypotheses are represented by dark (H1) and light (H2) grey shades.</p>
                <h3>References</h3>
                <p>
                The code and more information are available on <a target=_blank, href=https://github.com/sebastien-plutniak/archeofrag/>github</a> and in the following publications:
                <ul>
                  <li>Plutniak, S. 2021. “<a href=https://hal.archives-ouvertes.fr/hal-03419952 target=_blank>The Strength of Parthood Ties. Modelling Spatial Units and Fragmented Objects with the TSAR Method – Topological Study of Archaeological Refitting</a>”, <i>Journal of Archaeological Science</i>, 136, p. 105501. DOI: <a href=https://doi.org/10.1016/j.jas.2021.105501 target=_blank>10.1016/j.jas.2021.105501</a>.</li>
                  <li>Plutniak, S. 2022. “Archeofrag: an R package for Refitting and Spatial Analysis in Archaeology”, <i>Journal of Open Source Software</i>, 7 (75), p. 4335. DOI: <a href=https://doi.org/10.21105/joss.04335 target=_blank>10.21105/joss.04335</a>.</li>
                  <li>Plutniak, S. 2022. “<a href=https://rzine.fr/publication/20220811_archeofrag_joss target=_blank>Archeofrag: un package R pour les remontages et l'analyse spatiale en archéologie</a>”, <i>Rzine</i>.</li>
                  <li>
    Plutniak, S. 2022. “<a href=http://www.prehistoire.org/offres/doc_inline_src/515/0-BSPF_2022_1_2e_partie_Correspondance_PLUTNIAK.pdf target=_blank>L'analyse topologique des remontages archéologiques : la méthode TSAR et le package R archeofrag</a>”, <i>Bulletin de la Société préhistorique française</i>, 119 (1), p. 110–113.</li>
                </ul>
                </p></div>"))
                            ) # end column
                                   # ), #end conditionnal
                          ), #end tabPanel
                                   
               tabPanel("Measurements", # Measurements ----
               # conditionalPanel(condition = "typeof resultsTab !== 'undefined'",
                                br(),
                                tableOutput("resultsTab")
               # ) #end conditionnal
               ), #end tabPanel
               
               tabPanel("Simulations", # Simulations ---- 
                        column(10, align="center",
                               br(),
               conditionalPanel(condition = "typeof output.launched !== 'undefined'",
                                tags$div(
                                  HTML("<div style=width:500px;, align=left>
                    <p>Parameters (number of fragments, relations, etc.) have been extracted
                    from the input graph and used to generate a set of artificial
                    graphs. The values observed on the artificial graphs are 
                    reported in the figures below as density distributions 
                    and compared to the value of the input graph (given by 
                    the vertical bar). Artificial graphs were generated for 
                    two hypotheses about the formation of these layers:
                    <ol type='1'>
                     <li> assuming that the objects were buried in a single layer and
                    subsequently moved;</li>
                    <li> assuming that the objects were buried in two layers and
                    subsequently moved.</li>
                    </ol>
                    </p>
                 </div>") ),
                                   ), # end conditionnal panel
               br(),
               # conditionalPanel(condition = "typeof test.simul.edges.plot !== 'undefined'",
                                textOutput("launched"),
                                imageOutput("test.simul.edges.plot", height = "200px", width= "100%"),
                                imageOutput("test.simul.weightsum.plot", height = "200px", width= "100%"),
                                imageOutput("test.simul.disturbance.plot", height = "200px", width= "100%"),
                                imageOutput("test.simul.balance.plot", height = "200px", width= "100%"),
                                imageOutput("test.simul.admixture.plot", height = "200px", width= "100%"),
                                imageOutput("test.simul.cohesion.plot", height = "400px", width= "100%")
                                   # ) #end conditionalPanel
                            ) #end tabPanel
                           ), # end  tabsetPanel
                            ), # end column
                          width=10) # end mainPanel
               ) #sidebarLayout
           ) #end fluidPage
) #end  shinyUI



# DEFINE SERVER  ----    
server <- function(input, output, session) { 
  
  # title ----
  output$version <- renderUI({
    version <- paste("v", utils::packageVersion("archeofrag"), sep="")
    div(HTML(version))
  })
  
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
    
    if(input$use_example == T){
      edges.df <- df.cr
      objects.df <- fragments.info
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
    
    selectInput("units.pair", "Pair of spatial units",
                choices = choices.val, width= "90%")
  })
  
  
  output$resultsTab <- renderTable({  # numerical results ----
    req(graph())
    g.list <- graph()
    
    make.stat.table <- function(g){
      
      balance <- NA
      if(gorder(g) > 6){
        balance <- frag.get.parameters(g, "layer")$balance
      }
      
      cohesion <- frag.layers.cohesion(g, "layer")
      data.frame(
        "Pair of spatial units" = paste(sort(unique(V(g)$layer)), collapse=" / "),
        "Objects" =  as.integer(count_components(g)),
        "Fragments" = gorder(g),
        "Relations" = as.integer(gsize(g)),
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
  
  graph.selected <- reactive({
    req(graph(), input$units.pair)
    graph.list <- graph()
    
    graph.list[[as.numeric(input$units.pair)]]
  })
  
  
  hypotheses <- eventReactive(input$goButton, {
    req(input$replications)
    req(graph.selected())
    
    graph <- graph.selected()
    
    
    params <- frag.get.parameters(graph, "layer")
    
    output$launched <- renderText({""})
    
    hypothese1.res <- foreach(i=1:input$replications,  .combine = "rbind",
                              .errorhandling = "remove") %dopar%{
                                g <- frag.simul.process(initial.layers = 1,
                                                        n.components = params$n.components,
                                                        vertices = params$vertices,  
                                                        balance = params$balance,
                                                        components.balance = params$components.balance,
                                                        disturbance = params$disturbance,
                                                        aggreg.factor = params$aggreg.factor,
                                                        planar = T)
                                g <- frag.edges.weighting(g, "layer")
                                inter.layer.e <- E(g)[V(g)[V(g)$layer == 1] %--% V(g)[V(g)$layer == 2]]
                                data.frame(
                                  "structural_admixture" = frag.layers.admixture(g, "layer"),
                                  "cohes" = rbind(frag.layers.cohesion(g, "layer")),
                                  "v.obs" = gorder(g),
                                  "e.obs" = gsize(g),
                                  "bal.obs" = sort(table(V(g)$layer))[1] / sum(table(V(g)$layer)),
                                  "dist.obs" = length(inter.layer.e) / gsize(g),
                                  "weightsum" = sum(E(g)$weight)
                                )
                              }
    
    hypothese2.res <- foreach(i=1:input$replications,  .combine = "rbind",
                              .errorhandling = "remove") %dopar%{
                                g <- frag.simul.process(initial.layers = 2,
                                                        n.components = params$n.components,
                                                        vertices = params$vertices,  
                                                        balance = params$balance,
                                                        components.balance = params$components.balance,
                                                        disturbance = params$disturbance,
                                                        aggreg.factor = params$aggreg.factor,
                                                        planar = T)
                                g <- frag.edges.weighting(g, "layer")
                                inter.layer.e <- E(g)[V(g)[V(g)$layer == 1] %--% V(g)[V(g)$layer == 2]]
                                data.frame(
                                  "structural_admixture" = frag.layers.admixture(g, "layer"),
                                  "cohes" = rbind(frag.layers.cohesion(g, "layer")),
                                  "v.obs" = gorder(g),
                                  "e.obs" = gsize(g),
                                  "bal.obs" = sort(table(V(g)$layer))[1] / sum(table(V(g)$layer)),
                                  "dist.obs" = length(inter.layer.e) / gsize(g),
                                  "weightsum" = sum(E(g)$weight)
                                )
                              }
    
    hypothese1.res$hypothesis <- "1"
    hypothese2.res$hypothesis <- "2"
    rbind(hypothese1.res, hypothese2.res)
  })
  
  output$test.simul.edges.plot <- renderPlot({
    req(hypotheses())
    hypotheses.df <- hypotheses()
    obs.graph <- graph.selected()
    
    ggplot(hypotheses.df, aes(x= e.obs, fill = hypothesis)) +
      theme_light(base_size = 12) +
      geom_density(alpha=.5, linewidth=.3) +
      scale_fill_grey(start = .4, end = .9) +
      geom_vline(xintercept = gsize(obs.graph))  + 
      xlab("Edge count") + ggtitle("Edge count")
  })
  
  output$test.simul.weightsum.plot <- renderPlot({
    hypotheses.df <- hypotheses()
    obs.graph <- graph.selected()
    
    ggplot(hypotheses.df, aes(x = weightsum,  fill= hypothesis)) +
      theme_light(base_size = 12) +
      geom_density(alpha=.5, linewidth=.3) +
      scale_fill_grey(start = .4, end = .9)  +
      geom_vline(xintercept = sum(E(obs.graph)$weight)) +
      xlab("Edge weights sum") + ggtitle("Edge weighs sum")
  })
  
  output$test.simul.disturbance.plot <- renderPlot({
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
  
  output$test.simul.balance.plot <- renderPlot({
    hypotheses.df <- hypotheses()
    obs.graph <- graph.selected()
    
    stats <- frag.get.parameters(obs.graph, "layer")
    
    ggplot(hypotheses.df, aes(x= bal.obs, fill = hypothesis)) +
      theme_light(base_size = 12) +
      geom_density(alpha=.5, linewidth=.3, bw=.01)  +
      geom_vline(xintercept = stats$balance) +
      scale_x_continuous("Balance", breaks = seq(.2, .4, .02), limits = c(.25,.4)) +
      scale_fill_grey(start = .4, end = .9) + ggtitle("Balance")
  })
  
  output$test.simul.admixture.plot <- renderPlot({
    hypotheses.df <- hypotheses()
    obs.graph <- graph.selected() 
    
    ggplot(hypotheses.df, aes(x = structural_admixture, fill= hypothesis)) +
      theme_light(base_size = 12) +
      geom_density(alpha=.5, linewidth=.3) +
      scale_fill_grey("Hypothesis", start = .4, end = .9) +
      geom_vline(xintercept = frag.layers.admixture(obs.graph, "layer")) +
      xlab("Admixture") + ggtitle("Admixture")
  })
  
  output$test.simul.cohesion.plot <- renderPlot({
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
  
  
  
}

# Run app:
shinyApp(ui = ui, server = server)


