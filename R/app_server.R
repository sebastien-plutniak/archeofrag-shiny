server <- function(input, output, session) { 
  .data <- NULL  
  value <- NULL
  
  # parallelize box, count n workers
  output$parallelize.box <- renderUI({
    checkboxInput("parallelize",
                  paste0("Parallelize (n workers: ",
                         foreach::getDoParWorkers(), ")"), value=T)
  })
  
  # rubish generator
  generate.rubish <- function(){
    l13 <- archeofrag::frag.simul.process(n.components=24, vertices=70, disturbance=.4, balance = .6)
    igraph::V(l13)[igraph::V(l13)$layer == 2]$layer <- 3
    
    l24 <- archeofrag::frag.simul.process(n.components=20, vertices=44, balance=.6, disturbance=0)
    igraph::V(l24)[igraph::V(l24)$layer == 1]$layer <- 4
    igraph::V(l24)$name <- paste0(igraph::V(l24)$name, "l24")
    
    l5 <- archeofrag::frag.simul.process(n.components=5, vertices=20)
    igraph::V(l5)$layer  <- 5
    igraph::V(l5)$name <- paste0(igraph::V(l5)$name, "l5")
    
    l6 <- archeofrag::frag.simul.process(n.components=6, vertices=15)
    igraph::V(l6)$layer  <- 6
    igraph::V(l6)$name <- paste0(igraph::V(l6)$name, "l6")
    
    # merge
    g <- igraph::disjoint_union(l13, l24, l5, l6)
    igraph::graph_attr(g, "frag_type") <- "cr"
    # archeofrag::frag.graph.plot(g, "layer")
    
    # add connection between 1 and 2
    g <- igraph::add_edges(g, c(rbind(sample(igraph::V(g)[igraph::V(g)$layer == 1], 4, replace = F),
                                      sample(igraph::V(g)[igraph::V(g)$layer == 2], 4, replace = F))))
    
    # add connection between 1 and 3
    g <- igraph::add_edges(g, c(rbind(sample(igraph::V(g)[igraph::V(g)$layer == 1], 6, replace = F),
                                      sample(igraph::V(g)[igraph::V(g)$layer == 3], 6, replace = F))))
    
    # add connection between 2 and 3
    g <- igraph::add_edges(g, c(rbind(sample(igraph::V(g)[igraph::V(g)$layer == 2], 5, replace = TRUE),
                                      sample(igraph::V(g)[igraph::V(g)$layer == 3], 5, replace = TRUE))))
    
    # add connection between 3 and 4
    g <- igraph::add_edges(g, c(rbind(sample(igraph::V(g)[igraph::V(g)$layer == 3], 10, replace = TRUE),
                                      sample(igraph::V(g)[igraph::V(g)$layer == 4], 10, replace = TRUE))))
    
    # add connection between 4 and 5
    g <-  igraph::add_edges(g, c(rbind(sample(igraph::V(g)[igraph::V(g)$layer == 4], 2, replace = TRUE),
                                       sample(igraph::V(g)[igraph::V(g)$layer == 5], 2, replace = TRUE))))
    
    # add connection between 5 and 6
    g <-  igraph::add_edges(g, c(rbind(sample(igraph::V(g)[igraph::V(g)$layer == 5], igraph::gorder(l6) * 2, replace = TRUE),
                                       sample(igraph::V(g)[igraph::V(g)$layer == 6], igraph::gorder(l6) * 2, replace = TRUE ))))
    
    # extract tables and export
    list("connection" = igraph::as_edgelist(g), 
         "fragments" =  data.frame("id" =  igraph::V(g)$name, "layer" =  igraph::V(g)$layer))
  }
  
  rubish <- generate.rubish()
  
  output$rubish.text <- renderUI({
    if(input$use_example != "Rubish Site *") return()
    
    HTML("<div style=width:40%;, align=left>
    <h1>This is Rubish Site</h1>
         <p/>
         Rubish Site is an impressive archaeological location situated at N189 24' 0, W66 6' 0, on the slopes of In Silico Valley (Randomness county). It was extensively excavated from April, 1st, 1969 (2 am) to April, 1st, 1969 (3 am) by Professor Sauvignon & associates. Their efforts led to determining 5+1 stratigraphic units (=6). Intensive post-excavation studies were carried out the next day, despite the difficult conditions faced by the excavation team. Refits were tirelessly researched among fragments of glass bottle material, which excited the archaeologists for looking surprisingly similar to modern bottles they were familiar with.
         </p>
         <p>
         To demonstrate its potential (and save archaeologists' energy this day) the <a href=https://doi.org/10.21105/joss.04335 target=_blank>TSAR</a>  method was automatically applied, revealing the very nature of this unsuspected archaeological record.
         <ul>
          <li> First, the dissimilarity dendrogram revealed the <b>abnormal ordering</b> of Layers 1, 2, and 3.</li>
          <li> Intrigued, the team of experts measured and compared <i>cohesion</i> and <i>admixture</i> values, refering to <a href=10.1016/j.jas.2021.105501 target=_blank>Plutniak 2021</a>, Table 1 to interpret them:
            <ul> 
                <li> Layers 1 and 2 had <b>cohesion</b> values <b>highly</b> different and a <b>low admixture</b> value, suggesting movement of fragments from one certain unit (from Layer 1 to an uncertain Layer 2).</li>
                <li> Layers 1 and 3 showed a rather <b>low</b> difference between <b>cohesion</b> values and a <b>high admixture</b> value, suggesting transport of fragments within a single initial unit (i.e. Layer 1+3), contradicting the previous result.</li>
                <li> Layers 3 and 4 presented a rather <b>low</b> difference between <b>cohesion</b> values and a <b>low admixture</b> value as well, suggesting transport of fragments between two certain units.</li>
                <li> Layers 5 and 6 showed <b>high</b> difference between <b>cohesion</b> values and a <b>high admixture</b> value as well, desperately suggesting general uncertainty about those layers and their formation.</li>
            </ul>
          </li>
          <li> Plunged into confusion by such an enigma, they had to invoke <i>Simulation</i> to help them understand the mysterious formation processes of layers 1-2-3. The answers they received were astonishing: the examination of simulated cohesion values suggested that, most probably,
             <ul>
              <li> Layers 1 and 2 resulted from <b>two</b> independent deposition events </li>
              <li> whereas Layers 2 and 3 resulted from a <b>single</b> deposition event.  </li>
             </ul
          </li>
         </ul>
         Everything became crystal clear and Prof. Sauvignon shouted a loud 'EUREKA!': 
         the so-called Layers 2 and 3 were actually part of the same unit! Their distinction was irrelevant and most probably the result of suspicious post-depositional processes (or even due to Sauvignon's colleague's dubious ability to scrutinize subtle sedimental differences when excavating in hard conditions).
         </p>
         <p>
         And that's how, based on these breathtaking results afterwards reproduced using <i>R programming code</i>,  Prof. Sauvignon famously gave the site its name, known worldwide: Rubish Site.
         </p>
         </div>
         ")
  })
  
  
  # DATA INPUT ----
  userNodes <- reactive({
    req(input$inputNodes)
    validate(
      need(input$inputNodes,
           message = "Choose a node file or use the example data.")
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
  
  # datasets list ----
  datasets <- utils::data(package = "archeofrag")$result[, "Item"]
  data.names <- gsub(".*\\((.*)\\)","\\1", datasets)
  data.names <- gsub("([A-Z])"," \\1", data.names)
  data.names <- gsub("^ ", "", data.names)
  names(data.names) <-  gsub("^(.*)\\..*","\\1", datasets)
  
  # dataset selector ----
  output$dataset.selector <- renderUI({
    data.names <- sort(c("-", data.names, "Rubish Site *"))
    selectInput("use_example", "Load example data", 
                choices = c(data.names, use.names=FALSE), selected = "-")
  })
  
  
  graph.data <- reactive({
    req(input$use_example)
    
    if(input$use_example  %in% data.names) {
      idx <- which(data.names == input$use_example)
      eval(parse(text = paste0("edges.df <- archeofrag::",   names(data.names[idx]), ".connection" )))
      eval(parse(text = paste0("objects.df <- archeofrag::", names(data.names[idx]), ".fragments" )))
    } else if(input$use_example == "Rubish Site *"){
      edges.df <- rubish$connection
      objects.df <- rubish$fragments
    } else {
      query <- shiny::parseQueryString(session$clientData$url_search)
      
      if ( ! is.null(query[['objects']])) {
        objects.df <- utils::read.csv(url(as.character(query[['objects']])))
      } else{
        objects.df <- utils::read.csv(userNodes()$datapath, header = T, sep=input$sep)
      }
      if ( ! is.null(query[['relations']])) {
        edges.df <- utils::read.csv(url(as.character(query[['relations']])))
      } else{
        edges.df <- utils::read.csv(userEdges()$datapath, header = T, sep=input$sep)
      }
    }
    list("objects.df"=objects.df, "edges.df"=edges.df)
  })
  
  # spatial variable selector ----
  output$variable.selector <- renderUI({
    req(graph.data())
    
    g.data <- graph.data()
    objects.df <- g.data$objects.df
    
    choices.val <- names(objects.df)
    choices.val <- choices.val[ ! tolower(choices.val) == "id"]
    names(choices.val) <- names(choices.val)
    
    default.value <- choices.val[1]
    if(sum("layer" %in% choices.val) > 0 ){
      default.value <- "layer"
    } else if(sum("level" %in% choices.val) > 0 ){
      default.value <- "level"
    }
    
    selectInput("spatial.variable", "Spatial variable",
                selected = default.value,
                choices = choices.val, width= "90%")
  })
  
  graph.data2 <- reactive({ # add spatial variable ----
    req(input$spatial.variable)
    
    g.data <- graph.data()
    objects.df <- g.data$objects.df
    
    objects.df$spatial.variable <- as.character(eval(parse(text = paste0("objects.df$", input$spatial.variable ))))
    
    list("objects.df"=objects.df, "edges.df"=g.data$edges.df)
  })
  
  
  
  # SELECTORS ----
  # ... pair of units selector ----
  output$layers.selector <- renderUI({
    req(graph.data2())
    
    g.list <- graph.list()
    
    choices.val <- seq(1, length(g.list))
    names(choices.val) <- names(g.list)
    
    selectInput("units.pair", "Pair of spatial units",
                choices = choices.val, width= "90%")
  })
  
  # ... morpho selector ----
  output$morpho.selector <- renderUI({
    req(graph.data2())
    
    choices.val <- c("-", names(graph.data()$objects.df))
    choices.val <- choices.val[ ! tolower(choices.val) == "id"]
    
    selectInput("morpho.variable", "Morphometry variable",
                choices = choices.val, width= "90%")
  })
  
  # ... x selector ----
  output$x.selector <- renderUI({
    req(graph.data2())
    
    choices.val <- c("-", names(graph.data()$objects.df))
    choices.val <- choices.val[ ! tolower(choices.val) == "id"]
    
    selectInput("x.variable", "X coordinates",
                choices = choices.val, width= "90%")
  })
  
  # ... y selector ----
  output$y.selector <- renderUI({
    req(graph.data2())
    
    choices.val <- c("-", names(graph.data()$objects.df))
    choices.val <- choices.val[ ! tolower(choices.val) == "id"]
    
    selectInput("y.variable", "Y coordinates",
                choices = choices.val, width= "90%")
  })
  
  # ... z selector ----
  output$z.selector <- renderUI({
    req(graph.data2())
    
    choices.val <- c("-", names(graph.data()$objects.df))
    choices.val <- choices.val[ ! tolower(choices.val) == "id"]
    
    selectInput("z.variable", "Z coordinates",
                choices = choices.val, width= "90%")
  })
  
  
  
  # MAKE GRAPH LIST----
  graph.list <- reactive({ 
    req(graph.data2, input$spatial.variable)
    
    g.data <- graph.data2()
    
    graph <- archeofrag::make_frag_object(g.data$edges.df, fragments = g.data$objects.df)
    graph <- archeofrag::make_cr_graph(graph)
    
    # check if the data is complete for weighting parameter
    check.and.delete.frag <- function(g, var){
      values <- vertex_attr(g, var)
      idx <- is.na(values) | values == ""
      if(sum(idx)){ 
        g <- delete_vertices(graph, idx) 
        showNotification(paste0("Incomplete values in '", var, "'. ", as.character(sum(idx)), " fragments removed."),
                         duration = 10, type = "message")
      }
      g
    }
    
    if( ! is.null(input$morpho.variable)){ graph <- check.and.delete.frag(graph, input$morpho.variable)}
    if( ! is.null(input$x.variable)){ graph <- check.and.delete.frag(graph, input$x.variable)}
    if( ! is.null(input$y.variable)){ graph <- check.and.delete.frag(graph, input$y.variable)}
    if( ! is.null(input$z.variable)){ graph <- check.and.delete.frag(graph, input$z.variable)}
    
    pairs <- utils::combn(sort(unique(igraph::V(graph)$spatial.variable)), 2)
    
    g.list <- lapply(seq_len(ncol(pairs)), function(x,
                                                    morpho.var = input$morpho.variable, 
                                                    x.var = input$x.variable, 
                                                    y.var = input$y.variable, 
                                                    z.var = input$z.variable){    
      g <- archeofrag::frag.get.layers.pair(graph, "spatial.variable", pairs[, x], verbose = FALSE)
      if(is.null(g)){ return() }
      if(length(unique(igraph::V(g)$spatial.variable)) != 2){ return() }
      
      archeofrag::frag.edges.weighting(g, "spatial.variable", morphometry = morpho.var, 
                                       x = x.var, y = y.var, z = z.var, verbose = FALSE)
    })
    names(g.list) <- sapply(seq_len(ncol(pairs)), function(x)
      paste(pairs[1, x], "/", pairs[2, x]))
    
    g.list[ ! sapply(g.list, is.null)]
  })
  
  graph.selected <- reactive({
    req(graph.list(), input$units.pair)
    graph.list <- graph.list()
    
    graph.list[[as.numeric(input$units.pair)]]
  })
  
  # GET GRAPH PARAMS ----
  input.graph.params <- reactive({ 
    req(graph.selected())
    archeofrag::frag.get.parameters(graph.selected(), "spatial.variable", verbose = FALSE)
  })
  
  output$n.components <- renderUI({
    req(graph.list()) 
    numericInput("n.components", "Initial objects number", value = input.graph.params()$n.components, width = "100%")
  })
  
  output$components.balance <- renderUI({
    req(input.graph.params())
    sliderInput("components.balance", "Estimated initial objects balance",
                min = 0, max= 1, step = .01, 
                value = input.graph.params()$components.balance, width = "100%")
  })
  
  output$balance <- renderUI({
    req(input.graph.params())
    sliderInput("balance", "Estimated fragments balance",  min = 0, max= 1, step = .01,
                value = input.graph.params()$balance, width = "100%")
  })
  
  output$n.final.fragments <- renderUI({
    req(graph.list())
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
    
    planar <- input.graph.params()$planar
    if(is.na(planar)) { 
      planar <- FALSE 
      showNotification("The RBGL package is not installed: the `planarity` value cannot be determinated and the 'Generate only planar graphs' is set to FALSE", duration = 10, type = "warning")
    }
    
    checkboxInput("planar", "Generate only planar graphs", value = planar)
  })
  
  
  # MEASUREMENT-----
  
  stats.table <- reactive({    # stats table ----
    req(graph.list, input$morpho.variable)
    g.list <- graph.list()
    
    make.stat.table <- function(g){
      g.stats <- list(balance = NA, components.balance = NA)
      if(igraph::gorder(g) > 6){
        g.stats <- archeofrag::frag.get.parameters(g, layer.attr = "spatial.variable", verbose = FALSE)
      }
      cohesion <- round(archeofrag::frag.layers.cohesion(g, "spatial.variable", verbose = FALSE), 2)
      cohesion.diff <- sort(cohesion)
      cohesion.diff <- round(cohesion.diff[2] - cohesion.diff[1], 2)
      data.frame(
        "Pair of spatial units" = paste(sort(unique(igraph::V(g)$spatial.variable)), collapse=" / "),
        "N. Objects" =  as.integer(igraph::components(g)$no),
        "N. Fragments" = as.integer(igraph::gorder(g)),
        "N. Relations" = as.integer(igraph::gsize(g)),
        "Frag. Balance" = g.stats$balance,
        "Objects Balance" = g.stats$components.balance,
        "Cohesion 1st unit" = cohesion[1],
        "Cohesion 2nd unit" = cohesion[2],
        "Cohesion diff" = cohesion.diff,
        "Admixture" =  round(archeofrag::frag.layers.admixture(g, "spatial.variable", verbose = FALSE), 2)
      )
    }
    
    df <- lapply(g.list, make.stat.table)
    df <- do.call(rbind, df)
    colnames(df) <- gsub("\\.", " ", colnames(df))
    df
  })
  
  output$resultsTab <- DT::renderDT({ 
    DT::datatable(stats.table(), rownames=F,  escape=F, style = "default", selection = 'none',
                  options = list(dom = 'tp'))
  })
  
  admixTab <- reactive({  # admix table ----
    req(stats.table(), graph.data2())
    stats.table <- stats.table()
    
    stats.table$unit1 <- gsub("(.*) / .*", "\\1", stats.table[,1])
    stats.table$unit2 <- gsub("^.* / (.*$)", "\\1", stats.table[,1])
    
    pairs <- utils::combn(sort(unique(graph.data2()$objects.df$spatial.variable)), 2)
    pairs <- t(as.data.frame(pairs))
    pairs <- rbind(pairs, pairs[, 2:1])
    
    colnames(pairs) <- c("unit1", "unit2")
    pairs <- merge(pairs,
                   stats.table[, c("Admixture", "unit1", "unit2")], 
                   by = c("unit1", "unit2"), all.x = T)
    
    if(input$normalise.diss){
      pairs$Admixture <- ( pairs$Admixture - min(pairs$Admixture, na.rm = T)) / 
        (max(pairs$Admixture, na.rm = T) - min(pairs$Admixture, na.rm = T))
    }
    
    diss <- stats::reshape(pairs, timevar = "unit1", idvar = "unit2",  v.names = "Admixture", direction = "wide")
    colnames(diss) <- gsub("^Admixture.", "", colnames(diss))
    rownames(diss) <- diss[, 1]
    diss <- diss[, -1]
    
    diss[ order(rownames(diss)), order(colnames(diss))]
  })
  
  output$admixTab <- renderTable({ 
    req(admixTab())
    admixTab()
  }, rownames = T, colnames = T, na = "-")
  
  # admix plot ----
  admix.dendr <- reactive({  
    req(admixTab())
    admixTab <- 1 - admixTab()
    admixTab[is.na(admixTab)] <- 1
    
    admixTab <- stats::as.dist(admixTab)
    
    dend.plot <- stats::as.dendrogram(stats::hclust(admixTab, method = "complete"))
    sort(dend.plot, decreasing = T)
  })
  
  output$admix.plot <- renderPlot({  
    req(admix.dendr)
    
    plot(admix.dendr(), horiz = T, main = input$spatial.variable,
         xlab ="Dissimilarity: 1 - admixture. An alphanumerical ordering constraint is applied to the branches of the dendrogram") 
  })
  
  
  output$admix.download <- downloadHandler(
    filename = paste0("archeofrag-dissimilarity-",  input$spatial.variable, ".svg"),
    content = function(file) {
      grDevices::svg(file)
      plot(admix.dendr(), horiz = T, main = input$spatial.variable)
      grDevices::dev.off()
    }
  )
  
  output$admix.download.button <- renderUI({
    downloadButton("admix.download", "Download as SVG")
  })
  
  # SIMULATION ####
  
  # Function to execute the simulation and retrieve some measures:
  exec.simulation  <- function(initial.layers, n.components, vertices,
                               balance, components.balance, disturbance,
                               aggreg.factor, planar, asymmetric.transport.from,
                               edge.loss, vertice.loss){
    
    g <- archeofrag::frag.simul.process(initial.layers, n.components, vertices, edges = Inf,
                                        balance, components.balance, disturbance,
                                        aggreg.factor, planar, asymmetric.transport.from)
    
    if(edge.loss != 0){
      g <- archeofrag::frag.observer.failure(g, likelihood = edge.loss / 100, remove.vertices=TRUE)[[1]]
    }
    if(vertice.loss != 0){
      n.frag.to.remove <- round((vertice.loss / 100) * igraph::gorder(g), 0)
      g <- archeofrag::frag.graph.reduce(g, n.frag.to.remove = n.frag.to.remove, conserve.objects.nr=FALSE)
    }
    
    data.frame(
      "admixture" = round(archeofrag::frag.layers.admixture(g, "layer", verbose = FALSE), 3),
      "cohes" = rbind(archeofrag::frag.layers.cohesion(g, "layer", verbose = FALSE)),
      "e.obs" = igraph::gsize(g),
      "balance.obs" = table(igraph::V(g)$layer)[1] / igraph::gorder(g),
      "weights.sum" = sum(igraph::E(g)$weight),
      "weights.median" = stats::median(igraph::E(g)$weight),
      "weights.sd" = stats::mad(igraph::E(g)$weight)
    )    
  }
  
  
  hypotheses <- eventReactive(input$goButton, { # run simulation ####
    
    req(input$replications)
    req(graph.selected())
    graph <- graph.selected()
    
    start.time <- Sys.time()  # save start time
    
    if(input$replications < 30 | input$replications > 1000) {
      showNotification("The number of replication must be in [30, 1000].", type="warning")
      return(NULL)
    }
    
    asymmetric <- input$asymmetric
    if(asymmetric == "none") asymmetric <- NULL
    
    params <- list("n.components" = input$n.components,
                   "n.final.fragments" = input$n.final.fragments,  
                   "balance" = input$balance,
                   "components.balance" = input$components.balance,
                   "disturbance" = input$disturbance,
                   "aggreg.factor" = input$aggreg.factor,
                   "planar" = input$planar,
                   "edge.loss" = input$edge.loss,
                   "vertice.loss" = input$vertice.loss)
    
    
    if(input$parallelize){
      hypothese1.res <- foreach::foreach(i = seq_len(input$replications), .combine = "rbind",
                                         .errorhandling = "remove") %dopar%{
                                           exec.simulation(initial.layers = 1,
                                                           n.components = params$n.components,
                                                           vertices = params$n.final.fragments,  
                                                           balance = params$balance,
                                                           components.balance = params$components.balance,
                                                           disturbance = params$disturbance,
                                                           aggreg.factor = params$aggreg.factor,
                                                           planar = params$planar,
                                                           asymmetric.transport.from = asymmetric,
                                                           edge.loss = params$edge.loss,
                                                           vertice.loss = params$vertice.loss)
                                         }
      
      hypothese2.res <- foreach::foreach(i = seq_len(input$replications), .combine = "rbind",
                                         .errorhandling = "remove") %dopar%{
                                           exec.simulation(initial.layers = 2,
                                                           n.components = params$n.components,
                                                           vertices = params$n.final.fragments,  
                                                           balance = params$balance,
                                                           components.balance = params$components.balance,
                                                           disturbance = params$disturbance,
                                                           aggreg.factor = params$aggreg.factor,
                                                           planar = params$planar,
                                                           asymmetric.transport.from = asymmetric,
                                                           edge.loss = params$edge.loss,
                                                           vertice.loss = params$vertice.loss)
                                         }
      
    } else {
      hypothese1.res <- foreach::foreach(i = seq_len(input$replications),  .combine = "rbind",
                                         .errorhandling = "remove") %do%{
                                           exec.simulation(initial.layers = 1,
                                                           n.components = params$n.components,
                                                           vertices = params$n.final.fragments,  
                                                           balance = params$balance,
                                                           components.balance = params$components.balance,
                                                           disturbance = params$disturbance,
                                                           aggreg.factor = params$aggreg.factor,
                                                           planar = params$planar,
                                                           asymmetric.transport.from = asymmetric,
                                                           edge.loss = params$edge.loss,
                                                           vertice.loss = params$vertice.loss)
                                         }
      
      hypothese2.res <- foreach::foreach(i = seq_len(input$replications), .combine = "rbind",
                                         .errorhandling = "remove") %do%{
                                           exec.simulation(initial.layers = 2,
                                                           n.components = params$n.components,
                                                           vertices = params$n.final.fragments,  
                                                           balance = params$balance,
                                                           components.balance = params$components.balance,
                                                           disturbance = params$disturbance,
                                                           aggreg.factor = params$aggreg.factor,
                                                           planar = params$planar,
                                                           asymmetric.transport.from = asymmetric,
                                                           edge.loss = params$edge.loss,
                                                           vertice.loss = params$vertice.loss)
                                         }
    } # end else
    
    if(nrow(hypothese1.res[complete.cases(hypothese1.res),]) < 31 | 
       nrow(hypothese2.res[complete.cases(hypothese2.res),]) < 31){
      showNotification("Excessive information loss. For each hypothesis, less than 30 graphs with valid cohesion values generated. Increase the number of replications or decrease information loss parameters.", duration = 12)
      return(NULL)
    }
    
    hypothese1.res$hypothesis <- "1"
    hypothese2.res$hypothesis <- "2"
    
    hypotheses.df <- rbind(hypothese1.res, hypothese2.res)
    exec.time <- Sys.time() - start.time
    
    comment(hypotheses.df) <- paste(round(as.numeric(exec.time), 0), units(exec.time))
    hypotheses.df
  })
  
  output$simul.graph.nr <- renderUI({
    req(hypotheses)
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    hypotheses.df <- hypotheses.df[complete.cases(hypotheses.df),]
    HTML("<b>", paste( round(nrow(hypotheses.df)/ 2, 0), "</b> graphs with valid cohesion values generated in <b>",
                       comment(hypotheses.df), "</b>."))
  })
  
  summary.tab <- eventReactive(input$goButton, {# summary table  ----
    req(hypotheses)
    
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    
    colnames(hypotheses.df) <- c("admixture", "cohesion1", "cohesion2", "edges", "balance", "weightsum", "weights.median", "weights.sd", "hypothesis")
    hypotheses.df <- hypotheses.df[, c("admixture", "cohesion1", "cohesion2", "edges", "balance", "weightsum", "hypothesis")]
    
    summary.df <- archeofrag::frag.simul.summarise(graph.selected(), 
                                                   layer.attr = "spatial.variable", 
                                                   res.h1 = hypotheses.df[hypotheses.df$hypothesis == "1", -ncol(hypotheses.df)], 
                                                   res.h2 = hypotheses.df[hypotheses.df$hypothesis == "2", -ncol(hypotheses.df)], 
                                                   cohesion1.attr = "cohesion1", cohesion2.attr = "cohesion2", 
                                                   admixture.attr = "admixture", 
                                                   verbose = FALSE)
    colnames(summary.df)  <- c("H1 != H2?", "p.value", "Obs. value/H1", "Obs. value/H2")
    summary.df
  })
  
  output$summary.tab <- renderTable({summary.tab()}, rownames=T)
  
  
  # .. plot cohesion ####
  test.simul.cohesion.plot <- eventReactive(input$goButton, {   
    req(hypotheses)
    
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    
    obs.graph <- graph.selected()
    
    hypotheses.df2 <- stats::reshape(hypotheses.df, dir = "long",
                                     varying = c("cohes.cohesion1", "cohes.cohesion2"),
                                     v.names = "cohesion", timevar="Layer")
    
    hypotheses.df2$Layer <- factor(hypotheses.df2$Layer, labels = c("1", "2"))
    hypotheses.df2$hypothesis <- factor(hypotheses.df2$hypothesis, levels = c("1", "2"),
                                        labels=c("Hypothesis 1", "Hypothesis 2"))
    
    cohes.values <- archeofrag::frag.layers.cohesion(obs.graph, "spatial.variable", verbose = FALSE)
    
    ggplot2::ggplot(hypotheses.df2, ggplot2::aes(x = .data[["cohesion"]], fill = .data[["Layer"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3) +
      ggplot2::geom_boxplot(outlier.shape = 21) +
      ggplot2::geom_vline(xintercept = cohes.values[1],  color = "#440154FF") +
      ggplot2::geom_vline(xintercept = cohes.values[2], color = "#BBDF27FF") +
      ggplot2::facet_wrap(~hypothesis, ncol=1) +
      ggplot2::scale_fill_manual("spatial unit", values = c("#440154FF", "#BBDF27FF")) +
      ggplot2::scale_x_continuous("Cohesion", limits=c(0,1)) + ggplot2::ggtitle("Cohesion by spatial unit") +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 13))
  })
  output$test.simul.cohesion.plot <- renderPlot({test.simul.cohesion.plot()})
  
  output$cohesion.plot.download <- downloadHandler(
    filename = paste0("archeofrag-cohesion-", input$spatial.variable, "-",
                      gsub(" / ", "-", names(graph.list())[as.numeric(input$units.pair)]), ".svg"),
    content = function(file) {
      ggplot2::ggsave(file, plot = test.simul.cohesion.plot(), device = "svg", width=10, height=5, pointsize = 14)
    }
  )
  
  output$cohesion.plot.download.button <- renderUI({
    if(is.null(test.simul.cohesion.plot())) return()
    downloadButton("cohesion.plot.download", "as SVG") 
  })
  
  
  
  # .. plot admixture ####
  test.simul.admixture.plot <- eventReactive(input$goButton, {   
    req(hypotheses)
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    obs.graph <- graph.selected() 
    
    ggplot2::ggplot(hypotheses.df, ggplot2::aes(x = .data[["admixture"]], fill= .data[["hypothesis"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3) +
      ggplot2::scale_fill_grey(start = .4, end = .9) +
      ggplot2::geom_vline(xintercept = round(archeofrag::frag.layers.admixture(obs.graph,
                                                                               "spatial.variable", verbose=FALSE), 3)) +
      ggplot2::xlab("Admixture") + ggplot2::ggtitle("Admixture")
  })
  
  output$test.simul.admixture.plot <- renderPlot({ test.simul.admixture.plot()  })
  
  output$admixture.plot.download <- downloadHandler(
    filename = paste0("archeofrag-admixture-", input$spatial.variable, "-",
                      gsub(" / ", "-", names(graph.list())[as.numeric(input$units.pair)]), ".svg"),
    content = function(file) {
      ggplot2::ggsave(file, plot = test.simul.admixture.plot(), device = "svg", width=10, height=3, pointsize = 14)
    }
  )
  
  output$admixture.plot.download.button <- renderUI({
    if(is.null(test.simul.admixture.plot())) return()
    downloadButton("admixture.plot.download", "as SVG") 
  })
  
  # .. plot edge count ####
  test.simul.edges.plot <- eventReactive(input$goButton, {   
    req(hypotheses)
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    obs.graph <- graph.selected()
    
    ggplot2::ggplot(hypotheses.df, ggplot2::aes(x= .data[["e.obs"]], fill = .data[["hypothesis"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3) +
      ggplot2::scale_fill_grey(start = .4, end = .9) +
      ggplot2::geom_vline(xintercept = igraph::gsize(obs.graph))  + 
      ggplot2::xlab("Relations count") + ggplot2::ggtitle("Relations count")
  })
  
  output$test.simul.edges.plot <- renderPlot({test.simul.edges.plot()})
  
  output$edges.plot.download <- downloadHandler(
    filename = paste0("archeofrag-relation-count-", input$spatial.variable, "-",
                      gsub(" / ", "-", names(graph.list())[as.numeric(input$units.pair)]), ".svg"),
    content = function(file) {
      ggplot2::ggsave(file, plot = test.simul.edges.plot(), device = "svg", width=10, height=3, pointsize = 14)
    }
  )
  
  output$edges.plot.download.button <- renderUI({
    if(is.null(test.simul.edges.plot())) return()
    downloadButton("edges.plot.download", "as SVG") 
  })
  
  # .. plot weights ####
  test.simul.weights.plot <- eventReactive(input$goButton, { 
    req(hypotheses)
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    obs.graph <- graph.selected()
    
    w.sum.df <- cbind(hypotheses.df[, c("hypothesis", "weights.sum")], var = "Sum")
    w.sd.df <- cbind(hypotheses.df[, c("hypothesis", "weights.sd")], var = "Median absolute deviation")
    w.median.df <- cbind(hypotheses.df[, c("hypothesis", "weights.median")], var = "Median")
    colnames(w.sum.df)[2] <- "value"
    colnames(w.sd.df)[2] <- "value"
    colnames(w.median.df)[2] <- "value"
    weights.df <- rbind(w.sum.df, w.sd.df, w.median.df)
    
    vlines <-  data.frame("var" = c("Median absolute deviation", "Median", "Sum"),
                          "value" = c(stats::mad(igraph::E(obs.graph)$weight), 
                                      stats::median(igraph::E(obs.graph)$weight),
                                      sum(igraph::E(obs.graph)$weight)) )
    
    ggplot2::ggplot(weights.df, ggplot2::aes(x = .data[["value"]],  fill = .data[["hypothesis"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3) +
      ggplot2::scale_fill_grey(start = .4, end = .9)  +
      ggplot2::geom_vline(data = vlines,  ggplot2::aes(xintercept = value)) + 
      ggplot2::facet_wrap(~var, ncol=1, scales = "free") +
      ggplot2::xlab("Connection strength") + ggplot2::ggtitle("Connection strength") +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 13))
  })
  
  output$test.simul.weights.plot <-  renderPlot({test.simul.weights.plot()})
  
  output$weights.plot.download <- downloadHandler(
    filename = paste0("archeofrag-weights-", input$spatial.variable, "-",
                      gsub(" / ", "-", names(graph.list())[as.numeric(input$units.pair)]), ".svg"),
    content = function(file) {
      ggplot2::ggsave(file, plot = test.simul.weights.plot(), device = "svg", width=10, height=3, pointsize = 14)
    }
  )
  
  output$weights.plot.download.button <- renderUI({
    if(is.null(test.simul.weights.plot())) return()
    downloadButton("weights.plot.download", "as SVG") 
  })
  
  # .. plot frag. balance ####
  test.simul.balance.plot <-  eventReactive(input$goButton, {  
    req(hypotheses())
    hypotheses.df <- hypotheses()
    if(is.null(hypotheses.df)) return()
    obs.graph <- graph.selected()
    
    ggplot2::ggplot(hypotheses.df, ggplot2::aes(x= .data[["balance.obs"]], fill = .data[["hypothesis"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3, bw=.01)  +
      ggplot2::geom_vline(xintercept = input.graph.params()$balance) +
      ggplot2::scale_x_continuous("Fragments balance") + #, breaks = seq(.2, .4, .02)) + #) +
      ggplot2::scale_fill_grey(start = .4, end = .9) + ggplot2::ggtitle("Fragments balance")
  })
  output$test.simul.balance.plot <- renderPlot({test.simul.balance.plot()})
  
  output$balance.plot.download <- downloadHandler(
    filename = paste0("archeofrag-fragment-balance-", input$spatial.variable, "-",
                      gsub(" / ", "-", names(graph.list())[as.numeric(input$units.pair)]), ".svg"),
    content = function(file) {
      ggplot2::ggsave(file, plot = test.simul.balance.plot(), device = "svg", width=10, height=3, pointsize = 14)
    }
  )
  
  output$balance.plot.download.button <- renderUI({
    if(is.null(test.simul.balance.plot())) return()
    downloadButton("balance.plot.download", "as SVG") 
  })
  
  
  # VISUALISATION ####
  output$visualisation.title <- renderText({
    units.pair <- names(graph.list())[as.numeric(input$units.pair)]
    
    paste0("Fragmentation graph for spatial units <b>", units.pair, 
           "</b> from the <b>",  input$spatial.variable, 
           "</b> variable. 
            <ul>
              <li>lines: connection relationships</li> 
              <li>nodes: fragments</li>
              <li>color: spatial unit associated with the fragments (<font color=red>red</font> for <b>",  
           gsub("^(.*)/.*", "\\1", units.pair), 
           "</b>, <font color=purple>purple</font> for <b>", gsub("^.*/(.*)", "\\1", units.pair), 
           "</b>)</li></ul>")
  })
  
  frag.graph.viz <- reactive({   
    req(graph.selected())
    g <- graph.selected()
    igraph::E(g)$weight <- 1 # temporary workaround to deal with unexpected 'negative' weights from frag.edge.weight
    g
  })
  
  output$frag.graph.viz.plot <- renderPlot({ 
    archeofrag::frag.graph.plot(frag.graph.viz(), layer.attr = "spatial.variable") 
  })
  
  
  output$frag.graph.viz.download <- downloadHandler(
    filename = paste0("archeofrag-frag-graph-", input$spatial.variable, "-",
                      gsub(" / ", "-", names(graph.list())[as.numeric(input$units.pair)]), ".svg"),
    content = function(file) {
      grDevices::svg(file)
      archeofrag::frag.graph.plot(frag.graph.viz(), layer.attr = "spatial.variable") 
      grDevices::dev.off()
    }
  )
  
  output$frag.graph.viz.download.button <- renderUI({
    if(is.null(frag.graph.viz())) return()
    downloadButton("frag.graph.viz.download", "Download as SVG")
  })
  
  
  
  # R CODE ----
  
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
             "              g <- archeofrag::frag.observer.failure(g, likelihood = ", input$edge.loss, " / 100,<br>",
             "                                                     remove.vertices = TRUE)[[1]]<br>",
             "              n.frag.to.remove <- round((", input$vertice.loss, " / 100) * igraph::gorder(g), 0)<br>",
             "              g <- archeofrag::frag.graph.reduce(g, n.frag.to.remove = n.frag.to.remove,<br>",
             "                                                   conserve.objects.nr = FALSE)<br>",
             "              data.frame(<br>",
             "                 'admixture'       = round(frag.layers.admixture(g, 'layer'), 3),<br>",
             "                 'cohesion'        = rbind(frag.layers.cohesion(g, 'layer')),<br>",
             "                 'relations'       = igraph::gsize(g),<br>",
             "                 'balance'         = table(igraph::V(g)$layer)[1] / igraph::gorder(g),<br>",
             "                 'weights.sum'     = sum(igraph::E(g)$weight),<br>",
             "                 'weights.median'  = stats::median(igraph::E(g)$weight),<br>",
             "                 'weights.sd'      = stats::sd(igraph::E(g)$weight)<br>",
             "              )<br>",
             "       }", 
             "</pre>")
    }
    
    
    parallel.string <- ""
    if(input$parallelize) parallel.string <- "<br>library(doParallel)<br>registerDoParallel()"
    
    paste0("<pre>library(archeofrag) <br>library(igraph) <br>library(foreach)", parallel.string, "</pre>",
           generate.run.code(1), 
           "<br><br>", 
           generate.run.code(2))
    
  }) # end reactive
  
}
