# DEFINE SERVER  ----    
server <- function(input, output, session) { 
  .data <- NULL  
  
  # parallelize box, count n workers
  output$parallelize.box <- renderUI({
    checkboxInput("parallelize",
                  paste0("Parallelize (n workers: ",
                         foreach::getDoParWorkers(), ")"), value=T)
  })
  
  # data input ----
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
  
  graph.data <- reactive({
    
    if(input$use_example != "-") {
      if(input$use_example == "Liang Abu"){
        utils::data("LiangAbu")
        edges.df <- df.cr
        objects.df <- fragments.info
      } else if(input$use_example == "Tai"){
        utils::data("Tai")
        edges.df <- tai.connection
        objects.df <- tai.fragments
      } else if(input$use_example == "Font-Juvenal"){
        utils::data("Fontjuvenal")
        edges.df <- fontjuvenal.connection
        objects.df <- fontjuvenal.fragments
      }      
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
  # pair of units selector ----
  output$layers.selector <- renderUI({
    req(graph())
    
    g.list <- graph()
    
    choices.val <- 1:length(g.list)
    names(choices.val) <- names(g.list)
    
    selectInput("units.pair", "Pair of spatial units",
                selected = choices.val[1],
                choices = choices.val, width= "90%")
  })
  
  output$morpho.selector <- renderUI({
    req(graph.data())
    
    g.data <- graph.data()
    objects.df <- g.data$objects.df
    
    choices.val <- c("-", names(objects.df))
    
    selectInput("morpho.variable", "Morphometry variable",
                choices = choices.val, width= "90%")
  })
  
  output$x.selector <- renderUI({
    req(graph.data())
    
    g.data <- graph.data()
    objects.df <- g.data$objects.df
    
    choices.val <- c("-", names(objects.df))
    
    selectInput("x.variable", "X coordinates",
                choices = choices.val, width= "90%")
  })
  
  output$y.selector <- renderUI({
    req(graph.data())
    
    g.data <- graph.data()
    objects.df <- g.data$objects.df
    
    choices.val <- c("-", names(objects.df))
    
    selectInput("y.variable", "Y coordinates",
                choices = choices.val, width= "90%")
  })
  
  output$z.selector <- renderUI({
    req(graph.data())
    
    g.data <- graph.data()
    objects.df <- g.data$objects.df
    
    choices.val <- c("-", names(objects.df))
    
    selectInput("z.variable", "Z coordinates",
                choices = choices.val, width= "90%")
  })
  
  
  
  # MAKE GRAPH ----
  graph <- reactive({ 
    req(graph.data2)
    
    g.data <- graph.data2()
    objects.df <- g.data$objects.df
    edges.df <- g.data$edges.df
    
    graph <- make_frag_object(edges.df, fragments = objects.df)
    graph <- make_cr_graph(graph)
    
    pairs <- utils::combn(sort(unique(igraph::V(graph)$spatial.variable)), 2)
    
    morpho.variable <- ""
    x.variable <- ""
    y.variable <- ""
    z.variable <- ""
    if( ! is.null(input$morpho.variable)){
      if( input$morpho.variable != "-"){
        morpho.variable <- input$morpho.variable
        x.variable <- input$x.variable
        y.variable <- input$y.variable
        z.variable <- input$z.variable
      }
    }
    
    g.list <- lapply(seq(1, ncol(pairs)), function(x, morpho.var = morpho.variable, 
                                                   x.var = x.variable, 
                                                   y.var = y.variable, 
                                                   z.var = z.variable){
      g <- frag.get.layers.pair(graph, "spatial.variable", pairs[, x]  )
      if(is.null(g)){ return() }
      if(length(unique(igraph::V(g)$spatial.variable)) != 2){ return() }
      
      frag.edges.weighting(g, "spatial.variable", morphometry = morpho.var, 
                           x = x.var, y = y.var, z = z.var)
    })
    names(g.list) <- sapply(seq(1, ncol(pairs)), function(x)
      paste(pairs[1, x], "/", pairs[2, x]))
    
    g.list[ ! sapply(g.list, is.null)]
  })
  
  # GET GRAPH PARAMS ----
  input.graph.params <- reactive({ 
    req(graph.selected())
    archeofrag::frag.get.parameters(graph.selected(), "spatial.variable")
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
        balance <- archeofrag::frag.get.parameters(g, "spatial.variable")$balance
      }
      cohesion <- round(archeofrag::frag.layers.cohesion(g, "spatial.variable"), 2)
      cohesion.ratio <- sort(cohesion)
      cohesion.ratio <- cohesion.ratio[2] / cohesion.ratio[1]
      data.frame(
        "Pair of spatial units" = paste(sort(unique(igraph::V(g)$spatial.variable)), collapse=" / "),
        "N. Objects" =  as.integer(igraph::count_components(g)),
        "N. Fragments" = as.integer(igraph::gorder(g)),
        "N. Relations" = as.integer(igraph::gsize(g)),
        "Balance" = balance,
        "Cohesion 1st unit" = cohesion[1],
        "Cohesion 2nd unit" = cohesion[2],
        "Cohesion ratio" = round(cohesion.ratio, 1),
        "Admixture" =  round(archeofrag::frag.layers.admixture(g, "spatial.variable"), 2)
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
    
    res <- stats::reshape(pairs, timevar = "unit1", idvar = "unit2",  v.names = "Admixture", direction = "wide")
    colnames(res) <- gsub("^.*\\.(.*)", "\\1", colnames(res))
    rownames(res) <- res[, 1]
    res <- res[, -1]
    res[ order(rownames(res)), order(colnames(res))]
    
    # reshape2::acast(pairs, unit2 ~ unit1, value.var = "Admixture", na.rm = F, drop = F)
  })
  
  output$admixTab <- renderTable({ 
    req(admixTab())
    admixTab()
  }, rownames = T, colnames = T, na = "-")
  
  
  output$admix.plot <- renderPlot({  # admix plot ----
    req(admixTab())
    admixTab <- 1 - admixTab()
    admixTab[is.na(admixTab)] <- 1
    
    admixTab <- stats::as.dist(admixTab)
    
    dend.plot <- stats::as.dendrogram(stats::hclust(admixTab, method = "complete"))
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

 # Function to exetute the simulation and retrieve some measures:
 exec.simulation  <- function(initial.layers, n.components, vertices,
                              balance, components.balance, disturbance,
                              aggreg.factor, planar, asymmetric.transport.from){
      g <- frag.simul.process(initial.layers, n.components, vertices, edges = Inf,
                              balance, components.balance, disturbance,
                              aggreg.factor, planar, asymmetric.transport.from)
      g <- frag.edges.weighting(g, "layer")
      inter.layer.e <- igraph::E(g)[igraph::V(g)[igraph::V(g)$layer == 1] %--% igraph::V(g)[igraph::V(g)$layer == 2]]
      data.frame(
        "structural_admixture" = round(frag.layers.admixture(g, "layer"), 3),
        "cohes" = rbind(frag.layers.cohesion(g, "layer")),
        "v.obs" = igraph::gorder(g),
        "e.obs" = igraph::gsize(g),
        "bal.obs" = sort(table(igraph::V(g)$layer))[1] / sum(table(igraph::V(g)$layer)),
        "dist.obs" = length(inter.layer.e) / igraph::gsize(g),
        "weightsum" = sum(igraph::E(g)$weight)
      )    
    }
    
        
    if(input$parallelize){
    hypothese1.res <- foreach(i = seq(1, input$replications),  .combine = "rbind",
                              .errorhandling = "remove") %dopar%{
                                exec.simulation(initial.layers = 1,
                                                n.components = params$n.components,
                                                vertices = params$n.final.fragments,  
                                                balance = params$balance,
                                                components.balance = params$components.balance,
                                                disturbance = params$disturbance,
                                                aggreg.factor = params$aggreg.factor,
                                                planar = params$planar,
                                                asymmetric.transport.from = asymmetric)
                              }
    
    hypothese2.res <- foreach(i = seq(1, input$replications), .combine = "rbind",
                              .errorhandling = "remove") %dopar%{
                                exec.simulation(initial.layers = 2,
                                                n.components = params$n.components,
                                                vertices = params$n.final.fragments,  
                                                balance = params$balance,
                                                components.balance = params$components.balance,
                                                disturbance = params$disturbance,
                                                aggreg.factor = params$aggreg.factor,
                                                planar = params$planar,
                                                asymmetric.transport.from = asymmetric)
                              }
    
    } else {
      hypothese1.res <- foreach(i = seq(1, input$replications),  .combine = "rbind",
                                .errorhandling = "remove") %do%{
                                  exec.simulation(initial.layers = 1,
                                                  n.components = params$n.components,
                                                  vertices = params$n.final.fragments,  
                                                  balance = params$balance,
                                                  components.balance = params$components.balance,
                                                  disturbance = params$disturbance,
                                                  aggreg.factor = params$aggreg.factor,
                                                  planar = params$planar,
                                                  asymmetric.transport.from = asymmetric)
                                }
      
      hypothese2.res <- foreach(i = seq(1, input$replications),  .combine = "rbind",
                                .errorhandling = "remove") %do%{
                                  exec.simulation(initial.layers = 2,
                                                  n.components = params$n.components,
                                                  vertices = params$n.final.fragments,  
                                                  balance = params$balance,
                                                  components.balance = params$components.balance,
                                                  disturbance = params$disturbance,
                                                  aggreg.factor = params$aggreg.factor,
                                                  planar = params$planar,
                                                  asymmetric.transport.from = asymmetric)
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
                                       layer.attr = "spatial.variable", 
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
    
    ggplot2::ggplot(hypotheses.df, ggplot2::aes(x= .data[["e.obs"]], fill = .data[["hypothesis"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3) +
      ggplot2::scale_fill_grey(start = .4, end = .9) +
      ggplot2::geom_vline(xintercept = igraph::gsize(obs.graph))  + 
      ggplot2::xlab("Relations count") + ggplot2::ggtitle("Relations count")
  })
  
  output$test.simul.edges.plot <- renderPlot({test.simul.edges.plot()})
  
  
  test.simul.weightsum.plot <- eventReactive(input$goButton, { # plot weights ####
    hypotheses.df <- hypotheses()
    obs.graph <- graph.selected()
    
    ggplot2::ggplot(hypotheses.df, ggplot2::aes(x = .data[["weightsum"]],  fill= .data[["hypothesis"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3) +
      ggplot2::scale_fill_grey(start = .4, end = .9)  +
      ggplot2::geom_vline(xintercept = sum(igraph::E(obs.graph)$weight)) +
      ggplot2::xlab("Sum of relation weights") + ggplot2::ggtitle("Sum of relation weights")
  })
  
  output$test.simul.weightsum.plot <-  renderPlot({test.simul.weightsum.plot()})
  
  
  test.simul.disturbance.plot <-  eventReactive(input$goButton, {  # plot disturbance ####
    hypotheses.df <- hypotheses()
    obs.graph <- graph.selected()
    
    stats <- archeofrag::frag.get.parameters(obs.graph, "spatial.variable")
    
    ggplot2::ggplot(hypotheses.df, ggplot2::aes(x= .data[["dist.obs"]], fill= .data[["hypothesis"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3, adjust=1.5) +
      ggplot2::geom_vline(xintercept = stats$disturbance) +
      ggplot2::scale_fill_grey(start = .4, end = .9) +
      ggplot2::xlab("Disturbance") + ggplot2::ggtitle("Disturbance")
  })  
  
  output$test.simul.disturbance.plot <- renderPlot({ test.simul.disturbance.plot()})
  
  
  
  test.simul.balance.plot <-  eventReactive(input$goButton, {  # plot balance ####
    hypotheses.df <- hypotheses()
    obs.graph <- graph.selected()
    
    stats <- archeofrag::frag.get.parameters(obs.graph, "spatial.variable")
    
    ggplot2::ggplot(hypotheses.df, ggplot2::aes(x= .data[["bal.obs"]], fill = .data[["hypothesis"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3, bw=.01)  +
      ggplot2::geom_vline(xintercept = stats$balance) +
      ggplot2::scale_x_continuous("Balance", limits = c(.0, .5)) + #, breaks = seq(.2, .4, .02)) + #) +
      ggplot2::scale_fill_grey(start = .4, end = .9) + ggplot2::ggtitle("Balance")
  })
  output$test.simul.balance.plot <- renderPlot({test.simul.balance.plot()})
  
  
  test.simul.admixture.plot <- eventReactive(input$goButton, {   # plot admixture ####
    hypotheses.df <- hypotheses()
    obs.graph <- graph.selected() 
    
    ggplot2::ggplot(hypotheses.df, ggplot2::aes(x = .data[["structural_admixture"]], fill= .data[["hypothesis"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3) +
      ggplot2::scale_fill_grey("Hypothesis", start = .4, end = .9) +
      ggplot2::geom_vline(xintercept = round(frag.layers.admixture(obs.graph, "spatial.variable"), 3)) +
      ggplot2::xlab("Admixture") + ggplot2::ggtitle("Admixture")
  })
  
  output$test.simul.admixture.plot <- renderPlot({ test.simul.admixture.plot()  })
  
  
  test.simul.cohesion.plot <- eventReactive(input$goButton, {   # plot cohesion ####
    hypotheses.df <- hypotheses()
    obs.graph <- graph.selected()
    
    hypotheses.df2 <- stats::reshape(hypotheses.df, dir = "long", varying = names(hypotheses.df)[2:3],
                              v.names = "value", timevar="Layer")
    
    hypotheses.df2$Layer <- factor(hypotheses.df2$Layer, labels = c("1", "2"))
    hypotheses.df2$hypothesis <- factor(hypotheses.df2$hypothesis, 
                                        labels=c("Hypothesis 1", "Hypothesis 2"))
    
    ggplot2::ggplot(hypotheses.df2, ggplot2::aes(x = .data[["value"]], fill= .data[["Layer"]])) +
      ggplot2::theme_light(base_size = 12) +
      ggplot2::geom_density(alpha=.5, linewidth=.3) +
      ggplot2::geom_boxplot(outlier.shape = 21) +
      ggplot2::scale_fill_grey(start = .9, end = .4) +
      ggplot2::geom_vline(xintercept = frag.layers.cohesion(obs.graph, "spatial.variable")[1], linetype=1) +
      ggplot2::geom_vline(xintercept = frag.layers.cohesion(obs.graph, "spatial.variable")[2], linetype=2) +
      ggplot2::facet_wrap(~hypothesis, ncol=1) +
      ggplot2::scale_x_continuous("Cohesion", limits=c(0,1)) + ggplot2::ggtitle("Cohesion")
  })
  output$test.simul.cohesion.plot <- renderPlot({test.simul.cohesion.plot()})
  
  
  
  output$visualisation.plot <- renderPlot({   # VISUALISATION ####
    req(graph.selected())
    frag.graph.plot(graph.selected(), layer.attr = "spatial.variable")
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
    "              inter.layer.e <- igraph::E(g)[igraph::V(g)[igraph::V(g)$layer == 1] %--% igraph::V(g)[igraph::V(g)$layer == 2]]<br>",
    "              data.frame(<br>",
    "                 'admixture'       = round(frag.layers.admixture(g, 'layer'), 3),<br>",
    "                 'cohesion'        = rbind(frag.layers.cohesion(g, 'layer')),<br>",
    "                 'relations'       = igraph::gsize(g),<br>",
    "                 'balance'         = sort(table(igraph::V(g)$layer))[1] / sum(table(igraph::V(g)$layer)),<br>",
    "                 'disturbance'     = length(inter.layer.e) / igraph::gsize(g),<br>",
    "                 'weights.sum'     = sum(igraph::E(g)$weight)<br>",
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
