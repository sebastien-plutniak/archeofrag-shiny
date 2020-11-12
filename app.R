library(shiny)
library(shinythemes)
library(archeofrag)
library(ggplot2)
library(dplyr)
library(foreach)
library(tidyr) # for pivot_longer()
library(doParallel)
registerDoParallel()
data(LiangAbu)


ui <- shinyUI(fluidPage(
  theme = shinytheme("slate"),  # slate  flatly
  
  fluidRow(
    column(2,
           h3("Archeofrag"),
           p(
             a("S. Plutniak", href="https://sebastien-plutniak.github.io",  target="_blank"),
             "/",
             a("Github", href="https://github.com/sebastien-plutniak/archeofrag/",  target="_blank")
           ),
           h4("Input"),
           fileInput('inputEdges', 'csv file (edges):',
                     accept=c('text/csv', 'text/comma-separated-values,text/plain')),
           fileInput('inputNodes', 'csv file (nodes):',
                     accept=c('text/csv', 'text/comma-separated-values,text/plain')),
           radioButtons(inputId = 'sep', label = 'Separator:', 
                        choices = c("," =',' , ";"=';'
                                    ,"tab"='\t'), inline=T, selected = ','),
           
           checkboxInput("use_example", "use example data", value = F),
      
           h4("Compare to simulation"),
           actionButton("goButton", "Run"),
           numericInput("replications", "Replications", 50, min=30, max=500, width="50%")
    ),
    column(10, align="center",
        conditionalPanel(condition = "typeof output.resultsTab == 'undefined'",    
        tags$div(
          HTML("<div style=width:370px;, align=left>
                <br><br><br><br>
                <p><b>Welcome to <i>archeofrag</i></b></p>
                <p>This is the companion application of the <i>archeofrag</i>
                R package, a package for the analysis of refitting pieces 
                and stratigraphy in archaeology. Its main purpose is to
                measure the cohesion of two layers and their admixture, from 
                the distribution of the refitting relationships between
                fragments and their topology.</p>
                <p>This application demonstrates some of the features 
                of the package, which can be applied either to the example data
                set (refitting data from the Liang Abu rock shelter, Borneo) or
                to your data.</p>
                <p>Use the two buttons on the left to upload your csv files. 
                The edges table must have a row for each a refit, with 
                two columns containing the identifiers of the two fragments;
                the nodes table must have a row for each fragment, the first
                column contains the fragments identifiers and the second column
                contains their layer.</p>
                <p>
                The code and more information are available on
                <a target=_blank, href=https://github.com/sebastien-plutniak/archeofrag/>github</a>.
                </p></div>") )
    ),
    conditionalPanel(condition = "typeof resultsTab !== 'undefined'",
        tableOutput("resultsTab")
    ),
    
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
                 </div>") )
    ),
    conditionalPanel(condition = "typeof test.simul.edges.plot !== 'undefined'",
          textOutput("launched"),
          imageOutput("test.simul.edges.plot", height = "200px", width= "100%"),
          imageOutput("test.simul.weightsum.plot", height = "200px", width= "100%"),
          imageOutput("test.simul.disturbance.plot", height = "200px", width= "100%"),
          imageOutput("test.simul.balance.plot", height = "200px", width= "100%"),
          imageOutput("test.simul.admixture.plot", height = "200px", width= "100%"),
          imageOutput("test.simul.cohesion.plot", height = "400px", width= "100%")
          ) #end conditionalPanel
         
    ) #end column
  ) # end fluid row 
  ) # end fluid page
) #end UI



# DEFINE SERVER  ----    
server <- function(input, output) { 
  
  userNodes <- reactive({
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
  
  graph <- reactive({
    if(input$use_example == F){
      edges <- read.csv(userEdges()$datapath, header=T, sep=input$sep)
      nodes <- read.csv(userNodes()$datapath, header=T, sep=input$sep)
      nodes$layer <- nodes[,2]
      
      graph <- make_frag_object(edges, fragments = nodes)
      graph <- make_cr_graph(graph)
      graph <- frag.get.layers.pair(graph, "layer", unique(nodes$layer))
      frag.edges.weighting(graph, "layer")
      
    }else{
      graph <- make_frag_object(df.cr, fragments = fragments.info)
      graph <- make_cr_graph(graph)
      graph <- frag.get.layers.pair(graph, "layer", c(1,2))
      frag.edges.weighting(graph, "layer")
    }
  })
  
  output$resultsTab <- renderTable({  # numerical results ----
    req(graph())
    g <- graph()
    stats <- frag.get.parameters(g, "layer")
    cohesion <- frag.layers.cohesion(g, "layer")
    data.frame(
      "Objects" =  clusters(g)$n,
      "Fragments" = gorder(g),
      "Relations" = as.integer(gsize(g)),
      "Balance" = stats$balance,
      "Cohesion layer 1" = cohesion[1],
      "Cohesion layer 2" = cohesion[2],
      "Admixture" =  frag.layers.admixture(g, "layer")
    )
  })
  
  hypotheses <- eventReactive(input$goButton, {
    req(input$replications)
    req(graph())
    graph <- graph()
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
    obs.graph <- graph()
    ggplot(hypotheses.df, aes(x= e.obs, fill = hypothesis)) +
      theme_light(base_size = 12) +
      geom_density(alpha=.5, size=.3) +
      scale_fill_grey(start = .4, end = .9) +
      geom_vline(xintercept = gsize(obs.graph))  + 
      xlab("Edge count") + ggtitle("Edge count")
  })
    
  output$test.simul.weightsum.plot <- renderPlot({
    hypotheses.df <- hypotheses()
    obs.graph <- graph()
    ggplot(hypotheses.df, aes(x = weightsum,  fill= hypothesis)) +
      theme_light(base_size = 12) +
      geom_density(alpha=.5, size=.3) +
      scale_fill_grey(start = .4, end = .9)  +
      geom_vline(xintercept = sum(E(obs.graph)$weight)) +
      xlab("Edge weights sum") + ggtitle("Edge weighs sum")
  })
  
  output$test.simul.disturbance.plot <- renderPlot({
    hypotheses.df <- hypotheses()
    obs.graph <- graph()
    stats <- frag.get.parameters(obs.graph, "layer")
    
    ggplot(hypotheses.df, aes(x= dist.obs, fill= hypothesis)) +
      theme_light(base_size = 12) +
      geom_density(alpha=.5, size=.3, adjust=1.5) +
      geom_vline(xintercept = stats$disturbance) +
      scale_fill_grey(start = .4, end = .9) +
      xlab("Disturbance") + ggtitle("Disturbance")
  })  
    
  output$test.simul.balance.plot <- renderPlot({
    hypotheses.df <- hypotheses()
    obs.graph <- graph()
    stats <- frag.get.parameters(obs.graph, "layer")
    
    ggplot(hypotheses.df, aes(x= bal.obs, fill = hypothesis)) +
      theme_light(base_size = 12) +
      geom_density(alpha=.5, size=.3, bw=.01)  +
      geom_vline(xintercept = stats$balance) +
      scale_x_continuous("Balance", breaks = seq(.2, .4, .02), limits = c(.25,.4)) +
      scale_fill_grey(start = .4, end = .9) + ggtitle("Balance")
  })
    
  output$test.simul.admixture.plot <- renderPlot({
      hypotheses.df <- hypotheses()
      obs.graph <- graph()    
      
      ggplot(hypotheses.df, aes(x = structural_admixture, fill= hypothesis)) +
      theme_light(base_size = 12) +
      geom_density(alpha=.5, size=.3) +
      scale_fill_grey("Hypothesis", start = .4, end = .9) +
      geom_vline(xintercept = frag.layers.admixture(obs.graph, "layer")) +
      xlab("Admixture") + ggtitle("Admixture")
    })
    
  output$test.simul.cohesion.plot <- renderPlot({
      hypotheses.df <- hypotheses()
      obs.graph <- graph()
      
      hypotheses.df2 <- pivot_longer(hypotheses.df, c("cohes.1", "cohes.2"),
                                   names_to = "Layer")
      hypotheses.df2$Layer <- factor(hypotheses.df2$Layer, labels = c("1", "2"))
      hypotheses.df2$hypothesis <- factor(hypotheses.df2$hypothesis, 
                                          labels=c("Hypothesis 1", "Hypothesis 2"))
      
      ggplot(hypotheses.df2, aes(x = value, fill=Layer, linetype=Layer)) +
        theme_light(base_size = 12) +
        geom_density(alpha=.5, size=.3) +
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


