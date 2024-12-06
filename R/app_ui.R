


ui <- shinyUI(fluidPage(  # UI ----
                          theme = shinytheme("cosmo"),  # slate  flatly
                          sidebarLayout(
                            sidebarPanel(                  
                              h3(div(HTML("<a href=https://github.com/sebastien-plutniak/archeofrag.gui title='Go to the archeofrag.gui page' target=_blank>archeofrag-gui</a> v",  as.character(utils::packageVersion("archeofrag.gui")) ))),
                              div(HTML("using <a href=https://github.com/sebastien-plutniak/archeofrag title='Go to the archeofrag page' target=_blank>archeofrag</a> v",  as.character(utils::packageVersion("archeofrag")) )),
                              h3("Input data"),
                              selectInput("use_example", "Load example data", 
                                          choices = c("-", "Font-Juvenal", "Liang Abu", "Tai"), selected = "-"),
                              fileInput('inputEdges', 'Relations (CSV file):',
                                        width="70%",
                                        accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                              fileInput('inputNodes', 'Fragments (CSV file):',
                                        width="70%",
                                        accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                              radioButtons(inputId = 'sep', label = 'Separator:', 
                                           choices = c("," =',' , ";"=';'
                                                       ,"tab"='\t'), inline=T, selected = ','),
                              h3("Variable selection"),
                              uiOutput("variable.selector"),
                              uiOutput("layers.selector"),
                              width=2), # end sidebarpanel
                            
                            mainPanel(
                              tabsetPanel(id="tabs",
                                          tabPanel("Introduction", # Introduction ----      
                                                   column(10, align="center",
                                                          tags$div(
                                                            HTML("<div style=width:40%;, align=left>
                <h1><i>archeofrag-gui</i></h1>
                <p>
                This application implements some features of the
                <i><a href=https://cran.r-project.org/web/packages/archeofrag/index.html target=_blank>archeofrag</a></i> R package for spatial analysis in archaeology from the study of refitting fragments of objects. Based on the TSAR method (Topological Study of Archaeological Refitting),  it includes functions to <b>evaluate and validate</b> the distinction between <b>archaeological spatial units</b> (e.g. layers), from the distribution and the topology of the refitting relationships between the fragments contained in these units.
                </p>
                <h3>Input Data</h3>
                <p>
                  Use the menu on the left to upload your “relations” and “fragments” data as CSV files. 
                  <ul>
                    <li>The <b>relations</b> table must have a row for each refitting relationship and two columns containing the identifiers of each pair of refitting fragments, respectively;</li>
                    <li>the <b>fragments</b> table must have a row for each fragment and, at least, a column for fragments unique identifiers and a column for the spatial units they belong to. Optionally, columns with morphometric data (e.g. length, surface) and with X, Y, Z coordinates of the fragments can be used.</li>
                  </ul>
                  Alternatively, load one of the example datasets.
                </p>
                <h3>Variable selection</h3>
                  <p>Use the menu on the left to select:
                     <ul>
                      <li>the <b>spatial variable</b>  to consider (i.e. the spatial unit containing the fragments)
                      <li>the <b>pair of spatial units</b> to consider: this selection determines the plot generated in the “Visualisation” tab and the simulation presets in the “Simulation” tab.</li>
                    </ul>
                  </p>
                <h3>Measurements</h3>
                <p>In this tab, statistics are reported for all pairs of spatial units for the selected “Spatial variable”: number of fragments and refitting relationships, etc. The <b>cohesion</b> and <b>admixture</b> values are calculated using the TSAR method. Tables and figures facilitate the exploration of the results.</p>
                <h3>Comparison with simulated data</h3>
                <p>This tab presents functions to investigate the formation process of the selected pair of spatial units. 
                Simulation is used to compare it to similar artificial data, while controlling differences in some parameters.
                Here, the selected pair of spatial units is compared to simulated data generated under two formation hypotheses:
                <ul>
                    <li>H1, the archaeological material studied comes from a <b>single deposition event</b>.</li>
                    <li>H2, the material was deposited during <b>two deposition events</b>.</li>
                </ul>
                 Using the <i>archeofrag</i> R package, other hypotheses can be tested by adjusting the simulator parameters.
                </p>
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
                  <li><b>Font-Juvénal</b>:  Caro J. 2020. <i>Productions céramiques et dynamiques des sociétés au Ve millénaire avant notre ère : la transition du Néolithique ancien au Néolithique moyen dans le bassin Nord-occidental de la Méditerranée</i>. PhD Dissertation, Toulouse University, HAL:  <a href=https://theses.hal.science/tel-03613599  target=_blank>tel-036135996</a> </li>                
                  <li><b>Liang Abu</b>: Plutniak S. 2021. “Refitting Pottery Fragments from the Liang Abu Rockshelter, Borneo”. DOI: <a href=https://doi.org/10.5281/zenodo.4719577 target=_blank>10.5281/zenodo.4719577</a> </li>
                  <li><b>Taï</b>:  Caro J., Plutniak S. 2022. “Refitting and Matching Neolithic Pottery Fragments from the Taï site, France”. DOI:  <a href=https://doi.org/10.5281/zenodo.7408706 target=_blank>10.5281/zenodo.7408706</a> </li>
                </ul>
                <br>
                                                                 </div>"))
                                                   ) # end column
                                          ), #end tabPanel
                                          
                                          tabPanel("Measurements", # Measurements ----
                                                   br(),
                                                   h1("Weighting options"),
                                                   fluidRow(
                                                     column(2, uiOutput("morpho.selector")),
                                                     column(1, uiOutput("x.selector")),
                                                     column(1, uiOutput("y.selector")),
                                                     column(1, uiOutput("z.selector"))
                                                   ), #end fluidrow
                                                   "Note that these weighting options are not supported by the simulation function.",
                                                   h1("Stats by pair of spatial units"),
                                                   DT::DTOutput("resultsTab",  width="80%"), 
                                                   HTML("
                                                        <ul>
                                                          <li><b>Balance</b>: proportion of fragments in the poorest spatial unit</li>
                                                          <li><b>Cohesion ratio</b>: highest cohesion value / lower cohesion value </li>
                                                        </ul>
                                                        "),
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
                      <li>Select the pair of spatial units to compare in the menu.</li>
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
                      Charts are generated to compare parameter values measured on the empirical fragmentation graph and on the artificial graphs: 
                      <ul>
                      <li>the value observed on the empirical graph is represented by a vertical bar, </li>
                      <li>the distribution of values for each hypotheses are represented by dark (H1) and light (H2) grey shades, respectively.</li>
                      </ul>
                      </p>
                    </p>
                 </div>") )
                                                   ) # end column
                                                   ), # end fluidrow
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
                                                            column(2,  uiOutput("parallelize.box"),
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
