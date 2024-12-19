
css <- '
.tooltip {
  pointer-events: none;
}
.tooltip > .tooltip-inner {
  pointer-events: none;
  background-color: #FFFFFF;
  color: #000000;
  border: 1px solid black;
  padding: 5px;
  font-size: 13px;
  text-align: left;
  max-width: 300px; 
}
.tooltip > .arrow::before {
  border-right-color: #73AD21;
}
'

js <- "
$(function () {
  $('[data-toggle=tooltip]').tooltip()
})
"



ui <- shinyUI(fluidPage(  # UI ----
                          theme = shinytheme("cosmo"),  # slate  flatly
                          tags$head(
                            tags$style(HTML(css)),
                            tags$script(HTML(js))
                          ),
                          sidebarLayout(
                            sidebarPanel(
                              h3(div(HTML("<a href=https://github.com/sebastien-plutniak/archeofrag.gui title='Go to the archeofrag.gui page' target=_blank>archeofrag.gui</a> v",  as.character(utils::packageVersion("archeofrag.gui")) ))),
                              div(HTML("using <a href=https://github.com/sebastien-plutniak/archeofrag title='Go to the archeofrag page' target=_blank>archeofrag</a> v",  as.character(utils::packageVersion("archeofrag")) )),
                              h3("Input data"),
                              uiOutput("dataset.selector"),
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
                                                   column(11, align="center",
                                                          tags$div(
                                                            HTML("<div style=width:40%;, align=left>
                <h1><i>archeofrag.gui</i></h1>
                <br>
                <img width='100%' src=www/general-idea.png><br><br>
                <p>
                This application implements and complements some features of the
                <i><a href=https://cran.r-project.org/web/packages/archeofrag/index.html target=_blank>archeofrag</a></i> R package for spatial analysis in archaeology from the study of refitting fragments of objects. Based on the TSAR method (Topological Study of Archaeological Refitting),  it includes functions to <b>evaluate and validate</b> the distinction between <b>archaeological spatial units</b> (e.g. layers), from the distribution and the topology of the refitting relationships between the fragments contained in these units.
                </p>
                <h3>Input Data</h3>
                <p>
                  Use the menu on the left to upload your 'relations' and 'fragments' data as CSV files. 
                  <ul>
                    <li>The <b>relations</b> table must have a row for each refitting relationship and two columns containing the identifiers of each pair of refitting fragments, respectively;</li>
                    <li>The <b>fragments</b> table must have a row for each fragment and, at least, a column for fragments' unique identifiers and a column for the spatial units they belong to. Optionally, columns with morphometric data (e.g. length, surface) and with the fragments' X, Y, Z coordinates  can be used.</li>
                  </ul>
                  Alternatively, load one of the real-world example datasets (or explore Rubish Site data).
                </p>
                <h3>Variable selection</h3>
                  <p>Use the menu on the left to select:
                     <ul>
                      <li>the <b>spatial variable</b>  to consider (i.e. the spatial unit containing the fragments),
                      <li>the <b>pair of spatial units</b> to consider: this selection determines the plot generated in the 'Visualisation' tab and the simulation presets in the 'Simulation' tab.</li>
                    </ul>
                  </p>
                <h3>Measurements</h3>
                <p>In this tab, statistics are reported for all pairs of spatial units for the selected 'Spatial variable': number of fragments and refitting relationships, etc. The <b>cohesion</b> and <b>admixture</b> values are calculated using the TSAR method. Tables and figures facilitate the exploration of the results.</p>
                <h3>Comparison with simulated data</h3>
                <p>This tab presents functions to investigate the formation process of the selected pair of spatial units. 
                Simulation is used to compare it to similar artificial data while controlling differences in some parameters.
                Here, the chosen pair of spatial units is compared to simulated data generated under two formation hypotheses:
                <ul>
                    <li>H1, the archaeological material studied comes from a <b>single deposition event</b>.</li>
                    <li>H2, the material was deposited during <b>two deposition events</b>.</li>
                </ul>
                 Using the <i>archeofrag</i> R package, other hypotheses can be tested by adjusting the simulator parameters.
                </p>
                <br>
                </div>"))
                                                   ) # end column
                                          ), #end tabPanel
                                          
                                          tabPanel("Measurements", # Measurements ----
                                                   h1("Weighting options"), # .. weigthting options----
                                                   fluidRow(
                                                     column(2, uiOutput("morpho.selector")),
                                                     column(2, uiOutput("x.selector")),
                                                     column(2, uiOutput("y.selector")),
                                                     column(2, uiOutput("z.selector"))
                                                   ), #end fluidrow
                                                   fluidRow(
                                                   column(10, align="center",
                                                   HTML(
                                                  "<div style=width:40%;, align=left>
                                                   <p>Use the <b>morphometry</b> variable to include any sort of morpho-metrical information about the objects in the computation (e.g. length, surface, volume, weight). Use at least two <b>coordinates</b> to include physical distances between the object found places in the computation (whatever the unit: metre, centimetre, inch, etc.). Details about the method are given in Plutniak, Caro, Manen 2023.</p>
                                                   <p>Note that these weighting options are not supported by the simulation function.</p>
                                                  </div>"
                                                   ), #end HTML
                                                   uiOutput("rubish.text")
                                                   ) #end columns
                                                   ), # end fluidrow
                                                   fluidRow(
                                                   h1("Stats by pair of spatial units"),
                                                   column(12, align="center",
                                                   DT::DTOutput("resultsTab",  width="90%"), 
                                                   ), # end column
                                                   column(10, align="left",
                                                   HTML("
                                                        <ul>
                                                          <li><b>Fragments balance</b>: considering only the fragments with connection relationships within their spatial unit, the proportion of fragments in the spatial unit whose label comes first alphanumerically</li>                               
                                                          <li><b>Objects balance</b>: considering only the fragments with connection relationships within their spatial unit, the proportion of objects (i.e. sets of refitted fragments) in the spatial unit whose label comes first alphanumerically</li>
                                                          <li><b>Cohesion</b>: for a pair of spatial units, the measure of the consistency of each unit, how it is 'self-adherent' to itself (see Plutniak 2021)</li>
                                                          <li><b>Cohesion difference</b>: highest cohesion value - lowest cohesion value </li>
                                                        </ul>
                                                        "),
                                                   ) # end column
                                                   ), #end fluirow
                                                   h1("Dissimilarity between spatial units"), # .. dissimilarity ----
                                                   column(10, align="center",
                                                   HTML("<div  style=width:40%;, align=left> 
                                                   <p>The dissimilarity between spatial units A and B is calculated as 1 - admixture(A, B). Results can be normalised (feature scaling) by ticking the box.</p>
                                                   <p>The higher the dissimilarity value, the more likely it is that these two archaeological units correspond to different depositional units. Theoretically, a spatial unit is expected to be more similar to those near it. </p>
                                                   <p>In the case of stratigraphic layers, a layer is expected to be more related to the layers directly above and below it. The dendrogram's branches are ordered alphanumerically according to their label (following the stratigraphic order of the layers). Anomalies are revealed when, despite this ordering constraint, the expected order of superposition is not observed in the result (see <a href=https://doi.org/10.4324/9781003350026-1 target=_blank>Plutniak <i>et al.</i> 2023</a>).
                                                  </p>
                                                  </div> "),
                                                   checkboxInput("normalise.diss", "Normalise", value = F),
                                                   tableOutput("admixTab"),
                                                   imageOutput("admix.plot",  width= "70%"),
                                                   uiOutput("admix.download.button"),
                                                   br(), br()
                                                   ) # end column
                                          ), #end tabPanel
                                          tabPanel("Visualisation", # Visualisation ----
                                                   fluidRow(
                                                     h1("Fragmentation graph"),
                                                     column(10, align="center",
                                                      br(),
                                                       HTML("<div  style=width:40%;, align=left>"),
                                                       uiOutput("visualisation.title"),
                                                       HTML("</div>"),
                                                       br(),
                                                  uiOutput("frag.graph.viz.download.button"),
                                                  imageOutput("frag.graph.viz.plot", height = "800px", width= "100%"),
                                                     ) #end column
                                                   ) # end fluidrow
                                          ), #end tabPanel
                                          tabPanel("Simulation", # Simulation ---- 
                                                   fluidRow(
                                                     h1("Information"),
                                                   column(10, align="center",
                                                            HTML("<div style=width:40%;, align=left>
                    <h2>Instruction</h2>
                    <p>
                      <ul>
                      <li>Select the pair of spatial units to compare in the sidebar menu.</li>
                      <li>The parameters of the simulation are automatically filled with the values measured on the graph corresponding to the two spatial units chosen. However, those parameters can be edited to test alternative hypotheses. The final number of refitting relations is not constrained.</li>
                      <li> Set the number of simulated graphs to generate for each hypothesis, and click on the 'Run' button. Enabling parallelization uses half of the available cores to speed up the computation (however if it raises an error, untick the box, re-run, and be patient).</li>
                      </ul>
                      For details about the site formation model implemented in this simulator see <a href=https://doi.org/10.1016/j.jas.2021.105501 target=_blank>Plutniak 2021</a>, Fig. 7 in particular.
                    </p>
                    <h2>Procedure</h2>
                    <p>
                    Parameters (number of objects, fragments balance, <a href=https://en.wikipedia.org/wiki/Planar_graph target=_blank>planarity</a>, etc.) are extracted from the input graph for the selected pair of spatial units, and used to generate a series of artificial graphs. These graphs are generated for two deposition hypotheses:
                    <ol type='1'>
                     <li> The objects were buried in a <b>single spatial unit</b>, and subsequently moved, afrom what archaeologists distinguished between two spatial units;</li>
                     <li> The objects were buried in <b>two spatial units</b>, and subsequently moved, from what archaeologists distinguished between two spatial units.</li>
                    </ol>
                    </p>
                    <p>
                    <h2>Results</h2>
                    The table below summarises the results for some parameters, indicating:
                   <ul>
                     <li>whether the simulated values for H1 and H2 are significantly different (<a href=https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test target=_blank>Wilcoxon test</a>, 'H1 != H2?' and 'p.value' columns), and 
                     <li>whether the observed value is lower / within / higher than  the interquartile range of values simulated for H1 and H2, respectively ('Obs. value/H1' and 'Obs. value/H2' columns).</li>
                     </ul>
                      Charts are generated to compare parameter values measured on the empirical fragmentation graph and on the artificial graphs: 
                      <ul>
                      <li>The value observed on the empirical graph is represented by a vertical bar, </li>
                      <li>The distributions of values for each hypothesis are represented by dark (H1) and light (H2) grey shades, respectively (except for cohesion).</li>
                      </ul>
                      </p>
                    </p>
                 </div>") 
                                                   ) # end column
                                                   ), # end fluidrow
                                                  fluidRow( # .. parameters ----
                                                     h1("Model parameters set up"),
                                                     h2("Initial state"),
                                                     column(2, 
                                                        span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                              title = "Initial number of objects to create.",
                                                            uiOutput("n.components")
                                                        ) #end span
                                                     ), #end column
                                                     column(3, 
                                                         span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                               title = "Estimated initial proportion of objects in the first (alphanumerically) spatial unit.",
                                                            uiOutput("components.balance")
                                                         ) #end span
                                                     ) #end column
                                                  ), #end fluidrow
                                                  fluidRow(
                                                    h2("Formation process"),
                                                    column(2, 
                                                      span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                        title = "Whether generating or not only planar graphs. Activating this option makes the computation slower.",
                                                           uiOutput("planar")
                                                      ) #end span
                                                    ), #end column
                                                    column(3, 
                                                      span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                                title = "Estimated proportion of fragments to generate in the first (alphanumerically) spatial unit, regardless of disturbance.",
                                                           uiOutput("balance")
                                                      ) #end span
                                                    ), #end column
                                                    column(3, 
                                                      span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                                title = "Higher values increase the likelihood that  largest components are selected when adding fragments or connections.",
                                                           uiOutput("aggreg.factor")
                                                      ) #end span
                                                    ), #end column
                                                    column(2, 
                                                      span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                            title = "Applying disturbance only to fragments from a specific spatial unit.",
                                                           uiOutput("asymmetric")
                                                      ) #end span
                                                    ) #end column
                                                  ), #end fluidrow
                                                  fluidRow(
                                                    column(5, h2("Final state")),
                                                    column(5, h2("Information loss")),
                                                  ),
                                                  fluidRow(
                                                    column(2,
                                                       span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                            title = "Final number of fragments.",
                                                           uiOutput("n.final.fragments")
                                                       ) #end span
                                                    ), #end column
                                                    column(3,
                                                        span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                        title = "Final proportion of fragments moved from a spatial unit to the other.",
                                                           uiOutput("disturbance")
                                                        ) #end span
                                                    ), #end column
                                                  # ), #end fluidrow
                                                  # fluidRow(
                                                    column(3,
                                                           span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                                title = "Proportion of relationships to remove from the final fragmentation graph, to reproduce any relevant reason not to archaeologically observe them. Isolated fragments are also removed.",
                                                                sliderInput("edge.loss", "Connection loss (%)",
                                                                            min = 0, max = 100, step = 1, 
                                                                            value = 0, width = "100%")
                                                           ) #end span
                                                    ), #end column  
                                                    column(3,
                                                           span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                                title = "Proportion of fragments to remove from the final fragmentation graph, to reproduce any relevant reason  not to archaeologically observe them.",
                                                                sliderInput("vertice.loss", "Fragments loss (%)",
                                                                            min = 0, max= 100, step = 1, 
                                                                            value = 0, width = "100%")
                                                           ) #end span
                                                    ), #end column
                                                  ), #end fluidrow
                                                  fluidRow(
                                                      h1("Computation set up"),
                                                      column(1, 
                                                             span(`data-toggle` = "tooltip", `data-placement` = "bottom",
                                                                  title = "[30;1000].",
                                                             numericInput("replications", "Replications",
                                                                          60, min=30, max=1000, width = "100%")
                                                       ) #end span
                                                      ), #end column
                                                      column(1, actionButton("goButton", "Run"), style="padding:27px;"),
                                                      column(2,  uiOutput("parallelize.box"),
                                                             style="padding:27px;")
                                                  ), #end fluidrow
                                                  fluidRow( # .. plots----
                                                      h1("Results"),
                                                      uiOutput("simul.graph.nr"),
                                                        column(10, align="center",
                                                          tableOutput("summary.tab"))
                                                  ), # end fluidrow 
                                                  fluidRow(
                                                    h2("Cohesion by spatial unit"),
                                                    column(10, align="center",
                                                           HTML("<div style=width:40%;, align=left><p>
                                                                Comparing cohesion values is the main purpose of the TSAR method. Because cohesion is a complex measurement combining multiple aspects, this is where differences between compared hypotheses might be more evident and useful for archaeological interpretation. For each hypothesis (top and bottom part of the chart), compare the cohesion values observed for each spatial unit on the empirical graph (purple and yellow vertical bars) and the simulated values (density curves and boxplots).
                                                                </p></div>")
                                                           )),
                                                  fluidRow(column(10,
                                                                  imageOutput("test.simul.cohesion.plot", height = "400px", width= "100%")),
                                                           column(1, uiOutput("cohesion.plot.download.button"),
                                                                  style="padding-top:180px;")
                                                           
                                                  ), # end fluidrow
                                                  fluidRow(
                                                    h2("Admixture"),
                                                    column(10, align="center",
                                                           HTML("<div style=width:40%;, align=left><p>
                                                                The admixture value summarises a pair of cohesion values. Less informative, it is nevertheless simpler and convenient to examine.
                                                                </p></div>")
                                                    )),
                                                  fluidRow(column(10,
                                                                  imageOutput("test.simul.admixture.plot", height = "200px", width= "100%")),
                                                           column(1, uiOutput("admixture.plot.download.button"),
                                                                  style="padding-top:80px;")
                                                  ), #end fluidrow
                                                  fluidRow(
                                                    h2("Fragments balance"),
                                                    column(10, align="center",
                                                           HTML("<div style=width:40%;, align=left><p>
                                                                Fragment balance, here, is a descriptive statistic simply defined as the proportion of fragments included in the first spatial unit, whatever their initial spatial unit (see code in 'R code' tab). Note that this definition differs from that of 'estimated' fragment balance value presented in the 'Measurements' tab  and used to set up the simulation. The rationale behind this is to test whether the empirical balance can be obtained in simulated results from a guessed estimated initial balance.
                                                                </p></div>")
                                                    )),
                                                  fluidRow(column(10,
                                                          imageOutput("test.simul.balance.plot", height = "200px", width= "100%")),
                                                          column(1, uiOutput("balance.plot.download.button"),
                                                                 style="padding-top:80px;")
                                                  ), #end fluidrow
                                                  fluidRow(
                                                    h2("Edge count"),
                                                    column(10, align="center",
                                                           HTML("<div style=width:40%;, align=left><p>
                                                                The number of connection relationships (edges of the graph) is not constrained in this use of the simulator. Consequently, the edge count variability can be used to compare the empirical and simulated fragmentation graphs. Note that this variable is not essential in the TSAR method.
                                                                </p></div>")
                                                    )),
                                                  fluidRow(column(10,
                                                           imageOutput("test.simul.edges.plot", height = "200px", width= "100%")),
                                                           column(1, uiOutput("edges.plot.download.button"),
                                                                  style="padding-top:80px;")
                                                  ), #end fluidrow
                                                  fluidRow(
                                                    h2("Connection strength"),
                                                    column(10, align="center",
                                                           HTML("<div style=width:40%;, align=left><p>
                                                                In the TSAR method, attributing values to the  connection relationships ('edge weighting'), to represent their 'strength', is a crucial step before computing cohesion. In this regard, three statistics are calculated on the edge weights of the simulated graphs to complement the exploration of the simulated results: their median, <a href=https://en.wikipedia.org/wiki/Median_absolute_deviation target=_blank>median absolute deviation</a>, and sum. When comparing results generated about different hypotheses, the distribution of these statistics can help distinguish between graphs mostly made of 'weak' or 'strong' connection relationships.
                                                                </p></div>")
                                                    )),
                                                  fluidRow(column(10,
                                                          imageOutput("test.simul.weights.plot", height = "600px", width= "100%")),
                                                          column(1,  br(), br(), br(),
                                                                 uiOutput("weights.plot.download.button"),
                                                                                style="padding-top:230px;")
                                                  ), #end fluidrow
                                                   ), # end tabPanel
                                          tabPanel("R code", # R code ----
                                                   column(10, 
                                                          br(),
                                                          HTML("The following R code runs the simulation with the current settings and returns a series of values for each hypothesis:
                                                          <ul>
                                                            <li>admixture</li>
                                                            <li>cohesion value for the spatial units 1 and 2</li> 
                                                            <li>number of refitting relations</li>
                                                            <li>fragments balance</li>
                                                            <li>sum, median, and standard deviation for relation weights</li>"),
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
                                          tabPanel("References", # References ----  
                                                   column(10, align="center",
                                                          tags$div(
                                                            HTML("<div style=width:40%;, align=left>
                <h2>About <i>archeofrag</i></h2>
                <p>
                To cite <i>archeofrag</i> or <i>archeofrag.gui</i>, please use <b>Plutniak 2022a</b>. 
                <ul>
                  <li><b>Plutniak, S. 2022a</b>. 'Archeofrag: an R package for Refitting and Spatial Analysis in Archaeology', <i>Journal of Open Source Software</i>, 7 (75), p. 4335. doi: <a href=https://doi.org/10.21105/joss.04335 target=_blank>10.21105/joss.04335</a>.</li>
                  <li><b>Plutniak, S. 2022b</b>. '<a href=https://rzine.fr/ressources/20220811_archeofrag_joss/ target=_blank>Archeofrag: un package R pour les remontages et l'analyse spatiale en archeologie</a>', <i>Rzine</i>.</li>
                </ul>
                The open source programming code of this software is available on the <a target=_blank, href=https://cran.r-project.org/package=archeofrag>CRAN</a> and on <a target=_blank, href=https://github.com/sebastien-plutniak/archeofrag/>github</a>.
                </p>
                <h2>About the TSAR method</h2>
                <p>
                <ul>
                  <li><b>Plutniak, S. 2021</b>. '<a href=https://hal.archives-ouvertes.fr/hal-03419952 target=_blank>The Strength of Parthood Ties. Modelling Spatial Units and Fragmented Objects with the TSAR Method - Topological Study of Archaeological Refitting</a>', <i>Journal of Archaeological Science</i>, 136, p. 105501. doi: <a href=https://doi.org/10.1016/j.jas.2021.105501 target=_blank>10.1016/j.jas.2021.105501</a>.</li>
                  <li><b>Plutniak, S. 2022c</b>. '<a href=http://www.prehistoire.org/offres/doc_inline_src/515/0-BSPF_2022_1_2e_partie_Correspondance_PLUTNIAK.pdf target=_blank>L'analyse topologique des remontages archeologiques : la methode TSAR et le package R archeofrag</a>', <i>BSPF</i>, 119 (1), p. 110-113.</li>
                  <li><b>Plutniak, S., J. Caro, C. Manen 2023</b>. '<a href=https://hal.science/hal-04355706 target=_blank>Four Problems for Archaeological Fragmentation Studies. Discussion and Application to the Tai Cave's Neolithic Pottery Material (France)</a>', in A. Sorman, A. Noterman, M. Fjellstrom (eds.) <i>Broken Bodies, Places and Objects. New Perspectives on Fragmentation in Archaeology</i>, London: Routledge, p. 124-142. doi: <a href=https://doi.org/10.4324/9781003350026-1 target=_blank>10.4324/9781003350026-11</a>.</li>
                </ul>
                </p>
                <h2>Datasets</h2> 
                <ul>
                  <li><b>Bout des Vergnes</b>:  Ihuel, E. (dir.),  M. Baillet, A. Barbeyron, M. Brenet, H. Camus, E. Claud, N. Mercier., A. Michel, F. Sellami. 2020. <i>Le Bout des Vergnes, Bergerac (Dordogne, Nouvelle-Aquitaine), Contournement ouest de Bergerac, RD 709</i>, Excavation report, Perigueux. </li>
                  <li><b>Chauzeys</b>: Chadelle J.-P. (dir.),  M. Baillet, A. Barbeyron, M. Brenet, H. Camus, E. Claud, F. Jude, S. Kreutzer, A. Michel,  N. Mercier, M. Rabanit, S. Save, F. Sellami, A. Vaughan-Williams. 2021. <i>Chauzeys, Saint-Medard-de-Mussidan (Dordogne, Nouvelle-Aquitaine)</i>, Excavation report, Perigueux. </li>
                  <li><b>Font-Juvenal</b>: Caro J. 2024. 'Font-Juvenal_Refiting', <i>Zenodo</i>, doi:  <a href=https://doi.org/10.5281/zenodo.14515444 target=_blank>10.5281/zenodo.14515444</a>.</li>       
                  <li><b>Grande Rivoire</b>: Angelin A., A. Bridault, J.-L. Brochier L. Chaix, L. Chesnaux, B. Marquebielle, L. Martin, P.-Y. Nicod, R. Picavet, D. Vannieuwenhuyse. 2016. 'The First Mesolithic in the French Alps: New data from La Grande Rivoire rockshelter (Vercors range, Isere, France)', <i>Quaternary International</i>, vol. 423, p. 193-212, doi: <a href=https://doi.org/10.1016/j.quaint.2015.06.027 target=_blank>10.1016/j.quaint.2015.06.027</a></li>
                  <li><b>Liang Abu</b>: Plutniak S. 2021. 'Refitting Pottery Fragments from the Liang Abu Rockshelter, Borneo', <i>Zenodo</i>, doi: <a href=https://doi.org/10.5281/zenodo.4719577 target=_blank>10.5281/zenodo.4719577</a> </li>
                  <li><b>Tai</b>:  Caro J., Plutniak S. 2022. 'Refitting and Matching Neolithic Pottery Fragments from the Tai site, France', <i>Zenodo</i>, doi:  <a href=https://doi.org/10.5281/zenodo.7408706 target=_blank>10.5281/zenodo.7408706</a>.</li>
                </ul>
                <br>
                                                                 </div>"))
                                                   ) # end column
                                          ), #end tabPanel                                          
                                          ), # end  tabsetPanel
                              width=10) # end mainPanel
                          ) #sidebarLayout
) #end fluidPage
) #end  shinyUI
