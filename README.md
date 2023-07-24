# Archeofrag (Shiny application)

This [R Shiny application](https://analytics.huma-num.fr/Sebastien.Plutniak/archeofrag/) demonstrates some features of the  [archeofrag package](https://github.com/sebastien-plutniak/archeofrag). R package for the spatial analysis of refitting objects in archaeology.

It includes methods to measure the cohesion and admixture of pairs of archaeological spatial units (e.g. layers), from the distribution and the topology of the refitting relationships between the fragments contained in these units. The package also makes it possible to compare the measured values to simulated fragmentation graphs.

## Input Data
Either load the example data set (refitting data from the Liang Abu rock shelter, Borneo) or upload your data. Use the menu on the left to upload your edges and nodes data as CSV files.

* The edges table must have a row for each refitting relationship, with two columns containing the identifiers of the two fragments;
* the nodes table must have a row for each fragment, the first column contains the fragments identifiers and the second column contains their layer.

## Comparison with simulated data

The observed data can be compared to similar simulated data for two formation hypothesis:

* H1, the archaeological material studied comes from a single deposition episode, within which archaeologists distinguished two subsets;
* H2, the material was deposited during two deposition episodes, that archaeologists could not distinguish due to subsequent perturbations, admixture, and sampling resulting either from human or non-human action

Select the pair of spatial units to compare in the menu, set the number of simulated data sets to generate, and click on the “Run” button. Depending on the size of the data set, the computing time can be long. Charts are generated for various parameters measured on the fragmentation graphs: the value observed on the empirical graph is represented by a vertical bar, the distribution of values for each hypotheses are represented by dark (H1) and light (H2) grey shades.

## References

The code and more information are available on github and in the following publications:

* Plutniak, S. 2021. “[The Strength of Parthood Ties. Modelling Spatial Units and Fragmented Objects with the TSAR Method – Topological Study of Archaeological Refitting](https://hal.archives-ouvertes.fr/hal-03419952)”, **Journal of Archaeological Science**, 136, p. 105501. DOI: [10.1016/j.jas.2021.105501](https://doi.org/10.1016/j.jas.2021.105501).
* Plutniak, S. 2022. “Archeofrag: an R package for Refitting and Spatial Analysis in Archaeology”, **Journal of Open Source Software**, 7 (75), p. 4335. DOI: [10.21105/joss.04335](https://doi.org/10.21105/joss.04335).
* Plutniak, S. 2022. “[Archeofrag: un package R pour les remontages et l'analyse spatiale en archéologie](https://rzine.fr/publication/20220811_archeofrag_joss)”, **Rzine**.
* Plutniak, S. 2022. “[L'analyse topologique des remontages archéologiques : la méthode TSAR et le package R archeofrag](http://www.prehistoire.org/offres/doc_inline_src/515/0-BSPF_2022_1_2e_partie_Correspondance_PLUTNIAK.pdf)”, **Bulletin de la Société préhistorique française**, 119 (1), p. 110–113.
