# shiny-floramap
shiny app mapping floristic data from Germany

The shiny app creates a map using leaflet package. 
The intended workflow is:
* search for scientific taxon names by entering at least 3 letters of a genus name and optionally some letters of a species epithet name and hitting the button "Trefferliste"
* the default taxon "Adonis vernalis" is replaced by the names that match the search string
* select one of the names found from the dropdown list
* hit the button Atlas-Daten and see the distribution data fom the German Atlas (2013) on the map
* hit the button "GBIF-Daten": if you have selected a common species you will have to wait a few minutes until the data from GBIF will appear on the map. This is due to the fact that the GBIF API always sends an XML-structure with all result fields which poduces large datasets of some Megabytes, consuming time an network capacities.
* hit the button "Artenfinder-Daten" and see additional records from the Artenfnder API

