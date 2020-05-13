#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(shinycssloaders)
library(shinybusy)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(rgbif)
library(sp)
library(stringr)
library(dplyr)
library(xml2)
library(XML)
library(htmltools)
library(rlist)

# Define UI for application that draws a histogram
ui <- fluidPage(title = "FloraMap - Beobachtungen und Verbreitung",
                tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
  add_busy_bar(centered=TRUE, color = "#FF0000"),
#  use_busy_spinner(spin = "double-bounce", color = "#112446",position = "top-left"),
#  add_busy_spinner(spin="fading-circle",position = "bottom-right", margins = c(200,200)),                
# Application title
  titlePanel(div(h1("FloraMap - Verbreitungsatlas und Beobachtungen"),align="center",style="color:darkgreen")),
# sidebarlayout
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id="mytabs",type = "tabs",
        tabPanel(title = "Beobachtungen", value = "obs",
                 br(),
          textInput("suchName",label="Wortanfang Gattung Art"),
          actionButton("taxSuche","Trefferliste"),
          br(),br(),
          selectizeInput("artWahl","Trefferauswahl",choices=setNames(80,"Adonis vernalis")),
          br(),hr(),
          actionButton("distMap","Atlas Verbreitung"),
          actionButton("gbifMap","GBIF Daten"),
          actionButton("afMap", "Artenfinder Daten"),
          br(),hr(),br(),
          checkboxInput("cb_florkart", label = "Atlas", value = FALSE),
          checkboxInput("cb_gbif", label = "GBIF", value = FALSE),
          checkboxInput("cb_artenfinder", label = "Artenfinder", value = FALSE)),
        tabPanel(title = "Overlaykarten", value = "overlays",
          checkboxInput("cb_pnv", label = "Pot.Nat.Vegetation", value = FALSE),
          checkboxInput("cb_nsg", label = "Naturschutzgebiete", value = FALSE),
          checkboxInput("cb_np", label = "Nationalparke", value = FALSE), 
          checkboxInput("cb_ffh", label = "FFH-Gebiete", value = FALSE),
          checkboxInput("cb_bsr", label = "Biosphärenreservate", value = FALSE),
          checkboxInput("cb_rgl", label = "Naturräume", value = FALSE)) 
    ) # tabsetpanel
    ), # sidebarpanel  
    mainPanel(
      textOutput("atlasrecs"),textOutput("gbifrecs"),textOutput("afrecs"),
      leafletOutput("myMap",height = 550))
  ) # sidebarlayout
) # fluidpage

# Define server logic required to draw a map
server <- function(input, output, session) {

#  EPSG_25832 <- leafletCRS(crsClass = "L.Proj.CRS", code = "EPSG:25832",
#             proj4def = "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
#             resolutions = 2^(13:-1), # 8192 down to 0.5
#             origin = c(0, 0)
#  )
# initialize map output
  output$myMap <- renderLeaflet(leaflet() %>% 
    setView(lng=10,lat=51.2, zoom = 6) %>% addTiles(group = "OSM") %>%
    addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Sat") %>%
    addWMSTiles(baseUrl = "http://geodienste.bfn.de/ogc/wms/pnv500?",
                group="PNV",
                layers = c("Vegetationsgebiete","PNV500"),
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7),
                attribution = "Overlaykarten: (c) Bundesamt für Naturschutz (BfN) 2015") %>%
    addWMSTiles(baseUrl = "http://geodienste.bfn.de/ogc/wms/schutzgebiet?",
                group="NSG",
                layers = "Naturschutzgebiete",
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7),
                attribution = "Overlaykarten: (c) Bundesamt für Naturschutz (BfN) 2015" ) %>%
    addWMSTiles(baseUrl = "http://geodienste.bfn.de/ogc/wms/schutzgebiet?",
                group="NP",
                layers = "Nationalparke",
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7),
                attribution = "Overlaykarten: (c) Bundesamt für Naturschutz (BfN) 2015") %>%
    addWMSTiles(baseUrl = "http://geodienste.bfn.de/ogc/wms/schutzgebiet?",
                group="FFH",
                layers = "Fauna_Flora_Habitat_Gebiete",
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7),
                attribution = "Overlaykarten: (c) Bundesamt für Naturschutz (BfN) 2015" ) %>%
    addWMSTiles(baseUrl = "http://geodienste.bfn.de/ogc/wms/schutzgebiet?",
                group="BSR",
                layers = "Biosphaerenreservate",
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7),
                attribution = "Overlaykarten: (c) Bundesamt für Naturschutz (BfN) 2015" ) %>%
    addWMSTiles(baseUrl = "http://geodienste.bfn.de/ogc/wms/gliederungen?",
                group="RGL",
                layers = "Naturraeume",
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7),
                attribution = "Overlaykarten: (c) Bundesamt für Naturschutz (BfN) 2015" ) %>%
    addLayersControl(
      baseGroups = c("OSM", "Topo", "ESRI Sat"),
#      overlayGroups = c("PNV","NSG","NP","FFH","BSR","RGL"),
      options = layersControlOptions(collapsed = TRUE)) %>%
    addResetMapButton()
    )

# map-proxy for changing map-content witout redrawing the complete map
  proxy <- leafletProxy("myMap")

# reactive functions 
# search names according genus and species nameparts
  er_Namen <- eventReactive(input$taxSuche,{
    sn <- str_split(input$suchName," ",simplify = TRUE)
    if (nchar(sn[1])<=2)
    {print("Bitte mindesten 3 Gattungsbuchstaben")}
    else
    {
      if (is.na(sn[2])){sn[2]<-""}
      df_namen <- fromJSON(paste0("http://www.floraweb.de/pflanzenarten/taxa_json.xsql?gat=",sn[1],"&art=",sn[2]),simplifyDataFrame = TRUE)
      setNames(df_namen$taxonId,df_namen$latName)
    }
  })
  # retrieve and process data from florkart database (bfn)
  er_Florkart <- eventReactive(input$distMap,{
    df <- fromJSON(paste0("http://www.floraweb.de/pflanzenarten/atlas_json.xsql?suchnr=",input$artWahl,"&grid=quad"),simplifyDataFrame = TRUE)
#    updateTextInput(session,inputId = "taxName",value = df$taxname)
    numPoints <- length(df$records$lat)-1 # json-Ausgabe der records hat immer einen n/a am Ende
    output$atlasrecs <- renderText(paste0("Fertig: ",as.character(numPoints)," Atlas MTB-Quaranten"))
    if (numPoints >= 1){updateCheckboxInput(session,"cb_florkart",value = TRUE)} 
    SpatialPointsDataFrame(cbind(as.double(df$records$lon[1:numPoints]),
                                 as.double(df$records$lat[1:numPoints])),
                           data.frame(zeit=df$records$zeitraum[1:numPoints],
                                      rad=df$records$radius[1:numPoints]-500,
                                      plabel=paste("FloraWeb Atlaspunkt",
                                                   "<em>TK25-Quadrant: </em>",df$records$gridcode[1:numPoints],
                                                   "<br/><em>Nachweiszeitraum: </em>",df$records$zeitraum_text[1:numPoints],
                                                   "<br/><em>Status: </em>",df$records$status[1:numPoints])))
  })
  # retrieve and process data from GBIF
  er_Gbif <- eventReactive(input$gbifMap,{
    df_name <- fromJSON(paste0("http://www.floraweb.de/pflanzenarten/taxonbyid_json.xsql?taxon_id=",input$artWahl),simplifyDataFrame = TRUE)
    sname <- URLencode(df_name$records$sciName)
    gf <- occ_search(scientificName = sname, country = "DE", hasCoordinate = TRUE, return = "data", limit = 10000, 
                     fields = c("decimalLongitude","decimalLatitude","institutionCode","collectionCode","locality","verbatimLocality",
                                "coordinateUncertaintyInMeters","month","year","occurrenceID","references"))
    gf <- filter(gf, gf$institutionCode != "BfN")
    output$gbifrecs <- renderText(paste0("Fertig: ",as.character(length(gf$decimalLatitude))," GBIF-Beobachtungen"))
    if (length(gf$decimalLatitude) >= 1) {
      updateCheckboxInput(session,"cb_gbif", value = TRUE)
      if("occurenceId" %in% colnames(gf)){
      SpatialPointsDataFrame(cbind(as.double(gf$decimalLongitude),as.double(gf$decimalLatitude)),
             data.frame(gLabel=ifelse(is.na(gf$occurrenceID),
                        paste0('<em>Institution: </em>',gf$institutionCode,"/",gf$collectionCode,
                               '<br/><em>Fundort: </em>',paste0(gf$locality,"/",gf$verbatimLocality),
                               '<br/><em>Unschärferadius: </em>',gf$coordinateUncertaintyInMeters,
                               '<br/><em>Datum: </em>',gf$month,"/",gf$year),
                        paste0('<em>Institution: </em>',gf$institutionCode,"/",gf$collectionCode,
                               '<br/><em>Fundort: </em>',paste0(gf$locality,"/",gf$verbatimLocality),
                               '<br/><em>Unschärferadius: </em>',gf$coordinateUncertaintyInMeters,
                               '<br/><em>Datum: </em>',gf$month,"/",gf$year,
                               '<br/><a href="',gf$occurrenceID,'" target="_blank">publizierter Nachweis</a>'))))
      } else {
        SpatialPointsDataFrame(cbind(as.double(gf$decimalLongitude),as.double(gf$decimalLatitude)),
                               data.frame(gLabel=paste0('<em>Institution: </em>',gf$institutionCode,"/",gf$collectionCode,
                                                 '<br/><em>Fundort: </em>',
                                                 ifelse("verbatimLocality" %in% colnames(gf),paste0(gf$locality,"/",gf$verbatimLocality),gf$locality),
                                                 '<br/><em>Unschärferadius: </em>',gf$coordinateUncertaintyInMeters,
                                                 '<br/><em>Datum: </em>',gf$month,"/",gf$year)))
      }
    }
  })
# retrieve and process data from Artenfinder
  er_Artenfinder <- eventReactive(input$afMap,{
    df_name <- fromJSON(paste0("http://www.floraweb.de/pflanzenarten/taxonbyid_json.xsql?taxon_id=",input$artWahl),simplifyDataFrame = TRUE)
    sname <- URLencode(df_name$records$sciName)
    af_url <- paste0("https://artenfinder.rlp.de/api/v2/sichtbeobachtungen?format=xml&restrict=id,lat,lon,datum,bemerkung,foto&titel_wissenschaftlich=",sname)
    af_xml <- read_xml(af_url)
    af_xmldoc <- xmlInternalTreeParse(af_xml,encoding = "utf-8")
    af_df <- xmlToDataFrame(nodes = getNodeSet(af_xmldoc,"//xml/result/row"),stringsAsFactors=FALSE)
    output$afrecs <- renderText(paste0("Fertig: ",as.character(length(af_df$lat))," Artenfinder-Beobachtungen"))
    if (length(af_df$lat) >= 1){
      updateCheckboxInput(session,"cb_artenfinder", value = TRUE)
      af_sp <- SpatialPointsDataFrame(cbind(as.double(af_df$lon),as.double(af_df$lat)),
                                      proj4string = CRS("+init=epsg:25832"),
                                      data.frame(afLabel=ifelse(is.na(af_df$foto),
                                                                paste0("<em>Artenfinder Beobachtung<br/>Datum: </em>",af_df$datum,
                                                                       "<br/><em>Bemerkung: </em>",af_df$bemerkung,
                                                                       "<br/>kein Foto"),
                                                                paste0("<em>Artenfinder Beobachtung<br/>Datum: </em>",af_df$datum,
                                                                       "<br/><em>Bemerkung: </em>",af_df$bemerkung,
                                                                       "<br/><a href='",af_df$foto,"' target='_blank'>Fotolink</a>"))))
      spTransform(af_sp,CRS("+proj=longlat +datum=WGS84 +no_defs"))
    }
  })
# observers
# checking checkboxes for map overlays
  observe({
    if (input$cb_pnv) 
    {proxy %>% showGroup("PNV")}
    else {proxy %>% hideGroup("PNV")}
    if (input$cb_nsg) {
      legNSG <- "<img src='http://geodienste.bfn.de/ogc/wms/schutzgebiet?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=Naturschutzgebiete&'>"
      proxy %>% addControl(html = legNSG, position = "bottomright",layerId="nsg", className = "legend")
      proxy %>% showGroup("NSG")
      }
    else {      
      proxy %>% removeControl("nsg")
      proxy %>% hideGroup("NSG")}
    if (input$cb_np) {
      legNP <- "<img src='http://geodienste.bfn.de/ogc/wms/schutzgebiet?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=Nationalparke&'>"
      proxy %>% addControl(html = legNP, position = "bottomright",layerId = "np", className = "legend")
      proxy %>% showGroup("NP")}
    else {
      proxy %>% removeControl("np")
      proxy %>% hideGroup("NP")}
    if (input$cb_ffh) {
      legFFH <- "<img src='http://geodienste.bfn.de/ogc/wms/schutzgebiet?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=Fauna_Flora_Habitat_Gebiete&'>"
      proxy %>% addControl(html = legFFH, position = "bottomright",layerId = "ffh", className = "legend")
      proxy %>% showGroup("FFH")}
    else {
      proxy %>% removeControl("ffh")
      proxy %>% hideGroup("FFH")}
    if (input$cb_bsr) {
      legBSR <- "<img src='http://geodienste.bfn.de/ogc/wms/schutzgebiet?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=Biosphaerenreservate&'>"
      proxy %>% addControl(html = legBSR, position = "bottomright",layerId = "bsr", className = "legend")
      proxy %>% showGroup("BSR")}
    else {
      proxy %>% removeControl("bsr")
      proxy %>% hideGroup("BSR")}
    if (input$cb_rgl) {
      legRGL <- "<img src='http://geodienste.bfn.de/ogc/wms/gliederungen?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=Naturraeume&'>Naturraeume<br/>"
      proxy %>% addControl(html = legRGL, position = "bottomright",layerId = "rgl", className = "legend")
      proxy %>% showGroup("RGL")}
    else {
      proxy %>% removeControl("rgl")
      proxy %>% hideGroup("RGL")}
  })
# checking checkboxes for distribution data overlays
  observe({
    if (input$cb_florkart)
    {proxy %>% showGroup("FlorKart")}
    else {proxy %>% hideGroup("FlorKart")}
    if (input$cb_gbif)
    {proxy %>% showGroup("GBIF")}
    else {proxy %>% hideGroup("GBIF")}
    if (input$cb_artenfinder)
    {proxy %>% showGroup("AF")}
    else {proxy %>% hideGroup("AF")}
  })
# event observer for selectize box: clear distribution overlays, when selected species changes
  observeEvent(input$artWahl,{
    output$atlasrecs <- renderText("Atlas Quadranten...")
    output$gbifrecs <- renderText("GBIF Beobachtungen...")
    output$afrecs <- renderText("Artenfinder Beobachtungen...")
    proxy %>% clearGroup("AF") %>% clearGroup("GBIF") %>% clearGroup("FlorKart")
  })
# event observers for buttonclick reactive functions
# fill selectize box with new hits from namesearch and clear distribution overlays
  observeEvent(er_Namen(),{
    updateSelectizeInput(session,"artWahl",choices = er_Namen())
    updateCheckboxInput(session,"cb_florkart",value = FALSE)
    updateCheckboxInput(session,"cb_gbif",value = FALSE)
    updateCheckboxInput(session,"cb_artenfinder",value = FALSE)
    proxy %>% clearGroup("AF") %>% clearGroup("GBIF") %>% clearGroup("FlorKart")
  })
# show Florkart distribution data
  observeEvent(er_Florkart(), {
    pal <- colorNumeric(c("red", "yellow", "green"), domain = c(1,2,3))
    proxy %>% addCircles(data=er_Florkart(),group="FlorKart",color=~pal(zeit),stroke = FALSE, fillOpacity = 0.7,
                                           popup=~plabel,radius=~rad)
  })
# show GBIF distribution data (takes some minutes...)
  observeEvent(er_Gbif(), {
      proxy %>% addCircleMarkers(data = er_Gbif(),group="GBIF",popup = ~gLabel)
  })
# show Artenfinder distribution data  
  observeEvent(er_Artenfinder(), {
        proxy %>% addCircleMarkers(data = er_Artenfinder(), group = "AF", popup = ~afLabel, color = "purple")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
