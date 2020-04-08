#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(leaflet)
library(jsonlite)
library(rgbif)
library(sp)
library(stringr)
library(dplyr)
library(xml2)
library(XML)
library(htmltools)

# Define UI for application that draws a histogram
ui <- fluidPage(title = "FloraMap - Beobachtungen und Verbreitung",
                tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
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
          selectizeInput("artWahl","Trefferauswahl",choices=""),
          br(),hr(),
          actionButton("distMap","Atlas Verbreitung"),
          actionButton("gbifMap","GBIF Daten"),
          actionButton("afMap", "Artenfinder Daten"),
          br(),hr(),br(),
          checkboxInput("cb_florkart", label = "Atlas", value = FALSE),
          checkboxInput("cb_gbif", label = "GBIF", value = FALSE),
          checkboxInput("cb_artenfinder", label = "Artenfinder", value = FALSE)),
        tabPanel(title = "Overlaykarten", value = "overlays",
          checkboxInput("cb_pnv", label = "PNV", value = FALSE),
          checkboxInput("cb_nsg", label = "Naturschutzgebiete", value = FALSE),
          checkboxInput("cb_np", label = "Nationalparke", value = FALSE)) 
      ) # tabsetpanel
    ), # sidebarpanel  
    mainPanel(
      textInput("taxName",""),textOutput("afrecs"),textOutput("gbifrecs"),
      withSpinner(leafletOutput("myMap",height = 550)))
  ) # sidebarlayout
) # fluidpage

# Define server logic required to draw a map
server <- function(input, output, session) {

#  EPSG_25832 <- leafletCRS(crsClass = "L.Proj.CRS", code = "EPSG:25832",
#             proj4def = "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
#             resolutions = 2^(13:-1), # 8192 down to 0.5
#             origin = c(0, 0)
#  )
# Initiale Kartenausgabe
  output$myMap <- renderLeaflet(leaflet() %>% 
    setView(lng=10,lat=51.2, zoom = 6) %>% addTiles(group = "OSM") %>%
    addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Sat") %>%
    addWMSTiles(baseUrl = "http://geodienste.bfn.de/ogc/wms/pnv500?",
                group="PNV",
                layers = c("Vegetationsgebiete","PNV500"),
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7)) %>%
    addWMSTiles(baseUrl = "http://geodienste.bfn.de/ogc/wms/schutzgebiet?",
                group="NSG",
                layers = "Naturschutzgebiete",
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7)) %>%
    addWMSTiles(baseUrl = "http://geodienste.bfn.de/ogc/wms/schutzgebiet?",
                group="NP",
                layers = "Nationalparke",
                options = WMSTileOptions(format="image/png",transparent=TRUE,opacity=0.7)) %>%
    addLayersControl(
      baseGroups = c("OSM", "Topo", "ESRI Sat"))
  )

# Proxy für Änderungen in der Kartenausgabe
  proxy <- leafletProxy("myMap")

# Observer für Kartenlayer
  observe({
    if (input$cb_pnv) 
      {proxy %>% showGroup("PNV")}
    else  
      {proxy %>% hideGroup("PNV")}
    if (input$cb_nsg) 
    {proxy %>% showGroup("NSG")}
    else  
    {proxy %>% hideGroup("NSG")}
    if (input$cb_np) 
    {proxy %>% showGroup("NP")}
    else  
    {proxy %>% hideGroup("NP")}
  })

# Reaktive Funktion für die Namenssuche
  erNamen <- eventReactive(input$taxSuche,{
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
  observeEvent(erNamen(),{
    updateSelectizeInput(session,"artWahl",choices = erNamen())
  })
  # Button-Klick zur Verbreitungs-Kartenausgabe
  observeEvent(input$distMap, {
      df <- fromJSON(paste0("http://www.floraweb.de/pflanzenarten/atlas_json.xsql?suchnr=",input$artWahl,"&grid=quad"),simplifyDataFrame = TRUE)
      updateTextInput(session,inputId = "taxName",value = df$taxname)
      numPoints <- length(df$records$lat)-1 # json-Ausgabe der records hat immer einen n/a am Ende
      updateCheckboxInput(session,"cb_florkart",value = FALSE)
      updateCheckboxInput(session,"cb_gbif",value = FALSE)
      updateCheckboxInput(session,"cb_artenfinder",value = FALSE)
      if (numPoints >= 1){updateCheckboxInput(session,"cb_florkart",value = TRUE)} 
      dist_sp <- SpatialPointsDataFrame(cbind(as.double(df$records$lon[1:numPoints]),
                                              as.double(df$records$lat[1:numPoints])),
                      data.frame(zeit=df$records$zeitraum[1:numPoints],
                                 
                                 rad=df$records$radius[1:numPoints]-500,
                                 plabel=paste("FloraWeb Atlaspunkt",
                                              "<em>TK25-Quadrant: </em>",df$records$gridcode[1:numPoints],
                                              "<br/><em>Nachweiszeitraum: </em>",df$records$zeitraum_text[1:numPoints],
                                              "<br/><em>Status: </em>",df$records$status[1:numPoints])))
      pal <- colorNumeric(c("red", "yellow", "green"), domain = c(1,2,3))
      proxy %>% clearGroup("AF") %>% clearGroup("GBIF") %>% clearGroup("FlorKart") %>% addCircles(data=dist_sp,group="FlorKart",color=~pal(zeit),stroke = FALSE, fillOpacity = 0.7,
                                           popup=~plabel,radius=~rad)
  })
  observeEvent(input$gbifMap, {
    proxy %>% clearGroup("GBIF") %>% clearGroup("AF")
    gf <- occ_search(scientificName = input$taxName, country = "DE", hasCoordinate = TRUE, return = "data", limit = 10000, 
            fields = c("decimalLongitude","decimalLatitude","institutionCode","locality","verbatimLocality",
                       "coordinateUncertaintyInMeters","month","year","occurrenceID","references"))
    gf <- filter(gf, gf$institutionCode != "BfN")
    output$gbifrecs <- renderText(paste0("Fertig: ",as.character(length(gf$decimalLatitude))," GBIF-Beobachtungen"))
    if (length(gf$decimalLatitude) >= 1) {updateCheckboxInput(session,"cb_gbif", value = TRUE)}
    gf1 <- filter(gf, ! is.na(gf$occurrenceID)) # in occurenceID sind links auf die Originalseiten
    if (length(gf1$decimalLatitude) >= 1)
    {gf1_sp <- SpatialPointsDataFrame(cbind(as.double(gf1$decimalLongitude),as.double(gf1$decimalLatitude)),
                      data.frame(gLabel=paste0('<em>Institution: </em>',gf1$institutionCode,
                                 '<br/><em>Fundort: </em>',paste0(gf1$locality,gf1$verbatimLocality),
                                 '<br/><em>Unschärferadius: </em>',gf1$coordinateUncertaintyInMeters,
                                 '<br/><em>Datum: </em>',gf1$month,"/",gf1$year,
                                 '<br/><a href="',gf1$occurrenceID,'" target="_blank">publizierter Nachweis</a>')))
        proxy %>% addCircleMarkers(data = gf1_sp,group="GBIF",popup = ~gLabel)
      }      
      gf2 <- filter(gf, is.na(gf$occurrenceID))
      if (length(gf1$decimalLatitude) >= 1)
      {
        gf2_sp <- SpatialPointsDataFrame(cbind(as.double(gf2$decimalLongitude),
                                              as.double(gf2$decimalLatitude)),
                            data.frame(gLabel=paste0('<em>Institution: </em>',gf2$institutionCode,
                                        '<br/><em>Fundort: </em>',paste0(gf2$locality,gf2$verbatimLocality),
                                        '<br/><em>Unschärferadius: </em>',gf2$coordinateUncertaintyInMeters,
                                        '<br/><em>Datum: </em>',gf2$month,"/",gf2$year)))
        proxy %>% addCircleMarkers(data = gf2_sp,group="GBIF",popup = ~gLabel)# %>% withSpinner()
      }
  })
  observeEvent(input$afMap, {
      af_url <- paste0("https://artenfinder.rlp.de/api/v2/sichtbeobachtungen?format=xml&restrict=id,lat,lon,datum,bemerkung,foto&titel_wissenschaftlich=",URLencode(input$taxName))
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
                                                  "<br/><a href='",af_df$foto,"' target='_blank'>Fotolink</a>")
                                                  )
                                   ))
        af_sp_ll <- spTransform(af_sp,CRS("+proj=longlat +datum=WGS84 +no_defs"))
#        browser()
        proxy %>% addCircleMarkers(data = af_sp_ll, group = "AF", popup = ~afLabel, color = "purple")
        }
  })
# Observer für Verbreitungsdaten checkboxen
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
}
# Run the application 
shinyApp(ui = ui, server = server)
