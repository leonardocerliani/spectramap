library(shiny)
library(rgdal)  # Used for raster. Need to be loaded manually otherwise shinyapps.io complains
library(shinyWidgets)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(dplyr)
library(feather)
library(geosphere)
library(DT)
library(plotly)
library(ncdf4)
library(raster)
library(highcharter)
library(htmltools)
library(bsplus)
library(shinycssloaders)
library(markdown)


select <- dplyr::select  # to avoid conflicts with other libraries containing a 'select method'

# Load the estimated ratios, i.e. the result of modelling R.FR, B.R, B.G as a function
# of AOD_500nm, H2O, O3
R.FR <- read_feather("data/df_R.FR.feather")
B.R <- read_feather("data/df_B.R.feather")
B.G <- read_feather("data/df_B.G.feather")


# Load the list of stations which have measurements (contained in R.FR, B.R and B.G, so just choose one)
df_Stations <- R.FR %>% select(Station, lat, lon)


# Load the db of the Materials with the correction factors for R.FR, B.R, B.G.
df_Materials <- read_feather("data/materials.feather")


# Load the Surface Incoming Direct radiation ncdf4 file downloaded from CM SAF
# NB: for Sarah data use "SID, for Clara data use "SIS" both in the file name 
#     and in the variable name 
nfile <- "data/SISmm_yearmonmean.nc"
ncin <- nc_open(nfile)
SID <- ncvar_get(ncin, "SIS")
SID_lon <- ncvar_get(ncin, "lon")
SID_lat <- ncvar_get(ncin, "lat")


# Transformation from SID to DLI
#
# SID measures are in W/m2, i.e. J/(m2*sec). 
# When Sarah and Clara are downloaded as monthly averages with mean as statistics, 
# the values refer to the units of SID per second in any day of the month
#
# DLI measures are in mol/(m2*day), therefore to convert SID to DLI we need to calculate 
# the amount of SID/sec units in a day. This means that the value in the SID netcdf4 
# obtained by the CM SAF must be multiplied by the amount of seconds in a day: 24 * 60 * 60
SID_2_DLI <- function(origSID) {
  origSID * 0.45 * 4.484 * (24 * 60 * 60) / 1000000
}


# Formula to calculate DLI from Surface Incoming Direct radiations
# https://wui.cmsaf.eu/safira/action/viewProduktDetails?fid=28&eid=21837_22007 <- Sarah
# https://wui.cmsaf.eu/safira/action/viewProduktDetails?fid=18&eid=21814 <- Clara
# DLI = SID * 0.45 * 4.484 * (24 * 60 * 60) / 1000000
DLI <- SID_2_DLI(SID)


# ===============================  UI  =======================================================

ui <- navbarPage(
  title = "SpectraMap",
  collapsible = TRUE,
  
  tabPanel(
    title = "DLI/ Ratios",
    
    # CSS stylesheet
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "spectraStyle.css")
    ),
    
    fluidRow(
      
      column(width = 4,
             
         h4("Daily Light Integral"),     
         
         fluidRow(
           style='padding:0px',
           
           # mappa
           withSpinner(
             leafletOutput("mappa", height = 280),
             type = 6
           )
         ),
         
         fluidRow(
           style='padding:10px',
           
           # message data Aeronet found
           h5(textOutput("dataFound")),
           
           # months slider
           chooseSliderSkin("Shiny", color = "#82d3e3"),
           sliderTextInput("month", label = NULL, choices = month.abb,
                           selected = months(Sys.Date(), abbreviate = TRUE),
                           grid = TRUE,
                           width = '100%', hide_min_max = TRUE, animate = FALSE)
         ),
         
         fluidRow(
           style='padding:0px',
           # highcharter
           highchartOutput("chartContainer", height = "250px")
         )
      ),

      column(width = 4,

             h4(textOutput("screenEfficiencyMonth")),
             
             # choose material Company
             pickerInput(
               inputId = "materialCompany", label = NULL, width = "100%",
               choices = NULL, selected = NULL, multiple = TRUE,
               options = list(
                 `actions-box` = TRUE,
                 `selected-text-format`= "static",
                 title = "Company",
                 style = "btn-default"
               )
             ),
             
             # choose material Purpose
             pickerInput(
               "materialPurpose", label = NULL, width = "100%",
               choices = 'NULL', selected = NULL, multiple = TRUE,
               options = list(
                 `actions-box` = TRUE,
                 `selected-text-format`= "static",
                 title = "Purpose"
               )
             ),
             
             # choose material Model
             pickerInput(
               "materialModel", label = NULL, width = "100%",
               choices = NULL, selected = NULL, multiple = TRUE,
               options = list(
                 size = 10,
                 `virtual-scroll` = 100,
                 `actions-box` = TRUE,
                 `selected-text-format`= "static",
                 title = "Model",
                 `style` = "btn-default"
               )
             ),
             
             # select and plot
             actionButton(
               inputId = "clickButton",
               label = " Plot ",
               # width = "100%",
               icon = icon("recycle", lib = "font-awesome")
             ),
             
             # spiderplot
             plotlyOutput("spiderplot", height = "50%")
             
      ),
      column(width = 4,
        style='padding:0px',
             
        # table of input materials
        DT::dataTableOutput("corrRatiosTable"),

        # # carousel of materials
        # tags$br(),
        # textOutput("plaseSelectRows"),
        uiOutput("selectedMaterialsImages")
        
      )
    )
  ),
  
  tabPanel(
    title = "About",
    includeMarkdown("www/About.md")
    
  )
)






# ===============================  Server Logic  ==============================================
server <- function(input, output, session) {
  
# Define lon/lat for the click, and a default location
  locus <- reactive({
    if (is.null(input$mappa_click)) {
      list(
        "lon" = 7.9385,
        "lat" = 44.7793
      )
    } else {
      list(
        "lon" = input$mappa_click$lng,
        "lat" = input$mappa_click$lat
      )
    }
  })
  
  # Screen efficiency text on top of button selectors
  output$screenEfficiencyMonth <- renderText(
    paste0("Screen efficiency in ", month.name[which(month.abb == input$month)])
  )
  
  # Render the Map
  output$mappa <- renderLeaflet(mappaMondo(input$month))   
    
  
  # Update markers on the mappa caused by input$click
  observe({
    
    closestStation <- getClosestStation(locus(), df_Stations)
    
    # clicked location
    leafletProxy("mappa", session) %>%
      clearMarkers() %>%
      addMarkers(lng = locus()$lon, lat = locus()$lat)
    
  })
  
  
  # Update DLI overlay caused by changes in input$month
  #
  observe({
    # Prepare the DLI map
    idx_month <- which(month.abb == input$month)
    SID_month <- raster(nfile, band = idx_month)
    
    # DLI_month <- SID_month * 0.45 * 4.484 * 60 * 1440 / 1000000
    DLI_month <- SID_2_DLI(SID_month)
    pal <- colorNumeric("RdYlBu", values(DLI_month), na.color = "transparent", reverse=TRUE)
    
    # # uncomment the following to reproduce Faust 2018 maps
    # faustPalette <- c("#864264","#D54799","#A76CA0","#30639A","#3E83C8","#6DBAA4","#6AAD63","#BEBE41","#F6E52A","#FDEDA1","#ECB80C","#D6664A","#E14635")
    # pal <- colorBin(
    #   faustPalette, seq(0,65), na.color = "transparent",
    #   reverse=FALSE, bins = 13, pretty = FALSE
    # )
    
    leafletProxy("mappa", session) %>%
      clearImages() %>%
      clearControls() %>% 
      addRasterImage(DLI_month, colors = pal, opacity = 0.5, group = "DLI") %>%
      addLegend(pal = pal, values = values(DLI_month), title = "DLI", group = "DLI")
  })
  
  
  
  # Calculate and display the closest station
  output$dataFound <- renderText({
    if (is.null(input$mappa_click)) {
      'Please tap the desired location on the map'
    } else {
      closestStation <- getClosestStation(locus(), df_Stations)
      paste0('Aeronet station ', closestStation$distance, ' km away', ' in ', closestStation$name)
    }
  })
  
  
  # Add the highcharter month average chart 
  output$chartContainer <- renderHighchart({
    drawDLI(locus()$lon, locus()$lat, SID_lon, SID_lat)
  })
  
  
  # Pick the estimated ratios (R.FR, B.G, B.R) for the selected station and month
  ratios <- reactive({
    req(input$month)
    closestStation <- getClosestStation(locus(), df_Stations)
    list(
      "R.FR" = R.FR %>% filter(Station == closestStation$name) %>% select(input$month) %>% pull,
      "B.R"  = B.R %>% filter(Station == closestStation$name) %>% select(input$month) %>% pull,
      "B.G"  = B.G %>% filter(Station == closestStation$name) %>% select(input$month) %>% pull
    )
  })
  
  
  # Update the selection of input$materialCompany / materialPurpose / materialModel
  selectCompany(session) 
  observe({  selectPurpose(input$materialCompany, session, input$clickButton)  })
  observe({  selectShading(input$materialCompany, input$materialPurpose, session)  })

  
  # Filter the correction factors in df_Materials for the chosen material/type/model
  # and correct the estimated ratios for the selected location.
  # Note that the input$Material / purpose / model usw are updated above
  #
  # -------------------  NB: Supercool eventReactive!!!  ---------------------------
  # corrRatios was previously a simple reactive. Adding an eventReactive allows
  # updating plot and table only after pushing a butto. This is perfect in case of 
  # hundreds of items as we start to have now.
  corrRatios <- eventReactive(input$clickButton, {
    precision = 4
    
    df_Materials %>% 
      filter(Company %in% input$materialCompany & 
             Purpose %in% input$materialPurpose &
             Model %in% input$materialModel) %>% 
      mutate(cR.FR = (R.FR_corrfactor * ratios()$R.FR) %>% round(precision) ) %>% 
      mutate(cB.G  = (B.G_corrfactor  * ratios()$B.G) %>% round(precision) ) %>% 
      mutate(cB.R  = (B.R_corrfactor  * ratios()$B.R) %>% round(precision) ) %>% 
      select(idx_material, Company, Purpose, Model, cR.FR, cB.G, cB.R)
    
  }, ignoreNULL = FALSE)
  

  
  # Output the table of the corrected ratios -> to be passed to Spideplot
  output$corrRatiosTable <- DT::renderDataTable({
    
    tmpTable <- corrRatios()
    
    # handle the case in which no material has been chosen (e.g. when opening the app)
    if (nrow(tmpTable) == 0) {
      tmpTable <- df_Materials[1,] %>%
        mutate(cR.FR = 1) %>%
        mutate(cB.G  = 1) %>%
        mutate(cB.R  = 1) %>%
        select(idx_material, Company, Purpose, Model, cR.FR, cB.G, cB.R)
    }
    
    DT::datatable(
      tmpTable %>% select(-idx_material),  # Select out idx_material since it never needs to be visible
      # caption = month.name[which(month.abb == input$month)],
      rownames = FALSE,
      colnames = c("Company", "Purpose", "Model", "R.FR", "B.G", "B.R"),
      extensions = c("Buttons","Responsive"),
      options = list(
        dom = 'tB',
        # buttons = c('csv','excel','pdf', I('colvis')),  # 'copy' breaks on mobile
        buttons = list(
          c('csv','excel'),
          list(
            extend = 'colvis', columns = c(0,1) # enable column visibility selection
          )
        ),
        columnDefs = list(
          list(
            targets = c(0,1), visible = FALSE   # hides Company and Purpose by default
          ),
          list(
            width = '25%', targets = "c(2)"    # remove all long Model (i.e. insert spaces) for this to work!!!
          )
        ),
        scrollX = TRUE,
        scrollY = "300px",
        paging = FALSE,
        scrollCollapse = FALSE,
        autoWidth = FALSE  # NB: TRUE gives some resizing problems on mobile
      )
    )  %>% 
      formatRound(c('cR.FR', 'cB.G', 'cB.R'), 2) %>%
      formatStyle(columns = c("Company", "Purpose", "Model", "cR.FR", "cB.G", "cB.R"), backgroundColor = "#ECF0F5")
      
    
  })
  
  

  # Spiderplot of the corrRatiosTable FROM SELECTED ITEMS OF THE input$corrRatiosTable
  output$spiderplot <- renderPlotly({
    drawSpiderplotSelected(corrRatios(), input$corrRatiosTable_rows_selected, input$month)
  })

  
  # Carousel of the images of the selected materials
  output$selectedMaterialsImages <- renderUI({
    
    # Show a message instead of the carousel if no rows have been selected
    output$plaseSelectRows <- renderText({
      if(is.null(input$corrRatiosTable_rows_selected)) {
        'Tap the desired screens in the table above to see a picture'
      } else {
        'Selected Screens'
      }
    })
    
    tmpTable <- corrRatios()
    
    # handle the case in which no material has been chosen (e.g. when opening the app)
    if (nrow(tmpTable) == 0) {
      tmpTable <- df_Materials[1,] %>%
        mutate(cR.FR = 1) %>%
        mutate(cB.G  = 1) %>%
        mutate(cB.R  = 1) %>%
        select(idx_material, Company, Purpose, Model, cR.FR, cB.G, cB.R)
    }
    
    idx_rows <- tmpTable$idx_material  # This is the row index in the original df_Material table
    
    whichImages2Show <- input$corrRatiosTable_rows_selected
    
    if(is.null(whichImages2Show)) {
      whichImages2Show <- 1
    }
    # end of handling the case in which no material has been chosen
  
    
    listImagesIdx <- c()

    for (i in whichImages2Show) {
      
      imgLocation <- paste0("img/", df_Materials$imgFileName[idx_rows[i]], '.jpg')

      # deal with missing images
      if (!file.exists(paste0(getwd(),"/www/img/", df_Materials$imgFileName[idx_rows[i]], '.jpg'))) {
        imgLocation <- "img/noimage.jpg"
      } 
      
      # create a list to pass to Reduce in order to prepare the HTML code for the UI
      listImagesIdx <- c(
        listImagesIdx,
        list(
          list(
            "content" = bs_carousel_image(src = imgLocation),
            "caption" = bs_carousel_caption(
              df_Materials$Model[idx_rows[i]], 
              paste(df_Materials$Purpose[idx_rows[i]], df_Materials$Company[idx_rows[i]], sep = " : ")
            )
          )
        )
      )
    }
    
    Reduce(
      bs_append, 
      listImagesIdx, 
      init = bs_carousel(id = "carousel", use_indicators = TRUE, use_controls = TRUE)
    )
  })
  
  
} # end of server logic


# ===============================  Auxiliary functions  =======================================




# Draw Spiderplot FROM SELECTED ITEMS OF THE input$corrRatiosTable
drawSpiderplotSelected <- function(corrRatios, rows_selected, month) {

  # (0) Handles the case of when the app is first launched, i.e. corrRatios has 0 rows 
  if (nrow(corrRatios) == 0) {
    corrRatios <- df_Materials[1,] %>%
      mutate(cR.FR = 1) %>%
      mutate(cB.G  = 1) %>%
      mutate(cB.R  = 1) %>%
      select(idx_material, Company, Purpose, Model, cR.FR, cB.G, cB.R)
  }
  
  # (1) All other cases after app initialization
  p <- plot_ly(
    type = 'scatterpolar', mode = 'lines+markers', fill = 'toself', opacity = 0.5
  ) %>% config(displayModeBar = F)
  
  # VERY COOL!
  # Handle the case in which no row of the DT::table has been selected
  # (i.e. when the table is initialized)
  # In the latter case, ALL the selected materials/colors/models will be shown
  if (is.null(rows_selected)) {
    selection = 1:nrow(corrRatios)
  } else {
    selection = rows_selected
  }
  
  
  for (idx in selection) {

  nums <- corrRatios[idx, c("cR.FR","cB.G","cB.R")] %>% as.numeric
  # print(nums)

    p <- p %>%
      add_trace(
        r = c(nums, nums[1]),
        theta = c('R.FR','B.G','B.R', 'R.FR'),
        name = paste(corrRatios[idx, "Model"], collapse = " "),
        line = list(
          dash = "solid",
          shape = "spline",
          smoothing = 1,
          width = 2
        )
      ) %>%
      layout(legend = list(x = 0, y = -0.5)) %>%
      layout(margin = list(l = 0, pad = 20)) %>%
      # layout(legend = list(orientation="h")) %>%
      layout(paper_bgcolor='transparent')
  }
  
  
  # Hide the legend if there are too many items
  if (length(selection) > 6) {
    p <- p %>% layout(showlegend = FALSE)
  }
  
  # # If you want a title uncomment this
  # p <- p %>%
  #   layout(
  #     title = list(
  #       text = month.name[which(month.abb == month)],
  #       x = 0.5
  #     )
  #   )

  return(p)
}




# Choose the materialCompany
selectCompany <- function(session) {
  chosenMaterial <- df_Materials %>% select(Company) %>% unique() %>% pull
  updatePickerInput(session, "materialCompany", choices = chosenMaterial, selected = chosenMaterial[1],)
}

# Choose the materialPurpose
selectPurpose <- function(materialCompany, session, clickButton) {
  chosenPurpose <- df_Materials %>% filter(Company %in% materialCompany) %>% select(Purpose) %>% unique() %>% pull
  updatePickerInput(session, "materialPurpose", choices = chosenPurpose, selected = chosenPurpose)
}

# Choose the model
selectShading <- function(materialCompany, materialPurpose, session) {
  chosenModel <- df_Materials %>% filter(Company %in% materialCompany & Purpose %in% materialPurpose) %>%
    select(Model) %>% unique() %>% pull
  updatePickerInput(session, "materialModel", choices = chosenModel, selected = chosenModel)
}



# Add the highcharter month average chart
drawDLI <- function(clickedLon, clickedLat, SID_lon, SID_lat) {
  # calculate the distance of every SID location from the clicked location
  dLon <- abs(SID_lon - clickedLon)
  dLat <- abs(SID_lat - clickedLat)
  
  # impose a limit of 2 degrees for finding the closest SID location
  thr_distance_deg <- 2
  idx_closest_SID_lon <- which( (dLon == min(dLon)) & (min(dLon) < thr_distance_deg) )
  idx_closest_SID_lat <- which( (dLat == min(dLat)) & (min(dLat) < thr_distance_deg) )
  
  highchart() %>% 
    hc_chart(type = "areaspline") %>%  # try also column and areaspline 
    # hc_chart(margin = c(0,0,0,0)) %>%
    # hc_title(text = "Daily Light Integral") %>%
    hc_xAxis(categories = month.abb) %>% 
    hc_add_series(data = DLI[idx_closest_SID_lon, idx_closest_SID_lat, ], 
                  name = "DLI",
                  fillOpacity = 0.1,
                  showInLegend = FALSE
    ) %>% 
    hc_tooltip(valueDecimals = 0) %>%
    hc_chart(backgroundColor = "transparent") %>%
    # hc_add_theme(hc_theme_chalk() )
    hc_add_theme(hc_theme_ffx() )
}



# Calculate the closest station
getClosestStation <- function(locus, df_Stations) {

  # Division by 1000 to get distances in km
  D <- geosphere::distm(df_Stations %>% select(lon, lat),
                        c(locus$lon, locus$lat))/1000
  
  # print(df_Stations[which.min(D),])
  
  closestStation <- list(
    "idx" = which.min(D),
    "name" = df_Stations[which.min(D),]$Station,
    "lat" = df_Stations[which.min(D),]$lat,
    "lon" = df_Stations[which.min(D),]$lon,
    "distance" = as.integer(D[which.min(D)])
  )
}



# Render the map
urlbase_OWM_rain="http://{s}.tile.openweathermap.org/map/precipitation_new/{z}/{x}/{y}.png?appid="
urlbase_OWM_temp="http://{s}.tile.openweathermap.org/map/temp/{z}/{x}/{y}.png?appid="

API_KEY=Sys.getenv("OWM_KEY")

# A function to render the map is required since we need to pass the current month
# NB: the marker and DLI map are added by the leafletProxy in the server logic
mappaMondo <- function(month) {
  
  leaflet(
    options = leafletOptions(
      # dragging = FALSE, tap = FALSE    # good for mobile: prevent dragging the map around with 1 finger
    )
  ) %>% 
    setView(7.9385, 44.7793, zoom = 3) %>%   # initial zoom on Monteu Roero
    addProviderTiles(providers$CartoDB.Positron) %>%
    addTiles(paste0(urlbase_OWM_rain,API_KEY), group = "rain", options = tileOptions(opacity = 1)) %>%
    addTiles(paste0(urlbase_OWM_temp,API_KEY), group = "temp", options = tileOptions(opacity = 0.3)) %>%
    addLayersControl( overlayGroups = c("temp", "rain", "DLI") ) %>% 
    hideGroup(c("rain", "temp")) 
  # garnishMap(addMouseCoordinates) %>%
  # addSearchOSM()
}





shinyApp(ui = ui, server = server)

