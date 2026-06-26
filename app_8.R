# ============================================================
#  App Shiny - Transporte Público de Valencia (EMT)
#  Datos GTFS del Ayuntamiento de Valencia
#  Requiere: shiny, leaflet, dplyr, readr, DT, shinydashboard,
#            leaflet.extras, shinyjs, lubridate
# ============================================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(readr)
library(DT)
library(shinyjs)
library(lubridate)
library(jsonlite)

# ── Colores corporativos EMT Valencia ────────────────────────
EMT_RED    <- "#FE0000"
EMT_DARK   <- "#1a1a2e"
EMT_ACCENT <- "#ff6b35"
BG_LIGHT   <- "#f8f9fa"

# ============================================================
#  CARGA DE DATOS
# ============================================================
# ── Tiempo en tiempo real (Open-Meteo, sin API key) ─────────
get_weather_valencia <- function() {
  tryCatch({
    url  <- paste0("https://api.open-meteo.com/v1/forecast",
                   "?latitude=39.4699&longitude=-0.3763",
                   "&current=temperature_2m,relative_humidity_2m,weather_code",
                   "&timezone=Europe/Madrid")
    resp <- jsonlite::fromJSON(url)
    cur  <- resp$current
    # Emoji según weather_code WMO
    emoji <- dplyr::case_when(
      cur$weather_code == 0                      ~ "☀️",
      cur$weather_code %in% 1:3                  ~ "⛅",
      cur$weather_code %in% c(45,48)             ~ "🌫️",
      cur$weather_code %in% c(51,53,55,61,63,65) ~ "🌧️",
      cur$weather_code %in% c(71,73,75)          ~ "❄️",
      cur$weather_code %in% c(80,81,82)          ~ "🌦️",
      cur$weather_code %in% c(95,96,99)          ~ "⛈️",
      TRUE                                        ~ "🌡️"
    )
    list(
      temp     = round(cur$temperature_2m, 1),
      humidity = cur$relative_humidity_2m,
      emoji    = emoji,
      ok       = TRUE
    )
  }, error = function(e) list(ok = FALSE))
}

weather_valencia <- get_weather_valencia()

message("Cargando datos GTFS...")

routes     <- read_csv("routes.txt",     show_col_types = FALSE)
stops      <- read_csv("stops.txt",      show_col_types = FALSE)
trips      <- read_csv("trips.txt",      show_col_types = FALSE)
stop_times <- read_csv("stop_times.txt", show_col_types = FALSE)
shapes     <- read_csv("shapes.txt",     show_col_types = FALSE)
calendar   <- read_csv("calendar.txt",   show_col_types = FALSE)

# Limpiar nombres de rutas para el selector
routes_clean <- routes %>%
  distinct(route_id, route_short_name, route_long_name) %>%
  mutate(label = paste0(route_short_name, " · ", 
                        gsub("^\\d+ - |^[A-Z]\\d+ - ", "", route_long_name)))

# Opciones para selectores
route_choices <- setNames(routes_clean$route_id, routes_clean$label)

stop_choices <- setNames(
  stops$stop_id,
  paste0("[", stops$stop_code, "] ", stops$stop_name)
)

message("Datos cargados: ", nrow(stops), " paradas · ", 
        nrow(routes_clean), " rutas · ", nrow(trips), " viajes")

# ============================================================
#  UI
# ============================================================
ui <- dashboardPage(
  title = "EMT Valencia",
  skin = "red",
  
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://www.emtvalencia.es/wp/wp-content/uploads/2022/11/logoEMTValencia.png",
               height = "32px", style = "margin-right:8px; vertical-align:middle;",
               onerror = "this.style.display='none'"),
      "EMT Valencia"
    ),
    titleWidth = 280,
    if (weather_valencia$ok) {
      tags$li(
        class = "dropdown",
        style = "padding: 8px 20px 0 0;",
        tags$div(
          style = paste0(
            "display:inline-flex; align-items:center; gap:10px;",
            "background:rgba(255,255,255,.15); border-radius:20px;",
            "padding:4px 14px; font-size:13px; color:white; font-weight:500;"
          ),
          tags$span(weather_valencia$emoji, style = "font-size:18px;"),
          tags$span(paste0(weather_valencia$temp, " °C")),
          tags$span(style = "opacity:.5;", "·"),
          tags$span(paste0("💧 ", weather_valencia$humidity, "%"))
        )
      )
    }
  ),
  
  dashboardSidebar(
    width = 280,
    useShinyjs(),
    
    tags$style(HTML("
      .skin-red .main-sidebar { background-color: #1a1a2e; }
      .skin-red .sidebar-menu > li > a { color: #ccc; font-size: 13px; }
      .skin-red .sidebar-menu > li.active > a,
      .skin-red .sidebar-menu > li > a:hover { 
        background-color: #FE0000 !important; color: white !important; 
      }
      .sidebar-menu .treeview-menu > li > a { color: #aaa !important; }
      .info-box { min-height: 70px; }
      .info-box-icon { height: 70px; line-height: 70px; font-size: 28px; }
      .info-box-content { padding: 8px 10px; }
      .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #FE0000; }
      .select2-container--default .select2-selection--single { 
        border-radius: 4px; border: 1px solid #ccc; 
      }
      #mapa_rutas { border-radius: 8px; }
    ")),
    
    sidebarMenu(
      id = "menu",
      menuItem("Mapa de Rutas",      tabName = "mapa",      icon = icon("map")),
      menuItem("Horarios",           tabName = "horarios",  icon = icon("clock")),
      menuItem("Buscar Ruta",        tabName = "buscador",  icon = icon("route")),
      menuItem("Cómo llegar",        tabName = "llegar",    icon = icon("location-dot")),
      menuItem("Resumen de la Red",  tabName = "resumen",   icon = icon("chart-bar"))
    ),
    
    tags$hr(style = "border-color:#333; margin: 10px 15px;"),
    tags$p(style = "color:#666; font-size:11px; padding:0 15px; line-height:1.4;",
           "Datos abiertos · Ayuntamiento de Valencia · EMT")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body, .content-wrapper, .right-side { background-color: #f0f2f5; }
        .box { border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,.08); }
        .box-header { border-radius: 8px 8px 0 0; }
        .btn-emt { background-color: #FE0000; color: white; border: none;
                   border-radius: 6px; font-weight: 600; }
        .btn-emt:hover { background-color: #cc0000; color: white; }
        .route-badge { display: inline-block; background: #FE0000; color: white;
                       border-radius: 4px; padding: 2px 8px; font-weight: 700;
                       font-size: 14px; min-width: 36px; text-align: center; }
        .stop-card { background: white; border-radius: 8px; padding: 12px 16px;
                     margin-bottom: 8px; border-left: 4px solid #FE0000;
                     box-shadow: 0 1px 4px rgba(0,0,0,.06); }
        .time-chip { display: inline-block; background: #fff3f3; color: #c00;
                     border: 1px solid #fcc; border-radius: 20px;
                     padding: 2px 10px; font-size: 12px; margin: 2px; }
        .time-chip-next { background: #FE0000 !important; color: white !important;
                          border-color: #cc0000 !important; font-weight: 700; }
        .time-chip-soon { background: #fff7e6 !important; color: #c67000 !important;
                          border-color: #ffd080 !important; font-weight: 600; }
        .countdown-badge { display:inline-block; background:#f0f0f0; color:#555;
                           border-radius:4px; font-size:10px; padding:1px 6px;
                           margin-left:4px; vertical-align:middle; }
        .no-service { color:#aaa; font-style:italic; font-size:12px; padding:4px 0; }
      ")),
      tags$script(HTML("
        $(document).on('click', '#btn_buscar', function() {
          var now = new Date();
          var mins = now.getHours() * 60 + now.getMinutes();
          Shiny.setInputValue('client_time_mins', mins, {priority: 'event'});
          if (navigator.geolocation) {
            navigator.geolocation.getCurrentPosition(function(pos) {
              Shiny.setInputValue('user_lat', pos.coords.latitude,  {priority: 'event'});
              Shiny.setInputValue('user_lon', pos.coords.longitude, {priority: 'event'});
            });
          }
        });
        
        // Capturar ubicación al pulsar Cómo llegar
        $(document).on('click', '#btn_como_llegar', function() {
          var now = new Date();
          var mins = now.getHours() * 60 + now.getMinutes();
          Shiny.setInputValue('llegar_time_mins', mins, {priority: 'event'});
          if (navigator.geolocation) {
            navigator.geolocation.getCurrentPosition(
              function(pos) {
                Shiny.setInputValue('llegar_user_lat', pos.coords.latitude,  {priority: 'event'});
                Shiny.setInputValue('llegar_user_lon', pos.coords.longitude, {priority: 'event'});
              },
              function(err) {
                Shiny.setInputValue('llegar_geo_error', err.message, {priority: 'event'});
              }
            );
          } else {
            Shiny.setInputValue('llegar_geo_error', 'Geolocalización no disponible', {priority: 'event'});
          }
        });
        
        // Enter en el campo dirección dispara geocodificación
        $(document).on('keypress', '#llegar_dir_texto', function(e) {
          if (e.which == 13) $('#btn_geocodificar').click();
        });
      "))
    ),
    
    tabItems(
      
      # ── TAB 1: MAPA DE RUTAS ─────────────────────────────
      tabItem(tabName = "mapa",
        fluidRow(
          box(width = 3, title = "Filtros", status = "danger", solidHeader = TRUE,
            selectInput("sel_ruta", "Línea de autobús:",
                        choices = c("— Todas las rutas —" = "", route_choices)
                        selected = ""),
            tags$small(style = "color:#888;",
                       "Selecciona una línea para ver su recorrido completo."),
            tags$hr(),
            checkboxInput("show_stops_all", "Mostrar todas las paradas", value = FALSE),
            actionButton("btn_limpiar_mapa", "Limpiar selección", 
                         class = "btn-emt", style = "width:100%; margin-top:8px;"),
            tags$hr(),
            uiOutput("ui_info_ruta")
          ),
          box(width = 9, title = "Mapa interactivo · Valencia", 
              status = "danger", solidHeader = TRUE,
            leafletOutput("mapa_rutas", height = "580px")
          )
        )
      ),
      
      # ── TAB 2: HORARIOS ──────────────────────────────────
      tabItem(tabName = "horarios",
        fluidRow(
          box(width = 4, title = "Consultar horarios", 
              status = "danger", solidHeader = TRUE,
            selectInput("hor_ruta", "Línea:", choices = route_choices),
            selectInput("hor_dia", "Tipo de día:",
                        choices = c("Laborable (L-J)" = "laborable",
                                    "Viernes"          = "viernes",
                                    "Sábado"           = "sabado",
                                    "Domingo/Festivo"  = "domingo")),
            selectInput("hor_direccion", "Dirección:", choices = NULL),
            tags$hr(),
            tags$p(style = "font-weight:600; font-size:13px; margin-bottom:4px;",
                   "Filtrar por parada (opcional)"),
            tags$p(style = "font-size:11px; color:#888; margin-top:0;",
                   "Deja en blanco para ver todas las paradas del recorrido."),
            selectInput("hor_parada", "Parada:", 
                        choices = c("— Todas las paradas —" = ""),
                        selectize = TRUE),
            tags$hr(),
            actionButton("btn_ver_horarios", "Ver horarios", class = "btn-emt",
                         style = "width:100%;"),
            tags$hr(),
            uiOutput("ui_resumen_servicio")
          ),
          box(width = 8, title = "Horarios", 
              status = "danger", solidHeader = TRUE,
            uiOutput("ui_horarios_tabla")
          )
        )
      ),
      
      # ── TAB 3: BUSCADOR ──────────────────────────────────
      tabItem(tabName = "buscador",
        fluidRow(
          box(width = 12, title = "Buscar rutas entre dos paradas",
              status = "danger", solidHeader = TRUE,
            fluidRow(
              column(5,
                selectInput("busq_origen", "Parada de origen:",
                            choices = stop_choices, selectize = TRUE),
                tags$div(style = "font-size:11px; color:#888; margin-top:-10px;",
                         "Escribe el nombre o número de parada")
              ),
              column(2, 
                tags$div(style = "text-align:center; padding-top:25px;",
                  actionButton("btn_invertir", "⇄", 
                               style = "font-size:20px; background:#FE0000; 
                                        color:white; border:none; border-radius:50%;
                                        width:45px; height:45px; padding:0;"))
              ),
              column(5,
                selectInput("busq_destino", "Parada de destino:",
                            choices = stop_choices, selectize = TRUE)
              )
            ),
            fluidRow(
              column(12,
                actionButton("btn_buscar", "Buscar líneas", class = "btn-emt",
                             style = "margin-top:10px; padding: 8px 30px;"),
                tags$hr()
              )
            ),
            uiOutput("ui_resultados_busqueda")
          )
        ),
        fluidRow(
          box(width = 12, title = "Mapa del resultado",
              status = "danger", solidHeader = TRUE,
            leafletOutput("mapa_busqueda", height = "350px")
          )
        )
      ),
      
      # ── TAB 4: CÓMO LLEGAR ───────────────────────────────
      tabItem(tabName = "llegar",
        # CSS específico de este tab
        tags$style(HTML("
          .paso-header { font-size:11px; font-weight:700; color:#FE0000;
                         text-transform:uppercase; letter-spacing:1px; margin-bottom:6px; }
          .paso-box { background:white; border-radius:10px; padding:14px 16px;
                      margin-bottom:12px; box-shadow:0 1px 6px rgba(0,0,0,.07); }
          .dest-tab-btn { border:none; background:#f0f0f0; border-radius:6px;
                          padding:5px 12px; font-size:12px; cursor:pointer; margin-right:4px; }
          .dest-tab-btn.active { background:#FE0000; color:white; font-weight:600; }
          .linea-card { background:white; border-radius:10px; padding:14px 16px;
                        margin-bottom:10px; border-left:4px solid #FE0000;
                        box-shadow:0 1px 5px rgba(0,0,0,.07); cursor:pointer;
                        transition: box-shadow .15s; }
          .linea-card:hover { box-shadow:0 3px 12px rgba(0,0,0,.15); }
          .linea-card.selected { border-left-color:#006600;
                                 background:#f6fff6; }
          .step-circle { display:inline-flex; align-items:center; justify-content:center;
                         width:24px; height:24px; border-radius:50%;
                         background:#FE0000; color:white; font-size:12px;
                         font-weight:700; margin-right:8px; flex-shrink:0; }
          .walk-pill { display:inline-flex; align-items:center; gap:5px;
                       background:#e8f4ff; border-radius:20px; padding:3px 10px;
                       font-size:12px; color:#1a6fcc; font-weight:600; }
          .tramo-stop { display:flex; align-items:center; gap:8px; padding:4px 0;
                        font-size:12px; }
          .tramo-line { width:2px; background:#FE0000; margin:0 10px;
                        flex-shrink:0; align-self:stretch; }
        ")),
        
        fluidRow(
          # ── Panel izquierdo: pasos 1-2-3 ──────────────────
          column(width = 4,
            
            # PASO 1: destino
            tags$div(class = "paso-box",
              tags$div(class = "paso-header",
                tags$span(class = "step-circle", "1"), "¿A dónde quieres ir?"),
              
              # Botones para elegir modo de destino
              tags$div(style = "margin-bottom:10px;",
                tags$button("Dirección", id = "dest_modo_dir",
                            class = "dest-tab-btn active",
                            onclick = "
                              document.getElementById('dest_modo_dir').classList.add('active');
                              document.getElementById('dest_modo_parada').classList.remove('active');
                              document.getElementById('dest_modo_mapa').classList.remove('active');
                              Shiny.setInputValue('dest_modo', 'direccion', {priority:'event'});
                            "),
                tags$button("Parada", id = "dest_modo_parada",
                            class = "dest-tab-btn",
                            onclick = "
                              document.getElementById('dest_modo_dir').classList.remove('active');
                              document.getElementById('dest_modo_parada').classList.add('active');
                              document.getElementById('dest_modo_mapa').classList.remove('active');
                              Shiny.setInputValue('dest_modo', 'parada', {priority:'event'});
                            "),
                tags$button("Mapa", id = "dest_modo_mapa",
                            class = "dest-tab-btn",
                            onclick = "
                              document.getElementById('dest_modo_dir').classList.remove('active');
                              document.getElementById('dest_modo_parada').classList.remove('active');
                              document.getElementById('dest_modo_mapa').classList.add('active');
                              Shiny.setInputValue('dest_modo', 'mapa', {priority:'event'});
                            ")
              ),
              
              uiOutput("ui_llegar_modo_destino"),
              uiOutput("ui_destino_confirmado")
            ),
            
            # PASO 2: tipo de día
            tags$div(class = "paso-box",
              tags$div(class = "paso-header",
                tags$span(class = "step-circle", "2"), "¿Cuándo viajas?"),
              selectInput("llegar_dia", NULL,
                          choices = c("Laborable (L-J)" = "laborable",
                                      "Viernes"          = "viernes",
                                      "Sábado"           = "sabado",
                                      "Domingo/Festivo"  = "domingo"),
                          width = "100%")
            ),
            
            # PASO 3: buscar
            tags$div(class = "paso-box",
              tags$div(class = "paso-header",
                tags$span(class = "step-circle", "3"), "Calcular"),
              tags$p(style = "font-size:12px; color:#888; margin-bottom:10px;",
                     "Necesitamos tu ubicación para encontrar las paradas más cercanas a ti."),
              actionButton("btn_como_llegar", "🔍 Buscar cómo llegar",
                           class = "btn-emt", style = "width:100%; padding:10px;
                                                        font-size:14px;"),
              uiOutput("ui_llegar_ubi_estado")
            ),
            
            # Panel de líneas encontradas
            uiOutput("ui_llegar_lineas")
          ),
          
          # ── Mapa ──────────────────────────────────────────
          column(width = 8,
            tags$div(class = "paso-box", style = "padding:0; overflow:hidden;",
              leafletOutput("mapa_llegar", height = "640px")
            ),
            tags$div(style = "display:flex; gap:16px; font-size:11px; color:#666;
                              margin-top:6px; flex-wrap:wrap;",
              tags$span("🟢 Tu posición"),
              tags$span("🔵 Paradas cercanas a ti"),
              tags$span("🔴 Paradas cercanas al destino"),
              tags$span("⭐ Destino"),
              tags$span("— Ruta seleccionada")
            )
          )
        ),
        
        # Panel de detalle del tramo al seleccionar una línea
        uiOutput("ui_llegar_detalle")
      ),
      
      # ── TAB 5: RESUMEN ───────────────────────────────────
      tabItem(tabName = "resumen",
        fluidRow(
          infoBox("Líneas activas", nrow(routes_clean), 
                  icon = icon("bus"), color = "red", fill = TRUE),
          infoBox("Paradas",        nrow(stops), 
                  icon = icon("map-pin"), color = "orange", fill = TRUE),
          infoBox("Viajes/día",     format(nrow(trips), big.mark = "."), 
                  icon = icon("route"), color = "yellow", fill = TRUE)
        ),
        fluidRow(
          box(width = 6, title = "Líneas de la red EMT Valencia",
              status = "danger", solidHeader = TRUE,
            DTOutput("tabla_rutas")
          ),
          box(width = 6, title = "Paradas con más servicios",
              status = "danger", solidHeader = TRUE,
            DTOutput("tabla_paradas_top")
          )
        )
      )
    )
  )
)

# ============================================================
#  SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ── Mapa base ────────────────────────────────────────────
  output$mapa_rutas <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -0.3763, lat = 39.4699, zoom = 13) %>%
      addFullscreenControl()
  })
  
  output$mapa_busqueda <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -0.3763, lat = 39.4699, zoom = 13)
  })
  
  # ── Datos reactivos de ruta seleccionada ─────────────────
  ruta_data <- reactive({
    req(input$sel_ruta != "")
    rid <- input$sel_ruta
    
    route_info  <- routes_clean %>% filter(route_id == rid)
    route_trips <- trips %>% filter(route_id == rid)
    
    # Shape de la ruta (primer shape_id disponible)
    shape_ids <- unique(route_trips$shape_id)
    shape_data <- shapes %>% filter(shape_id %in% shape_ids[1:min(2,length(shape_ids))]) %>%
      arrange(shape_id, shape_pt_sequence)
    
    # Paradas de la ruta
    trip_ids    <- unique(route_trips$trip_id)
    st_ruta     <- stop_times %>% filter(trip_id %in% trip_ids[1]) %>%
      arrange(stop_sequence)
    stops_ruta  <- stops %>% filter(stop_id %in% st_ruta$stop_id)
    
    list(info = route_info, shape = shape_data, stops = stops_ruta,
         trips = route_trips, stop_times = st_ruta)
  })
  
  # ── Actualizar mapa cuando cambia ruta ───────────────────
  observe({
    if (input$sel_ruta == "") {
      leafletProxy("mapa_rutas") %>%
        clearShapes() %>% clearMarkers() %>% clearControls()
      
      if (input$show_stops_all) {
        leafletProxy("mapa_rutas") %>%
          addCircleMarkers(data = stops, lng = ~stop_lon, lat = ~stop_lat,
                           radius = 4, color = "#FE0000", weight = 1,
                           fillOpacity = 0.6, fillColor = "white",
                           popup = ~paste0("<b>", stop_name, "</b><br>Parada: ", stop_code))
      }
      return()
    }
    
    rd <- ruta_data()
    proxy <- leafletProxy("mapa_rutas") %>%
      clearShapes() %>% clearMarkers() %>% clearControls()
    
    # Trazar el recorrido
    for (sid in unique(rd$shape$shape_id)) {
      s <- rd$shape %>% filter(shape_id == sid)
      proxy <- proxy %>%
        addPolylines(lng = s$shape_pt_lon, lat = s$shape_pt_lat,
                     color = "#FE0000", weight = 4, opacity = 0.85,
                     label = rd$info$label[1])
    }
    
    # Paradas de la ruta
    if (nrow(rd$stops) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(data = rd$stops, lng = ~stop_lon, lat = ~stop_lat,
                         radius = 6, color = "#FE0000", weight = 2,
                         fillColor = "white", fillOpacity = 1,
                         popup = ~paste0(
                           "<div style='font-family:sans-serif;'>",
                           "<b>", stop_name, "</b><br>",
                           "<span style='color:#888;'>Parada nº ", stop_code, "</span>",
                           "</div>"
                         )) %>%
        fitBounds(lng1 = min(rd$stops$stop_lon), lat1 = min(rd$stops$stop_lat),
                  lng2 = max(rd$stops$stop_lon), lat2 = max(rd$stops$stop_lat))
    }
  })
  
  observeEvent(input$btn_limpiar_mapa, {
    updateSelectInput(session, "sel_ruta", selected = "")
  })
  
  # ── Info panel de ruta ───────────────────────────────────
  output$ui_info_ruta <- renderUI({
    req(input$sel_ruta != "")
    rd <- ruta_data()
    n_paradas <- nrow(rd$stops)
    n_viajes  <- nrow(rd$trips)
    
    tags$div(
      tags$hr(),
      tags$div(class = "route-badge", rd$info$route_short_name[1]),
      tags$br(), tags$br(),
      tags$p(style = "font-size:12px; color:#555; margin:0;",
             gsub("^\\d+ - |^[A-Z]\\d+ - ", "", rd$info$route_long_name[1])),
      tags$p(style = "margin-top:8px;",
             tags$b(n_paradas), " paradas · ", tags$b(n_viajes), " viajes")
    )
  })
  
  # ── Horarios: actualizar dirección según ruta ─────────────
  observeEvent(input$hor_ruta, {
    rid <- input$hor_ruta
    route_trips <- trips %>% filter(route_id == rid)
    dirs <- unique(route_trips$trip_headsign)
    dirs <- dirs[!is.na(dirs)]
    updateSelectInput(session, "hor_direccion", choices = setNames(dirs, dirs))
    # Resetear parada al cambiar línea
    updateSelectInput(session, "hor_parada", 
                      choices = c("— Todas las paradas —" = ""), selected = "")
  })
  
  # ── Horarios: actualizar paradas según ruta + dirección ───
  observeEvent(list(input$hor_ruta, input$hor_direccion), {
    rid <- input$hor_ruta
    dir <- input$hor_direccion
    req(rid, dir)
    
    # Un trip representativo de esta dirección
    trip_ej <- trips %>%
      filter(route_id == rid, trip_headsign == dir) %>%
      slice(1) %>%
      pull(trip_id)
    
    if (length(trip_ej) == 0) return()
    
    # Paradas en orden de recorrido
    paradas_ruta <- stop_times %>%
      filter(trip_id == trip_ej) %>%
      arrange(stop_sequence) %>%
      left_join(stops %>% select(stop_id, stop_name, stop_code), by = "stop_id") %>%
      mutate(label = paste0(stop_sequence, ". [", stop_code, "] ", stop_name))
    
    choices_paradas <- c("— Todas las paradas —" = "",
                         setNames(paradas_ruta$stop_id, paradas_ruta$label))
    
    updateSelectInput(session, "hor_parada", choices = choices_paradas, selected = "")
  }, ignoreInit = TRUE)
  
  horarios_data <- eventReactive(input$btn_ver_horarios, {
    rid      <- input$hor_ruta
    dir      <- input$hor_direccion
    dia      <- input$hor_dia
    parada   <- input$hor_parada   # puede ser "" (todas) o un stop_id
    
    # Seleccionar columna de día
    col_dia <- switch(dia,
      "laborable" = "monday",
      "viernes"   = "friday",
      "sabado"    = "saturday",
      "domingo"   = "sunday"
    )
    
    # Obtener service_ids activos para ese tipo de día
    svc_activos <- calendar %>%
      filter(!!sym(col_dia) == 1) %>%
      pull(service_id)
    
    # Trips de la ruta, dirección y servicio
    trips_filtrados <- trips %>%
      filter(route_id == rid,
             trip_headsign == dir,
             service_id %in% svc_activos)
    
    if (nrow(trips_filtrados) == 0) return(NULL)
    
    # Stop times de esos trips
    st <- stop_times %>%
      filter(trip_id %in% trips_filtrados$trip_id) %>%
      arrange(stop_sequence, departure_time)
    
    # Unir con nombres de paradas
    st_named <- st %>%
      left_join(stops %>% select(stop_id, stop_name, stop_code), by = "stop_id") %>%
      select(stop_id, stop_sequence, stop_code, stop_name, trip_id, departure_time)
    
    # Ordenar trips cronológicamente por su primera parada
    first_times <- st_named %>%
      group_by(trip_id) %>%
      slice_min(stop_sequence, n = 1) %>%
      arrange(departure_time) %>%
      pull(trip_id)
    
    # Si hay parada seleccionada, filtrar solo esa parada (todos los trips)
    if (!is.null(parada) && parada != "") {
      st_named <- st_named %>% filter(stop_id == parada)
      # No limitar viajes en vista de parada única
      return(list(data = st_named, modo = "parada", trips_orden = first_times))
    }
    
    # Vista completa: limitar a 20 viajes para no colapsar
    max_trips <- min(20, length(unique(first_times)))
    trips_sel <- first_times[1:max_trips]
    st_named <- st_named %>% filter(trip_id %in% trips_sel)
    
    list(data = st_named, modo = "todas", trips_orden = trips_sel)
  })
  
  # Helper: extraer prefijo de ruta del service_id
  route_id_prefix <- function(x) sub("_.*", "", x)
  route_short <- function(rid) {
    routes_clean %>% filter(route_id == rid) %>% pull(route_short_name) %>% first()
  }
  
  output$ui_horarios_tabla <- renderUI({
    res <- horarios_data()
    if (is.null(res) || nrow(res$data) == 0) {
      return(tags$div(class = "stop-card",
                      tags$p("No hay servicios para los filtros seleccionados.",
                             style = "color:#888;")))
    }
    
    data <- res$data
    
    # ── MODO PARADA ÚNICA ─────────────────────────────────
    if (res$modo == "parada") {
      info_parada <- data %>% slice(1)
      horas_fmt <- sapply(sort(unique(data$departure_time)), function(h) {
        hh <- as.integer(substr(h, 1, 2)) %% 24
        mm <- substr(h, 4, 5)
        sprintf("%02d:%s", hh, mm)
      })
      
      # Agrupar por franja horaria para mejor lectura
      horas_df <- data.frame(hora = horas_fmt, stringsAsFactors = FALSE) %>%
        mutate(franja = cut(as.integer(substr(hora, 1, 2)),
                            breaks = c(-1, 6, 10, 13, 16, 20, 24),
                            labels = c("Madrugada", "Mañana", "Mediodía",
                                       "Tarde", "Tarde-Noche", "Noche")))
      
      franjas <- split(horas_df$hora, horas_df$franja)
      franjas <- franjas[sapply(franjas, length) > 0]
      
      bloques <- lapply(names(franjas), function(f) {
        chips <- lapply(franjas[[f]], function(h) tags$span(class = "time-chip", h))
        tags$div(style = "margin-bottom:14px;",
          tags$p(style = "font-size:11px; font-weight:700; color:#888; 
                          text-transform:uppercase; letter-spacing:1px; margin-bottom:4px;",
                 f),
          tags$div(chips)
        )
      })
      
      tagList(
        # Cabecera de la parada
        tags$div(class = "stop-card",
          style = "border-left-color:#1a1a2e; background:#f8f9ff;",
          tags$div(style = "display:flex; align-items:center; gap:12px;",
            tags$div(style = paste0("background:#1a1a2e; color:white; border-radius:50%;",
                                    "width:36px; height:36px; display:flex;",
                                    "align-items:center; justify-content:center;",
                                    "font-weight:700; font-size:14px; flex-shrink:0;"),
                     info_parada$stop_code[1]),
            tags$div(
              tags$b(style = "font-size:15px;", info_parada$stop_name[1]),
              tags$br(),
              tags$span(style = "color:#888; font-size:12px;",
                        paste0(length(horas_fmt), " paso(s) en este día"))
            )
          )
        ),
        tags$div(style = "padding: 8px 4px;", bloques)
      )
    } else {
      
    # ── MODO TODAS LAS PARADAS ────────────────────────────
    paradas <- data %>%
      arrange(stop_sequence) %>%
      group_by(stop_sequence, stop_code, stop_name) %>%
      summarise(horas = list(sort(unique(departure_time))), .groups = "drop") %>%
      arrange(stop_sequence)
    
    cards <- lapply(1:nrow(paradas), function(i) {
      p <- paradas[i, ]
      horas_fmt <- sapply(p$horas[[1]], function(h) {
        hh <- as.integer(substr(h, 1, 2)) %% 24
        mm <- substr(h, 4, 5)
        sprintf("%02d:%s", hh, mm)
      })
      horas_chips <- lapply(horas_fmt[1:min(15, length(horas_fmt))], function(h) {
        tags$span(class = "time-chip", h)
      })
      if (length(horas_fmt) > 15) 
        horas_chips <- c(horas_chips, list(tags$span(class = "time-chip", 
                                                      paste0("+", length(horas_fmt)-15, " más"))))
      
      tags$div(class = "stop-card",
        tags$div(style = "display:flex; align-items:center; margin-bottom:6px;",
          tags$span(style = paste0("background:#FE0000; color:white; border-radius:50%;",
                                   "width:24px; height:24px; display:inline-flex;",
                                   "align-items:center; justify-content:center;",
                                   "font-size:11px; font-weight:700; margin-right:10px;"),
                    i),
          tags$b(p$stop_name),
          tags$span(style = "color:#aaa; font-size:11px; margin-left:8px;",
                    paste0("Parada ", p$stop_code))
        ),
        tags$div(horas_chips)
      )
    })
    
    tagList(cards)
    }
  })
  
  output$ui_resumen_servicio <- renderUI({
    res <- horarios_data()
    if (is.null(res)) return(NULL)
    data <- res$data
    
    if (res$modo == "parada") {
      n_pasos <- nrow(data)
      tags$div(
        tags$p(tags$b(n_pasos), " paso(s) por esta parada"),
        tags$p(style = "font-size:11px; color:#888;", "Horarios completos del día")
      )
    } else {
      n_viajes  <- length(unique(data$trip_id))
      n_paradas <- length(unique(data$stop_id))
      tags$div(
        tags$p(tags$b(n_viajes), " expediciones · ", tags$b(n_paradas), " paradas"),
        tags$p(style = "font-size:11px; color:#888;", "Mostrando hasta 20 expediciones")
      )
    }
  })
  
  # ── Buscador: invertir paradas ────────────────────────────
  observeEvent(input$btn_invertir, {
    o <- input$busq_origen
    d <- input$busq_destino
    updateSelectInput(session, "busq_origen", selected = d)
    updateSelectInput(session, "busq_destino", selected = o)
  })
  
  # ── Buscador: encontrar rutas ─────────────────────────────
  resultados_busqueda <- eventReactive(input$btn_buscar, {
    stop_origen   <- input$busq_origen
    stop_destino  <- input$busq_destino
    ahora_mins    <- isolate(input$client_time_mins)  # minutos desde medianoche
    
    if (stop_origen == stop_destino) return(list(error = "Elige paradas diferentes"))
    
    # Trips que pasan por origen
    trips_con_origen  <- stop_times %>%
      filter(stop_id == stop_origen) %>%
      select(trip_id, seq_origen = stop_sequence, hora_origen = departure_time)
    
    # Trips que pasan por destino DESPUÉS del origen
    trips_con_destino <- stop_times %>%
      filter(stop_id == stop_destino) %>%
      select(trip_id, seq_destino = stop_sequence, hora_destino = departure_time)
    
    # Intersección: viaje pasa por ambas paradas en orden correcto
    comunes <- inner_join(trips_con_origen, trips_con_destino, by = "trip_id") %>%
      filter(seq_destino > seq_origen)
    
    if (nrow(comunes) == 0) return(list(error = "No hay línea directa entre estas paradas"))
    
    # Unir con info de ruta y service_id
    comunes_info <- comunes %>%
      left_join(trips %>% select(trip_id, route_id, trip_headsign, service_id), by = "trip_id") %>%
      left_join(routes_clean, by = "route_id")
    
    # Líneas únicas para la cabecera
    rutas_unicas <- comunes_info %>%
      distinct(route_id, route_short_name, label, trip_headsign) %>%
      arrange(route_short_name)
    
    # ── Próximos horarios por línea ──────────────────────────
    # Convertir hora GTFS "HH:MM:SS" a minutos desde medianoche
    gtfs_to_mins <- function(h) {
      as.integer(substr(h, 1, 2)) * 60 + as.integer(substr(h, 4, 5))
    }
    fmt_hora <- function(h) {
      hh <- as.integer(substr(h, 1, 2)) %% 24
      mm <- substr(h, 4, 5)
      sprintf("%02d:%s", hh, mm)
    }
    
    # Hora actual en minutos (si no llegó del cliente, usar medianoche)
    now_mins <- if (!is.null(ahora_mins) && !is.na(ahora_mins)) ahora_mins else 0L
    
    # Para cada línea, buscar los 5 próximos pasos por origen y destino
    proximos_por_linea <- lapply(unique(comunes_info$route_id), function(rid) {
      df <- comunes_info %>% filter(route_id == rid) %>%
        mutate(
          mins_origen  = sapply(hora_origen,  gtfs_to_mins),
          mins_destino = sapply(hora_destino, gtfs_to_mins)
        ) %>%
        # Solo viajes que aún no han pasado (o que pasan pronto mañana si no quedan hoy)
        arrange(mins_origen)
      
      # Próximos a partir de ahora (puede haber horarios >1440 en GTFS para noche)
      proximos <- df %>% filter(mins_origen >= now_mins) %>% slice_head(n = 5)
      
      # Si no quedan viajes hoy, coger los primeros del día siguiente
      if (nrow(proximos) == 0) {
        proximos <- df %>% slice_head(n = 5) %>%
          mutate(es_siguiente_dia = TRUE)
      } else {
        proximos <- proximos %>% mutate(es_siguiente_dia = FALSE)
      }
      
      proximos %>%
        mutate(
          route_id       = rid,
          hora_o_fmt     = sapply(hora_origen,  fmt_hora),
          hora_d_fmt     = sapply(hora_destino, fmt_hora),
          mins_espera    = pmax(0L, mins_origen - now_mins),
          .keep = "all"
        ) %>%
        select(route_id, trip_id, trip_headsign, hora_o_fmt, hora_d_fmt,
               mins_espera, es_siguiente_dia)
    })
    
    names(proximos_por_linea) <- unique(comunes_info$route_id)
    
    # Distancia y tiempo caminando a la parada de origen
    user_lat <- isolate(input$user_lat)
    user_lon <- isolate(input$user_lon)
    
    mins_caminando <- if (!is.null(user_lat) && !is.null(user_lon)) {
      stop_o_info <- stops %>% filter(stop_id == stop_origen)
      dlat <- (stop_o_info$stop_lat - user_lat) * pi / 180
      dlon <- (stop_o_info$stop_lon - user_lon) * pi / 180
      a    <- sin(dlat/2)^2 + cos(user_lat * pi/180) *
              cos(stop_o_info$stop_lat * pi/180) * sin(dlon/2)^2
      dist_km <- 6371 * 2 * atan2(sqrt(a), sqrt(1 - a))
      ceiling(dist_km / 5 * 60)  # minutos a pie a 5 km/h
    } else {
      NULL
    }
    
    list(
      rutas            = rutas_unicas,
      proximos         = proximos_por_linea,
      stop_o           = stops %>% filter(stop_id == stop_origen),
      stop_d           = stops %>% filter(stop_id == stop_destino),
      now_mins         = now_mins,
      mins_caminando   = mins_caminando
    )
  })
  
  output$ui_resultados_busqueda <- renderUI({
    res <- resultados_busqueda()
    
    if (!is.null(res$error)) {
      return(tags$div(class = "stop-card",
                      tags$p(res$error, style = "color:#c00; font-weight:600;")))
    }
    
    # Hora actual formateada para el encabezado
    now_h  <- res$now_mins %/% 60
    now_m  <- res$now_mins %%  60
    now_str <- sprintf("%02d:%02d", now_h, now_m)
    
    cards <- lapply(1:nrow(res$rutas), function(i) {
      r       <- res$rutas[i, ]
      prox    <- res$proximos[[as.character(r$route_id)]]
      
      # Filas de próximos horarios
      filas_prox <- if (!is.null(prox) && nrow(prox) > 0) {
        lapply(1:nrow(prox), function(j) {
          p <- prox[j, ]
          
          # Chip de hora en origen con color según urgencia
          chip_class <- if (j == 1 && !p$es_siguiente_dia) {
            if (p$mins_espera <= 5)  "time-chip time-chip-next"
            else if (p$mins_espera <= 15) "time-chip time-chip-soon"
            else "time-chip"
          } else "time-chip"
          
          # Etiqueta de espera
          espera_lbl <- if (p$es_siguiente_dia) {
            tags$span(class = "countdown-badge", "mañana")
          } else if (p$mins_espera == 0) {
            tags$span(class = "countdown-badge", style = "background:#FE0000;color:white;",
                      "¡ahora!")
          } else if (p$mins_espera < 60) {
            tags$span(class = "countdown-badge", paste0(p$mins_espera, " min"))
          } else {
            NULL
          }
          
          tags$div(style = "display:flex; align-items:center; gap:8px; padding:3px 0;",
            # Origen
            tags$div(style = "min-width:90px;",
              tags$span(class = chip_class, p$hora_o_fmt),
              espera_lbl
            ),
            tags$span(style = "color:#ccc; font-size:12px;", "→"),
            # Destino
            tags$div(style = "min-width:70px;",
              tags$span(class = "time-chip",
                        style = "background:#f0fff0; color:#006600; border-color:#99cc99;",
                        p$hora_d_fmt)
            )
          )
        })
      } else {
        list(tags$p(class = "no-service", "Sin servicio disponible ahora"))
      }
      
      tags$div(class = "stop-card",
        # Cabecera de la línea
        tags$div(style = "display:flex; align-items:center; gap:12px; margin-bottom:10px;",
          tags$span(class = "route-badge", r$route_short_name),
          tags$div(
            tags$b(r$trip_headsign),
            tags$br(),
            tags$small(style = "color:#888;", r$label)
          )
        ),
        # Encabezado de columnas
        tags$div(style = "display:flex; gap:8px; font-size:10px; color:#aaa;
                          font-weight:700; text-transform:uppercase; 
                          letter-spacing:0.5px; margin-bottom:4px; padding-left:2px;",
          tags$div(style = "min-width:90px;",
                   paste0("Salida origen")),
          tags$div(style = "width:16px;"),
          tags$div("Llegada destino")
        ),
        # Filas de horarios
        tagList(filas_prox)
      )
    })
    
    n <- nrow(res$rutas)
    
    aviso_caminando <- if (!is.null(res$mins_caminando)) {
      t <- res$mins_caminando
      parada_o <- res$stop_o$stop_name
      tags$div(
        style = "background:#f0f7ff; border-left:4px solid #4a90d9; border-radius:8px;
                 padding:10px 14px; margin-bottom:12px; font-size:13px;",
        tags$span("🚶 "),
        tags$b(paste0(t, " min caminando")),
        paste0(" hasta ", parada_o),
        if (t > 5) tags$span(
          style = "color:#888; font-size:11px; display:block; margin-top:3px;",
          "Distancia en línea recta · ten en cuenta el tiempo al ver los horarios"
        )
      )
    } else NULL
    
    tagList(
      tags$div(style = "display:flex; align-items:center; gap:12px; margin-bottom:12px;",
        tags$p(style = "color:#006600; font-weight:600; margin:0;",
               paste0("✓ ", n, " línea(s) directa(s) encontrada(s)")),
        tags$span(style = "color:#888; font-size:12px;",
                  paste0("· Hora consultada: ", now_str))
      ),
      aviso_caminando,
      tags$div(style = "display:flex; gap:16px; font-size:11px; margin-bottom:12px;",
        tags$span(tags$span(class="time-chip time-chip-next", "HH:MM"), " Próximo (≤5 min)"),
        tags$span(tags$span(class="time-chip time-chip-soon", "HH:MM"), " Pronto (≤15 min)"),
        tags$span(tags$span(class="time-chip", "HH:MM"), " Salida origen"),
        tags$span(tags$span(class="time-chip",
                            style="background:#f0fff0;color:#006600;border-color:#99cc99;",
                            "HH:MM"), " Llegada destino")
      ),
      tagList(cards)
    )
  })
  
  # Actualizar mapa de búsqueda con las dos paradas
  observe({
    res <- resultados_busqueda()
    if (is.null(res) || !is.null(res$error)) return()
    
    leafletProxy("mapa_busqueda") %>%
      clearMarkers() %>% clearShapes() %>%
      addCircleMarkers(data = res$stop_o, lng = ~stop_lon, lat = ~stop_lat,
                       radius = 12, color = "#00aa00", fillColor = "#00cc44",
                       fillOpacity = 1, weight = 2,
                       label = ~paste("ORIGEN:", stop_name)) %>%
      addCircleMarkers(data = res$stop_d, lng = ~stop_lon, lat = ~stop_lat,
                       radius = 12, color = "#cc0000", fillColor = "#FE0000",
                       fillOpacity = 1, weight = 2,
                       label = ~paste("DESTINO:", stop_name)) %>%
      addPolylines(
        lng = c(res$stop_o$stop_lon, res$stop_d$stop_lon),
        lat = c(res$stop_o$stop_lat, res$stop_d$stop_lat),
        color = "#888", weight = 2, dashArray = "6,6"
      ) %>%
      fitBounds(
        lng1 = min(res$stop_o$stop_lon, res$stop_d$stop_lon) - 0.005,
        lat1 = min(res$stop_o$stop_lat, res$stop_d$stop_lat) - 0.005,
        lng2 = max(res$stop_o$stop_lon, res$stop_d$stop_lon) + 0.005,
        lat2 = max(res$stop_o$stop_lat, res$stop_d$stop_lat) + 0.005
      )
  })
  
  # ══════════════════════════════════════════════════════════
  #  LÓGICA: CÓMO LLEGAR
  # ══════════════════════════════════════════════════════════
  
  # ── Helpers ───────────────────────────────────────────────
  haversine_km <- function(lat1, lon1, lat2, lon2) {
    dlat <- (lat2 - lat1) * pi / 180
    dlon <- (lon2 - lon1) * pi / 180
    a <- sin(dlat/2)^2 + cos(lat1*pi/180) * cos(lat2*pi/180) * sin(dlon/2)^2
    6371 * 2 * atan2(sqrt(a), sqrt(1 - a))
  }
  mins_walk  <- function(dist_km) ceiling(dist_km / 5 * 60)
  gtfs_mins  <- function(h) as.integer(substr(h,1,2))*60L + as.integer(substr(h,4,5))
  fmt_hora   <- function(h) sprintf("%02d:%s", as.integer(substr(h,1,2))%%24L, substr(h,4,5))
  
  # ── Estado reactivo ───────────────────────────────────────
  destino_rv    <- reactiveValues(lat=NULL, lon=NULL, label=NULL, tipo=NULL)
  linea_sel_rv  <- reactiveValues(idx=NULL)   # índice de opción seleccionada
  
  # ── Mapa base ─────────────────────────────────────────────
  output$mapa_llegar <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng=-0.3763, lat=39.4699, zoom=14) %>%
      addFullscreenControl()
  })
  
  # ── UI dinámica según modo de destino ─────────────────────
  output$ui_llegar_modo_destino <- renderUI({
    modo <- if (is.null(input$dest_modo)) "direccion" else input$dest_modo
    switch(modo,
      "direccion" = tags$div(
        textInput("llegar_dir_texto", NULL,
                  placeholder = "Ej: Calle Colón 1, Valencia", width="100%"),
        tags$div(style="display:flex; gap:6px;",
          actionButton("btn_geocodificar", "Buscar", 
                       style="background:#1a1a2e;color:white;border:none;
                              border-radius:6px;font-size:12px;padding:5px 14px;flex:1;"),
          actionButton("btn_limpiar_dest", "✕",
                       style="background:#eee;border:none;border-radius:6px;
                              padding:5px 10px;")
        )
      ),
      "parada" = tags$div(
        selectInput("llegar_parada_sel", NULL,
                    choices = stop_choices, selectize = TRUE, width="100%"),
        actionButton("btn_confirmar_parada", "Confirmar parada",
                     style="width:100%;background:#1a1a2e;color:white;border:none;
                            border-radius:6px;font-size:12px;padding:5px;margin-top:4px;")
      ),
      "mapa" = tags$div(
        tags$p(style="font-size:12px;color:#888;margin:0;",
               "Haz clic en el mapa para fijar tu destino.")
      )
    )
  })
  
  # ── Geocodificación con Nominatim ─────────────────────────
  observeEvent(input$btn_geocodificar, {
    dir <- trimws(input$llegar_dir_texto)
    req(nchar(dir) > 3)
    query <- if (!grepl("valencia", tolower(dir)))
      paste0(dir, ", Valencia, España") else dir
    url <- paste0("https://nominatim.openstreetmap.org/search?q=",
                  utils::URLencode(query),
                  "&format=json&limit=1&countrycodes=es")
    tryCatch({
      resp   <- readLines(url, warn=FALSE, encoding="UTF-8")
      parsed <- jsonlite::fromJSON(paste(resp, collapse=""))
      if (length(parsed)==0 || nrow(parsed)==0) {
        destino_rv$lat   <- NULL; destino_rv$lon <- NULL
        destino_rv$label <- "❌ Dirección no encontrada"
        destino_rv$tipo  <- NULL
      } else {
        destino_rv$lat   <- as.numeric(parsed$lat[1])
        destino_rv$lon   <- as.numeric(parsed$lon[1])
        destino_rv$label <- parsed$display_name[1]
        destino_rv$tipo  <- "direccion"
        linea_sel_rv$idx <- NULL
        leafletProxy("mapa_llegar") %>%
          removeMarker("destino_pin") %>%
          addCircleMarkers(lng=destino_rv$lon, lat=destino_rv$lat,
                           radius=14, color="#886600", fillColor="#ffcc00",
                           fillOpacity=1, weight=2, layerId="destino_pin",
                           popup=paste0("<b>⭐ Destino</b><br>", parsed$display_name[1])) %>%
          setView(lng=destino_rv$lon, lat=destino_rv$lat, zoom=15)
      }
    }, error=function(e) {
      destino_rv$label <- paste0("❌ Error: ", conditionMessage(e))
    })
  })
  
  # Enter en campo dirección
  observeEvent(input$llegar_dir_texto, {
    # activar solo si ya pulsó enter (gestionado vía JS abajo en tags$script)
  }, ignoreInit=TRUE)
  
  # ── Destino por parada ────────────────────────────────────
  observeEvent(input$btn_confirmar_parada, {
    sid  <- input$llegar_parada_sel
    req(sid)
    info <- stops %>% filter(stop_id == sid)
    destino_rv$lat   <- info$stop_lat
    destino_rv$lon   <- info$stop_lon
    destino_rv$label <- paste0("Parada ", info$stop_code, " · ", info$stop_name)
    destino_rv$tipo  <- "parada"
    linea_sel_rv$idx <- NULL
    leafletProxy("mapa_llegar") %>%
      removeMarker("destino_pin") %>%
      addCircleMarkers(lng=info$stop_lon, lat=info$stop_lat,
                       radius=14, color="#886600", fillColor="#ffcc00",
                       fillOpacity=1, weight=2, layerId="destino_pin",
                       popup=paste0("<b>⭐ Destino</b><br>", info$stop_name))
  })
  
  # ── Destino por clic en mapa ──────────────────────────────
  observeEvent(input$mapa_llegar_click, {
    req(input$dest_modo == "mapa")
    clk <- input$mapa_llegar_click
    destino_rv$lat   <- clk$lat
    destino_rv$lon   <- clk$lng
    destino_rv$label <- sprintf("📍 %.5f, %.5f", clk$lat, clk$lng)
    destino_rv$tipo  <- "mapa"
    linea_sel_rv$idx <- NULL
    leafletProxy("mapa_llegar") %>%
      removeMarker("destino_pin") %>%
      addCircleMarkers(lng=clk$lng, lat=clk$lat,
                       radius=14, color="#886600", fillColor="#ffcc00",
                       fillOpacity=1, weight=2, layerId="destino_pin",
                       popup="<b>⭐ Destino seleccionado</b>")
  })
  
  # ── Limpiar destino ───────────────────────────────────────
  observeEvent(input$btn_limpiar_dest, {
    destino_rv$lat   <- NULL; destino_rv$lon <- NULL
    destino_rv$label <- NULL; destino_rv$tipo <- NULL
    linea_sel_rv$idx <- NULL
    updateTextInput(session, "llegar_dir_texto", value="")
    leafletProxy("mapa_llegar") %>% removeMarker("destino_pin") %>%
      clearGroup("tramo") %>% clearGroup("orig_stops") %>% clearGroup("dest_stops")
  })
  
  # ── Confirmación visible del destino ─────────────────────
  output$ui_destino_confirmado <- renderUI({
    lbl <- destino_rv$label
    if (is.null(lbl)) return(NULL)
    ok  <- !startsWith(lbl, "❌")
    tags$div(style=paste0("margin-top:8px;font-size:11px;border-radius:6px;padding:6px 10px;",
                           "background:", if(ok) "#f0fff0" else "#fff0f0", ";",
                           "color:", if(ok) "#006600" else "#cc0000", ";"),
      if (ok) "✓ " else "", lbl)
  })
  
  # ── Estado de ubicación usuario ───────────────────────────
  output$ui_llegar_ubi_estado <- renderUI({
    lat <- input$llegar_user_lat
    err <- input$llegar_geo_error
    if (!is.null(err))
      return(tags$p(style="color:#c00;font-size:11px;margin-top:8px;",
                    "⚠ Ubicación denegada. Activa el permiso en tu navegador."))
    if (!is.null(lat))
      return(tags$p(style="color:#006600;font-size:11px;margin-top:8px;", "✓ Ubicación detectada"))
    NULL
  })
  
  # ── Cálculo principal ─────────────────────────────────────
  llegar_data <- eventReactive(input$btn_como_llegar, {
    user_lat <- isolate(input$llegar_user_lat)
    user_lon <- isolate(input$llegar_user_lon)
    geo_err  <- isolate(input$llegar_geo_error)
    now_mins <- isolate(input$llegar_time_mins)
    dia      <- input$llegar_dia
    dst_lat  <- isolate(destino_rv$lat)
    dst_lon  <- isolate(destino_rv$lon)
    dst_lbl  <- isolate(destino_rv$label)
    
    if (!is.null(geo_err))
      return(list(error="Ubicación no disponible. Activa el permiso en tu navegador."))
    if (is.null(user_lat))
      return(list(error="No se pudo obtener tu ubicación. Pulsa el botón de nuevo."))
    if (is.null(dst_lat))
      return(list(error="Indica primero tu destino (dirección, parada o clic en el mapa)."))
    
    now_mins <- if (!is.null(now_mins)) now_mins else 0L
    
    # Service IDs activos
    col_dia     <- switch(dia, laborable="monday", viernes="friday",
                          sabado="saturday", domingo="sunday")
    svc_activos <- calendar %>% filter(!!sym(col_dia)==1) %>% pull(service_id)
    
    # Calcular distancia de todas las paradas al usuario y al destino (una sola vez)
    stops_dist <- stops %>%
      mutate(dist_o = mapply(function(a,b) haversine_km(user_lat,user_lon,a,b), stop_lat, stop_lon),
             dist_d = mapply(function(a,b) haversine_km(dst_lat,dst_lon,a,b),  stop_lat, stop_lon))
    
    # Función que busca opciones dado un número de paradas candidatas
    buscar_opciones <- function(n_orig, n_dest) {
      po <- stops_dist %>% arrange(dist_o) %>% slice_head(n=n_orig)
      pd <- stops_dist %>% arrange(dist_d) %>% slice_head(n=n_dest)
      ops <- list()
      for (i in seq_len(nrow(po))) {
        so <- po[i,]
        for (j in seq_len(nrow(pd))) {
          sd <- pd[j,]
          if (so$stop_id == sd$stop_id) next
          # Evitar duplicados con los ya encontrados
          ya <- any(sapply(ops, function(x)
            x$stop_orig$stop_id == so$stop_id & x$stop_dest$stop_id == sd$stop_id))
          if (ya) next
          
          trips_o <- stop_times %>% filter(stop_id==so$stop_id) %>%
            select(trip_id, seq_o=stop_sequence, hora_o=departure_time)
          trips_d <- stop_times %>% filter(stop_id==sd$stop_id) %>%
            select(trip_id, seq_d=stop_sequence, hora_d=departure_time)
          
          comunes <- inner_join(trips_o, trips_d, by="trip_id") %>%
            filter(seq_d > seq_o) %>%
            left_join(trips %>% select(trip_id,route_id,trip_headsign,service_id), by="trip_id") %>%
            filter(service_id %in% svc_activos) %>%
            left_join(routes_clean, by="route_id") %>%
            mutate(mins_o = sapply(hora_o, gtfs_mins),
                   mins_d = sapply(hora_d, gtfs_mins)) %>%
            filter(mins_o >= now_mins) %>%
            arrange(mins_o)
          
          if (nrow(comunes)==0) next
          
          proximos <- comunes %>%
            group_by(route_id, route_short_name, label, trip_headsign) %>%
            slice_head(n=5) %>% ungroup() %>%
            mutate(hora_o_fmt = sapply(hora_o, fmt_hora),
                   hora_d_fmt = sapply(hora_d, fmt_hora),
                   mins_espera = mins_o - now_mins)
          
          ops[[length(ops)+1]] <- list(
            stop_orig  = so,
            stop_dest  = sd,
            proximos   = proximos,
            lineas     = proximos %>% distinct(route_id,route_short_name,label,trip_headsign),
            walk_orig  = mins_walk(so$dist_o),
            walk_dest  = mins_walk(sd$dist_d)
          )
        }
      }
      ops
    }
    
    # Ampliar radio progresivamente hasta encontrar resultados
    # Rondas: (orig, dest) = (3,3) → (5,5) → (8,8) → (12,12)
    opciones <- list()
    for (ronda in list(c(5,5), c(10,10), c(20,20), c(40,40), c(80,80))) {
      opciones <- buscar_opciones(ronda[1], ronda[2])
      if (length(opciones) > 0) break
    }
    
    # Si aun así no hay nada, informar con la distancia máxima buscada
    if (length(opciones)==0) {
      dist_max_o <- stops_dist %>% arrange(dist_o) %>% slice(80) %>% pull(dist_o)
      dist_max_d <- stops_dist %>% arrange(dist_d) %>% slice(80) %>% pull(dist_d)
      return(list(error=paste0(
        "No se encontraron líneas directas revisando hasta ",
        round(dist_max_o*1000), " m desde tu posición y ",
        round(dist_max_d*1000), " m desde el destino. ",
        "Prueba un destino más cercano o un día diferente."
      )))
    }
    
    # Guardar las paradas que finalmente se usaron para el mapa
    paradas_orig <- stops_dist %>% arrange(dist_o) %>%
      filter(stop_id %in% sapply(opciones, function(x) x$stop_orig$stop_id)) %>% slice_head(n=5)
    paradas_dest <- stops_dist %>% arrange(dist_d) %>%
      filter(stop_id %in% sapply(opciones, function(x) x$stop_dest$stop_id)) %>% slice_head(n=5)
    
    # Ordenar por primer bus disponible
    opciones <- opciones[order(sapply(opciones, function(x) min(x$proximos$mins_o)))]
    
    list(opciones=opciones, user_lat=user_lat, user_lon=user_lon,
         dst_lat=dst_lat, dst_lon=dst_lon, dst_lbl=dst_lbl,
         paradas_orig=paradas_orig, paradas_dest=paradas_dest, now_mins=now_mins)
  })
  
  # ── Mapa tras calcular ────────────────────────────────────
  observe({
    res <- llegar_data()
    if (is.null(res) || !is.null(res$error)) return()
    
    proxy <- leafletProxy("mapa_llegar") %>%
      clearGroup("orig_stops") %>% clearGroup("dest_stops") %>%
      clearGroup("user_pos")  %>% clearGroup("tramo")
    
    # Usuario (verde)
    proxy <- proxy %>%
      addCircleMarkers(lng=res$user_lon, lat=res$user_lat,
                       radius=12, color="#006600", fillColor="#00cc44",
                       fillOpacity=1, weight=2, group="user_pos",
                       popup="<b>Tu posición</b>")
    
    # Paradas cercanas al usuario (azul)
    proxy <- proxy %>%
      addCircleMarkers(data=res$paradas_orig, lng=~stop_lon, lat=~stop_lat,
                       radius=8, color="#1a6fcc", fillColor="#4a90d9",
                       fillOpacity=0.9, weight=2, group="orig_stops",
                       popup=~paste0("<b>",stop_name,"</b><br>Parada ",stop_code,
                                     "<br>",round(dist_o*1000)," m · ",
                                     mins_walk(dist_o)," min caminando"))
    
    # Líneas punteadas usuario → paradas origen
    for (i in seq_len(nrow(res$paradas_orig))) {
      p <- res$paradas_orig[i,]
      proxy <- proxy %>%
        addPolylines(lng=c(res$user_lon, p$stop_lon),
                     lat=c(res$user_lat, p$stop_lat),
                     color="#4a90d9", weight=1.5, dashArray="5,5",
                     opacity=0.6, group="orig_stops")
    }
    
    # Paradas cercanas al destino (rojo)
    proxy <- proxy %>%
      addCircleMarkers(data=res$paradas_dest, lng=~stop_lon, lat=~stop_lat,
                       radius=8, color="#cc0000", fillColor="#FE0000",
                       fillOpacity=0.9, weight=2, group="dest_stops",
                       popup=~paste0("<b>",stop_name,"</b><br>Parada ",stop_code,
                                     "<br>",round(dist_d*1000)," m del destino"))
    
    # Destino (estrella amarilla, ya estaba en el mapa)
    # Ajustar vista
    all_lons <- c(res$user_lon, res$dst_lon,
                  res$paradas_orig$stop_lon, res$paradas_dest$stop_lon)
    all_lats <- c(res$user_lat, res$dst_lat,
                  res$paradas_orig$stop_lat, res$paradas_dest$stop_lat)
    proxy %>% fitBounds(min(all_lons)-.008, min(all_lats)-.008,
                        max(all_lons)+.008, max(all_lats)+.008)
  })
  
  # ── Lista de líneas encontradas ───────────────────────────
  output$ui_llegar_lineas <- renderUI({
    res <- llegar_data()
    if (is.null(res)) return(NULL)
    if (!is.null(res$error))
      return(tags$div(class="paso-box",
               tags$p(style="color:#c00;font-size:12px;margin:0;", res$error)))
    
    now_h <- res$now_mins %/% 60; now_m <- res$now_mins %% 60
    
    cards <- lapply(seq_along(res$opciones), function(k) {
      op <- res$opciones[[k]]
      is_sel <- !is.null(linea_sel_rv$idx) && linea_sel_rv$idx == k
      
      # Primer bus
      primer <- op$proximos %>% slice_head(n=1)
      espera <- primer$mins_espera[1]
      chip_cl <- if (espera<=5) "time-chip time-chip-next"
                 else if (espera<=15) "time-chip time-chip-soon"
                 else "time-chip"
      
      # Chips de los próximos horarios (máx 4)
      prox_viajes <- op$proximos %>%
        distinct(trip_id, .keep_all=TRUE) %>% slice_head(n=4)
      chips <- lapply(seq_len(nrow(prox_viajes)), function(vi) {
        v <- prox_viajes[vi,]
        cl <- if(vi==1) chip_cl else "time-chip"
        bdg <- if(v$mins_espera<60)
          tags$span(class="countdown-badge", paste0(v$mins_espera," min")) else NULL
        tags$span(tags$span(class=cl, v$hora_o_fmt), bdg, " ")
      })
      
      lineas_str <- paste(unique(op$lineas$route_short_name), collapse=" · ")
      
      # Card clicable que llama a Shiny
      tags$div(
        class=paste("linea-card", if(is_sel) "selected" else ""),
        id=paste0("linea_card_",k),
        onclick=paste0("Shiny.setInputValue('linea_click', ", k,
                       ", {priority:'event'});"),
        
        tags$div(style="display:flex;align-items:center;gap:10px;margin-bottom:8px;",
          tags$span(class="route-badge", lineas_str),
          tags$div(
            tags$b(style="font-size:12px;", op$lineas$trip_headsign[1]),
            tags$br(),
            tags$span(class="walk-pill", "🚶", paste0(op$walk_orig," min"))
          ),
          if(is_sel) tags$span(style="margin-left:auto;color:#006600;font-size:16px;","✓")
        ),
        
        tags$div(style="margin-bottom:6px;font-size:10px;color:#aaa;
                        text-transform:uppercase;letter-spacing:.5px;",
                 "Próximas salidas desde tu parada:"),
        tags$div(chips),
        
        tags$div(style="margin-top:8px;font-size:11px;color:#888;",
          "📍 ", op$stop_orig$stop_name,
          " → 🏁 ", op$stop_dest$stop_name
        )
      )
    })
    
    tags$div(
      tags$div(style="display:flex;align-items:center;gap:8px;margin-bottom:10px;",
        tags$b(style="color:#006600;", paste0("✓ ", length(res$opciones), " opción(es)")),
        tags$span(style="color:#888;font-size:11px;",
                  sprintf("· %02d:%02d", now_h, res$now_mins%%60))
      ),
      tagList(cards)
    )
  })
  
  # ── Al seleccionar una línea ──────────────────────────────
  observeEvent(input$linea_click, {
    k   <- input$linea_click
    res <- llegar_data()
    req(!is.null(res), is.null(res$error), k <= length(res$opciones))
    
    linea_sel_rv$idx <- k
    op  <- res$opciones[[k]]
    
    # Obtener trip representativo y su shape
    trip_rep <- op$proximos$trip_id[1]
    shape_id_rep <- trips %>% filter(trip_id==trip_rep) %>% pull(shape_id) %>% first()
    shape_rep <- shapes %>% filter(shape_id==shape_id_rep) %>% arrange(shape_pt_sequence)
    
    # Paradas del tramo (entre stop_orig y stop_dest en orden)
    st_tramo <- stop_times %>%
      filter(trip_id==trip_rep) %>%
      arrange(stop_sequence)
    
    seq_o <- st_tramo %>% filter(stop_id==op$stop_orig$stop_id) %>% pull(stop_sequence) %>% first()
    seq_d <- st_tramo %>% filter(stop_id==op$stop_dest$stop_id) %>% pull(stop_sequence) %>% first()
    
    stops_tramo <- st_tramo %>%
      filter(stop_sequence >= seq_o, stop_sequence <= seq_d) %>%
      left_join(stops %>% select(stop_id,stop_name,stop_code,stop_lat,stop_lon),
                by="stop_id")
    
    # Actualizar mapa: dibujar solo el tramo relevante del shape
    proxy <- leafletProxy("mapa_llegar") %>% clearGroup("tramo")
    
    # Recortar el shape al tramo entre las dos paradas:
    # encontrar los puntos del shape más cercanos a parada origen y destino
    if (nrow(shape_rep) > 0 && nrow(stops_tramo) >= 2) {
      stop_o_info <- stops_tramo %>% slice(1)
      stop_d_info <- stops_tramo %>% slice(n())
      
      # Índice del punto de shape más cercano a cada parada
      dist_to_o <- mapply(function(la, lo)
        haversine_km(stop_o_info$stop_lat, stop_o_info$stop_lon, la, lo),
        shape_rep$shape_pt_lat, shape_rep$shape_pt_lon)
      dist_to_d <- mapply(function(la, lo)
        haversine_km(stop_d_info$stop_lat, stop_d_info$stop_lon, la, lo),
        shape_rep$shape_pt_lat, shape_rep$shape_pt_lon)
      
      idx_o <- which.min(dist_to_o)
      idx_d <- which.min(dist_to_d)
      
      # Asegurarse de que origen < destino en el shape
      if (idx_o > idx_d) { tmp <- idx_o; idx_o <- idx_d; idx_d <- tmp }
      
      shape_tramo <- shape_rep[idx_o:idx_d, ]
      
      proxy <- proxy %>%
        addPolylines(lng = shape_tramo$shape_pt_lon,
                     lat = shape_tramo$shape_pt_lat,
                     color = "#FE0000", weight = 5, opacity = 0.9,
                     group = "tramo",
                     label = paste("Línea", op$lineas$route_short_name[1]))
    }
    
    # Paradas intermedias del tramo (sin origen ni destino)
    stops_mid <- stops_tramo %>% slice(-1, -n())
    if (nrow(stops_mid) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(data = stops_mid, lng = ~stop_lon, lat = ~stop_lat,
                         radius = 5, color = "#FE0000", fillColor = "white",
                         fillOpacity = 1, weight = 2, group = "tramo",
                         popup = ~paste0("<b>", stop_name, "</b><br>Parada ", stop_code))
    }
    
    # Parada de subida (verde)
    proxy <- proxy %>%
      addCircleMarkers(data = stops_tramo %>% slice(1),
                       lng = ~stop_lon, lat = ~stop_lat,
                       radius = 11, color = "#006600", fillColor = "#00cc44",
                       fillOpacity = 1, weight = 2, group = "tramo",
                       popup = ~paste0("<b>⬆ SUBIR AQUÍ</b><br>", stop_name,
                                       "<br>Parada ", stop_code)) %>%
      # Parada de bajada (rojo)
      addCircleMarkers(data = stops_tramo %>% slice(n()),
                       lng = ~stop_lon, lat = ~stop_lat,
                       radius = 11, color = "#cc0000", fillColor = "#FE0000",
                       fillOpacity = 1, weight = 2, group = "tramo",
                       popup = ~paste0("<b>⬇ BAJAR AQUÍ</b><br>", stop_name,
                                       "<br>Parada ", stop_code))
    
    # Línea punteada desde usuario → parada de subida
    proxy <- proxy %>%
      addPolylines(lng = c(res$user_lon, stops_tramo$stop_lon[1]),
                   lat = c(res$user_lat, stops_tramo$stop_lat[1]),
                   color = "#006600", weight = 2, dashArray = "6,4",
                   opacity = 0.7, group = "tramo")
    
    # Línea punteada desde parada de bajada → destino
    proxy <- proxy %>%
      addPolylines(lng = c(stops_tramo$stop_lon[nrow(stops_tramo)], res$dst_lon),
                   lat = c(stops_tramo$stop_lat[nrow(stops_tramo)], res$dst_lat),
                   color = "#886600", weight = 2, dashArray = "6,4",
                   opacity = 0.7, group = "tramo")
    
    # Zoom ajustado al tramo completo (usuario → destino)
    all_lons <- c(res$user_lon, res$dst_lon, stops_tramo$stop_lon)
    all_lats <- c(res$user_lat, res$dst_lat, stops_tramo$stop_lat)
    proxy %>% fitBounds(min(all_lons) - .006, min(all_lats) - .006,
                        max(all_lons) + .006, max(all_lats) + .006)
  })
  
  # ── Detalle del tramo seleccionado ────────────────────────
  output$ui_llegar_detalle <- renderUI({
    k   <- linea_sel_rv$idx
    res <- llegar_data()
    if (is.null(k) || is.null(res) || !is.null(res$error)) return(NULL)
    
    op       <- res$opciones[[k]]
    now_mins <- res$now_mins
    
    # Paradas del tramo (usar trip representativo)
    trip_rep  <- op$proximos$trip_id[1]
    st_tramo  <- stop_times %>%
      filter(trip_id==trip_rep) %>% arrange(stop_sequence)
    seq_o <- st_tramo %>% filter(stop_id==op$stop_orig$stop_id) %>% pull(stop_sequence) %>% first()
    seq_d <- st_tramo %>% filter(stop_id==op$stop_dest$stop_id) %>% pull(stop_sequence) %>% first()
    stops_tramo <- st_tramo %>%
      filter(stop_sequence>=seq_o, stop_sequence<=seq_d) %>%
      left_join(stops %>% select(stop_id,stop_name,stop_code), by="stop_id")
    
    n_paradas <- nrow(stops_tramo)
    
    # Viajes disponibles (todos, para la tabla de horarios)
    viajes <- op$proximos %>% distinct(trip_id, .keep_all=TRUE)
    
    fluidRow(
      box(width=12, status="danger", solidHeader=TRUE,
          title=tags$span(
            tags$span(class="route-badge",
                      paste(unique(op$lineas$route_short_name), collapse=" / ")),
            tags$span(style="margin-left:10px;",
                      op$lineas$trip_headsign[1])
          ),
        fluidRow(
          # Columna izquierda: resumen del viaje
          column(4,
            tags$div(style="background:#f8f8ff;border-radius:8px;padding:14px;",
              # A pie hasta la parada
              tags$div(style="display:flex;align-items:flex-start;gap:10px;margin-bottom:12px;",
                tags$span("🚶", style="font-size:20px;margin-top:2px;"),
                tags$div(
                  tags$b(paste0(op$walk_orig, " min caminando")),
                  tags$br(),
                  tags$span(style="font-size:12px;color:#555;",
                            paste0("hasta parada ", op$stop_orig$stop_code,
                                   " · ", op$stop_orig$stop_name)),
                  tags$br(),
                  tags$span(style="font-size:11px;color:#aaa;",
                            paste0(round(op$stop_orig$dist_o*1000), " m en línea recta"))
                )
              ),
              tags$hr(style="margin:8px 0;"),
              # Bus
              tags$div(style="display:flex;align-items:flex-start;gap:10px;margin-bottom:12px;",
                tags$span("🚌", style="font-size:20px;margin-top:2px;"),
                tags$div(
                  tags$b(paste0(n_paradas-1, " paradas en autobús")),
                  tags$br(),
                  tags$span(style="font-size:12px;color:#555;",
                            paste0("de ", op$stop_orig$stop_name,
                                   " a ", op$stop_dest$stop_name))
                )
              ),
              tags$hr(style="margin:8px 0;"),
              # A pie desde la parada de bajada
              tags$div(style="display:flex;align-items:flex-start;gap:10px;",
                tags$span("🏁", style="font-size:20px;margin-top:2px;"),
                tags$div(
                  tags$b(paste0(op$walk_dest, " min caminando")),
                  tags$br(),
                  tags$span(style="font-size:12px;color:#555;",
                            paste0("desde parada ", op$stop_dest$stop_code,
                                   " hasta tu destino")),
                  tags$br(),
                  tags$span(style="font-size:11px;color:#aaa;",
                            paste0(round(op$stop_dest$dist_d*1000), " m en línea recta"))
                )
              )
            )
          ),
          
          # Columna centro: paradas del tramo
          column(4,
            tags$p(style="font-size:11px;font-weight:700;color:#888;
                          text-transform:uppercase;letter-spacing:1px;margin-bottom:8px;",
                   paste0("Recorrido · ", n_paradas, " paradas")),
            tags$div(style="max-height:220px;overflow-y:auto;",
              lapply(seq_len(nrow(stops_tramo)), function(i) {
                p   <- stops_tramo[i,]
                col <- if(i==1) "#006600" else if(i==nrow(stops_tramo)) "#cc0000" else "#FE0000"
                tags$div(class="tramo-stop",
                  tags$div(style=paste0("width:10px;height:10px;border-radius:50%;",
                                        "background:",col,";flex-shrink:0;")),
                  tags$span(style=paste0("font-size:12px;",
                                         if(i==1||i==nrow(stops_tramo)) "font-weight:700;" else ""),
                            p$stop_name),
                  tags$span(style="color:#ccc;font-size:10px;margin-left:auto;",
                            p$stop_code)
                )
              })
            )
          ),
          
          # Columna derecha: próximas salidas
          column(4,
            tags$p(style="font-size:11px;font-weight:700;color:#888;
                          text-transform:uppercase;letter-spacing:1px;margin-bottom:8px;",
                   "Próximas salidas"),
            lapply(seq_len(nrow(viajes)), function(vi) {
              v   <- viajes[vi,]
              cl  <- if(vi==1) {
                if(v$mins_espera<=5) "time-chip time-chip-next"
                else if(v$mins_espera<=15) "time-chip time-chip-soon"
                else "time-chip"
              } else "time-chip"
              espera_str <- if(v$mins_espera==0) "¡ahora!"
                            else if(v$mins_espera<60) paste0(v$mins_espera," min")
                            else ""
              tags$div(style="display:flex;align-items:center;gap:8px;padding:4px 0;",
                tags$span(class=cl, v$hora_o_fmt),
                tags$span(style="color:#ccc;font-size:12px;","→"),
                tags$span(class="time-chip",
                          style="background:#f0fff0;color:#006600;border-color:#99cc99;",
                          v$hora_d_fmt),
                if(nchar(espera_str)>0)
                  tags$span(class="countdown-badge", espera_str)
              )
            })
          )
        )
      )
    )
  })
  
  # ── Resumen: tablas ───────────────────────────────────────
  output$tabla_rutas <- renderDT({
    routes_clean %>%
      select(Línea = route_short_name, Nombre = route_long_name) %>%
      mutate(Nombre = gsub("^\\d+ - |^[A-Z]\\d+ - ", "", Nombre)) %>%
      datatable(options = list(pageLength = 15, dom = 'ftp',
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
                rownames = FALSE, class = "compact hover")
  })
  
  output$tabla_paradas_top <- renderDT({
    stop_times %>%
      count(stop_id, name = "n_pasos") %>%
      left_join(stops %>% select(stop_id, stop_name, stop_code), by = "stop_id") %>%
      arrange(desc(n_pasos)) %>%
      slice_head(n = 50) %>%
      select(Código = stop_code, Parada = stop_name, `Pasos/día` = n_pasos) %>%
      datatable(options = list(pageLength = 15, dom = 'ftp',
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
                rownames = FALSE, class = "compact hover")
  })
}

# ============================================================
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

