# 🚌 EMT Valencia · App Shiny GTFS

Aplicación interactiva para explorar el transporte público de Valencia
con datos abiertos del Ayuntamiento (formato GTFS).

## Estructura de carpetas

```
valencia-transit/
├── app.R          ← Aplicación principal
├── setup.R        ← Instalador de paquetes + lanzador
├── README.md
├── routes.txt     ← Copia aquí todos los archivos GTFS
├── stops.txt
├── trips.txt
├── stop_times.txt
├── shapes.txt
└── calendar.txt
```

## Puesta en marcha

1. Copia todos los archivos `.txt` del GTFS en esta carpeta.
2. Abre R o RStudio y ejecuta:

```r
source("setup.R")
```

O bien, desde RStudio, abre `app.R` y pulsa **Run App**.

## Funcionalidades

### 🗺 Mapa de Rutas
- Selecciona cualquier línea EMT en el desplegable
- Visualiza el trazado completo de la ruta en rojo
- Haz clic en cualquier parada para ver su nombre y código
- Activa "Mostrar todas las paradas" para ver la red completa

### 🕐 Horarios
- Escoge línea, tipo de día (L-J / V / S / D) y dirección
- Ve todos los horarios por parada ordenados cronológicamente
- Muestra hasta 20 expediciones por consulta

### 🔍 Buscar Ruta
- Selecciona parada de origen y destino (por nombre o código)
- La app encuentra todas las líneas directas entre las dos paradas
- El mapa marca origen (verde) y destino (rojo)
- Botón ⇄ para invertir origen y destino

### 📊 Resumen de la Red
- Estadísticas globales: líneas, paradas y viajes totales
- Tabla completa de todas las líneas
- Ranking de paradas con mayor tráfico

## Paquetes R necesarios

```r
install.packages(c(
  "shiny", "shinydashboard", "leaflet", "leaflet.extras",
  "dplyr", "readr", "DT", "shinyjs", "lubridate"
))
```

## Datos

- **Fuente**: Ayuntamiento de Valencia · EMT (datos abiertos)
- **Formato**: GTFS (General Transit Feed Specification)
- **Vigencia**: Verano 2026 (03/06/2026 – 03/09/2026)
