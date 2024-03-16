





#IMPORT DATA POINTS
#---------------------------------------------------------------
points <- st_read("data/points.shp") %>% 
  dplyr::select(rand_point, NAME_1, NAME_2, x, y, geometry) %>%
  st_sf() %>% 
  st_make_valid() %>% 
  st_transform(32633)



weather.nasapower <- function(points) {
  # Nombre total de points
  total_points <- nrow(points)
  
  points %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      Weather = {
        # Afficher la progression tous les 50 points
        if ((row_number() %% 50 == 0) || row_number() == total_points) {
          message("Traitement du point ", row_number(), " sur ", total_points)
        }
        download_and_process_weather_data(y, x)
      }
    ) %>%
    tidyr::unnest(cols = c(Weather)) %>%
    dplyr::ungroup() %>%
    add_derived_variables()
}

# # Sous-fonction pour télécharger et pré-traiter les données météo
download_and_process_weather_data <- function(latitude, longitude) {
  weather_data <- purrr::pmap(
    list(lat = latitude, lon = longitude),
    download_weather
  )
  process_weather_data(weather_data)
}

download_and_process_weather_data <- function(latitude, longitude, start_date, end_date, retries = 3) {
  tryCatch({
    # Votre logique de téléchargement ici
  }, error = function(e) {
    if (retries > 0) {
      message("Échec du téléchargement, tentative de réessai...")
      Sys.sleep(5)  # Pause de 5 secondes avant de réessayer
      download_and_process_weather_data(latitude, longitude, start_date, end_date, retries - 1)
    } else {
      stop("Échec du téléchargement après plusieurs tentatives : ", e$message)
    }
  })
}

# Fonction pour télécharger les données depuis NASA-POWER
download_weather <- function(lat, lon, sta, end) {
  nasapower::get_power(
    community = "AG",
    temporal_api = "daily",
    dates = c("2016-01-01", "2023-12-31"),
    lonlat = c(lon, lat),
    pars = c("T2M_MIN", "T2M_MAX", "RH2M", "PRECTOTCORR", "SG_DAY_HOURS",
             "ALLSKY_SFC_SW_DWN", "GWETPROF", "SG_SAA", "PS", "WD2M", "WS2M")
  )
}

# Fonction pour traiter et organiser les données météorologiques
process_weather_data <- function(weather_data) {
  weather_data %>%
    purrr::map(as.data.frame) %>%
    purrr::map(reformat_and_select_data)
}

# Fonction pour reformater et sélectionner les données
reformat_and_select_data <- function(data) {
  data %>%
    mutate(
      yday = lubridate::yday(YYYYMMDD),
      Year = lubridate::year(YYYYMMDD),
      Month = lubridate::month(YYYYMMDD),
      Day = lubridate::mday(YYYYMMDD)
    ) %>%
    dplyr::select(yday, Year, Month, Day, YYYYMMDD, T2M_MIN, T2M_MAX, RH2M,
                  PRECTOTCORR, SG_DAY_HOURS, ALLSKY_SFC_SW_DWN, GWETPROF, SG_SAA,
                  PS, WD2M, WS2M) %>%
    rename_with(
      ~ c("DOY", "Year", "Month", "Day", "Date", "Tmin", "Tmax", "RH", "PP",
          "DL", "Rad", "GWETPROF", "SG_SAA", "PS", "WD2M", "WS2M")
    )
}

# Fonction pour ajouter des variables dérivées
add_derived_variables <- function(weather_df) {
  weather_df %>%
    mutate(Tmean = (Tmax + Tmin)/2,
           es = 0.6108 * exp((17.27 * Tmean) / (Tmean + 237.3)),
           ea = es * (RH / 100),
           VPD = es - ea)
}

p_2023 <- weather.nasapower(points)

save(p_2023, file = "p_2023.RData")
beepr::beep()