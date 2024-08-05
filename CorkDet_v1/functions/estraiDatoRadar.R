# script per l'estrazione del dato radar per ogni pluvio
# First version ---
# Edoardo 15.01.2023
# Edoardo 16.01.2023
# Edoardo 28.01.2023
# Edoardo 29.01.2023
# New version ---
# Edoardo Luglio 2024


estraiDatoRadar <- function(Radar_aggregated, 
                            info_stazioni,
                            CoordSystem, bufferRadar){
  
  # library(sf)
  # library(raster)
  
  # ============================================================================
  # Impostazioni iniziali e lettura parametri funzione =========================
  
  info_stazioni_radar <- info_stazioni
  
  info_stazioni_radar[, c("LAT", "LON")] <- info_stazioni_radar[, c("LAT", "LON")]/100000
  
  RadarStaz <- info_stazioni_radar[,c("ID","LAT","LON")]
  
  RadarStaz$RADAR <- NA
  
  CoordRefSyst <- paste0("+init=epsg:", CoordSystem)
  
  # Trasforma i punti in oggetti sf
  mypoints <- st_as_sf(RadarStaz, coords = c("LON", "LAT"), crs = CoordSystem)
  
  # ============================================================================
  # Estrazione del dato radar in corrispondeza dei pluviometri =================
  
  # La funzione da applicare a ciascun raster della lista ______________________
  
  buffer_radar_estrai <- function(fileTif) {
    
    RadarStaz$RADAR <- NA
    
    # Imposta il sistema di coordinate del raster
    crs(fileTif) <- CRS(CoordRefSyst)
    
    # Estrai i valori raster con un buffer
    RadarStaz$RADAR <- raster::extract(fileTif, 
                                       mypoints,
                                       buffer = bufferRadar,     # buffer size, units depend on CRS
                                       fun = max)       # what value to extract
    
    # Sostituisci i valori negativi con -9999
    RadarStaz$RADAR[which(RadarStaz$RADAR < 0)] <- -9999
    
    # Restituisci il data frame modificato
    return(RadarStaz)
  }
  
  # Applicazione della funzione alla lista di raster ___________________________
  
  RadarSuPluvio <- lapply(Radar_aggregated, buffer_radar_estrai)
  
  # ============================================================================
  # Ritorno output e chiusura funzione =========================================
  
  output <- RadarSuPluvio
  
  return(output)
}
# Fine funzione




# My dear old version ==========================================================

# #****** OFFLINE: in real time la scansione è di un singolo istante alla volta
# #******
# # Ciclo per ogni raster radar aggregato ed estrazione dal raster con buffer attorno al pluvio
# 
# RadarSuPluvio <- list() # lista dove immagazzinare il dato estratto
# 
# for(i in 1:length(Radar_aggregated)){
#   
#   # i=1
#   
#   # Old version --------------------------------------------------------------
#   # locationStaz <- RadarStaz[,c("LON","LAT")]
#   # coordinates(locationStaz) <- ~LON+LAT
#   # mypoints <- SpatialPoints(locationStaz,proj4string = CRS(CoordRefSyst))
#   # Fine old version ---------------------------------------------------------
#   
#   # Punti su cui centrare il buffer del radar (tipicamente i pluviuometri)
#   mypoints <- st_as_sf(RadarStaz, coords = c("LON","LAT"), crs = CoordSystem)
#   
#   fileTif <- Radar_aggregated[[i]]
#   crs(fileTif) <-  CRS(CoordRefSyst)
#   crs(fileTif)
#   
#   # # Verifica con coordinate proiettate -------------------------------------
#   # 
#   # mypoints_utm <- st_transform(mypoints, crs = 32632)
#   # 
#   # fileTif_utm <- raster::projectRaster(fileTif, crs = "+proj=utm +zone=32 +datum=WGS84")
#   # crs(fileTif_utm)
#   # # Fine verifica coordinate proiettate ------------------------------------
#   
#   RadarStaz$RADAR <- raster::extract(fileTif, 
#                                      mypoints,
#                                      buffer = bufferRadar,     # buffer size, units depend on CRS
#                                      fun = max)       # what to value to extract
#   
#   RadarStaz$RADAR[which(RadarStaz$RADAR < 0)] <- NA  # inserisco NA al posto di valori negativi radar (tipicamente MSETT)
#   
#   # # Verifica con coordinate proiettate -------------------------------------
#   # RadarStaz$RADAR_utm <- raster::extract(fileTif_utm, 
#   #                                        mypoints,
#   #                                        buffer = 1500,     # buffer size, units depend on CRS
#   #                                        fun = max)       # what to value to extract
#   # 
#   # RadarStaz$RADAR_utm[which(RadarStaz$RADAR_utm < 0)] <- NA 
#   # # Fine verifica coordinate proiettate ------------------------------------
#   
#   #****** OFFLINE: Salvataggio singolo istante di controllo in lista complessiva dell'evento
#   #******
#   RadarSuPluvio[[i]] <- RadarStaz
#   names(RadarSuPluvio)[[i]] <- names(Radar_aggregated)[[i]]
#   
# }


