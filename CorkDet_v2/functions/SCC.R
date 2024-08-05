# script per l'esecuzione dello Spatial Consistency Check
# Edoardo ed Angelo 30.01.2023

# SCCRaggio <- 5000
# 
# SCCifFailed <- 0.5
# 
# dfPluvio <- Pluvio[[i]]

scc <- function(dfPluvio, SCCRaggio, SCCifFailed, CoordSystem){
  
  # library(geosphere)
  # library(dplyr)
  # library(sf)
  # library(lwgeom)
  # library(stats)
  
  # Aggiungo colonna per risutato check
  
  dfPluvio$SCC <- NA
  dfPluvio$SCCinfo <- NA
  
  # Calcolo stazioni nel raggio di influenza per ciascuna stazione
  
  dfPluvio[, c("LAT", "LON")] <- dfPluvio[, c("LAT", "LON")]/100000
  
  coord_dfPluvio <- dfPluvio[, c("LON", "LAT")]
  coordinates(coord_dfPluvio) <- ~LON+LAT
  
  distParStaz <- distm(coord_dfPluvio,coord_dfPluvio)
  colnames(distParStaz) <- dfPluvio$ID
  rownames(distParStaz) <- dfPluvio$ID
  
  for(is in 1:nrow(dfPluvio)){
    
    # is=96
    
    # Calcola stazioni in prossimità di quella target
    
    distStaz <- distParStaz[,which(dfPluvio$ID[is]==colnames(distParStaz))]
    
    StazInRaggio <- data.frame(Distanza=distStaz[distStaz <= SCCRaggio], #& distStaz != 0], # mantengo anche la riga corrispettiva alla stazione target, per analisi successive
                               stringsAsFactors = F)
    
    StazInRaggio$ID <- rownames(StazInRaggio)
    
    StazInRaggio[, c("LAT", "LON", "RAIN")] <- dfPluvio[dfPluvio$ID %in% StazInRaggio$ID, c("LAT", "LON", "RAIN")]
    
    # # Trova azimut di ciscuna stazione vicina con quella target --------------------------------
    # 
    # StazInRaggio$Azimut <- NA
    # 
    # for(ds in 1:length(StazInRaggio$ID)){
    #   # ds=58
    
    #   pointsAzimut <- st_sfc(st_point(as.numeric(dfPluvio[is, c("LAT", "LON")])),
    #                          st_point(as.numeric(StazInRaggio[ds, c("LAT", "LON")])),
    #                          crs = CoordSystem)
    #   
    #   # plot(pointsAzimut)
    #   
    #   # conversione da radianti a sessadecimali
    #   
    #   if(dfPluvio$ID[is]==StazInRaggio$ID[ds]){
    #     
    #     StazInRaggio$Azimut[ds] <- NA # la stazione target non ha azimut associato a se stessa con se stessa
    #     
    #   } else {
    #     
    #     AzimutPoints <- as.numeric(st_geod_azimuth(pointsAzimut))/pi*180
    #     
    #     if(AzimutPoints<0){
    #       
    #       # oltre 180 aggiungo 360 perche' fornische l'angolo negativo
    #       AzimutPoints <- as.numeric(st_geod_azimuth(pointsAzimut))/pi*180+360
    #       
    #     }
    #     
    #     StazInRaggio$Azimut[ds] <- AzimutPoints
    #     
    #   }
    # }
    
    # Q10 è la soglia di riferimento per il sottodominio -------------------------------
    
    Q10 <- as.numeric(quantile(as.numeric(StazInRaggio$RAIN), c(0.10)))
    
    if(dfPluvio$RAIN[is] < Q10){
      
      dfPluvio$SCC[is] <- FALSE
      
      dfPluvio$QI[is] <- dfPluvio$QI[is] - SCCifFailed
      
      dfPluvio$SCCinfo[is]  <- "Controllo FALLITO"
      
    }else{
      
      dfPluvio$SCC[is] <- TRUE
      
      dfPluvio$SCCinfo[is]  <- "Controllo superato"
      
    }
    
  }
  
  # Salvataggio output e chiusura funzione SCC ------------------------------------------
  
  output <- dfPluvio
  
  return(output)
  
}
# FINE FUNZIONE