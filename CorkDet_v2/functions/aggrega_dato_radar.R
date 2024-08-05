# script per l'aggregazione del dato radar
# Edoardo 09.01.2023
# Edoardo 14.01.2023
# Edoardo 16.01.2023: aggiunta di stackApply

# rm(list=ls())
# 
# sPathRadar <- "C:/POLITO_in_uso/TESI/analisi/input/Radar/202212081200_202212091800/"
# 
# finestra <- 30 # finestra di aggregazione in minuti

radar_aggrega <- function(sPathRadar, finestra){

# library(raster) 

# Carica raster da file

nameFileTiff <- list.files(sPathRadar, pattern = ".tif")

listRasterTIFF <- list()

for(i in 1:length(nameFileTiff)){
  
  # i=1
  
  listRasterTIFF[[i]] <- raster(paste0(sPathRadar,nameFileTiff[i]))
  names(listRasterTIFF)[i] <- substr(nameFileTiff[i],6,17)
  
}


# caratteristiche acquisizione radar e finestra di aggregazione

lagTime <- as.numeric(strptime(substr(nameFileTiff[2],6,17),"%Y%m%d%H%M") - strptime(substr(nameFileTiff[1],6,17), "%Y%m%d%H%M"))# in minuti

index <- finestra/lagTime

# aggrega raster --------------------------------------------------------------

listRasterTIFF_aggregated <- list()
index_aggregated <- 0

# aggrega

for(k in seq(from = 1, to=length(listRasterTIFF)-1, by=index)){
  # k=1
  
  inizio_aggregazione <- k
  prima_misura <- k + 1 # L'AGGREGAZIONE PARTE CON LA MISURA SUBITO DOPO L'ISTANTE DI AGGREGAZIONE (es. inizio=12:00 --> primo raster a 12:10)
  seconda_misura <- prima_misura + 1
  fine_aggregazione <- index+k
  
  listAggrega <- list(listRasterTIFF[[prima_misura]])
  #names(listAggrega)[1] <- substr(nameFileTiff[prima_misura],6,17) #FLAG CONTROL
  
  for(i in seconda_misura:fine_aggregazione){
    # i = seconda_misura
    listAggrega <- append(listAggrega,listRasterTIFF[[i]])
    #names(listAggrega)[2] <- substr(nameFileTiff[seconda_misura],6,17) #FLAG CONTROL
    
  }
  
  index_aggregated <- index_aggregated + 1
  
  # aggrega TIFF misurati nella finestra di aggregazione
  listAggrega_bricked <- brick(listAggrega)
  
  indices <- rep(1, length(listAggrega)) # indice unitario necessario per stackApply
  
  listRasterTIFF_aggregated[[index_aggregated]] <- stackApply(listAggrega_bricked, indices, fun = sum)
  
  # rinomino il raster aggregato
  names(listRasterTIFF_aggregated)[[index_aggregated]] <- substr(nameFileTiff[fine_aggregazione],6,17)
  
}

output <- listRasterTIFF_aggregated

return(output)
}
## Fine funzione