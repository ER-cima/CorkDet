# script per l'aggregazione del dato radar
# Edoardo 09.01.2023
# Edoardo 14.01.2023
# Edoardo 16.01.2023: aggiunta di stackApply
# Edoardo luglio 2024: restyling completo su medesima architettura

radar_aggrega_v2 <- function(radar_img_parameters, data_start, data_end, finestra){
  
  flag_find_img_radar <- NA
  
  # library(raster) 
  
  # CARICO RASTER ==============================================================
  
  # Creo sequenza temporale delle immagini richieste ===========================
  
  POSIX_data_start <- as.POSIXct(as.character(data_start), format="%Y%m%d%H%M")
  POSIX_data_end <- as.POSIXct(as.character(data_end), format="%Y%m%d%H%M")
  
  sequenza_immagini <- as.POSIXlt(seq(from = POSIX_data_start,
                                      to = POSIX_data_end, 
                                      by = radar_img_parameters$lowest_acquisition_time))
  
  # LOADING RADAR IMAGES =======================================================
  # Verifico casistica di struttura ad albero delle immagini salvate
  
  listRasterTIFF <- list()
  
  # Caso 1) "path_generale/yyyy/mm/dd/" ____
  
  if (radar_img_parameters$directory_tree_structure == "yyyy/mm/dd/"){
    
    flag_find = 0 # Falg necessario a ricerca scansioni mancanti
    
    for(is in 1:length(sequenza_immagini)){
      
      # Compongo il corretto path di dove sono immgazzinate le immagini radar ____
      
      data_stringa <- strftime(sequenza_immagini[is], format="%Y%m%d%H%M")
      
      yyyy <- sequenza_immagini[is]$year + 1900
      
      mm <- sprintf("%02d", sequenza_immagini[is]$mon + 1)
      
      dd <- sprintf("%02d", sequenza_immagini[is]$mday)
      
      img_path <- paste0(radar_img_parameters$main_path, yyyy,"/",mm, "/", dd, "/")
      
      img_name <- paste0(img_path,radar_img_parameters$filename_preDate,data_stringa,".tif")
      
      if(file.exists(img_name)){
        
        # Verifico il prodotto radar passato ____
        
        # 1) Caso RMI:
        if(radar_img_parameters$product_type == "RMI"){
          
          raster_radar_image <- calc(raster(img_name), function(x) x / 12) # conversione da [mm/h] a [mm]
          
        }
        
        # Carico l'immagine radar convertita in [mm] nella lista delle scansioni ____
        
        listRasterTIFF[is] <- raster_radar_image
        
        names(listRasterTIFF)[is] <- data_stringa
        
        # Trovo il primo raster da usare come maschera di NA per le scansioni mancanti ____
        
        if(!all(is.na(values(raster(img_name)))) & flag_find == 0){
          
          mask_raster <- raster(img_name)
          
          mask_raster[] <- NA
          names(mask_raster) <- NA
          
          flag_find = 1
          
        }
        
      } else {
        
        listRasterTIFF[is] <- NA
        
        names(listRasterTIFF)[is] <- data_stringa
        
      }
      
    }
    
  } # fine Caso 1 ____
  
  
  # Sostituisco gli NA con raster composti da NA ____
  
  # CONTROLLO ASSENZA DATI RADAR! Se flag = 0 esco dalla funzione, altrimenti vado avanti
  if(flag_find==0){
    
    message(paste0("WARNING: ASSENTI IMMAGINI RADAR PER IL PERIODO RICHIESTO ",
                   data_start, " - ", data_end, 
                   " e/o NELLA DIRECTORY SPECIFICATA (",
                   img_path,")"))
    
    output <- list(RasterTIFF_aggregated=NA,
                   flag_find_img_radar=0)
    
    return(output)
    
  } else {
    
    for(is in 1:length(listRasterTIFF)){
      
      if(is.na(listRasterTIFF[is])){
        
        listRasterTIFF[is] <- list(mask_raster)
        
      }
      
    }
    
    # RADAR IMAGES AGGREGATION ===================================================
    
    # Caratteristiche acquisizione radar e finestra di aggregazione ____
    
    lagTime <- as.numeric(as.POSIXct(names(listRasterTIFF)[2], format="%Y%m%d%H%M") - 
                            as.POSIXct(names(listRasterTIFF)[1], format="%Y%m%d%H%M"))# in minuti
    
    index <- finestra/lagTime # Rappresenta il valore delle scansioni radar in ciascuna finestra di aggregazione
    
    # Aggregazione immagini ____
    
    # Inizializzo lista immagini aggregate e indice corrispondente
    
    listRasterTIFF_aggregated <- list()
    
    index_aggregated <- 0
    
    # Avvio aggregazione
    
    for(k in seq(from = 1, to=length(listRasterTIFF), by=index)){
      
      # Definizione della finestra di aggregazione ____
      
      inizio_aggregazione <- k
      
      fine_aggregazione <- k + index - 1 
      
      index_aggregated <- index_aggregated + 1
      
      listAggrega <- list(listRasterTIFF[[inizio_aggregazione]])
      
      for(i in (inizio_aggregazione + 1):fine_aggregazione){
        
        listAggrega <- append(listAggrega,listRasterTIFF[[i]])
        
      }
      
      # Aggrego i TIFF misurati nella finestra di aggregazione appena definita ____
      
      listAggrega_bricked <- brick(listAggrega)
      
      indices <- rep(1, length(listAggrega)) # indice unitario necessario per stackApply
      
      listRasterTIFF_aggregated[[index_aggregated]] <- stackApply(listAggrega_bricked, indices, fun = sum) # aggregazione
      
      names(listRasterTIFF_aggregated)[[index_aggregated]] <- names(listRasterTIFF)[fine_aggregazione] # rinomino raster aggregato
      
    }
    
    output <- list(listRasterTIFF_aggregated=listRasterTIFF_aggregated,
                   flag_find_img_radar=1)
    
    return(output)
    
  }

}
## Fine funzione