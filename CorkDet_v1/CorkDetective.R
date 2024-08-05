# funzione CORK DETECTION
# Edoardo 29.01.2023
# Edoardo 30.01.2023
# Edoardo 12.07.2024

CorkDet <- function(GaugesData4CorkDetective, 
                    radar_img_parameters, 
                    parameters4CorkDetective){
  
  #=======================================================================================================================
  # Fase 0 ---------------------------------------------------------------------
  # Adattamento della old CorkDetective al nuovo formato degli input
  
  # Caricamento librerie necessarie alle funzioni di CorkDetective
  
  pacchetti_necessari <- c("sf","dplyr","raster","zoo","geosphere","lwgeom","stats") # da aggiungere terra(?)
  
  tryCatch(
    
    {
      
      suppressPackageStartupMessages(lapply(pacchetti_necessari, library, character.only = TRUE))
      
    }, error=function(e) {
      
      message("ERRORE CARICAMENTO LIBRERIE NECESSARIE PER CORKDETECTIVE")
      message("Messaggio di errore: ")
      message(conditionMessage(e))
      stop()
      
    }
    
  )
  
  # Lettura parametri ----------------------------------------------------------
  
  filtroNA <- as.logical(parameters4CorkDetective$filtroNA)
  
  # Converto i parametri in valori numerici
  
  parameters4CorkDetective <- lapply(parameters4CorkDetective, function(x) as.numeric(x))
  
  # Ora leggo i parametri

  finestra <- parameters4CorkDetective$finestra
  
  CoordSystem <- parameters4CorkDetective$CoordSystem

  bufferRadar <- parameters4CorkDetective$bufferRadar

  # parametri RCC

  RCC3shold_gauge <- parameters4CorkDetective$RCC3shold_gauge

  RCC3shold_radar <- parameters4CorkDetective$RCC3shold_radar

  RCCifFailed <- parameters4CorkDetective$RCCifFailed

  # parametri TCC

  TCCnOss <- parameters4CorkDetective$TCCnOss

  TCCifFailed <- parameters4CorkDetective$TCCifFailed

  # parametri SCC

  SCCRaggio <- parameters4CorkDetective$SCCRaggio

  SCCifFailed <- parameters4CorkDetective$SCCifFailed
  
  # Lettura dati di input ------------------------------------------------------
  
  rawPluvio <- GaugesData4CorkDetective$rawPluvio
  
  rawStatusP <- GaugesData4CorkDetective$rawStatusP
  
  #=======================================================================================================================
  # Fase 1 ---------------------------------------------------------------------
  
  # Lancio della old CorkDetective con finalmente i dati nel formato richiesto 
  # ed i parametri correttamente letti
  
  message("AVVIO CORK DETETECTIVE")
  
  # BONTA' TRASMISSIONE DATO: controllo iniziale sulla bontà di trasmissione del dato dalle stazioni ---------------------
  #                           e filtro sulle stazioni che trasmettono irregolarmente
  
  message(" Controllo bontà di trasmissione del dato pluvio dalle stazioni ")
  
  # carica funzioni controllo bontà trasmissione sugli NA registrati
  
  # source(paste0(sPathCork,"CorkDetectiveFunctions/StatusTrasmissioneOFFLINE.R"))
  
  # controllo
  
  rawStazioni <- rawPluvio
  
  rawStatusStaz <- rawStatusP
  
  StatusStaz <- StatusTrasmiss(rawStazioni, rawStatusStaz,
                                      finestra,
                                      filtroNA, filtroPrecisione)
  
  # PRE PROCESSING: aggregazione del dato sulla finestra temporale scelta ----------------------------------------
  
  message(paste0("* Inizio Pre Processing    ",substr(Sys.time(),12,20)))
  
  # carica funzioni per pre processing
  
  # source(paste0(sPathCork,"CorkDetectiveFunctions/aggrega_dato_radar.R"))
  
  # source(paste0(sPathCork,"CorkDetectiveFunctions/aggrega_dato_pluvio.R"))
  
  # aggregazione del dato sulla finestra di riferimento
  
  # DATO PLUVIO
  
  rawStatusP <- StatusStaz$rawStatusStaz
  
  # Controllo che permette di considerare se il dato è stato filtrato o meno in base alla qualità di trasmissione
  if(filtroNA == T){
  OKrawStatusP <- StatusStaz$OKrawStatusStaz
  } else {
    OKrawStatusP <- rawStatusP
  }
  
  listPluvio_aggregated <- pluvio_aggrega(rawPluvio, 
                                          OKrawStatusP, 
                                          finestra)
  
  Pluvio <- listPluvio_aggregated$Pluvio

  # DATO RADAR

  list_Radar_aggregated <- radar_aggrega_v2(radar_img_parameters, 
                                       data_start = rawPluvio$DTRF[1], 
                                       data_end = rawPluvio$DTRF[nrow(rawPluvio)], 
                                       finestra)
  
  Radar_aggregated <- list_Radar_aggregated$listRasterTIFF_aggregated
  
  # Controllo assenza dato radar: se assente la funzione è troncata, ritornando al main il flag 0
  
  if(list_Radar_aggregated$flag_find_img_radar == 0){
    
    message("WARNING: ASSENTE DATO RADAR, CHIUSURA CORKDETECTIVE! ")
    
    output <- list(flag_interruzione_funzione=0,
                   info_interruzione="ASSENTE DATO RADAR")
    return(output)
    
  } else {
    
    message(paste0("** Fine Pre Processing    ",substr(Sys.time(),12,20)))
    
    # ESTRAZIONE DATO RADAR SUI PLUVIO: dai raster radar aggregati sulla finestra estraggo dati puntuali dei pluviometri ------------
    
    message(paste0("*** Buffer pluvio\radar    ",substr(Sys.time(),12,20)))
    
    # source(paste0(sPathCork,"CorkDetectiveFunctions/estraiDatoRadar.R"))
    
    st_crs(CoordSystem)$proj4string # verifica unità di misura del sistema di coordinate usato
    
    info_stazioni <- OKrawStatusP
    
    RadarBuffPluvio <- estraiDatoRadar(Radar_aggregated,
                                       info_stazioni,
                                       CoordSystem, bufferRadar)
    
    message(paste0("**** Fine buffer pluvio\radar    ",substr(Sys.time(),12,20)))
    
    # CREA CHECK DATASET ----------------------------------------------------------------------------------------------------
    # il CheckDataset è costituito da n df per ciascuna misura dell'evento analizzato
    # a seconda di com'è stato aggregato il dato nel PRE processing (i.e. dato di ogni mezz'ora dell'evento)
    
    # Pluvio: contiene lista misure pluvio aggregate dell'evento, con df per ciascuna misura (i.e. dato di ogni mezz'ora dell'evento)
    
    # RadarBuffPluvio: contiene lista misure radar attorno a ciascun pluvio a seconda del buffer impostato, con df per ciascuna misura (i.e. dato di ogni mezz'ora dell'evento)
    
    
    # Aggrego alla lista delle stazioni pluvio di tutto l'evento (Pluvio) il dato radar estratto dal buffer
    
    for(i in 1:length(Pluvio)){
      # i=1
      
      indice <- which(names(RadarBuffPluvio)==names(Pluvio)[[i]])
      
      Pluvio[[i]]$RADAR <- NA
      
      for (k in 1:nrow(Pluvio[[i]])){
        # k=1
        
        Pluvio[[i]]$RADAR[k] <- RadarBuffPluvio[[indice]]$RADAR[RadarBuffPluvio[[indice]]$ID==Pluvio[[i]]$ID[k]]
        
      }
    }
    
    # ### NEW ###
    # # Aggrego alla lista delle stazioni pluvio di tutto l'evento (Pluvio) il dato radar estratto dal buffer, con raster riproiettato
    #
    # for(i in 1:length(Pluvio)){
    #   # i=1
    #
    #   indice <- which(names(RadarBuffPluvio)==names(Pluvio)[[i]])
    #
    #   Pluvio[[i]]$RADAR_utm <- NA
    #
    #   for (k in 1:nrow(Pluvio[[i]])){
    #     # k=1
    #
    #     Pluvio[[i]]$RADAR_utm[k] <- RadarBuffPluvio[[indice]]$RADAR_utm[RadarBuffPluvio[[indice]]$ID==Pluvio[[i]]$ID[k]]
    #
    #   }
    # }
    # ### NEW ###
    
    # Carico funzioni per checks -----------------------------------------------------------
    
    message(paste0("***** Inizio check: RCC + TCC + SCC    ",substr(Sys.time(),12,20)))
    
    #RCC
    # source(paste0(sPathCork,"CorkDetectiveFunctions/RCC.R"))
    
    #TCC
    # source(paste0(sPathCork,"CorkDetectiveFunctions/TCC.R"))
    
    #SCC
    # source(paste0(sPathCork,"CorkDetectiveFunctions/SCC.R"))
    
    CheckDataset <- list()
    
    Nmisure <- length(Pluvio)
    
    # CHECKS PER QI -------------------------------------------------------------------------------------
    
    # ATTENZIONE: L'ISTANTE i E' IL NOW PER OPERAZIONI OFFLINE
    
    for(i in 1:length(Pluvio)){
      # i=1
      
      # Istante considerato
      
      IDTime <- names(Pluvio)[i]
      
      # QI: Quality index
      # Inserisco una colonna unitaria che identifichi qualità pluvio prima dell'analisi
      
      Pluvio[[i]]$QI <- rep(1,nrow(Pluvio[[i]]))
      
      # RCC: Radar Conformity Check ------------------------------------------------------------------
      
      dfPluvio <- Pluvio[[i]]
      
      Pluvio[[i]] <- rcc(dfPluvio, RCC3shold_gauge, RCC3shold_radar, RCCifFailed)
      
      # TCC: Time Conformity Check ------------------------------------------------------------------
      
      if(i!=1){ # Sulla prima misura dell'evento non è possibile eseguire il TCC, non si ha storico
        
        dfPluvio <- Pluvio[[i]]
        
        TCCnOss_i <- TCCnOss
        
        if(i <= TCCnOss){ # check per realizzare il TCC anche con le osservazioni che non hanno sufficente storico
          
          TCCnOss_i <- i - 1 # prendo tutte le osservazioni storiche disponibili tranne quella now
          
        }
        
        Pluvio[[i]] <- tcc(dfPluvio, Pluvio, IDTime, TCCnOss_i, TCCifFailed)
        
      } # fine TCC check
      
      # SCC ---------------------------------------------------------------------------------------
      
      dfPluvio <- Pluvio[[i]]
      
      Pluvio[[i]] <- scc(dfPluvio, SCCRaggio, SCCifFailed, CoordSystem)
      
    }
    
    message(paste0("****** Fine check e chiusura Cork Detective    ",substr(Sys.time(),12,20)))
    
    # Chiusura funzione e restituzione output --------------------------------------------------------
    
    output <- list(Pluvio=Pluvio, 
                   rawStatusP=rawStatusP, 
                   OKrawStatusP=OKrawStatusP,
                   flag_interruzione_funzione=1)
    
    message("CHIUSURA CORK DETETECTIVE: bye bye")
    
    return(output)
    
  }

}
# FINE FUNZIONE




# # NB: trick 4 debugging                                                         ######################### !  !! ! !! ! !!
# save.image(file="/home/cfmi.arpal.org/validazione/temp/debugInsideCORK.RData")

