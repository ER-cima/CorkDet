# script per l'aggregazione del dato pluvio
# Edoardo 28.01.2023
# Edoardo 29.01.2023
# Edoardo 03.02.2023

pluvio_aggrega <- function(rawPluvio, OKrawStatusP, 
                           finestra){
  
  # library(zoo)
  
  # Pre-elaborazione dato misurato del df contenente le misurazioni ----------------------------------------------
  
  # Rinomino DTRF con IDdate
  colnames(rawPluvio)[which(names(rawPluvio)=="DTRF")] <- "IDdate" # NB: data nel formato YYYYMMDDhhmm !!!
  
  # Info acquisizione -------------------------------------------------------------------------------------------
  
  minLagTime <- as.numeric(rawPluvio$IDdate[2]) - as.numeric(rawPluvio$IDdate[1]) # in minuti
  MisureInFinestra <- finestra/minLagTime # nuemro massimo di misure nella finestra analizzata, ovvero se la stazione ha massima risoluzione
  n_finestre <- (nrow(rawPluvio)-1)/MisureInFinestra # numero di misure aggregate (ESCLUDO LO "0" DI MISURAZIONE)
  primaMisura = 1 # L'AGGREGAZIONE PARTE CON LA MISURA SUBITO DOPO L'ISTANTE DI MISURA (es. inizio=12:00 --> prima misura 12:10)
  
  
  # ----- AGGREGA dato pluvio -------------------------------------------------
  
  rawPluvioMisurate <- rawPluvio[primaMisura:nrow(rawPluvio),]
  
  rawPluvioMisurate_aggregated <- rollapply(rawPluvioMisurate[,2:ncol(rawPluvioMisurate)],
                                            MisureInFinestra,
                                            base::sum,
                                            by = MisureInFinestra,
                                            na.rm = TRUE)
  
  # Aggiungi IDdate --------------------------------------------------------------
  rawPluvioMisurate_aggregated <- data.frame(IDdate=NA, rawPluvioMisurate_aggregated)
  
  for(i in 1:nrow(rawPluvioMisurate_aggregated) ){
    # i=2
    is=i*MisureInFinestra
    rawPluvioMisurate_aggregated$IDdate[i] <- rawPluvioMisurate$IDdate[is]
  }
  
  # ---------------------------------------------------------------------------------------------
  #****** OFFLINE: Creo lista con data frame per ogni istante aggregato dell'evento
  #******
  
  Pluvio <- list()
  
  for(i in 1:nrow(rawPluvioMisurate_aggregated)){
    # i=1
    
    Pluvio[[i]] <- data.frame(ID=OKrawStatusP$ID,
                              LON=OKrawStatusP$LON,
                              LAT=OKrawStatusP$LAT,
                              RAIN=NA)
    
    names(Pluvio)[[i]] <- rawPluvioMisurate_aggregated$IDdate[i]
    
    for(k in 1:nrow(Pluvio[[i]])){
      # k=1
      
      Pluvio[[i]]$RAIN[k] <- rawPluvioMisurate_aggregated[i, Pluvio[[i]]$ID[k]==colnames(rawPluvioMisurate_aggregated)]
      
    }
  }
  
  # Creo variabile di output della funzione --------------------------------------------------------
  output <- list(Pluvio, rawPluvioMisurate_aggregated)
  
  names(output)[1] <- "Pluvio"
  names(output)[2] <- "rawPluvioMisurate_aggregated"
  
  return(output)
}
## Fine funzione