# script per controllare lo status dei pluviometri,
# ovvero per verificare se la ricezione del segnale è corretta (check sugli NA)
# Edoardo 03.02.2023

#****** ATTENZIONE: si identifica come:
#                   rawStazioni = il df contenente la variabile (i.e. pioggia, temp, ecc) con sulle colonne le stazioni
#                   rawStatusStaz = il df contenente le informazioni delle stazioni sulle colonne


# rawStazioni <- rawPluvio
# 
# rawStatusStaz <- rawStatusP


StatusTrasmiss <- function(rawStazioni, rawStatusStaz,
                                  finestra,
                                  filtroNA, filtroPrecisione){
  
  # library(dplyr)
  
  # Pre-elaborazione dato misurato del df contenente le misurazioni ----------------------------------------------
  
  # Rinomino DTRF con IDdate
  colnames(rawStazioni)[which(names(rawStazioni)=="DTRF")] <- "IDdate"
  
  # Info acquisizione -------------------------------------------------------------------------------------------
  
  minLagTime <- as.numeric(rawStazioni$IDdate[2]) - as.numeric(rawStazioni$IDdate[1]) # in minuti
  MisureInFinestra <- finestra/minLagTime # nuemro massimo di misure nella finestra analizzata, ovvero se la stazione ha massima risoluzione
  n_finestre <- nrow(rawStazioni)/MisureInFinestra # numero di misure aggregate (ESCLUDO LO "0" DI MISURAZIONE)
  # primaMisura = 1 # L'AGGREGAZIONE PARTE CON LA MISURA SUBITO DOPO L'ISTANTE DI MISURA (es. inizio=12:00 --> prima misura 12:10)
  
  # ------ 1° CHECK NA: rilevo la compatibilità degli NA di ciascuna finestra di aggregazione ----------------------------------
  #                     con la precisione di ciascuna stazione
  
  noNA <- "OK"
  rawStazioniFiltered <- rawStazioni # data frame da filtrare
  
  for(i in 2:ncol(rawStazioni)){ # scansione della finestra indice per ciascuna stazione i
    # i=5
    
    # Calcolo n NA per ciascuna stazione
    rawStatusStaz$nNA[which(rawStatusStaz$ID==colnames(rawStazioni)[i])] <- base::sum(is.na(rawStazioni[i]))
    
    # Verifico che n Na sia accettabile con la sua precisione
    
    n_misureXfinestra <- finestra/rawStatusStaz$Precisione[rawStatusStaz$ID==colnames(rawStazioni)[i]]
    
    n_misure <- n_finestre * n_misureXfinestra
    
    # Check se stazione ha precisione maggiore  del minLagTime
    if(rawStatusStaz$Precisione[rawStatusStaz$ID==colnames(rawStazioni)[i]] < minLagTime ){
      
      n_NA_compatibile <- 0
      
    } else {
      
      n_NA_compatibile <- MisureInFinestra * n_finestre - n_misure
      
    }
    
    if(n_NA_compatibile==rawStatusStaz$nNA[rawStatusStaz$ID==colnames(rawStazioni)[i]]){
      
      rawStatusStaz$Status[rawStatusStaz$ID==colnames(rawStazioni)[i]] <- noNA
      
    }
    
  }
  
  # ------ 2° CHECK NA: filtro sulle stazioni con numero di NA inconsistente con la precisione -------------------------
  
  if(filtroNA == T){
    
    # STAZIONI CON PROBLEMI DI MISURAZIONE: troppi NA rispetto a precisione o precisione non definibile
    
    # FILTRO sulle stazioni che non misurano nulla o che misurano in modo irregolare (se status=NA sono eliminate)
    
    OKrawStatusStaz <- filter(rawStatusStaz,!is.na(Status))
    
    rawStazioniFiltered <- rawStazioni[,c("IDdate", OKrawStatusStaz$ID)]
    
  }
  
  # Chiusura funzione e restituzione output ---------------------------------------------------
  
  if(filtroNA == T){
    
    output <- list(rawStazioniFiltered, OKrawStatusStaz, rawStatusStaz)
    names(output)[1] <- "rawStazioniFiltered"
    names(output)[2] <- "OKrawStatusStaz"
    names(output)[3] <- "rawStatusStaz"
    
  } else {
    
    output <- list(rawStatusStaz)
    names(output)[1] <- "rawStatusStaz"
    
  }
  
  
  return(output)
  
}
# FINE FUNZIONE