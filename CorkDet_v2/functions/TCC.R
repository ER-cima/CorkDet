# script per l'esecuzione del Radar Conformity Check
# Edoardo 29.01.2023

# dfPluvio <- Pluvio[[i]]
# IDTime <- names(Pluvio)[i]
# TCCnOss <- TCCnOss_i

tcc <- function(dfPluvio, Pluvio, IDTime, TCCnOss, TCCifFailed){
  
  # Aggiungo colonna per risutato check --------------------------------------------
  
  dfPluvio$TCC <- NA
  dfPluvio$TCCinfo <- NA
  
  dfTCC <- dfPluvio[c("ID","RAIN")]
  colnames(dfTCC)[which(colnames(dfTCC)=="RAIN")]<-IDTime
  
  # Aggiungo colonne delle n=TCCnOss osservazioni prima -----------------------------
  
  indiceNow <- which(names(Pluvio)==IDTime)
  indiceOlder <-  indiceNow - TCCnOss # si osservano le n osservazioni prima
  indiceYounger <- indiceNow - 1
  
  for(is in indiceOlder:indiceYounger){
    # is=indiceOlder
    
    Pluvio_new <- Pluvio[[is]]
    
    for(k in 1:nrow(dfTCC)){
      # k=1
      
    indiceStaz <- which(Pluvio_new$ID==dfTCC$ID[k])
    
    dfTCC$new[k] <- Pluvio_new$RAIN[indiceStaz]
    }
    
    colnames(dfTCC)[which(colnames(dfTCC)=="new")] <- names(Pluvio)[is]
    
  }
 
  # Check ----------------------------------------------------------------------------
  
  for(k in 1:nrow(dfPluvio)){
    # k=1
    
    RainValue <- as.numeric(dfPluvio$RAIN[k])
    
    indiceStaz <- which(dfTCC$ID==dfTCC$ID[k])
    
    dfPluvio$TCC[k] <- F
    
    for(is in 1:TCCnOss){
      # is=1
      
      oldRainValue <- as.numeric(dfTCC[indiceStaz, 2 + is])
        
      if(!isTRUE(all.equal(RainValue,oldRainValue))){
        
        dfPluvio$TCC[k] <- T
        
        dfPluvio$TCCinfo <- "Misura non ripetuta"
        
      }
    }
    
    # Operazione sul QI -------------------------------------------------------------
    
    if(isFALSE(as.logical(dfPluvio$RCC[k])) & isFALSE(as.logical(dfPluvio$TCC[k]))){
      
      dfPluvio$QI[k] <- dfPluvio$QI[k] - TCCifFailed
      
      dfPluvio$TCCinfo <- "Controllo FALLITO"
      
    } else if (isFALSE(as.logical(dfPluvio$TCC[k]))){
      
      dfPluvio$TCCinfo <- "Misura ripetuta ma NON CRITICA"
      
    }
    
  }
  
  # # Operazione sul QI -------------------------------------------------------------
  # 
  # for(k in 1:nrow(dfPluvio)){
  #   # k=which(dfPluvio$ID=="RICCO")
  #   
  #   if(isFALSE(as.logical(dfPluvio$RCC[k])) & isFALSE(as.logical(dfPluvio$TCC[k]))){
  #     
  #     dfPluvio$QI[k] <- dfPluvio$QI[k] - TCCifFailed
  #     
  #   }
  #   
  # }
  
  output <- dfPluvio
  
  return(output)
}
# FINE FUNZIONE
