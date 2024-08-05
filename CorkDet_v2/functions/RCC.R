# script per l'esecuzione del Radar Conformity Check
# Edoardo 29.01.2023

# dfPluvio <- Pluvio[[i]]

rcc <- function(dfPluvio, RCC3shold_gauge, RCC3shold_radar, RCCifFailed){
  
  # Aggiungo colonna per risutato check
  
  dfPluvio$RCC <- NA
  dfPluvio$RCCinfo <- NA
  
  # Check
  
  for(is in 1:nrow(dfPluvio)){
    # is=1
    
    #check se dato radar è NA (i.e. MSETT)
    if(!is.na(dfPluvio$RADAR[is])){
      
      if(dfPluvio$RAIN[is] < RCC3shold_gauge){
        
        if(dfPluvio$RADAR[is] > RCC3shold_radar){
          
          dfPluvio$RCC[is] <- F
          
          dfPluvio$RCCinfo[is] <- "Controllo FALLITO"
          
          dfPluvio$QI[is] <- dfPluvio$QI[is] - RCCifFailed
          
        }else if(dfPluvio$RADAR[is] < RCC3shold_radar){
          
          dfPluvio$RCC[is] <- T
          
          dfPluvio$RCCinfo[is] <- "Stima radar sottosoglia"
          
        }
      }else if(dfPluvio$RAIN[is] > RCC3shold_gauge){
        
        if(dfPluvio$RADAR[is] > RCC3shold_radar){
          
          dfPluvio$RCC[is] <- T
          
          dfPluvio$RCCinfo[is] <- "Controllo superato"
          
        }else if(dfPluvio$RADAR[is] < RCC3shold_radar){
          
          dfPluvio$RCCinfo[is] <- "SOTTOSTIMA radar"
          
        }
      }
    }# fine controllo dato radar diverso da NA
    
  }
  
  output <- dfPluvio
  
  return(output)
}
# FINE FUNZIONE
