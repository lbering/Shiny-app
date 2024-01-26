check.data <- function(data, periods = seq(2000,2024,6)){

    data$loc<-as.factor(data$loc)
    data$Gear<-as.factor(data$Gear)
    data$Quarter<-as.factor(data$Quarter)
    data$SedimentDK<-as.factor(data$SedimentDK)
    data$timeCont <- data$time
    data$time <- factor(data$Year)

    data$period <- cut(data$timeCont, periods)

    return(data)
}


get.mini.model <- function(model,
                           check.terms = c("period","depth",
                                           "Quarter","SedimentDK"),
                           select = FALSE,
                           verbose = TRUE
                           ){

    formi <- model$formula
    formi.split <- strsplit(as.character(formi),"\\+")
    dati <- model$model

    ## Exclude smooth terms (year or depth) if they have been shrunk by select=TRUE
    if(select){
        sumi <- summary(model)
        indi <- which(sumi$s.table[,4] > 0.05)
        indi2 <- NULL
        if(length(indi)){
            for(i in 1:length(indi)){
                indi2 <- c(indi2,grep(substr(rownames(sumi$s.table)[indi[i]],3,
                                             nchar(rownames(sumi$s.table)[indi[i]])-1),
                                      formi.split[[3]]))
            }
            model <- gam(as.formula(paste0("cpue ~ ", paste(formi.split[[3]][-indi2],
                                                            collapse = "+"))),
                         data = dati,
                         family = tw)
            formi <- model$formula
            formi.split <- strsplit(as.character(formi),"\\+")
            dati <- model$model
        }

        ## Exclude parameteric effects based on AIC
        alt.mods <- list(model)
        remi <- NULL
        for(i in 1:length(check.terms)){
            indi <- grep(check.terms[i], formi.split[[3]])
            if(length(indi) > 0){
                alt.mods <- c(alt.mods,list(gam(as.formula(paste0("cpue ~ ",
                                                                  paste(formi.split[[3]][-indi],
                                                                        collapse = "+"))),
                                                data = dati,
                                                family = tw)))
                remi <- c(remi, indi)
            }else{
                print(paste0("check.terms ",check.terms[i], " was not found in the model. Did you spell it correctly?"))
            }
        }

        ## TODO: exclude all combinations as well!

        sapply(seq(2,length(c(1,2,4)),1),function(x) combn(c(1,2,4),x))


    }else{

        alt.mods <- list(model)
        matched <- unlist(sapply(check.terms,
                          function(x) grep(x, formi.split[[3]])))
        combis <- sapply(seq(1,length(matched),1),function(x) combn(matched,x))
        unmatched <- seq(1,length(formi.split[[3]]),1)[-matched]

        if(verbose) writeLines(paste0("Covariate(s) ",
                          paste(formi.split[[3]][unmatched], collapse = ","),
                          " were not matched by 'check.terms' and are kept in all models!"))

        for(i in 1:length(combis)){
            for(j in 1:ncol(combis[[i]])){
                if(length(unmatched) > 0){
                    expli <- paste(c(formi.split[[3]][combis[[i]][,j]],
                                     formi.split[[3]][unmatched]),
                                     collapse = "+")
                }else{
                    expli <- paste(formi.split[[3]][combis[[i]][,j]],
                                   collapse = "+")
                }
                alt.mods <- c(alt.mods,
                              list(gam(as.formula(
                                  paste0("cpue ~ ", expli)),
                                  data = dati,
                                  family = tw)))
            }
        }

    }

    ## special case if all smooth terms and all parametric terms removed, then check null model
    if(length(matched) == length(formi.split[[3]])){
        alt.mods <- c(alt.mods, list(gam(cpue ~ 1,
                                         data = dati,
                                         family = tw)))
    }

    ## Model with best AIC
    indi2 <- which.min(sapply(alt.mods,AIC))

    model.final <- alt.mods[[indi2]]

    if(verbose){
        if(indi2 == 1){
            writeLines("No better model!")
        }else{
            writeLines(paste0("Explanatory variables of best model: ",
                              as.character(model.final$formula)[3]))
        }
    }

    return(model.final)
}
