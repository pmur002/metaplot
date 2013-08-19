
# Convert arbitrary meta-analysis result into a
# standardised set of information (for plotting)
meta2DF <- function(meta, ...) {
    UseMethod("meta2DF")
}

##=====================metabin======================##
meta2DF.metabin <- function(meta,
                            add=NULL, rowOrder=NULL, 
                            title=NULL, subtitle=NULL, ...) {
    ## get summary data 
    sum.meta <- summary(meta)                            
    ## generate data columns    
    study <- meta$studlab
    n.e <-  meta$n.e
    event.e <-  meta$event.e
    n.c <-  meta$n.c
    event.c <- meta$event.c 
    effect <- exp(meta$TE)
    w.fixed <- meta$w.fixed / sum(meta$w.fixed) * 100
    w.random <- meta$w.random / sum(meta$w.fixed) * 100     
    group <- rep("", length(study))                           
    se <- meta$seTE    
    mean <- meta$TE
    lower <- sum.meta$study$lower
    upper <- sum.meta$study$upper
    e.lower <- exp(sum.meta$study$lower)
    e.upper <- exp(sum.meta$study$upper)  
    summary.fixed <- data.frame(study = "Fixed effect", 
                          n.e = sum(meta$n.e), event.e = "", 
                          n.c = sum(meta$n.c), event.c = "", 
                          effect = exp(meta$TE.fixed), se = meta$seTE.fixed, 
                          w.fixed = 100, w.random = 0, 
                          mean = meta$TE.fixed, 
                          lower = sum.meta$fixed$lower,
                          upper = sum.meta$fixed$upper,
                          e.lower = exp(sum.meta$fixed$lower),
                          e.upper = exp(sum.meta$fixed$upper),  
                          group = "")
    summary.random <- data.frame(study = "Random effects", 
                          n.e = "", event.e = "", 
                          n.c = "", event.c = "", 
                          effect = exp(meta$TE.random), se = meta$seTE.random, 
                          w.fixed = 0, w.random = 100, 
                          mean = meta$TE.random, 
                          lower = sum.meta$random$lower,
                          upper = sum.meta$random$upper,
                          e.lower = exp(sum.meta$random$lower),
                          e.upper = exp(sum.meta$random$upper),  
                          group = "")

  ## create data frame 
    DF <- data.frame(study = study,
                     n.e = n.e, event.e = event.e,
                     n.c = n.c, event.c = event.c,
                     effect = effect, se = se,
                     w.fixed = w.fixed, w.random = w.random, 
                     mean = mean, lower =lower, upper = upper,
                     e.lower = e.lower, e.upper = e.upper, 
                     group = group)
    
   ## attach additional columns to data frame    
    if (!is.null(add)) { 
      DF <- cbind(DF, add)
    # adds empty columns to summary data frames
      addspace <- lapply(add, function(x){x <- ""})
    # is.na(addspace) <- c(1:length(addspace))
      summary.fixed <- cbind(summary.fixed, addspace)
      summary.random <- cbind(summary.random, addspace)
    }

   ## specify row order        
    nr <- NROW(DF)  
    if (!is.null(rowOrder)) { 
        order <- order(DF[, rowOrder], ...)
        DF <- DF[order, ]  
    }

   ## heterogenity info    
    hetero <- c(Q = sum.meta$Q, df = sum.meta$k - 1, 
                p = pchisq(sum.meta$Q, sum.meta$k - 1, lower.tail = FALSE),
                tau2 = sum.meta$tau^2,
                H = sum.meta$H$TE, 
                H.lower = sum.meta$H$lower, 
                H.upper = sum.meta$H$upper,
                I2 = sum.meta$I2$TE, 
                I2.lower = sum.meta$I2$lower, 
                I2.upper = sum.meta$I2$upper,
                Q.CMH = sum.meta$Q.CMH)

   ## titles    
    title <- title
    subtitle <- subtitle

   ## create list                              
    output <- list(DF = DF, 
                   summary.fixed = summary.fixed, 
                   summary.random = summary.random, 
                   hetero = hetero, title = title, subtitle = subtitle)
                   
    class(output) <- c("metabinDF", "metaDF")
    output
}
##=====================metacont======================##
meta2DF.metacont <-
function(meta, add=NULL, rowOrder=NULL, 
         title=NULL, subtitle=NULL, ...) {   
  #==== create data frame function ====#
   forestDf <- function(study, n.e, mean.e, sd.e, n.c, mean.c, sd.c, effect, 
                        se, w.fixed, w.random, mean, lower, upper, group) {
                   data.frame(study = study,
                              n.e = n.e, mean.e = mean.e, sd.e = sd.e,
                              n.c = n.c, mean.c = mean.c, sd.c = sd.c,
                              effect = effect, se = se,
                              w.fixed = w.fixed, w.random = w.random, 
                              mean = mean, lower =lower, upper = upper, 
                              group = group)
                        }
   ## get summary data 
    sum.meta <- summary(meta) 
   ## create main data frame    
    DF <- forestDf(meta$studlab,
                   meta$n.e, meta$mean.e, meta$sd.e,
                   meta$n.c, meta$mean.c, meta$sd.c,
                   meta$TE, meta$seTE,
                   meta$w.fixed / sum(meta$w.fixed) * 100,
                   meta$w.random / sum(meta$w.random) * 100, 
                   meta$TE, sum.meta$study$lower, sum.meta$study$upper, 
                   rep("", length(meta$studlab)))                    
   ## create summary rows 
    summary.fixed <- forestDf("Fixed effect", 
                              sum(meta$n.e), NA, NA, 
                              sum(meta$n.c), NA, NA,
                              meta$TE.fixed, meta$seTE.fixed, 
                              100, 0, 
                              meta$TE.fixed, sum.meta$fixed$lower, 
                              sum.meta$fixed$upper, 
                              "")
    summary.random <- forestDf("Random effects", 
                               NA, NA, NA, 
                               NA, NA, NA, 
                               meta$TE.random, meta$seTE.random, 
                               0, 100, 
                               meta$TE.random, sum.meta$random$lower,
                               sum.meta$random$upper, 
                               "")    
   ## attach additional columns to data frame    
    if (!is.null(add)) { 
      DF <- cbind(DF, add)
    # adds empty columns to summary data frames
      addspace <- lapply(add, function(x){x <- ""})
    # is.na(addspace) <- c(1:length(addspace))
      summary.fixed <- cbind(summary.fixed, addspace)
      summary.random <- cbind(summary.random, addspace)
    }
   ## specify row order        
    nr <- NROW(DF)  
    if (!is.null(rowOrder)) { 
        order <- order(DF[, rowOrder], ...)
        DF <- DF[order, ]  
    }
   ## dealing with grouped studies      
    if(!is.null(meta$byvar)) { 
      overall.fixed <- summary.fixed
      overall.random <- summary.random
      gp <- meta$byvar
      Group <- list()    
      for (i in 1:max(gp)) {
        df <- DF[gp==i,]
        df["group"] <- i
        summary.fixed <- forestDf("Fixed effect", 
                                  sum(meta$n.e[gp==i]), NA, NA, 
                                  sum(meta$n.c[gp==i]), NA, NA,
                                  sum.meta$within.fixed$TE[i], 
                                  sum.meta$within.fixed$seTE[i], 
                                  0, 0, 
                                  sum.meta$within.fixed$TE[i], 
                                  sum.meta$within.fixed$lower[i],
                                  sum.meta$within.fixed$upper[i],  
                                  i)
        summary.random <- forestDf("Random effects", 
                                   NA, NA, NA, 
                                   NA, NA, NA, 
                                   sum.meta$within.random$TE[i], 
                                   sum.meta$within.random$seTE[i], 
                                   0, 0, 
                                   sum.meta$within.random$TE[i], 
                                   sum.meta$within.random$lower[i],
                                   sum.meta$within.random$upper[i],  
                                   i)    
        hetero.w <- c(sum.meta$Q.w[i], 
                      sum.meta$k.w[i] - 1, 
                      pchisq(sum.meta$Q.w[i], sum.meta$k.w[i] - 1, 
                             lower.tail = FALSE)) 
        Group[[i]] <- list(DF = df,
                           summary.fixed = summary.fixed, 
                           summary.random = summary.random,
                           hetero = hetero.w)                     
      }
    }
   ## heterogeneity info    
    hetero <- c(meta$Q, 
                meta$k - 1, 
                pchisq(meta$Q, meta$k - 1, lower.tail = FALSE))
   ## titles    
    title <- title
    subtitle <- subtitle    
   
   ## create list                               
    if(!is.null(meta$byvar)) {
       output <- list(Group  = Group, 
                      overall.fixed = overall.fixed,
                      overall.random = overall.random, 
                      hetero = hetero, title = title, subtitle = subtitle)
       class(output) <- c("groupedMetaDF", "metacontDF", "metaDF")
    }
    else {
       output <- list(DF = DF,
                      summary.fixed = summary.fixed, 
                      summary.random = summary.random, 
                      hetero = hetero, title = title, subtitle = subtitle)
       class(output) <- c("metacontDF", "metaDF")
    } 
    output  
}
 
