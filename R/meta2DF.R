### set up generic function
meta2DF <- function(meta, ...) {
  UseMethod("meta2DF")
}

###========================meta=============================###

###=====metabin=====###
meta2DF.metabin <- function(meta, add = NULL, rowOrder = NULL,
                            title = NULL, subtitle = NULL, ...){
  ## step 1: set up the main data frame
  ## summary meta data
  summMeta <- summary(meta)
  # set up main data frame
  DF <- forestDF(meta, study = meta$studlab, n.e = meta$n.e,
                 event.e = meta$event.e, n.c = meta$n.c, se = meta$seTE,
                 event.c = meta$event.c, effect = exp(meta$TE),
                 w.fixed = (meta$w.fixed/sum(meta$w.fixed)*100),
                 w.random = (meta$w.random/sum(meta$w.random)*100),
                 mean = meta$TE, lower = summMeta$study$lower,
                 upper = summMeta$study$upper,
                 e.lower = exp(summMeta$study$lower),
                 e.upper = exp(summMeta$study$upper),
                 summary = FALSE)


  ## step 2: set up the fixed effect and the random effect
  ## fixed effect
  summaryFixed <- forestDF(meta, study = "Fixed effect",
                           n.e = sum(meta$n.e), event.e = NA,
                           n.c = sum(meta$n.c), event.c = NA,
                           effect = exp(meta$TE.fixed),
                           se = meta$seTE.fixed, w.fixed = 100,
                           w.random = 0, mean = meta$TE.fixed,
                           lower = summMeta$fixed$lower,
                           upper = summMeta$fixed$upper,
                           e.lower = exp(summMeta$fixed$lower),
                           e.upper = exp(summMeta$fixed$upper),
                           summary = TRUE)

  ## random effect
  summaryRandom <- forestDF(meta, study = "random effect",
                            n.e = NA, event.e = NA,
                            n.c = NA, event.c = NA,
                            effect = exp(meta$TE.random),
                            se = meta$seTE.random,
                            w.fixed = 0, w.random = 100,
                            mean = meta$TE.random,
                            lower = summMeta$random$lower,
                            upper = summMeta$random$upper,
                            e.lower = exp(summMeta$random$lower),
                            e.upper = exp(summMeta$random$upper),
                            summary = TRUE)


  ## step 3: customize the main data frame
  ## attach additional columns to the meta object
  if (!is.null(add)) {
    ## attach the additional column to the main data frame
    DF <- cbind(DF, add)
    ## attach the corresponding space to the summary data frame
    addspace <- lapply(add, function(x){x <- ""})
    summaryFixed <- cbind(summaryFixed, addspace)
    summaryRandom <- cbind(summaryRandom, addspace)
  }

  ## specify row orders
  if (!is.null(rowOrder)) {
    Order <- order(DF[, rowOrder], ...)
    DF <- DF[Order, ]
  }

  ## step 4: heterogeneity information
  hetero <- c(Q = summMeta$Q, df = summMeta$k - 1,
              p = pchisq(summMeta$Q, summMeta$k - 1, lower.tail = FALSE),
              tau2 = summMeta$tau^2,
              H = summMeta$H$TE,
              H.lower = summMeta$H$lower,
              H.upper = summMeta$H$upper,
              I2 = summMeta$I2$TE,
              I2.lower = summMeta$I2$lower,
              I2.upper = summMeta$I2$upper,
              Q.CMH = summMeta$Q.CMH)



  ## step 5: Grouped Studies
  if (!is.null(meta$byvar)){
    Group <- list()
    gp <- DF["group"]
    for (i in 1:max(gp)){
      ## set up of the main DF for the group
      df <- DF[gp == i, ]

      ## fixed effect for the group
      groupFixed <- forestDF(meta, study = "Fixed Effect",
                             n.e = sum(meta$n.e[gp == i]), event.e = NA,
                             n.c = sum(meta$n.c[gp == i]), event.c = NA,
                             effect = exp(summMeta$within.fixed$TE[i]),
                             se = summMeta$within.fixed$seTE[i], w.fixed = 0,
                             w.random = 0, mean = summMeta$within.fixed$TE[i],
                             lower = summMeta$within.fixed$lower[i],
                             upper = summMeta$within.fixed$upper[i],
                             e.lower = exp(summMeta$within.fixed$lower[i]),
                             e.upper = exp(summMeta$within.fixed$upper[i]),
                             summary = TRUE)

      ## random effect for the group
      groupRandom <- forestDF(meta, study = "Random Effect",
                              n.e = NA, event.e = NA,
                              n.c = NA, event.c = NA,
                              effect = exp(summMeta$within.random$TE[i]),
                              se = summMeta$within.random$seTE[i],
                              w.fixed = 0, w.random= 0,
                              mean = summMeta$within.random$TE[i],
                              lower = summMeta$within.random$lower[i],
                              upper = summMeta$within.random$upper[i],
                              e.lower = exp(summMeta$within.random$lower[i]),
                              e.upper = exp(summMeta$within.random$upper[i]),
                              summary = TRUE)

      ## heterogeneity information for the group
      hetero.w <- c(Q = summMeta$Q.w[i], df = summMeta$k.w[i] - 1,
                    p = pchisq(summMeta$Q.w[i], summMeta$k.w[i] - 1,
                               lower.tail = FALSE),
                    tau2 = summMeta$tau.w[i]^2,
                    H = summMeta$H.w$TE[i],
                    H.lower = summMeta$H.w$lower[i],
                    H.upper = summMeta$H.w$upper[i],
                    I2 = summMeta$I2.w$TE[i],
                    I2.lower = summMeta$I2.w$lower[i],
                    I2.upper = summMeta$I2.w$upper[i])

      ## set up the groups
      Group[[i]] <- list(DF = df, summaryFixed = groupFixed,
                         summaryRandom = groupRandom, hetero = hetero.w)

    }
  }

  ## step 6: set up titles
  title <- title
  subtitle <- subtitle

  ## step 7: wrap up
  if (!is.null(meta$byvar)){
    output <- list(Group = Group, overallFixed = summaryFixed,
                  overallRandom = summaryRandom, hetero = hetero,
                  title = title, subtitle = subtitle)
    class(output) <- c("groupedMetaDF", "metabinDF", "metaDF")
  } else{
    output <- list(DF = DF, summaryFixed = summaryFixed,
                   summaryRandom = summaryRandom, hetero = hetero,
                   title = title, subtitle = subtitle)
    class(output) <- c("metabinDF", "metaDF")
  }
  output
}

###=====metacont=====###
meta2DF.metacont <- function(meta, add = NULL, rowOrder = NULL,
                             title = NULL, subtitle = NULL, ...){
  ## step 1: set up the main data frame
  ## summary meta data
  summMeta <- summary(meta)
  ## set up the main data frame
  DF <- forestDF(meta, study = meta$studlab, n.e = meta$n.e,
                 mean.e = meta$mean.e, sd.e = meta$sd.e,
                 n.c = meta$n.c, mean.c = meta$mean.c, sd.c = meta$sd.c,
                 effect = meta$TE, se = meta$seTE,
                 w.fixed = meta$w.fixed/sum(meta$w.fixed)*100,
                 w.random = meta$w.random/sum(meta$w.random)*100,
                 mean = meta$TE, lower = summMeta$study$lower,
                 upper = summMeta$study$upper)

  ## step 2: set up the fixed effect and the random effect
  ## fixed effect
  summaryFixed <- forestDF(meta, study = "Fixed effect",
                           n.e = sum(meta$n.e), mean.e = NA, sd.e = NA,
                           n.c = sum(meta$n.c), mean.c = NA, sd.c = NA,
                           effect = meta$TE.fixed, se = meta$seTE.fixed,
                           w.fixed = 100, w.random = 0, mean = meta$TE.fixed,
                           lower = summMeta$fixed$lower,
                           upper = summMeta$fixed$upper, summary = TRUE)

  ## random effect
  summaryRandom <- forestDF(meta, study = "Random effect",
                            n.e = NA, mean.e = NA, sd.e = NA,
                            n.c = NA, mean.c = NA, sd.c = NA,
                            effect = meta$TE.random, se = meta$seTE.random,
                            w.fixed = 0, w.random = 100, mean = meta$TE.random,
                            lower = summMeta$random$lower,
                            upper = summMeta$random$upper, summary = TRUE)

  ## step 3: customize the main data frame
  ## attach additional columns to the meta object
  if (!is.null(add)){
    ## attach the additional column to the main data frame
    DF <- cbind(DF, add)
    ## attach the corresponding space to the summary data frame
    addspace <- lapply(add, function(x){x <- ""})
    summaryFixed <- cbind(summaryFixed, addspace)
    summaryRandom <- cbind(summaryRandom, addspace)
  }

  ## specify row orders
  if (!is.null(rowOrder)) {
    order <- order(DF[, rowOrder], ...)
    DF <- DF[order, ]
  }

  ## step 4: heterogenity information
  hetero <- c(Q = summMeta$Q, df = summMeta$k - 1,
              p = pchisq(summMeta$Q, summMeta$k - 1, lower.tail =FALSE),
              tau2 = summMeta$tau^2,
              H = summMeta$H$TE,
              H.lower = summMeta$H$lower,
              H.upper = summMeta$H$upper,
              I2 = summMeta$I2$TE,
              I2.lower = summMeta$I2$lower,
              I2.upper = summMeta$I2$upper,
              Q.CMH = summMeta$Q.CMH)

  ## step 5: grouped studies
  if (!is.null(meta$byvar)){
    Group <- list()
    gp <- DF["group"]
    for (i in 1:max(gp)){
      ## set up of the main DF for the group
      df <- DF[gp == i, ]
      ## fixed effect for the group
      groupFixed <- forestDF(meta, study = "Fixed Effect",
                             n.e = sum(meta$n.e[gp == i]),
                             mean.e = NA, sd.e = NA,
                             n.c = sum(meta$n.c[gp == i]),
                             mean.c = NA, sd.c = NA,
                             effect = summMeta$within.fixed$TE[i],
                             se = summMeta$within.fixed$seTE[i],
                             w.fixed = 0, w.random = 0,
                             mean = summMeta$within.fixed$TE[i],
                             lower = summMeta$within.fixed$lower[i],
                             upper = summMeta$within.fixed$upper[i],
                             summary = TRUE)

      ## random effect for the group
      groupRandom <-forestDF(meta, study = "Random Effect",
                             n.e = NA, mean.e = NA, sd.e = NA,
                             n.c = NA, mean.c = NA, sd.c = NA,
                             effect = summMeta$within.random$TE[i],
                             se = summMeta$within.random$seTE[i],
                             w.fixed = 0, w.random = 0,
                             mean = summMeta$within.random$TE[i],
                             lower = summMeta$within.random$lower[i],
                             upper = summMeta$within.random$upper[i],
                             summary = TRUE)

      ## heterogeneity information for the group
      hetero.w <- c(Q = summMeta$Q.w[i], df = summMeta$k.w[i] - 1,
                    p = pchisq(summMeta$Q.w[i], summMeta$k.w[i] - 1,
                               lower.tail = FALSE),
                    tau2 = summMeta$tau.w[i]^2,
                    H = summMeta$H.w$TE[i],
                    H.lower = summMeta$H.w$lower[i],
                    H.upper = summMeta$H.w$upper[i],
                    I2 = summMeta$I2.w$TE[i],
                    I2.lower = summMeta$I2.w$lower[i],
                    I2.upper = summMeta$I2.w$upper[i])

      ## set up the group
      Group[[i]] <- list(DF = df, summaryFixed = groupFixed,
                         summaryRandom = groupRandom, hetero = hetero.w)
    }
  }

  ## step 6: set up the titles
  title <- title
  subtitle <- subtitle

  ## step 7: the wrap up
  if (!is.null(meta$byvar)) {
    output <- list(Group = Group, overallFixed = summaryFixed,
                   overallRandom = summaryRandom, hetero = hetero,
                   title = title, subtitle = subtitle)
    class(output) <- c("groupedMetaDF", "metacontDF", "metaDF")
  }
  else{
    output <- list(DF = DF, summaryFixed = summaryFixed,
                   summaryRandom = summaryRandom, hetero = hetero,
                   title = title, subtitle = subtitle)
    class(output) <- c("metacontDF", "metaDF")
  }
  output
}

###========================rmeta=============================###

### meta.MH
meta2DF.meta.MH <- function(meta, add = NULL, sub = NULL, rowOrder = NULL,
                            title = NULL, subtitle = NULL, ...) {

  summMeta <- summary(meta)
  ## step 1: set up main data frame
  DF <- forestDF(object = meta, study = meta$names,
                 effect = summMeta$stats[, meta$statistic],
                 se = if (meta$statistic == "OR") {
                        meta$selogOR
                      } else {
                        meta$selogRR
                      },
                 rate = log(summMeta$stats[, meta$statistic]),
                 lower = log(summMeta$stats[, "(lower "]),
                 upper = log(summMeta$stats[, paste(100*meta$conf.level,
                                                    "% upper)", sep = "")]))

  ## step 2: set up fixed effect
  summaryFixed <- forestDF(object = meta, study = "Fixed effect",
                           effect = summMeta$MHci[2],
                           se = meta$selogMH,
                           rate = log(summMeta$MHci[2]),
                           lower = log(summMeta$MHci[1]),
                           upper = log(summMeta$MHci[3]))

  ## step 3: set up random effect
  summaryRandom <- NULL

  ## step 4: substitute the columns
  if (!is.null(sub)) {
    colNames <- names(sub)
    if (!all(colNames %in% colnames(DF))){
      stop("the columns to be substituted do not exist")
    }
    if (!all(sapply(sub, function(sub) length(sub$DF)) == nrow(DF))){
      stop("the length of the substituted column differs from its corresponding column")
    }
    DF[, names(sub)] <- sapply(sub,
                               function(sub, DF) {DF[, names(sub)] <- sub$DF},
                               DF = DF)
    summaryFixed[, names(sub)] <- sapply(sub,
                                         function(sub, DF) {
                                           DF[, names(sub)] <- sub$sum
                                         },
                                         DF = DF)
  }

  ## step 4: customize the main data frame
  ## attach additional columns to the meta object
  if (!is.null(add)) {
    ## attach the additional column to the main data frame
    DF <- cbind(DF, add)
    ## attach the corresponding space to the summary data frame
    addspace <- lapply(add, function(x){x <- ""})
    summaryFixed <- cbind(summaryFixed, addspace)
  }

  ## specify row orders
  if (!is.null(rowOrder)) {
    Order <- order(DF[, rowOrder], ...)
    DF <- DF[Order, ]
  }

  ## step 5: heterogeneity information
  hetero <- c(Q = meta$het[1], df = meta$het[2],
              p = meta$het[3], tau2 = NA,
              H = NA, H.lower = NA, H.upper = NA,
              I2 = NA, I2.lower = NA, I2.upper = NA,
              Q.CMH = NA, conf.level = meta$conf.level)

  ## step 6: set up the titles
  Title <- title
  Subtitle <- subtitle

  ## step 7: the wrap up
  output <- list(DF = DF, summaryFixed = summaryFixed,
                 summaryRandom = summaryRandom,
                 hetero = hetero, title = Title, subtitle = Subtitle)
  class(output) <- c("metabinDF", "metaDF")

  output
}

## meta.DSL
meta2DF.meta.DSL <- function(meta, add = NULL, sub = NULL, rowOrder = NULL,
                             title = NULL, subtitle = NULL, ...) {

  summMeta <- summary(meta)
  ## step 1: set up main data frame
  DF <- forestDF(object = meta, study = meta$names,
                 effect = summMeta$ors[, meta$statistic],
                 se = if (meta$statistic == "OR"){
                        meta$selogs
                      } else {
                        meta$selogs
                      },
                 rate = log(summMeta$ors[, meta$statistic]),
                 lower = log(summMeta$ors[, "(lower "]),
                 upper = log(summMeta$ors[, paste(100*meta$conf.level,
                                                  "% upper)", sep = "")]))


  ## step 2: set up fixed effect
  summaryFixed <- NULL

  ## step 3: set up random effect
  summaryRandom <- forestDF(object = meta, study = "Random effect",
                            effect = summMeta$ci[2],
                            se = meta$selogDSL,
                            rate = log(summMeta$ci[2]),
                            lower = log(summMeta$ci[1]),
                            upper = log(summMeta$ci[3]))

  ## step 4: substitute the columns
  if (!is.null(sub)) {
    colNames <- names(sub)
    if (!all(colNames %in% colnames(DF))){
      stop("the columns to be substituted do not exist")
    }
    if (!all(sapply(sub, function(sub) length(sub$DF)) == nrow(DF))){
      stop("the length of the substituted column differs from its correponding column")
    }
    DF[, names(sub)] <- sapply(sub,
                               function(sub, DF) {DF[, names(sub)] <- sub$DF},
                               DF = DF)
    summaryFixed[, names(sub)] <- sapply(sub,
                                         function(sub, DF) {
                                           DF[, names(sub)] <- sub$sum
                                         },
                                         DF = DF)
  }

  ## step 4: customization on the main data frame
  ## attach additional columns to the meta object
  if (!is.null(add)) {
    ## attach the additional column to the main data frame
    DF <- cbind(DF, add)
    ## attach the corresponding space to the summary data frame
    addspace <- lapply(add, function(x){x <- ""})
    summaryFixed <- cbind(summaryFixed, addspace)
  }

  ## specify row orders
  if (!is.null(rowOrder)) {
    Order <- order(DF[, rowOrder], ...)
    DF <- DF[Order, ]
  }

  ## step 5: heterogeneity information
  hetero <- c(Q = meta$het[1], df = meta$het[2],
              p = meta$het[3], tau2 = NA,
              H = NA, H.lower = NA, H.upper = NA,
              I2 = NA, I2.lower = NA, I2.upper = NA,
              Q.CMH = NA, conf.level = meta$conf.level)

  ## step 6: set up the titles
  Title <- title
  Subtitle <- subtitle

  ## step 7: the wrap up
  output <- list(DF = DF, summaryFixed = summaryFixed,
                 summaryRandom = summaryRandom,
                 hetero = hetero, title = Title, subtitle = Subtitle)
  class(output) <- c("metabinDF", "metaDF")

  output
}

###=============================metafor================================###
## rma.mh
meta2DF.rma.mh <- function(meta, add = NULL, sub = NULL, rowOrder = NULL,
                           title = NULL, subtitle = NULL, ...) {

  CI <- ciGen(meta)
  if (!any(names(sub) %in% "study")) {
    study.names <- paste("study", 1:length(CI$DF$mean))
  }
  ## step 1: set up main data frame
  DF <- forestDF(object = meta, study = study.names,
                 n.e = meta$ai, event.e = meta$ai + meta$bi,
                 n.c = meta$ci, event.c = meta$ci + meta$di,
                 effect = exp(CI$DF$mean), se = sqrt(meta$vi),
                 w.fixed = weights(meta),
                 mean = CI$DF$mean, lower = CI$DF$lower,
                 upper = CI$DF$upper)

  ## step 2: set up fixed effect
  summaryFixed <- forestDF(object = meta, study = "Fixed effect",
                           n.e = NA, event.e = NA,
                           n.c = NA, event.c = NA,
                           effect = exp(CI$FE$mean),
                           se = meta$se,
                           w.fixed = 100,
                           mean = CI$FE$mean, lower = CI$FE$lower,
                           upper = CI$FE$upper)

  ## step 3: set up random effect
  summaryRandom <- NULL

  ## step 4: substitute the columns
  if (!is.null(sub)) {
    colNames <- names(sub)
    if (!all(colNames %in% colnames(DF))){
      stop("the columns to be substituted do not exist")
    }
    if (!all(sapply(sub, function(sub) length(sub$DF)) == nrow(DF))){
      stop("the length of the substituted column differs from its correponding column")
    }
    DF[, names(sub)] <- sapply(sub,
                               function(sub, DF) {DF[, names(sub)] <- sub$DF},
                               DF = DF)
    summaryFixed[, names(sub)] <- sapply(sub,
                                         function(sub, DF) {
                                           DF[, names(sub)] <- sub$sum
                                         },
                                         DF = DF)
  }

  ## step 4: customization on the main data frame
  ## attach additional columns to the meta object
  if (!is.null(add)) {
    ## attach the additional column to the main data frame
    DF <- cbind(DF, add)
    ## attach the corresponding space to the summary data frame
    addspace <- lapply(add, function(x){x <- ""})
    summaryFixed <- cbind(summaryFixed, addspace)
  }

  ## specify row orders
  if (!is.null(rowOrder)) {
    Order <- order(DF[, rowOrder], ...)
    DF <- DF[Order, ]
  }

  ## step 5: heterogeneity information
  hetero <- c(Q = meta$QE, df = meta$k.yi - 1,
              p = meta$QEp, tau2 = meta$tau2,
              H = NA, H.lower = NA, H.upper = NA,
              I2 = NA, I2.lower = NA, I2.upper = NA,
              Q.CMH = meta$MH,
              conf.level = ifelse(meta$level > 1, meta$level/100, meta$level))

  ## step 6: set up the titles
  Title <- title
  Subtitle <- subtitle

  ## step 7: the wrap up
  output <- list(DF = DF, summaryFixed = summaryFixed,
                 summaryRandom = summaryRandom,
                 hetero = hetero, title = Title, subtitle = Subtitle)

  class(output) <- c("metabinDF", "metaDF")

  output
}

## rma.peto
meta2DF.rma.peto <- function(meta, add = NULL, sub = NULL, rowOrder = NULL,
                             title = NULL, subtitle = NULL, ...) {

  CI <- ciGen(meta)
  if (!any(names(sub) %in% "study")) {
    study.names <- paste("study", 1:length(CI$DF$mean))
  }
  ## step 1: set up main data frame
  DF <- forestDF(object = meta, study = study.names,
                 n.e = meta$ai, event.e = meta$ai + meta$bi,
                 n.c = meta$ci, event.c = meta$ci + meta$di,
                 effect = exp(CI$DF$mean), se = sqrt(meta$vi),
                 w.fixed = weights(meta),
                 mean = CI$DF$mean, lower = CI$DF$lower,
                 upper = CI$DF$upper)

  ## step 2: set up fixed effect
  summaryFixed <- forestDF(object = meta, study = "Fixed effect",
                           n.e = NA, event.e = NA,
                           n.c = NA, event.c = NA,
                           effect = exp(CI$FE$mean),
                           se = meta$se, w.fixed = 100,
                           mean = CI$FE$mean, lower = CI$FE$lower,
                           upper = CI$FE$upper)

  ## step 3: set up random effect
  summaryRandom <- NULL

  ## step 4: substitute the columns
  if (!is.null(sub)) {
    colNames <- names(sub)
    if (!all(colNames %in% colnames(DF))){
      stop("the columns to be substituted do not exist")
    }
    if (!all(sapply(sub, function(sub) length(sub$DF)) == nrow(DF))){
      stop("the length of the substituted column differs from its correponding column")
    }
    DF[, names(sub)] <- sapply(sub,
                               function(sub, DF) {DF[, names(sub)] <- sub$DF},
                               DF = DF)
    summaryFixed[, names(sub)] <- sapply(sub,
                                         function(sub, DF) {
                                           DF[, names(sub)] <- sub$sum
                                         },
                                         DF = DF)
  }

  ## step 4: customization on the main data frame
  ## attach additional columns to the meta object
  if (!is.null(add)) {
    ## attach the additional column to the main data frame
    DF <- cbind(DF, add)
    ## attach the corresponding space to the summary data frame
    addspace <- lapply(add, function(x){x <- ""})
    summaryFixed <- cbind(summaryFixed, addspace)
  }

  ## specify row orders
  if (!is.null(rowOrder)) {
    Order <- order(DF[, rowOrder], ...)
    DF <- DF[Order, ]
  }

  ## step 5: heterogeneity information
  hetero <- c(Q = meta$QE, df = meta$k.yi - 1,
              p = meta$QEp, tau2 = meta$tau2,
              H = NA, H.lower = NA, H.upper = NA,
              I2 = NA, I2.lower = NA, I2.upper = NA, Q.CMH = NA,
              conf.level = ifelse(meta$level > 1, meta$level/100, meta$level))

  ## step 6: set up the titles
  Title <- title
  Subtitle <- subtitle

  ## step 7: the wrap up
  output <- list(DF = DF, summaryFixed = summaryFixed,
                 summaryRandom = summaryRandom,
                 hetero = hetero, title = Title, subtitle = Subtitle)

  class(output) <- c("metabinDF", "metaDF")

  output
}


###===============rma.uni==================###
meta2DF.rma.uni <- function(meta, add = NULL, sub = NULL, rowOrder = NULL,
                            title = NULL, subtitle = NULL, ...) {

  CI <- ciGen(meta)
  if (!any(names(sub) %in% "study")) {
    study.names <- paste("study", 1:length(CI$DF$mean))
  }
  ## step 1: set up main data frame
  DF <- forestDF(object = meta, study = study.names,
                 n.e = meta$ai, event.e = meta$ai + meta$bi,
                 n.c = meta$ci, event.c = meta$ci + meta$di,
                 effect = exp(CI$DF$mean), se = sqrt(meta$vi),
                 w.random = weights(meta),
                 mean = CI$DF$mean, lower = CI$DF$lower,
                 upper = CI$DF$upper)

  ## step 2: set up fixed effect
  summaryRandom <- forestDF(object = meta, study = "Fixed effect",
                            n.e = NA, event.e = NA,
                            n.c = NA, event.c = NA,
                            effect = exp(CI$FE$mean),
                            se = meta$se, w.random = NA,
                            mean = CI$FE$mean, lower = CI$FE$lower,
                            upper = CI$FE$upper)

  ## step 3: set up random effect
  summaryRandom <- NULL

  ## step 4: substitute the columns
  if (!is.null(sub)) {
    colNames <- names(sub)
    if (!all(colNames %in% colnames(DF))){
      stop("the columns to be substituted do not exist")
    }
    if (!all(sapply(sub, function(sub) length(sub$DF)) == nrow(DF))){
      stop("the length of the substituted column differs from its correponding column")
    }
    DF[, names(sub)] <- sapply(sub,
                               function(sub, DF) {DF[, names(sub)] <- sub$DF},
                               DF = DF)
    summaryFixed[, names(sub)] <- sapply(sub,
                                         function(sub, DF) {
                                           DF[, names(sub)] <- sub$sum
                                         },
                                         DF = DF)
  }

  ## step 4: customize the main data frame
  ## attach additional columns to the meta object
  if (!is.null(add)) {
    ## attach the additional column to the main data frame
    DF <- cbind(DF, add)
    # attach the corresponding space to the summary data frame
    addspace <- lapply(add, function(x){x <- ""})
    summaryFixed <- cbind(summaryFixed, addspace)
  }

  ## specify row orders
  if (!is.null(rowOrder)) {
    Order <- order(DF[, rowOrder], ...)
    DF <- DF[Order, ]
  }

  ## step 5: heterogeneity information
  hetero <- c(Q = meta$QE, df = meta$k.yi - 1,
              p = meta$QEp, tau2 = meta$tau2,
              H = NA, H.lower = NA, H.upper = NA,
              I2 = NA, I2.lower = NA, I2.upper = NA, Q.CMH = NA,
              conf.level = ifelse(meta$level > 1, meta$level/100, meta$level))

  ## step 6: set up the titles
  Title <- title
  Subtitle <- subtitle

  ## step 7: the wrap up
  output <- list(DF = DF, summaryFixed = summaryFixed,
                 summaryRandom = summaryRandom,
                 hetero = hetero, title = Title, subtitle = Subtitle)

  class(output) <- c("metabinDF", "metaDF")

  output
}

