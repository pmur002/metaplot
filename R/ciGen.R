ciGen <- function(x, ...) {
  UseMethod("ciGen")
}

ciGen.rma.mh <- function(x, ...) {
  if (x$k == 1)
    stop("Stopped because k = 1.")
  alpha <- ifelse(x$level > 1, (100 - x$level)/100, 1 - x$level)
  if (x$measure == "OR" || x$measure == "RR" || x$measure == "IRR") {
    mean <- x$yi
    ci.lb <- x$yi - qnorm(alpha/2, lower.tail = FALSE) * sqrt(x$vi)
    ci.ub <- x$yi + qnorm(alpha/2, lower.tail = FALSE) * sqrt(x$vi)
    estimate <- x$b
    ci.lbEst <- x$b - qnorm(alpha/2, lower.tail = FALSE) * x$se
    ci.ubEst <- x$b + qnorm(alpha/2, lower.tail = FALSE) * x$se

  } else {
    mean <- log(x$yi)
    ci.lb <- log(x$yi - qnorm(alpha/2, lower.tail = FALSE) * sqrt(x$vi))
    ci.ub <- log(x$yi + qnorm(alpha/2, lower.tail = FALSE) * sqrt(x$vi))
    estimate <- log(x$b)
    ci.lbEst <- log(x$b - qnorm(alpha/2, lower.tail = FALSE) * x$se)
    ci.ubEst <- log(x$b + qnorm(alpha/2, lower.tail = FALSE) * x$se)
  }
  list(DF = list(mean = mean, lower = ci.lb, upper = ci.ub),
       FE = list(mean = estimate, lower = ci.lbEst, upper = ci.ubEst))
}

ciGen.rma.peto <- function(x, ...) {
  if (x$k == 1)
    stop("Stopped because k = 1.")
  alpha <- ifelse(x$level > 1, (100 - x$level)/100, 1 - x$level)

    mean <- x$yi
    ci.lb <- x$yi - qnorm(alpha/2, lower.tail = FALSE) * sqrt(x$vi)
    ci.ub <- x$yi + qnorm(alpha/2, lower.tail = FALSE) * sqrt(x$vi)
    estimate <- x$b
    ci.lbEst <- x$b - qnorm(alpha/2, lower.tail = FALSE) * x$se
    ci.ubEst <- x$b + qnorm(alpha/2, lower.tail = FALSE) * x$se

  list(DF = list(mean = mean, lower = ci.lb, upper = ci.ub),
       FE = list(mean = estimate, lower = ci.lbEst, upper = ci.ubEst))
}

ciGen.rma.uni <- function(x, ...) {
  if (x$k == 1)
    stop("Stopped because k = 1.")
  alpha <- ifelse(x$level > 1, (100 - x$level)/100, 1 - x$level)

  mean <- x$yi
  ci.lb <- x$yi - qnorm(alpha/2, lower.tail = FALSE) * sqrt(x$vi)
  ci.ub <- x$yi + qnorm(alpha/2, lower.tail = FALSE) * sqrt(x$vi)
  re <- unclass(predict(x))

  list(DF = list(mean = mean, lower = ci.lb, upper = ci.ub),
       FE = list(mean = re$pred, lower = re$ci.lb, upper = re$ci.ub))
}


