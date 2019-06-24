checkspeciation <- function(heatmap, method, output = NA, calledbyfunction = FALSE) {
  d <- read_csv(heatmap)
  d <- as.data.frame(d)
  threshold <- length(d[1,]) * 0.2 + 150

  meanx <- 0
  for (i in 2:length(d[length(d[,1]),])) {
    meanx <- meanx + d[length(d[,1]),i]
  }
  meanx <- meanx / (length(d[length(d[,1]),]) - 1)

  if (method == "completion" & !calledbyfunction) {
    d <- d[length(d[,1]),]
  }
  d2 <- apply(d[,-1], 1, function(x) {
    fit <- Mclust(x, G = 1, model = "V", verbose = FALSE, prior = priorControl())
    fit2 <- Mclust(x, G = 2, model = "V", verbose = FALSE, prior = priorControl())
    return(fit2$loglik - fit$loglik)
  })
  if (method == "initiation") {
    #Insert some function to deal with vec
    if (is.na(output)) {
      if (length(vec) > 0) {
        cat("2 Species")
      }
      else if (meanx < -0.5) {
        cat("Specialist")
      }
      else {
        cat("Generalist")
      }
    }
    else {
      if (length(vec) > 0) {
        cat("2 Species", file = output)
      }
      else if (meanx < -0.5) {
        cat("Specialist", file = output)
      }
      else {
        cat("Generalist", file = output)
      }
    }
  }
  else if (method == "completion") {
    if (is.na(output)) {
      if (d2[length(d2)] > threshold) {
        cat("2 Species")
      }
      else if (meanx < -0.5) {
        cat("Specialist")
      }
      else {
        cat("Generalist")
      }
    }
    else {
      if (rename(count = value) > threshold) {
        cat("2 Species", file = output)
      }
      else if (meanx < -0.5) {
        cat("Specialist", file = output)
      }
      else {
        cat("Generalist", file = output)
      }
    }
  }
  else {
    cat("Method for checking speciation is not valid")
  }

  if (calledbyfunction) {
    d2 <- cbind(as.data.frame(d[,1]), d2) %>% rename(Speciation = d2) %>% rename(Time = "d[, 1]")
    return(d2)
  }
}
