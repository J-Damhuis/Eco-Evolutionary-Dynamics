checkspeciation <- function(heatmap, method, output = NA, calledbyfunction = FALSE) {
  d <- read_csv(heatmap)
  d <- as.data.frame(d)
  threshold <- length(d[1,]) * 0.2 + 150

  meanx <- d[length(d[,1]),-1] %>% t() %>% mean()

  if (method == "completion" & !calledbyfunction) {
    d <- d[length(d[,1]),]
  }
  d2 <- apply(d[,-1], 1, function(x) {
    fit <- Mclust(x, G = 1, model = "V", verbose = FALSE, prior = priorControl())
    fit2 <- Mclust(x, G = 2, model = "V", verbose = FALSE, prior = priorControl())
    return(fit2$loglik - fit$loglik)
  })
  if (method == "initiation") {

    vec <- sapply(d2, function(x) {
      if (x > threshold) {
        return(1)
      }
      else {
        return(0)
      }
    }) %>% sum()

    if (is.na(output)) {
      if (vec > 10) {
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
      if (d2[length(d2)] > threshold) {
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
