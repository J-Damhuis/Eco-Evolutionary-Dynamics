outcomeplot <- function(filename, nsim, title = NA) {
  d <- read.csv(filename, header = FALSE)
  d2 <- as.data.frame(matrix(0, ncol = 6, nrow = length(d[,1])/nsim))

  colnames(d2) <- c("beta", "s", "Specialist", "Generalist", "2 Species", "Colour")

  for (i in 1:length(d[,1])) {
    if (i %% nsim == 1) {
      d2[(i %/% nsim) + 1,1] <- d[i,1]
      d2[(i %/% nsim) + 1,2] <- d[i,2]
    }
    if (d[i,3] == "Specialist") {
      d2[((i-1) %/% nsim) + 1,3] <- d2[((i-1) %/% nsim) + 1, 3] + 1
    }
    else if (d[i,3] == "Generalist") {
      d2[((i-1) %/% nsim) + 1,4] <- d2[((i-1) %/% nsim) + 1, 4] + 1
    }
    else if (d[i,3] == "2 Species") {
      d2[((i-1) %/% nsim) + 1,5] <- d2[((i-1) %/% nsim) + 1, 5] + 1
    }
  }

  colspec <- col2rgb("red")/255
  colgen <- col2rgb("blue")/255
  coltwo <- col2rgb("green")/255

  for (i in 1:length(d2[,1])) {
    newcolour <- colspec*d2[i,3] + colgen*d2[i,4] + coltwo*d2[i,5]
    d2[i,6] <- rgb(red = newcolour[1], green = newcolour[2], blue = newcolour[3], maxColorValue = nsim)
  }

  vec <- vector()
  for (i in 1:length(d2[,6])) {
    if (length(vec) == 0) {
      vec[1] <- d2[i,6]
    }
    else {
      test <- FALSE
      for (j in 1:length(vec)) {
        if (vec[j] == d2[i,6]) {
          test <- TRUE
        }
      }
      if (test == FALSE) {
        vec[length(vec)+1] <- d2[i,6]
      }
    }
  }

  d3 <- as.data.frame(matrix(ncol = 1, nrow = length(d2[,6])))
  for (i in 1:length(d2[,6])) {
    for (j in 1:length(vec)) {
      if (d2[i,6] == vec[j]) {
        d3[i,1] <- j
      }
    }
  }
  d2 <- cbind(d2, d3)

  plot <- ggplot(d2, aes(beta, s)) + geom_raster(aes(fill = factor(V1)), interpolate = FALSE) + theme(legend.position = "none") +
    scale_fill_manual(values = vec)

  if (!is.na(title)) {
    plot <- plot + ggtitle(title)
  }

  return(plot)
}
