checkdifferentiation <- function(heatmap, habitats, output = NA, calledbyfunction = FALSE) {
  d <- read_csv(heatmap)
  d2 <- read_csv(habitats)

  if (calledbyfunction) {
    d3 <- apply(d, 1, function(x) {
      x <- x[-1]
      eco <- Mclust(x, G = 2, model = "V", verbose = FALSE, prior = priorControl())$classification
      return(eco)
    }) %>% as.data.frame()

    d4 <- mapply(function(x,y) {
      z <- cbind(y,x)
      counts <- table(z[,1],z[,2]) %>% as.data.frame()
      num <- abs(counts[1,3] * counts[4,3] - counts[3,3] * counts[2,3])
      f <- (counts[1,3] + counts[3,3]) * (counts[2,3] + counts[4,3]) %>% as.numeric()
      g <- (counts[1,3] + counts[2,3]) * (counts[3,3] + counts[4,3]) %>% as.numeric()
      den <- sqrt(f * g)
      return(num / den)
    }, d3, as.data.frame(t(d2[,-1]))) %>% as.data.frame()

    d4 <- cbind(d[,1], d4)

    if (is.na(output)) {
      cat(d4[length(d4[,1]), 2])
    }
    else {
      cat(d4[length(d4[,1]), 2], file = output)
    }

    return(d4)
  }
  else {
    d <- as.data.frame(d)
    i <- length(d[,1])
    x <- t(d[i,-1])
    y <- t(d2[i,-1])
    ecotypes <- Mclust(x, G = 2, model = "V", verbose = FALSE, prior = priorControl())$classification
    counts <- table(y, ecotypes) %>% as.data.frame()

    num <- abs(counts[1,3] * counts[4,3] - counts[3,3] * counts[2,3])
    f <- (counts[1,3] + counts[3,3]) * (counts[2,3] + counts[4,3]) %>% as.numeric()
    g <- (counts[1,3] + counts[2,3]) * (counts[3,3] + counts[4,3]) %>% as.numeric()
    den <- sqrt(f * g)
    si <- num / den

    if (is.na(output)) {
      cat(si)
    }
    else {
      cat(si, file = output)
    }
  }
}
