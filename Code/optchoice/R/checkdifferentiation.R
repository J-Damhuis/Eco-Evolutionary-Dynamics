checkdifferentiation <- function(heatmap, habitats) {
  d <- read.csv(heatmap, header = TRUE)
  d2 <- read.csv(habitats, header = TRUE)
  d3 <- as.data.frame(matrix(nrow = length(d2[,1]), ncol = 2))

  for (i in 1:length(d[,1])) {
    counts <- as.data.frame(matrix(0, nrow = 2, ncol = 2))
    x <- t(d[i,-1])
    y <- t(d2[i,-1])
    ecotypes <- Mclust(x, G = 2, model = "V", verbose = FALSE, prior = priorControl())
    for (j in 1:length(ecotypes$classification)) {
      counts[y[j] + 1, ecotypes$classification[j]] <- counts[y[j] + 1, ecotypes$classification[j]] + 1
    }

    num <- abs(counts[1,1] * counts[2,2] - counts[1,2] * counts[2,1])
    den <- sqrt((counts[1,1] + counts[1,2]) * (counts[2,1] + counts[2,2]) * (counts[1,1] + counts[2,1]) * (counts[1,2] + counts[2,2]))
    si <- num / den

    d3[i,1] <- d[i,1]
    d3[i,2] <- si

    print(d[i,1])
  }

  return(d3)
}
