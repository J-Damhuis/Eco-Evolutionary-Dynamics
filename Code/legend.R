library(Ternary)

TernaryPlot(atip="Specialist", btip="2 Species", ctip="Generalist",
            point='up', axis.labels = FALSE, axis.tick = FALSE)

for (i in 0:10) {
  for (j in 0:(10-i)) {
    triangle <- matrix(c(
      (i*10)/11, (j*10)/11, ((11-i-j)*10)/11,
      ((i+1)*10)/11, (j*10)/11, ((10-i-j)*10)/11,
      (i*10)/11, ((j+1)*10)/11, ((10-i-j)*10)/11
    ), ncol = 3, byrow = TRUE)
    TernaryPolygon(triangle, col=rgb((i*10)/1.1,(j*10)/1.1,((11-i-j)*10)/1.1, maxColorValue = 100))
  }
}

for (i in 0:9) {
  for (j in 0:(9-i)) {
    triangle <- matrix(c(
      ((i+1)*10)/11, (j*10)/11, ((10-i-j)*10)/11,
      (i*10)/11, ((j+1)*10)/11, ((10-i-j)*10)/11,
      ((i+1)*10)/11, ((j+1)*10)/11, ((9-i-j)*10)/11
    ), ncol = 3, byrow = TRUE)
    TernaryPolygon(triangle, col=rgb(((i+1)*10)/1.1, ((j+1)*10)/1.1, ((10-i-j)*10)/1.1, maxColorValue = 100))
  }
}
