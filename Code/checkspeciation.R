library(ggplot2)
library(reshape)
library(grid)
library(gridExtra)
library(mclust)
heatmap <- "heatmap.csv" #Name of file which has data for heatmap
filename <- "test.csv" #Name of file which has data for evolution and fraction plots
threshold <- 400 #Cut-off point for speciation

checkspeciation <- function(heatmap, filename, threshold) {
  d <- read.csv(heatmap, header = TRUE)
  d2 <- as.data.frame(matrix(ncol = 2, nrow = length(d[,1])))
  d3 <- as.data.frame(matrix(ncol = 2, nrow = length(d[,1])-9))
  vec <- vector()
  for (i in 1:length(d[,1])) {
    x <- t(d[i,-1])
    fit <- Mclust(x, G = 1, model = "V", verbose = FALSE, prior = priorControl())
    fit2 <- Mclust(x, G = 2, model = "V", verbose = FALSE, prior = priorControl())
    d2[i,1] <- d[i,1]
    d2[i,2] <- fit2$loglik - fit$loglik
    
    if (i > 9) {
      average <- 0
      for (j in (i-9):i) {
        average <- average + d2[j,2]
      }
      average <- average / 10
      d3[i-9,1] <- d2[i-9,1]
      d3[i-9,2] <- average
      if (average > threshold) { #If speciation value is high enough to assume 2 species are present
        if (length(vec) == 0) { #If this is first time 2 species are present
          vec[1] <- d[i,1] #Set first value of vector to current generation
          vec[2] <- d[i,1] #Set second value of vector to current generation (this will change to last consecutive generation with 2 species present)
        }
        else if (d[i,1] - vec[length(vec)] > 1) { #If last generation did not have 2 species present
          vec[length(vec) + 1] <- d[i,1] #Set next value of vector to current generation
          vec[length(vec) + 1] <- d[i,1] #Set next value of vector to current generation
        }
        else { #If last generation also had 2 species present
          vec[length(vec)] <- d[i,1] #Change last value of vector to current generation
        }
      }
    }
    print(d[i,1])
  }
  
  d4 <- read.csv(filename, header = TRUE)
  meanx <- d4[length(d4[,2]),2] * d4[length(d4[,3]),3]
  if (d4[length(d4[,5]),5] > 0) {
    meanx <- meanx + d4[length(d4[,6]),6] * d4[length(d4[,5]),5]
  }
  d5 <- as.data.frame(matrix(ncol = 3, nrow = length(d4$Resource.1.2)-9))
  vec2 <- vector()
  if (length(vec) == 0) {
    for (i in 10:length(d4$Resource.1.2)) {
      average1 <- 0
      average2 <- 0
      for (j in (i-9):i) {
        average1 <- average1 + d4$Resource.1.2[j]
        average2 <- average2 + d4$Resource.2.2[j]
      }
      d5[i-9,1] <- d4$Time[i-9]
      d5[i-9,2] <- average1 / 10
      d5[i-9,3] <- average2 / 10
      if (average1 > 9.5 & average2 > 9.5) {
        if (length(vec2) == 0) { #If this is first time the species is a generalist
          vec2[1] <- d4$Time[i] #Set first value of vector to current generation
          vec2[2] <- d4$Time[i] #Set second value of vector to current generation (this will change to last consecutive generation species is a generalist)
        }
        else if (d4$Time[i] - vec2[length(vec2)] > 1) { #If last generation did not have generalist species
          vec2[length(vec2) + 1] <- d4$Time[i] #Set next value of vector to current generation
          vec2[length(vec2) + 1] <- d4$Time[i] #Set next value of vector to current generation
        }
        else { #If last generation also had generalist species
          vec2[length(vec2)] <- d4$Time[i] #Change last value of vector to current generation
        } 
      }
    }
  }
  
  if (length(vec) > 0) {
    cat("2 Species", file = "output.txt")
  }
  else if (length(vec2) == 0) {
    cat("Specialist", file = "output.txt")
  }
  else {
    cat("Generalist", file = "output.txt")
  }
  
  if (length(vec) > 0) {
    cat("2 Species", file = "output2.txt")
  }
  else if (meanx < -0.5) {
    cat("Specialist", file = "output2.txt")
  }
  else {
    cat("Generalist", file = "output2.txt")
  }
  
  output <- list(spec = d2, avgspec = d3, avgstrat = d5)
  #return(output)
}

checkspeciation(heatmap, filename, threshold)
