checkspeciation <- function(heatmap, filename, method, output = NA, threshold = 400, calledbyfunction = FALSE) {
  if (calledbyfunction) {
    d <- read.csv(heatmap, header = TRUE)
    d2 <- as.data.frame(matrix(ncol = 2, nrow = length(d[,1])))
    d3 <- as.data.frame(matrix(ncol = 2, nrow = length(d[,1])-9))
    vec <- vector()
    for (i in 1:(length(d[,1]))) {
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
        if (method == "initiation") {
          if (average > threshold) { #If speciation value is high enough to assume 2 species are present
            if (length(vec) == 0) { #If this is first time 2 species are present
              vec[1] <- d[i,1] #Set first value of vector to current generation
              vec[2] <- d[i,1] #Set second value of vector to current generation (this will change to last consecutive generation with 2 species present)
            }
            else if (i == length(d[,1])) { #If it is the last generation of the simulation
              if (d[i,1] - vec[length(vec)] == 1 & d[i,1] - vec[length(vec)-1] < 50) { #If speciation hasn't gone on for long enough
                if (length(vec) == 2) { #If this is the only speciation event
                  vec <- vector() #Clear vector
                }
                else { #If this is not the only speciation event
                  vec <- vec[1:(length(vec)-2)] #Only clear starting point of this speciation event
                }
              }
            }
            else if (d[i,1] - vec[length(vec)] > 1) { #If last generation did not have 2 species present
              vec[length(vec) + 1] <- d[i,1] #Set next value of vector to current generation
              vec[length(vec) + 1] <- d[i,1] #Set next value of vector to current generation
            }
            else { #If last generation also had 2 species present
              vec[length(vec)] <- d[i,1] #Change last value of vector to current generation
            }
          }
          else {
            if (length(vec) > 0) { #If speciation has occured
              if (d[i,1] - vec[length(vec)] == 1) { #If speciation occured last generation
                if (vec[length(vec)] - vec[length(vec)-1] < 50) { #If speciation didn't exist for long enough
                  if (length(vec) == 2) { #If this is the only speciation event
                    vec <- vector() #Clear vector
                  }
                  else { #If there are other speciation events
                    vec <- vec[1:(length(vec)-2)] #Only clear this speciation event
                  }
                }
              }
            }
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

    if (method == "initiation") {
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
        if (fit2$loglik - fit$loglik > threshold) {
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
        if (fit2$loglik - fit$loglik > threshold) {
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

    output <- list(spec = d2, avgspec = d3)
    return(output)
  }
  else {
    if (method == "initiation") {
      d <- read.csv(heatmap, header = TRUE)
      d2 <- as.data.frame(matrix(ncol = 2, nrow = length(d[,1])))
      d3 <- as.data.frame(matrix(ncol = 2, nrow = length(d[,1])-9))
      vec <- vector()
      for (i in 1:(length(d[,1]))) {
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
            else if (i == length(d[,1])) { #If it is the last generation of the simulation
              if (d[i,1] - vec[length(vec)] == 1 & d[i,1] - vec[length(vec)-1] < 50) { #If speciation hasn't gone on for long enough
                if (length(vec) == 2) { #If this is the only speciation event
                  vec <- vector() #Clear vector
                }
                else { #If this is not the only speciation event
                  vec <- vec[1:(length(vec)-2)] #Only clear starting point of this speciation event
                }
              }
            }
            else if (d[i,1] - vec[length(vec)] > 1) { #If last generation did not have 2 species present
              vec[length(vec) + 1] <- d[i,1] #Set next value of vector to current generation
              vec[length(vec) + 1] <- d[i,1] #Set next value of vector to current generation
            }
            else { #If last generation also had 2 species present
              vec[length(vec)] <- d[i,1] #Change last value of vector to current generation
            }
          }
          else {
            if (length(vec) > 0) { #If speciation has occured
              if (d[i,1] - vec[length(vec)] == 1) { #If speciation occured last generation
                if (vec[length(vec)] - vec[length(vec)-1] < 50) { #If speciation didn't exist for long enough
                  if (length(vec) == 2) { #If this is the only speciation event
                    vec <- vector() #Clear vector
                  }
                  else { #If there are other speciation events
                    vec <- vec[1:(length(vec)-2)] #Only clear this speciation event
                  }
                }
              }
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
      d <- read.csv(heatmap, header = TRUE)
      i <- length(d[,1])
      x <- t(d[i,-1])
      fit <- Mclust(x, G = 1, model = "V", verbose = FALSE, prior = priorControl())
      fit2 <- Mclust(x, G = 2, model = "V", verbose = FALSE, prior = priorControl())
      d4 <- read.csv(filename, header = TRUE)
      meanx <- d4[length(d4[,2]),2] * d4[length(d4[,3]),3]
      if (d4[length(d4[,5]),5] > 0) {
        meanx <- meanx + d4[length(d4[,6]),6] * d4[length(d4[,5]),5]
      }

      if (is.na(output)) {
        if (fit2$loglik - fit$loglik > threshold) {
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
        if (fit2$loglik - fit$loglik > threshold) {
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
  }
}
