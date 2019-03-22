library(ggplot2)
library(reshape)
filename <- "heatmap.csv"
stepsize <- 0.05
everyntimesteps <- 10

makeheatmap <- function(filename, stepsize = 0.05, everyntimesteps = 1) {
  
  d <- read.csv(filename, header = TRUE) #Create data frame of csv file
  d2 <- as.data.frame(matrix(0, 2 / stepsize, (length(d[, 1]) - 1) / everyntimesteps + 1)) #Create data frame of 0s with 2/stepsize rows and as many columns as the number of time steps
  
  for(i in 1:length(d[, 1])) { #Each timestep
    if((i - 1) %% everyntimesteps == 0) { #Οnly use every everyntimesteps time step
      for(k in 1:length(d2[, 1])) { #Each stepsize
        for(j in 2:length(d[1, ])) { #Each individual
          if(d[i, j] <= k * stepsize - 1 & d[i, j] >= (k - 1) * stepsize - 1) { #If the X value is within current step
            d2[k, (i - 1) / everyntimesteps + 1] <- d2[k, (i - 1) / everyntimesteps + 1] + 1 #Add one to respective step
          }
        }
      }
      print(i)
    }
  }
  
  times <- rep(seq(0.5 * everyntimesteps, (length(d2) - 1) * everyntimesteps + 0.5 * everyntimesteps, d[1 + everyntimesteps, 1] - d[1, 1]), each = 2/stepsize) #Create a list of the middle between time points
  
  X <- seq(-1 + stepsize / 2, 1 - stepsize / 2, stepsize) #Create a list of the middle between steps
  d2 <- cbind(X, d2) #Add step list to front of data frame
  
  d2 <- melt(d2, id.vars = "X") #Reshape data frame to make it able to be used by ggplot
  
  d2$time <- times #Add new column to data frame with values of time points
  colnames(d2)[colnames(d2)=="value"] <- "count" #Rename value column to count
  
  ggplot(d2, aes(time, X)) + geom_raster(aes(fill = count), interpolate = TRUE) #Create heat map
}

makeheatmap(filename, stepsize, everyntimesteps)
