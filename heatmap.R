library(ggplot2)
library(reshape)
library(gridExtra)
heatmap <- "heatmap.csv"
filename <- "test.csv"
stepsize <- 0.05
everyntimesteps <- 10

makeplots <- function(filename, heatmap, stepsize = 0.05, everyntimesteps = 1) {
  
  d <- read.csv(filename, header = TRUE) #Create data frame for lineplots of csv file
  
  plot1 <- ggplot(d, aes(x = Time)) + geom_line(aes(y = Resource.1, colour = "Resource 1")) + 
    geom_line(aes(y = Resource.2, colour = "Resource 2")) + ylab("Fraction of population") + ylim(min = 0, max = 1) +
    scale_colour_manual(values = c("Resource 1" = "blue", "Resource 2" = "red")) #Create lineplot for fractions of population feeding on resources
  
  plot2 <- ggplot(d, aes(x = Time)) + geom_line(aes(y = Resource.1.1, colour = "Resource 1")) + 
    geom_line(aes(y = Resource.2.1, colour = "Resource 2")) + ylab("Mean X value") + ylim(min = -1, max = 1) +
    scale_colour_manual(values = c("Resource 1" = "blue", "Resource 2" = "red")) #Create lineplot for mean X value of population feeding on resources
  
  d <- read.csv(heatmap, header = TRUE) #Create data frame for heatmap of csv file
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
  
  plot3 <- ggplot(d2, aes(time, X)) + geom_raster(aes(fill = count), interpolate = TRUE) #Create heatmap plot
  
  grid.arrange(plot1, plot2, plot3, ncol = 2) #Show all plots
}

makeplots(filename, heatmap, stepsize, everyntimesteps)
