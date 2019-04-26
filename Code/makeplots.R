library(ggplot2)
library(reshape)
library(grid)
library(gridExtra)
library(mclust)
heatmap <- "heatmap.csv" #Name of file which has data for heatmap
filename <- "test.csv" #Name of file which has data for evolution and fraction plots
stepsize <- 0.05 #Height of one band on heatmap (smaller = higher resolution)
everyntimesteps <- 10 #Width of one band on heatmap (smaller = higher resolution)
threshold <- 400 #Cut-off point for speciation

makeplots <- function(filename, heatmap, threshold, stepsize = 0.05, everyntimesteps = 1) {
  
  d <- read.csv(filename, header = TRUE) #Create data frame for lineplots of csv file
  
  plot1 <- ggplot(d, aes(x = Time)) + geom_line(aes(y = Resource.1, colour = "Resource 1")) + 
    geom_line(aes(y = Resource.2, colour = "Resource 2")) + ylab("Fraction of population") + ylim(min = 0, max = 1) +
    scale_colour_manual(values = c("Resource 1" = "blue", "Resource 2" = "red")) + 
    theme(legend.position = "top", legend.title = element_blank()) #Create lineplot for fractions of population feeding on resources
  
  png("fraction.png", type = "cairo") 
  print(plot1) 
  dev.off() 
  
  plot2 <- ggplot(d, aes(x = Time)) + geom_line(aes(y = Resource.1.1, colour = "Resource 1")) + 
    geom_line(aes(y = Resource.2.1, colour = "Resource 2")) + ylab("Mean X value") + ylim(min = -1, max = 1) +
    scale_colour_manual(values = c("Resource 1" = "blue", "Resource 2" = "red")) + 
    theme(legend.position = "top", legend.title = element_blank()) #Create lineplot for mean X value of population feeding on resources
  
  png("evolution.png", type = "cairo") 
  print(plot2) 
  dev.off()
  
  plot3 <- ggplot(d, aes(x = Time)) + geom_line(aes(y = Resource.1.2, colour = "Resource 1")) + 
    geom_line(aes(y = Resource.2.2, colour = "Resource 2")) + ylab("Fraction of resource utilised") + ylim(min = 0, max = 1) +
    scale_colour_manual(values = c("Resource 1" = "blue", "Resource 2" = "red")) + 
    theme(legend.position = "top", legend.title = element_blank()) #Create lineplot for mean amount of the resources utilised
  
  png("efficiency.png", type = "cairo") 
  print(plot3) 
  dev.off()
  
  d <- read.csv(heatmap, header = TRUE) #Create data frame for heatmap of csv file
  d2 <- as.data.frame(matrix(0, 2 / stepsize, (length(d[, 1]) - 1) / everyntimesteps + 1)) #Create data frame of 0s with 2/stepsize rows and as many columns as the number of time steps
  
  for (i in 1:length(d[, 1])) { #Each timestep
    if ((i - 1) %% everyntimesteps == 0) { #Only use every everyntimesteps time step
      k <- length(d2[, 1]) #Set k to highest step
      for (j in 2:length(d[1, ])) { #Each individual
        while (d[i, j] < (k - 1) * stepsize - 1) { #As long as the X value is smaller than lower bound of current step
          k <- k - 1 #Set k to the next step below the current step
        }
        d2[k, (i - 1) / everyntimesteps + 1] <- d2[k, (i - 1) / everyntimesteps + 1] + 1 #Add one to respective step
      }
      print(d[i,1])
    }
  }
  
  times <- rep(seq(0, (length(d2) - 1) * everyntimesteps, d[1 + everyntimesteps, 1] - d[1, 1]), each = 2/stepsize) #Create a list of the time points
  
  X <- seq(-1 + stepsize / 2, 1 - stepsize / 2, stepsize) #Create a list of the middle between steps
  d2 <- cbind(X, d2) #Add step list to front of data frame
  
  d2 <- melt(d2, id.vars = "X") #Reshape data frame to make it able to be used by ggplot
  
  d2$time <- times #Add new column to data frame with values of time points
  colnames(d2)[colnames(d2)=="value"] <- "count" #Rename value column to count
  
  plot4 <- ggplot(d2, aes(time, X)) + geom_raster(aes(fill = count), interpolate = TRUE) + theme(legend.position = "top") #Create heatmap plot
  
  png("heatmap.png", type = "cairo") 
  print(plot4) 
  dev.off() 
  
  list <- checkspeciation(heatmap, filename, threshold)
  d2 <- list$avgspec
  
  colnames(d2) <- c("Time", "Speciation") #Change column names of speciation data frame
  
  plot5 <- ggplot(d2, aes(x = Time)) + geom_line(aes(y = Speciation, colour = "Value", linetype = "Value")) + 
    geom_line(aes(y = threshold, colour = "Threshold", linetype = "Threshold")) +
    scale_colour_manual("Lines", values = c("Value" = "black", "Threshold" = "red")) + 
    scale_linetype_manual("Lines", values = c("Value" = 1, "Threshold" = 2)) + 
    theme(legend.position = "top", legend.title = element_blank()) #Create lineplot for speciation
  
  png("speciation.png", type = "cairo")
  print(plot5) 
  dev.off() 
  
  plots <- arrangeGrob(plot1, plot3, plot2, plot4, plot5, ncol = 2)
  
  png("plots.png", type = "cairo") 
  grid.draw(plots)
  dev.off() 
  
  graphics.off()
}

makeplots(filename, heatmap, threshold, stepsize, everyntimesteps) #Execute function
