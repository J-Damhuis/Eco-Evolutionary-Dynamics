library(ggplot2)
library(reshape)
library(grid)
library(gridExtra)
heatmap <- "heatmap.csv" #Name of file which has data for heatmap
filename <- "test.csv" #Name of file which has data for evolution and fraction plots
gaps <- "gaps.csv" #Name of file which has data for speciation plot
stepsize <- 0.05 #Height of one band on heatmap (smaller = higher resolution)
everyntimesteps <- 1 #Width of one band on heatmap (smaller = higher resolution)
threshold <- 1 #Cut-off point for speciation

makeplots <- function(filename, heatmap, gaps, threshold, stepsize = 0.05, everyntimesteps = 1) {
  
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
    theme(legend.position = "top", legend.title = element_blank()) #Create lineplot for mean X value of population feeding on resources
  
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
      print(i)
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
  
  d4 <- read.csv(gaps, header = TRUE)
  
  vec <- vector() #Create vector to keep track of generations when there are 2 species
  
  d5 <- as.data.frame(matrix(ncol = 2, nrow = length(d4[,1])))
  for (i in 1:length(d4[,1])) {
    d5[i,1] <- i-1
    d5[i,2] <- d4[i,2] / d4[i,3]
    
    if (d5[i,2] > threshold) { #If speciation value is high enough to assume 2 species are present
      if (length(vec) == 0) { #If this is first time 2 species are present
        vec[1] <- i #Set first value of vector to current generation
        vec[2] <- i #Set second value of vector to current generation (this will change to last consecutive generation with 2 species present)
      }
      else if (i - vec[length(vec)] > 1) { #If last generation did not have 2 species present
        vec[length(vec) + 1] <- i #Set next value of vector to current generation
        vec[length(vec) + 1] <- i #Set next value of vector to current generation
      }
      else { #If last generation also had 2 species present
        vec[length(vec)] <- i #Change last value of vector to current generation
      }
    }
  }
  
  cat("\n") #Go to new line
  if (length(vec) > 1) { #If speciation occured
    cat("There were two species during the following generations:\n") #Print that text
    for (i in 1:(length(vec)/2)) { #For half of the vector length
      cat(vec[i*2-1], vec[i*2], sep = ":") #Print the first and last consecutive generation with 2 species present
      cat("\n") #Go to new line
    }
  }
  else { #If no speciation occured
    cat("No speciation occured\n") #Print that text
  }
  cat("\n") #Go to new line
  
  colnames(d5) <- c("time", "speciation") #Change column names of speciation data frame
  
  plot5 <- ggplot(d5, aes(x = time)) + geom_line(aes(y = speciation, colour = "Value", linetype = "Value")) + 
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

makeplots(filename, heatmap, gaps, threshold, stepsize, everyntimesteps) #Execute function
