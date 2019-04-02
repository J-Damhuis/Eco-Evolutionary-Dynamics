library(ggplot2)
library(reshape)
library(gridExtra)
heatmap <- "heatmap.csv" #Name of file which has data for heatmap and speciation plot
filename <- "test.csv" #Name of file which has data for evolution and fraction plots
stepsize <- 0.05 #Height of one band on heatmap (smaller = higher resolution)
everyntimesteps <- 100 #Width of one band on heatmap (smaller = higher resolution)
threshold <- 26 #Cut-off point for speciation

makeplots <- function(filename, heatmap, threshold, stepsize = 0.05, everyntimesteps = 1) {
  
  d <- read.csv(filename, header = TRUE) #Create data frame for lineplots of csv file
  
  plot1 <- ggplot(d, aes(x = Time)) + geom_line(aes(y = Resource.1, colour = "Resource 1")) + 
    geom_line(aes(y = Resource.2, colour = "Resource 2")) + ylab("Fraction of population") + ylim(min = 0, max = 1) +
    scale_colour_manual(values = c("Resource 1" = "blue", "Resource 2" = "red")) + theme(legend.position = "top") #Create lineplot for fractions of population feeding on resources
  
  png("fraction.png", type = "cairo") #Create png file named fraction
  print(plot1) #Print plot1 onto fraction.png
  dev.off() #Close png file
  
  plot2 <- ggplot(d, aes(x = Time)) + geom_line(aes(y = Resource.1.1, colour = "Resource 1")) + 
    geom_line(aes(y = Resource.2.1, colour = "Resource 2")) + ylab("Mean X value") + ylim(min = -1, max = 1) +
    scale_colour_manual(values = c("Resource 1" = "blue", "Resource 2" = "red")) + theme(legend.position = "top") #Create lineplot for mean X value of population feeding on resources
  
  png("evolution.png", type = "cairo") #Create png file named evolution
  print(plot2) #Print plot2 onto evolution.png
  dev.off() #Close png file
  
  d <- read.csv(heatmap, header = TRUE) #Create data frame for heatmap of csv file
  d2 <- as.data.frame(matrix(0, 2 / stepsize, (length(d[, 1]) - 1) / everyntimesteps + 1)) #Create data frame of 0s with 2/stepsize rows and as many columns as the number of time steps
  
  for(i in 1:length(d[, 1])) { #Each timestep
    if((i - 1) %% everyntimesteps == 0) { #Only use every everyntimesteps time step
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
  
  plot3 <- ggplot(d2, aes(time, X)) + geom_raster(aes(fill = count), interpolate = TRUE) + theme(legend.position = "top") #Create heatmap plot
  
  png("heatmap.png", type = "cairo") #Create png file named heatmap
  print(plot3) #Print plot3 onto heatmap.png
  dev.off() #Close png file
  
  d4 <- as.data.frame(matrix(ncol = 2, nrow = length(d[,1]) - 1)) #Create data frame to keep track of speciation values
  vec <- vector() #Create vector to keep track of generations when there are 2 species
  for (i in 2:length(d[,1])) { #For every generation except generation 0
    d3 <- d[i,] #Write X values of current generation to new data frame
    d3 <- t(d3) #Transpose data frame
    d3 <- d3[-1,] #Remove time step from data frame
    clusters <- kmeans(d3, 2) #Calculate kmeans
    d4[i-1,1] <- i-1 #Set first column of speciation data frame to current generation
    d4[i-1,2] <- clusters$betweenss - clusters$tot.withinss #Set second column of speciation data frame to speciation value
    if (clusters$betweenss - clusters$tot.withinss > threshold) { #If speciation value is high enough to assume 2 species are present
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
  
  for (i in 1:(length(vec)/2)) { #For half of the vector length
    cat(vec[i*2-1], vec[i*2], sep = ":") #Print the first and last consecutive generation with 2 species present
    cat("\n") #Go to new line
  }
  
  colnames(d4) <- c("time", "speciation") #Change column names of speciation data frame
  
  plot4 <- ggplot(d4, aes(time, speciation)) + geom_line() #Create lineplot for speciation
  
  png("speciation.png", type = "cairo") #Create png file named speciation
  print(plot4) #Print plot4 onto speciation.png
  dev.off() #Close png file
  
  png("plots.png", type = "cairo") #Create png file named plots
  print(grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)) #Print all 4 plots in 2x2 grid onto plots.png
  dev.off() #Close png file
}

makeplots(filename, heatmap, threshold, stepsize, everyntimesteps) #Execute function
