makeplots <- function(filename, heatmap, stepsize = 0.05, everyntimesteps = 1, habitat = NA) {

  d <- read.csv(filename, header = TRUE) #Create data frame for lineplots of csv file

  plot1 <- ggplot(d, aes(x = Time)) + geom_line(aes(y = d[,2], colour = "Resource 1")) +
    geom_line(aes(y = d[,5], colour = "Resource 2")) + ylab("Fraction of population") + ylim(min = 0, max = 1) +
    scale_colour_manual(values = c("Resource 1" = "blue", "Resource 2" = "red")) +
    theme(legend.position = "top", legend.title = element_blank()) #Create lineplot for fractions of population feeding on resources

  png("fraction.png", type = "cairo")
  print(plot1)
  dev.off()

  plot2 <- ggplot(d, aes(x = Time)) + geom_line(aes(y = d[,3], colour = "Resource 1")) +
    geom_line(aes(y = d[,6], colour = "Resource 2")) + ylab("Mean X value") + ylim(min = -1, max = 1) +
    scale_colour_manual(values = c("Resource 1" = "blue", "Resource 2" = "red")) +
    theme(legend.position = "top", legend.title = element_blank()) #Create lineplot for mean X value of population feeding on resources

  png("evolution.png", type = "cairo")
  print(plot2)
  dev.off()

  plot3 <- ggplot(d, aes(x = Time)) + geom_line(aes(y = d[,4], colour = "Resource 1")) +
    geom_line(aes(y = d[,7], colour = "Resource 2")) + ylab("Fraction of resource utilised") + ylim(min = 0, max = 1) +
    scale_colour_manual(values = c("Resource 1" = "blue", "Resource 2" = "red")) +
    theme(legend.position = "top", legend.title = element_blank()) #Create lineplot for mean amount of the resources utilised

  png("efficiency.png", type = "cairo")
  print(plot3)
  dev.off()

  d2 <- read.csv(heatmap, header = TRUE) #Create data frame for heatmap of csv file
  d3 <- as.data.frame(matrix(0, 2 / stepsize, (length(d2[, 1]) - 1) / everyntimesteps + 1)) #Create data frame of 0s with 2/stepsize rows and as many columns as the number of time steps

  for (i in 1:length(d2[, 1])) { #Each timestep
    if ((i - 1) %% everyntimesteps == 0) { #Only use every everyntimesteps time step
      k <- length(d3[, 1]) #Set k to highest step
      for (j in 2:length(d2[1, ])) { #Each individual
        while (d2[i, j] < (k - 1) * stepsize - 1) { #As long as the X value is smaller than lower bound of current step
          k <- k - 1 #Set k to the next step below the current step
        }
        d3[k, (i - 1) / everyntimesteps + 1] <- d3[k, (i - 1) / everyntimesteps + 1] + 1 #Add one to respective step
      }
      print(d2[i,1])
    }
  }

  times <- rep(seq(0, (length(d3) - 1) * (d2[1 + everyntimesteps, 1] - d2[1, 1]), d2[1 + everyntimesteps, 1] - d2[1, 1]), each = 2/stepsize) #Create a list of the time points

  X <- seq(-1 + stepsize / 2, 1 - stepsize / 2, stepsize) #Create a list of the middle between steps
  d3 <- cbind(X, d3) #Add step list to front of data frame

  d3 <- melt(d3, id.vars = "X") #Reshape data frame to make it able to be used by ggplot

  d3$time <- times #Add new column to data frame with values of time points
  colnames(d3)[colnames(d3)=="value"] <- "count" #Rename value column to count

  plot4 <- ggplot(d3, aes(time, X)) + geom_raster(aes(fill = count), interpolate = TRUE) + theme(legend.position = "top") #Create heatmap plot

  png("heatmap.png", type = "cairo")
  print(plot4)
  dev.off()

  threshold <- length(d2[1,]) * 0.2 + 150

  list <- checkspeciation(heatmap = heatmap, filename = filename, method = "completion", calledbyfunction = TRUE)
  d4 <- list$avgspec

  colnames(d4) <- c("Time", "Speciation") #Change column names of speciation data frame

  plot5 <- ggplot(d4, aes(x = Time)) + geom_line(aes(y = Speciation, colour = "Value", linetype = "Value")) +
    geom_line(aes(y = threshold, colour = "Threshold", linetype = "Threshold")) +
    scale_colour_manual("Lines", values = c("Value" = "black", "Threshold" = "red")) +
    scale_linetype_manual("Lines", values = c("Value" = 1, "Threshold" = 2)) + ylim(0, NA) +
    theme(legend.position = "top", legend.title = element_blank()) #Create lineplot for speciation

  png("speciation.png", type = "cairo")
  print(plot5)
  dev.off()

  plots <- arrangeGrob(plot1, plot3, plot2, plot4, plot5, ncol = 2)

  if (!is.na(habitat)) {
    plot6 <- ggplot(d, aes(x = Time)) + geom_line(aes(y = d[,8], colour = "Resource 1")) +
      geom_line(aes(y = d[,11], colour = "Resource 2")) + ylab("Fraction of population") + ylim(min = 0, max = 1) +
      scale_colour_manual(values = c("Resource 1" = "blue", "Resource 2" = "red")) +
      theme(legend.position = "top", legend.title = element_blank()) #Create lineplot for fractions of population feeding on resources

    png("fraction2.png", type = "cairo")
    print(plot6)
    dev.off()

    plot7 <- ggplot(d, aes(x = Time)) + geom_line(aes(y = d[,9], colour = "Resource 1")) +
      geom_line(aes(y = d[,12], colour = "Resource 2")) + ylab("Mean X value") + ylim(min = -1, max = 1) +
      scale_colour_manual(values = c("Resource 1" = "blue", "Resource 2" = "red")) +
      theme(legend.position = "top", legend.title = element_blank()) #Create lineplot for mean X value of population feeding on resources

    png("evolution2.png", type = "cairo")
    print(plot7)
    dev.off()

    plot8 <- ggplot(d, aes(x = Time)) + geom_line(aes(y = d[,10], colour = "Resource 1")) +
      geom_line(aes(y = d[,13], colour = "Resource 2")) + ylab("Fraction of resource utilised") + ylim(min = 0, max = 1) +
      scale_colour_manual(values = c("Resource 1" = "blue", "Resource 2" = "red")) +
      theme(legend.position = "top", legend.title = element_blank()) #Create lineplot for mean amount of the resources utilised

    png("efficiency2.png", type = "cairo")
    print(plot8)
    dev.off()

    d5 <- checkdifferentiation(heatmap, habitat)

    plot9 <- ggplot(d5, aes(x = d5[,1], y = d5[,2])) + geom_line() + ylab("Degree of differentiation") + ylim(min = 0, max = 1) + xlab("Time")

    png("differentiation.png", type = "cairo")
    print(plot9)
    dev.off()

    plots <- arrangeGrob(plot1, plot6, plot4, plot2, plot7, plot5, plot3, plot8, plot9, nrow = 3)
  }

  png("plots.png", type = "cairo")
  grid.draw(plots)
  dev.off()

  graphics.off()
}
