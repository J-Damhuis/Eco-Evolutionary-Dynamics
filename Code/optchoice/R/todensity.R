todensity <- function(d, everyntimesteps = 1, binsize = 0.05) {
  dt <- seq(1, nrow(d), everyntimesteps)

  d <- d[dt,]

  bins <- seq(-1, 1, binsize)

  densities <- rep(0, length(bins)-1)

  d2 <- apply(d[,-1], 1, function(x) {

    d <- x %>% .bincode(bins) %>% table()
    densities[as.numeric(names(d))] <- d
    return (densities)

  }) %>% as.data.frame()

  X <- seq(-1 + binsize / 2, 1 - binsize / 2, binsize) #Create a list of the middle between steps
  d2 <- cbind(X, d2) #Add step list to front of data frame

  times <- rep(d$Time, each = 2 / binsize)

  d3 <- d2 %>% melt(id.vars = "X") %>% mutate(time = times) %>% rename(count = value)

  return(d3)
}
