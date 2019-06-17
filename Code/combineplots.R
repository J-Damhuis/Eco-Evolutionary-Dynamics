plot1 <- outcomeplot("Delta0.2AlloSpecNew.csv", 10)
plot2 <- outcomeplot("Delta0.05AlloSpecNew.csv", 10)
plot3 <- outcomeplot("Delta0.01AlloSpecNew.csv", 10)
plot4 <- outcomeplot("Delta0.2AlloSpatNew.csv", 10)
plot5 <- outcomeplot("Delta0.05AlloSpatNew.csv", 10)
plot6 <- outcomeplot("Delta0.01AlloSpatNew.csv", 10)

plots <- list(plot1, plot2, plot3, plot4, plot5, plot6)

cols <- c("\u03B4 = 0.2", "\u03B4 = 0.05", "\u03B4 = 0.01")
rows <- c("Ecological\nIsolation", "Spatial\nIsolation")

combine <- rbind(tableGrob(t(cols), theme = ttheme_minimal(), rows = ""),
                 cbind(tableGrob(rows, theme = ttheme_minimal()),
                       arrangeGrob(grobs = plots, nrow = length(rows), ncol = length(cols)),
                       size = "last"), size = "last")
grid.newpage()
grid.draw(combine)
