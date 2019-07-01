plot1 <- outcomeplot("Delta0.2SymSpatNew.csv", 10)
plot2 <- outcomeplot("Delta0.05SymSpatNew.csv", 10)
plot3 <- outcomeplot("Delta0.01SymSpatNew.csv", 10)
plot4 <- outcomeplot("Delta0.2ParaSpatNew.csv", 10)
plot5 <- outcomeplot("Delta0.05ParaSpatNew.csv", 10)
plot6 <- outcomeplot("Delta0.01ParaSpatNew.csv", 10)
plot7 <- outcomeplot("Delta0.2AlloSpatNew.csv", 10)
plot8 <- outcomeplot("Delta0.05AlloSpatNew.csv", 10)
plot9 <- outcomeplot("Delta0.01AlloSpatNew.csv", 10)

plots <- list(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9)

cols <- c("\u03B4 = 0.2", "\u03B4 = 0.05", "\u03B4 = 0.01")
rows <- c("Sympatry", "Parapatry", "Allopatry")

combine <- rbind(tableGrob(t(cols), theme = ttheme_minimal(), rows = ""),
                 cbind(tableGrob(rows, theme = ttheme_minimal()),
                       arrangeGrob(grobs = plots, nrow = length(rows), ncol = length(cols)),
                       size = "last"), size = "last")
grid.newpage()
grid.draw(combine)

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(plot1)

grid.newpage()
grid.draw(legend)
