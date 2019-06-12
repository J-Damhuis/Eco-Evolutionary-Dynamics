plot1 <- outcomeplot("Delta0.2SymSpat.csv", 10)
plot2 <- outcomeplot("Delta0.05SymSpat.csv", 10)
plot3 <- outcomeplot("Delta0.01SymSpat.csv", 10)
plot4 <- outcomeplot("Delta0.2ParaSpat.csv", 10)
plot5 <- outcomeplot("Delta0.05ParaSpat.csv", 10)
plot6 <- outcomeplot("Delta0.01ParaSpat.csv", 10)
plot7 <- outcomeplot("Delta0.2AlloSpat.csv", 10)
plot8 <- outcomeplot("Delta0.05AlloSpat.csv", 10)
plot9 <- outcomeplot("Delta0.01AlloSpat.csv", 10)

plots <- list(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9)

cols <- c("Delta = 0.2", "Delta = 0.05", "Delta = 0.01")
rows <- c("Sympatry", "Parapatry", "Allopatry")

combine <- rbind(tableGrob(t(cols), theme = ttheme_minimal(), rows = ""),
                 cbind(tableGrob(rows, theme = ttheme_minimal()),
                       arrangeGrob(grobs = plots, nrow = length(rows), ncol = length(cols)),
                       size = "last"), size = "last")
grid.newpage()
grid.draw(combine)
