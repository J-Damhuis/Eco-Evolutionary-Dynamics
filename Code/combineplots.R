plot1 <- outcomeplot("Delta0.5ShortOld.csv", 10)
plot2 <- outcomeplot("Delta0.2ShortOld.csv", 10)
plot3 <- outcomeplot("Delta0.05ShortOld.csv", 10)
plot4 <- outcomeplot("Delta0.5ShortNew.csv", 10)
plot5 <- outcomeplot("Delta0.2ShortNew.csv", 10)
plot6 <- outcomeplot("Delta0.05ShortNew.csv", 10)

plots <- list(plot1, plot2, plot3, plot4, plot5, plot6)

cols <- c("Delta = 0.5", "Delta = 0.2", "Delta = 0.05")
rows <- c("Trait value", "Resource found")

combine <- rbind(tableGrob(t(cols), theme = ttheme_minimal(), rows = ""),
                 cbind(tableGrob(rows, theme = ttheme_minimal()),
                       arrangeGrob(grobs = plots, nrow = length(rows), ncol = length(cols)),
                       size = "last"), size = "last")
grid.newpage()
grid.draw(combine)
