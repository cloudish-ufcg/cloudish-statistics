PlotResult <- function(plot, fig.filename, plot.width, plot.height, overwrite.plot) {
     if (!file.exists(fig.filename) | overwrite.plot) {
          png(filename = fig.filename, width = plot.width, height = plot.height)
          print(plot)
          dev.off()
     }
}