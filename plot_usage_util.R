library(ggplot2)

PlotHostUsage <- function(usage, datacenter.type, hosts = 1, output.dir, plot.width = 450 , plot.height = 450) {
     p <- ggplot(usage, aes(x = interval.index, group = 1)) + geom_line(aes(y = as.numeric(availableMips) * hosts, color = "available", group = 1)) + 
          geom_line(aes(y = as.numeric(p0Usage) * hosts, color = "p0", group = 1)) + geom_line(aes(y = as.numeric(p1Usage) * hosts, color = "p1", group = 1)) + 
          geom_line(aes(y = as.numeric(p2Usage) * hosts, color = "p2", group = 1)) + xlab("Index") + ylab("Usage") +  ggtitle(paste("Host Usage", datacenter.type," limited"))
     
     fig.filename <- paste(output.dir, "/", "host_usage.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}

PlotHostUtilizationAndAvailable <- function(usage, datacenter.type, host.capacity, output.dir, plot.width = 450 , plot.height = 450) {
     p <- ggplot(usage, aes(x = interval.index, group = 1)) + geom_line(aes(y = as.numeric(availableMips) / host.capacity, color = "available", group = 1)) +
          geom_line(aes(y = (as.numeric(p0Usage) + as.numeric(p1Usage) + as.numeric(p2Usage)) / host.capacity, color = "utilization", group = 1)) + 
          xlab("Time") + ylab("Utilization") +  ggtitle(paste("Host Utilization ", datacenter.type,"-limited", sep = ""))
     
     fig.filename <- paste(output.dir, "/", "host_util_and_available.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}

PlotHostUtilization <- function(usage, datacenter.type, host.capacity, output.dir, plot.width = 450 , plot.height = 450) {
     p <- ggplot(usage, aes(x = interval.index, group = 1)) + 
          geom_line(aes(y = (as.numeric(utilization.mean)) / host.capacity, group = 1)) + 
          xlab("Hour") + ylab("Allocation") + ggtitle(paste("Host Average Allocation for ", datacenter.type, sep = "")) + scale_y_continuous(limits = c(0.75, 1.0))
     
     fig.filename <- paste(output.dir, "/", "host_util.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}

PlotHostUtilizationByPriority <- function(usage, datacenter.type, host.capacity, output.dir, plot.width = 450 , plot.height = 450) {
     p <- ggplot(usage, aes(x = interval.index, color = as.factor(priority))) + geom_line(aes(y = as.numeric(utilization.mean)/host.capacity)) +
          xlab("Hour") + ylab("Allocation")  + ggtitle(paste("Host Average Allocation by Service Class for ", datacenter.type, sep = "")) 
     
     fig.filename <- paste(output.dir, "/", "host_util_by_priority.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}