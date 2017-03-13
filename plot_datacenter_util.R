library(ggplot2)

PlotAllocatedResources <- function(datacenter.info, output.dir, plot.width = 450, plot.height = 450) {
     p <- ggplot(datacenter.info, aes(x = time, group = 1)) + geom_line(aes(y = resourcesRunningP2/(resourcesRunningP2 + resourcesRunningP1 + resourcesRunningP0), color = "free", group = 1)) + 
          geom_line(aes(y = resourcesRunningP1/(resourcesRunningP2 + resourcesRunningP1 + resourcesRunningP0), color = "batch", group = 1)) + 
          geom_line(aes(y = resourcesRunningP0/(resourcesRunningP2 + resourcesRunningP1 + resourcesRunningP0), color = "prod", group = 1)) + 
          xlab("Time (s)") + ylab("Allocation") +  ggtitle("Allocated Resources for each class") + scale_y_continuous(limits = c(0.0, 1.0))
     
     fig.filename <- paste(output.dir, "/", "allocated_resources.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}

PlotAllocatedVms <- function(datacenter.info, output.dir, plot.width = 450 , plot.height = 450) {
     p <- ggplot(datacenter.info, aes(x = time, group = 1)) + geom_line(aes(y = vmsRunningP2/vmsRunning, color = "free", group = 1)) + 
          geom_line(aes(y = vmsRunningP1/vmsRunning, color = "batch", group = 1)) + geom_line(aes(y = vmsRunningP0/vmsRunning, color = "prod", group = 1)) + 
          xlab("Time (s)") + ylab("Allocation") +  ggtitle("Allocated VMs for each class") + scale_y_continuous(limits = c(0.0, 1.0))
     
     fig.filename <- paste(output.dir, "/", "allocated_vms.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}

PlotWaitingVms <- function(datacenter.info, limit, output.dir, plot.width = 450 , plot.height = 450) {
     p <- ggplot(datacenter.info, aes(x = time, group = 1)) + geom_line(aes(y = vmsForSchedulingP2, color = "free", group = 1)) + 
          geom_line(aes(y = vmsForSchedulingP1, color = "batch", group = 1)) + geom_line(aes(y = vmsForSchedulingP0, color = "prod", group = 1)) + 
          xlab("Time (s)") + ylab("Waiting Queue") +  ggtitle("VMs waiting for each class") + scale_y_continuous(limits = c(0.0, limit))
     
     fig.filename <- paste(output.dir, "/", "waiting_vms.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}

PlotWaitingResources <- function(datacenter.info, limit, output.dir, plot.width = 450 , plot.height = 450) {
     p <- ggplot(datacenter.info, aes(x = time, group = 1)) + geom_line(aes(y = resourcesWaitingP2, color = "free", group = 1)) + 
          geom_line(aes(y = resourcesWaitingP1, color = "batch", group = 1)) + geom_line(aes(y = resourcesWaitingP0, color = "prod", group = 1)) + 
          xlab("Time (s)") + ylab("Waiting Queue") +  ggtitle("Required resources waiting to be allocated for each class") + 
          scale_y_continuous(limits = c(0.0, limit))
     
     fig.filename <- paste(output.dir, "/", "waiting_resources.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}

PlotProportionOfAllocatedVms <- function(datacenter.info, output.dir, plot.width = 450 , plot.height = 450) {
     p <- ggplot(datacenter.info, aes(x = time, group = 1)) + geom_line(aes(y = vmsRunningP2/(vmsRunningP2 + vmsForSchedulingP2), color = "free", group = 1)) + 
          geom_line(aes(y = vmsRunningP1/(vmsRunningP1 + vmsForSchedulingP1), color = "batch", group = 1)) + 
          geom_line(aes(y = vmsRunningP0/(vmsRunningP0 + vmsForSchedulingP0), color = "prod", group = 1)) + 
          xlab("Time (s)") + ylab("% of allocation") +  ggtitle("% of allocated VMs for each class") + scale_y_continuous(limits = c(0.0, 1.0))
     
     fig.filename <- paste(output.dir, "/", "proportion_of_allocated_vms.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}

PlotProportionOfAllocatedResources <- function(datacenter.info, output.dir, plot.width = 450 , plot.height = 450 ) {
     p <- ggplot(datacenter.info, aes(x = time, group = 1)) + geom_line(aes(y = resourcesRunningP2/(resourcesRunningP2 + resourcesWaitingP2), color = "free", group = 1)) + 
          geom_line(aes(y = resourcesRunningP1/(resourcesRunningP1 + resourcesWaitingP1), color = "batch", group = 1)) + 
          geom_line(aes(y = resourcesRunningP0/(resourcesRunningP0 + resourcesWaitingP0), color = "prod", group = 1)) + 
          xlab("Time (s)") + ylab("% of allocation") +  ggtitle("% of allocated Resources for each class") + scale_y_continuous(limits = c(0.0, 1.0))
     
     fig.filename <- paste(output.dir, "/", "proportion_of_allocated_resources.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}