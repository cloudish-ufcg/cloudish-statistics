library(dplyr)
library(Rmisc)
library(ggplot2)

PlotAvailabilityXRuntime <- function(tasks, datacenter.type, sloTarget, output.dir, plot.width = 450 , plot.height = 450 ) {
     p <- ggplot(tasks, aes(x = runtime)) + geom_point(aes(y = availability)) + xlab("Runtime (s)") + ylab("Availability") +  
          ggtitle(paste("Availability of ", datacenter.type," VMs", sep = "")) + geom_line(aes(y = sloTarget)) + scale_y_continuous(limits = c(0.0, 1.0))
     
     fig.filename <- paste(output.dir, "/", "availability_vs_runtime.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}

PlotAvailabilityXSubmittime <- function(tasks, datacenter.type, sloTarget, output.dir, plot.width = 450 , plot.height = 450 ) {
     p <- ggplot(tasks, aes(x = submitTime)) + geom_point(aes(y = availability)) + xlab("Submit time (s)") + ylab("Availability") +  
          ggtitle(paste("Availability of ", datacenter.type," VMs", sep = "")) + geom_line(aes(y = sloTarget)) + scale_y_continuous(limits = c(0.0, 1.0))
     
     fig.filename <- paste(output.dir, "/", "availability_vs_submittime.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}

PlotAvailabilityXCPUReq <- function(tasks, datacenter.type, sloTarget, output.dir, plot.width = 450 , plot.height = 450 ) {
     p <- ggplot(tasks, aes(x = as.factor(cpuReqNor))) + geom_boxplot(aes(y = availability)) + xlab("CPU Requirement") + ylab("Availability") +  
          ggtitle(paste("Availability of ", datacenter.type," VMs", sep = "")) + geom_line(aes(y = sloTarget)) + scale_y_continuous(limits = c(0.0, 1.0))
     
     fig.filename <- paste(output.dir, "/", "availability_vs_CPUreq.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}

PlotConfidenceIntervalsAvailabilityXCPUReq <- function(tasks, datacenter.type, output.dir, plot.width = 450 , plot.height = 450 ) {
     
     CI <- tasks %>% group_by(cpuReqNor) %>% dplyr::summarise(upper = CI(availability, ci = 0.95)[1], mean = CI(availability, ci = 0.95)[2], lower = CI(availability, ci = 0.95)[3]) 
     p <-  ggplot(CI, aes(y = mean, x = cpuReqNor)) + geom_point() + geom_errorbar(aes(ymax = upper, ymin = lower)) + xlab("CPU Requirement") + ylab("CI of Availabilities") +  
          ggtitle(paste("Confidence Intervals of Availabilities of ", datacenter.type," VMs", sep = "")) + scale_y_continuous(limits = c(0.0, 1.0))
     
     fig.filename <- paste(output.dir, "/", "ci_availability_vs_CPUreq.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}

PlotBoxPlot <- function(tasks, interested.variable, output.dir, plot.width = 450 , plot.height = 450) {
     p <- boxplot(p0Usage~host_id,data = tasks, main = as.character(interested.variable), xlab = "host", ylab = as.character(interested.variable))
     
     fig.filename <- paste(output.dir, "/", "boxplot-example.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}

PlotBoxPlotMigrationsPerHour <- function(tasks, output.dir, plot.width = 450 , plot.height = 450) {
     tasks <- tasks %>% mutate(migrations.per.hour = migrations/(runtime/(60 * 60)))
     p <- boxplot(migrations.per.hour~priority, tasks, main = "Migrations per hour", xlab = "Service class", ylab = "Hourly migrations")
     
     fig.filename <- paste(output.dir, "/", "boxplot_migration_per_hour.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}

PlotBoxPlotRuntimePerHour <- function(tasks, output.dir, plot.width = 450 , plot.height = 450) {
     tasks <- tasks %>% mutate(runtime.hour = runtime/(60 * 60))
     p <- boxplot(runtime.hour~priority, tasks, main = "VM Runtimes", xlab = "Service class", ylab = "runtime (h)")
     
     fig.filename <- paste(output.dir, "/", "boxplot_runtime_per_hour.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}

PlotBoxPlotVmAvailability <- function(tasks, output.dir, plot.width = 450 , plot.height = 450) {
     p <- boxplot(availability~priority, tasks, main = "VM Availabilities", xlab = "Service class", ylab = "availability")
     
     fig.filename <- paste(output.dir, "/", "boxplot_vm_availability.png", sep = "")
     png(filename = fig.filename, width = plot.width, height = plot.height)
     print(p)
     dev.off()
}