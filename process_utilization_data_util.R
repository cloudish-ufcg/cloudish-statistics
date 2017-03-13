# Default interval.size is 5 min
SummariseUsageInterval <- function(usage, interval.index, interval.size = 300000000) {
     intervalSummary <- usage %>% filter(between(time, interval.index * interval.size, (interval.index + 1) * interval.size))
     return(intervalSummary)
}

# Default interval.size is 5 min
ProcessHostUsageData <- function(usage, interval.size = 300000000, scenario){
     
     hostUsageSummary <- data.frame()
     
     # the number of intervals depends on the size of interval
     max.interval = floor(2506179281020 / interval.size)
     
     for (index in 0:max.interval) {
          intervalUsageEntries <- SummariseUsageInterval(usage, index, interval.size)
          
          #write.csv(intervalUsageEntries, paste("data/interval_", index, "_", scenario, "_intervalsize_", interval.size, ".csv", sep=""))
          #WriteData(intervalUsageEntries, paste("data/interval_", index, "_", scenario, "_intervalsize_", interval.size, ".csv", sep=""))
          
          intervalSummary <- as.data.frame(intervalUsageEntries %>% 
                                                summarise(interval.index=index, time=max(time), p0Usage=mean(p0Usage), p1Usage=mean(p1Usage), p2Usage=mean(p2Usage), p0Vms=mean(p0Vms), p1Vms=mean(p1Vms), p2Vms=mean(p2Vms), availableMips=mean(availableMips)))
          
          hostUsageSummary <- rbind(hostUsageSummary, intervalSummary)
     }
     
     #write.csv(hostUsageSummary, paste("data/host_usage_summary_", scenario, "_intervalsize_", interval.size, ".csv", sep=""))
     WriteData(hostUsageSummary, paste("data/host_usage_summary_", scenario, "_intervalsize_", interval.size, ".csv", sep=""))
     
     return(hostUsageSummary)
}

# Default interval.size is 5 min
ProcessHostUsageDataNewScheme <- function(usage, interval.size = 300000000, max.interested.time = 2506179281020, scenario){
     hostUsageSummary <- data.frame()
     
     # the number of intervals depends on the size of interval
     max.interval = floor(max.interested.time / interval.size)
     
     for (index in 0:max.interval) {
          intervalUsageEntries <- SummariseUsageInterval(usage, index, interval.size)
          
          intervalSummary <- as.data.frame(intervalUsageEntries %>% 
                                                summarise(interval.index = index, time = max(time), p0Usage = mean(p0Usage), p1Usage = mean(p1Usage), 
                                                          p2Usage = mean(p2Usage), p0Vms = mean(p0Vms), p1Vms = mean(p1Vms), p2Vms = mean(p2Vms), availableMips = mean(availableMips)))
          
          hostUsageSummary <- rbind(hostUsageSummary, intervalSummary)
     }
     return(hostUsageSummary)
}

SummariseUtilization <- function(usage, host.capacity) {
     data.result <- usage %>% mutate(total.usage = p0Usage + p1Usage + p2Usage) %>% 
          mutate(utilization = total.usage/host.capacity, av = availableMips/host.capacity) %>% 
          summarise(utilization.mean = mean(utilization), av.mean = mean(av))
     return(data.result)
}

CollectHostUsage <- function(usage, initial.interval = 0, interval.size = 300000000, scenario) {
     hostUsageSummary <- data.frame()
     
     # the number of intervals depends on the size of interval
     max.interval = floor(2506179281020 / interval.size)
     
     #max.interval = 3
     previousUsage <- data.frame()
     
     for (index in initial.interval:max.interval) {
          
          intervalEntries <- usage %>% filter(between(time, (index) * interval.size, (index + 1) * interval.size)) %>% collect() 
          intervalEntries <- rbind(previousUsage, intervalEntries)
          
          hostUsage <- as.data.frame(intervalEntries %>% group_by(host_id) %>% filter(time == max(time)) %>% mutate(interval.index = index))
          
          previousUsage <- hostUsage %>% select(host_id, time, p0Usage, p1Usage, p2Usage, p0Vms, p1Vms, p2Vms, availableMips)
     }
     return(hostUsageSummary)
}

CollectHostUsageV2 <- function(usage, initial.interval = 0, interval.size = 300000000, max.interested.time = 2506179281020, 
                               scenario, from.db = T, collecting.intervals = -1) {
     hostUsageSummary <- data.frame()
     
     # the number of intervals depends on the size of interval
     max.interval = floor(max.interested.time / interval.size)
     
     #max.interval = 3
     previousUsage <- data.frame()
     collectedUsage <- data.frame()
     
     next.collecting <- max.interval + 1
     
     if (from.db) {
          if (collecting.intervals == -1) {
               print(paste("collecting from DB until interval", max.interval + 1))
               collectedUsage <- usage %>% filter(between(time, 0, (max.interval + 1) * interval.size)) %>% collect(n = Inf)  
          } else {
               print(paste("collecting from DB until interval", collecting.intervals))
               collectedUsage <- usage %>% filter(between(time, 0, (collecting.intervals + 1) * interval.size)) %>% collect(n = Inf)  
               next.collecting <- collecting.intervals + 1
          }
     }
     
     for (index in initial.interval:max.interval) {
          print(paste("index=", index))
          if (index == next.collecting) {
               print("collecting more usage from database")
               collectedUsage <- usage %>% filter(between(time, next.collecting, (next.collecting + collecting.intervals) * interval.size)) %>% collect(n = Inf)  
               next.collecting <- next.collecting + collecting.intervals
          }
          
          intervalEntries <- collectedUsage %>% filter(between(time, (index) * interval.size, (index + 1) * interval.size)) %>% collect() 
          intervalEntries <- rbind(previousUsage, intervalEntries)
          
          hostUsage <- as.data.frame(intervalEntries %>% group_by(host_id) %>% filter(time == max(time)) %>% mutate(interval.index = index))
          
          previousUsage <- hostUsage %>% select(host_id, time, p0Usage, p1Usage, p2Usage, p0Vms, p1Vms, p2Vms, availableMips)
          
          usageMean <- hostUsage %>% mutate(usage.total = (p0Usage + p1Usage + p2Usage)) %>% group_by(interval.index) %>% 
               summarise(utilization.mean = mean(usage.total), availability.mean = mean(availableMips))
          
          hostUsageSummary <- rbind(hostUsageSummary, usageMean)
     }
     
     return(hostUsageSummary)
}

CollectHostUsageNewScheme <- function(usage, initial.interval = 0, interval.size = 300000000, max.interested.time = 2506179281020, 
                                      scenario, from.db = T, collecting.intervals = -1) {
     hostUsageSummary <- data.frame()
     
     # the number of intervals depends on the size of interval
     max.interval = floor(max.interested.time / interval.size)
     previousUsage <- data.frame()
     collectedUsage <- data.frame()
     
     next.collecting <- max.interval + 1
     
     if (from.db) {
          if (collecting.intervals == -1) {
               print(paste("collecting from DB until interval", max.interval + 1))
               collectedUsage <- usage %>% filter(between(time, 0, (max.interval + 1) * interval.size)) %>% collect(n = Inf)  
          } else {
               print(paste("collecting from DB until interval", collecting.intervals))
               collectedUsage <- usage %>% filter(between(time, 0, (collecting.intervals + 1) * interval.size)) %>% collect(n = Inf)  
               next.collecting <- collecting.intervals + 1
          }
     }
     
     for (index in initial.interval:max.interval) {
          print(paste("index=", index))
          
          if (index == next.collecting) {
               print("collecting more usage from database")
               collectedUsage <- usage %>% filter(between(time, next.collecting, (next.collecting + collecting.intervals + 1) * interval.size)) %>% collect(n = Inf)  
               next.collecting <- next.collecting + collecting.intervals + 1
          }
          
          intervalEntries <- collectedUsage %>% filter(between(time, (index) * interval.size, (index + 1) * interval.size)) %>% collect(n = Inf) 
          intervalEntries <- rbind(previousUsage, intervalEntries)
          
          hostUsage <- as.data.frame(intervalEntries %>% group_by(hostId) %>% filter(time == max(time)) %>% mutate(interval.index = index))
          
          previous <- hostUsage %>% select(hostId, time, usage, vms, priority, availableMips)
          
          usageMean <- hostUsage %>% group_by(interval.index, hostId) %>% summarise(usage.total = sum(usage), available.total = mean(availableMips)) %>% 
               group_by(interval.index) %>% summarise(utilization.mean = mean(usage.total), availability.mean = mean(available.total))
          
          hostUsageSummary <- rbind(hostUsageSummary, usageMean)
     }
     return(hostUsageSummary)
}

CollectHostUsageNewSchemeByPriority <- function(usage, initial.interval = 0, interval.size = 300000000, max.interested.time = 2506179281020, 
                                                scenario, from.db = T, collecting.intervals = -1) {
     hostUsageSummary <- data.frame()
     
     # the number of intervals depends on the size of interval
     max.interval = floor(max.interested.time / interval.size)
     previousUsage <- data.frame()
     collectedUsage <- data.frame()
     
     next.collecting <- max.interval + 1
     
     if (from.db) {
          if (collecting.intervals == -1) {
               print(paste("collecting from DB until interval", max.interval + 1))
               collectedUsage <- usage %>% filter(between(time, 0, (max.interval + 1) * interval.size)) %>% collect(n = Inf)  
          } else {
               print(paste("collecting from DB until interval", collecting.intervals))
               collectedUsage <- usage %>% filter(between(time, 0, (collecting.intervals + 1) * interval.size)) %>% collect(n = Inf)  
               next.collecting <- collecting.intervals + 1
          }
     }
     
     for (index in initial.interval:max.interval) {
          print(paste("index=", index))
          if (index == next.collecting) {
               print("collecting more usage from database")
               collectedUsage <- usage %>% filter(between(time, next.collecting, (next.collecting + collecting.intervals + 1) * interval.size)) %>% collect(n = Inf)  
               next.collecting <- next.collecting + collecting.intervals + 1
          }
          
          #collecting interval entries
          intervalEntries <- collectedUsage %>% filter(between(time, (index) * interval.size, (index + 1) * interval.size)) %>% collect(n = Inf) 
          intervalEntries <- rbind(previousUsage, intervalEntries)
          
          hostUsage <- as.data.frame(intervalEntries %>% group_by(hostId) %>% filter(time == max(time)) %>% mutate(interval.index = index))
          
          usageMean <- as.data.frame(hostUsage %>% group_by(interval.index, hostId, priority) %>% summarise(usage.total = sum(usage), available.total = mean(availableMips)) %>% 
                                          group_by(interval.index, priority) %>% summarise(utilization.mean = mean(usage.total), availability.mean = mean(available.total)))
          
          hostUsageSummary <- rbind(hostUsageSummary, usageMean)
     }
     
     return(hostUsageSummary)
}

SummariseUsageByInterval <- function(usage) {
     data.result <- usage %>% group_by(interval.index) %>% 
          summarise(p0Usage = mean(p0Usage), p1Usage = mean(p1Usage), p2Usage = mean(p2Usage),
                    p0Vms = mean(p0Vms), p1Vms = mean(p1Vms), p2Vms = mean(p2Vms), availableMips = mean(availableMips))
     return(data.result)
}


