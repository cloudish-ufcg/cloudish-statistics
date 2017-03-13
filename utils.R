source("data_util.R")
source("process_task_data_util.R")
source("process_utilization_data_util.R")

input.dir.base <- "/local/cloudish"
simulation.date <- "03022017"
datacenter.type <- "blade"
preemption.policy <- "AvalAware-TTV"
input.file.checkpoint <- "vms-12583-hosts-86400.0"
     
tasks <- GetTaskData(input.dir.base, datacenter.type, simulation.date, preemption.policy)
checkpoints <- GetCheckpointData(input.dir.base, input.file.checkpoint)
datacenter <- GetDatacenterData(input.dir.base, datacenter.type, simulation.date, preemption.policy)
usage <- GetUsageData(input.dir.base, datacenter.type, simulation.date, preemption.policy)

vm.availability <- GenerateVmAvailability(tasks, 1)
vm.mean.availability <- CalcMeanVmAvailability(tasks)
migrations.per.hour.summary <- SummariseMigrationsPerHour(tasks)
migrations.summary <- SummariseMigrations(tasks, NULL, 1)
migrations.per.second.summary <- SummariseMigrationsPerSecond(tasks, NULL, 1)
slo.fulfillment <- GetSLOFulfillment(tasks, 0, 0.5)
preemption.backfilling.summary <- SummarisePreemptionsAndBackfilling(tasks, NULL, 1)

usage.interval.summary <- SummariseUsageInterval(usage, 1, interval.size = 300000000)
host.usage.analysis <- ProcessHostUsageData(usage, interval.size = 300000000, scenario)
utilization.summary <- SummariseUtilization(usage, host.capacity)     
host.usage <- CollectHostUsage(usage, initial.interval = 0, interval.size = 300000000, scenario)
usage.by.interval.summary <- SummariseUsageByInterval(usage)
          
# #write.csv(hostUsageSummary, paste("data/host_usage_summary_", scenario, "_intervalsize_", interval.size, ".csv", sep=""))
# WriteData(hostUsageSummary, paste("data/host_usage_summary_", scenario, "_intervalsize_", interval.size, ".csv", sep=""))

# #write.csv(hostUsageSummary, paste("data/host_usage_summary_", scenario, "_intervalsize_", interval.size, ".csv", sep=""))
# WriteData(hostUsageSummary, paste("data/host_usages_", scenario, "_by_intervalsize_", interval.size, ".csv", sep=""))
# 
# 
# #write.csv(hostUsageSummary, paste("data/host_usage_summary_", scenario, "_intervalsize_", interval.size, ".csv", sep=""))
# WriteData(hostUsageSummary, paste("data/host_usages_oldscheme_", scenario, "_", interval.size, ".csv", sep=""))
# 
# 
# #write.csv(hostUsageSummary, paste("data/host_usage_summary_", scenario, "_intervalsize_", interval.size, ".csv", sep=""))
# WriteData(hostUsageSummary, paste("data/host_usages_", scenario, "_by_intervalsize_", interval.size, ".csv", sep=""))
# 
# # WriteData(hostUsageSummary, paste("data/host_usages_", scenario, "_by_intervalsize_", interval.size, ".csv", sep=""))
