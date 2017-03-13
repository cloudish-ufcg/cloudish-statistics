input.dir.base <- "/local/cloudish"
simulation.date <- "03022017"
datacenter.type <- "blade"
preemption.policy <- "AvalAware-TTV"
input.file.checkpoint <- "vms-12583-hosts-86400.0"
output.dir <- "/local/cloudish/data"
     
host.capacity.list <- list("blade" = 0.52)

source("data_util.R")

output.dir.path <- CreateOutputDir(output.dir, simulation.date, datacenter.type, preemption.policy)

tasks <- GetTaskData(input.dir.base, datacenter.type, simulation.date, preemption.policy)
checkpoints <- GetCheckpointData(input.dir.base, input.file.checkpoint)
datacenter <- GetDatacenterData(input.dir.base, datacenter.type, simulation.date, preemption.policy)
usage <- GetUsageData(input.dir.base, datacenter.type, simulation.date, preemption.policy)

# Pre-processing task data ----------------------------------------------------------------------------------------
source("process_task_data_util.R")

vm.availability <- GenerateVmAvailability(tasks, 1)
vm.mean.availability <- CalcMeanVmAvailability(tasks)
migrations.per.hour.summary <- SummariseMigrationsPerHour(tasks)
migrations.summary <- SummariseMigrations(tasks, NULL, 1)
migrations.per.second.summary <- SummariseMigrationsPerSecond(tasks, NULL, 1)
slo.fulfillment <- GetSLOFulfillment(tasks, 0, 0.5)
preemption.backfilling.summary <- SummarisePreemptionsAndBackfilling(tasks, NULL, 1)

WriteData(vm.availability, output.dir.path[["data"]], "vm_availability.dat")
WriteData(vm.mean.availability, output.dir.path[["data"]], "vm_mean_availability.dat")
WriteData(migrations.per.hour.summary, output.dir.path[["data"]], "migrations_per_hour_summary.dat")
WriteData(migrations.per.second.summary, output.dir.path[["data"]], "migrations_per_second_summary.dat")
WriteData(migrations.summary, output.dir.path[["data"]], "migrations_summary.dat")
WriteData(slo.fulfillment, output.dir.path[["data"]], "slo_fulfillment.dat")
WriteData(preemption.backfilling.summary, output.dir.path[["data"]], "preemption_backfilling_summary.dat")

# Pre-processing usage data ---------------------------------------------------------------------------------------
source("process_utilization_data_util.R")

host.usage <- CollectHostUsage(usage, 0, 3600, max.interested.time = 3599)
host.usage.priority <- CollectHostUsageByPriority(usage, 0, 3600, max.interested.time = 3599)

WriteData(host.usage, output.dir.path[["data"]], "host_usage_by_priority.dat")
WriteData(host.usage.priority, output.dir.path[["data"]], "host_usage_by_priority.dat")

# Plot resoults ---------------------------------------------------------------------------------------------------
# source("plot_datacenter_util.R")
# source("plot_task_util.R")
source("plot_usage_util.R")

PlotHostUtilization(host.usage, datacenter.type, host.capacity.list[[datacenter.type]], output.dir.path[["img"]], 450 , 450)
PlotHostUtilizationByPriority(host.usage.priority, datacenter.type, host.capacity.list[[datacenter.type]], output.dir.path[["img"]], plot.width = 450 , plot.height = 450)
     