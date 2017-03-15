# Processing datacenter analysis ----------------------------------------------------------------------------------
DatacenterAnalysis <- function(generate.plot, generate.data, overwrite.data, params) {
     print("Nothing to do") 
     # datacenter <- GetDatacenterData(input.dir.base, datacenter.type, simulation.date, preemption.policy)
}
# Processing task analysis ----------------------------------------------------------------------------------------
TaskAnalysis <- function(generate.plot, generate.data, overwrite.data, params) {
     print("Nothing to do")
     # tasks <- GetTaskData(input.dir.base, datacenter.type, simulation.date, preemption.policy)
     # checkpoints <- GetCheckpointData(input.dir.base, input.file.checkpoint)
     # vm.availability <- GenerateVmAvailability(tasks, 1)
     # vm.mean.availability <- CalcMeanVmAvailability(tasks)
     # migrations.per.hour.summary <- SummariseMigrationsPerHour(tasks)
     # migrations.summary <- SummariseMigrations(tasks, NULL, 1)
     # migrations.per.second.summary <- SummariseMigrationsPerSecond(tasks, NULL, 1)
     # slo.fulfillment <- GetSLOFulfillment(tasks, 0, 0.5)
     # preemption.backfilling.summary <- SummarisePreemptionsAndBackfilling(tasks, NULL, 1)
     # 
     # WriteData(vm.availability, output.dir.data, "vm_availability.dat")
     # WriteData(vm.mean.availability, output.dir.data, "vm_mean_availability.dat")
     # WriteData(migrations.per.hour.summary, output.dir.data, "migrations_per_hour_summary.dat")
     # WriteData(migrations.per.second.summary, output.dir.data, "migrations_per_second_summary.dat")
     # WriteData(migrations.summary, output.dir.data, "migrations_summary.dat")
     # WriteData(slo.fulfillment, output.dir.data, "slo_fulfillment.dat")
     # WriteData(preemption.backfilling.summary, output.dir.data, "preemption_backfilling_summary.dat")
}

# Processing usage analysis ---------------------------------------------------------------------------------------
UsageAnalysis <- function(generate.plot, generate.data, overwrite.data, params) {
     
     # load input parameters
     input.dir.base <- params$input_dir
     simulation.date <- params$simulation_timestamp
     datacenter.type <- params$datacenter_type
     preemption.policy <- params$preemption_policy
     input.file.checkpoint <- params$checkpoint_file
     output.dir <- params$output_dir
     host.capacity.list <- list("blade" = 0.52, "drawer" = 8.42, "rack" = 33.51)
     host.capacity <- host.capacity.list[[datacenter.type]]

     output.dir.path <- CreateOutputDir(output.dir, simulation.date, datacenter.type, preemption.policy)
     output.dir.data <- output.dir.path[["data"]]

     host.usage <- data.frame()
     host.usage.priority <- data.frame()
     
     if (generate.data) {
          usage <- GetUsageData(input.dir.base, datacenter.type, simulation.date, preemption.policy)
          host.usage <- CollectHostUsage(usage, 0, 3600, max.interested.time = 3599)
          host.usage.priority <- CollectHostUsageByPriority(usage, 0, 3600, max.interested.time = 3599)
          
          WriteData(host.usage, output.dir.data, "host_usage.dat", overwrite.data = overwrite.data)
          WriteData(host.usage.priority, output.dir.data, "host_usage_by_priority.dat", overwrite.data = overwrite.data)
     } else {
          host.usage <- ReadData(output.dir.data, "host_usage.dat")
          host.usage.priority <- ReadData(output.dir.data, "host_usage_by_priority.dat")
     }
     
     if (generate.plot) {
          if (is.null(host.usage) | is.null(host.usage.priority)) {
               warning("Missing data plot")
          } else {
               output.dir.img <- output.dir.path[["img"]]
               PlotHostUtilization(host.usage, datacenter.type, host.capacity, output.dir.img, 450 , 450)
               PlotHostUtilizationByPriority(host.usage.priority, datacenter.type, host.capacity, output.dir.img, 450, 450)
          }
     }
}


# Importing source files ------------------------------------------------------------------------------------------
ImportSourceFiles <- function() {
     source("data_util.R")
     source("plot_util.R")
     source("process_utilization_data_util.R")
     source("process_task_data_util.R")
     source("plot_datacenter_util.R")
     source("plot_task_util.R")
     source("plot_usage_util.R")
}

library(argparser)

opts <- arg_parser('Options for generate results')
opts <- add_argument(opts, "--input-dir", 
                     help = "Path to directory of simulation results.",
                     short = "-I")

opts <- add_argument(opts, "--output-dir", 
                     help = "Path to directory of output analysis.",
                     short = "-O")

opts <- add_argument(opts, "--checkpoint-file", 
                     help = "Filename of the checkpoint file.",
                     short = "-C")

opts <- add_argument(opts, "--simulation-timestamp",
                     help = "Date em que a simulação foi gerada",
                     short = "-T")

opts <- add_argument(opts, "--datacenter-type",
                     help = "Simulated datacenter type: blade|drawer|rack",
                     short = "-D")

opts <- add_argument(opts, "--preemption-policy",
                     help = "Simulated preemption policy: AvalAware-FCFS|AvalAware-TTV",
                     short = "-P")

opts <- add_argument(opts, "--analysis-type",
                     help = "Analysis to perform: usage|task|datacenter",
                     short = "-A")

opts <- add_argument(opts, "--generate-plot",
                     help = "Perform the plot generation [default: T]",
                     short = "-p", default = T)

opts <- add_argument(opts, "--generate-data",
                     help = "Perform the data generation [default: T]",
                     short = "-d", default = T)

opts <- add_argument(opts, "--overwrite-data",
                     help = "Overwrite persisted data [default: T]",
                     short = "-o", default = T)

args <- commandArgs(trailingOnly = TRUE)
params <- parse_args(opts, args)

analysis.list <- list("usage" = UsageAnalysis, "task" = TaskAnalysis, "datacenter" = DatacenterAnalysis)

ImportSourceFiles()

generate.plot <- params$generate_plot
generate.data <- params$generate_data
overwrite.data <- params$overwrite_data

# Testing the case without ploting generation and with data generation
if (!generate.plot & generate.data) {
     warning("Forcing overwrite of generated data")
     overwrite.data <- T
}

analysis.list[[params$analysis_type]](generate.plot, generate.data, overwrite.data, params)