library(dplyr)

# Read data from sqlite database
LoadDatabaseData <- function(filename, tablename) {
     db <- src_sqlite(filename)
     db.data <- tbl(db, tablename)
     return(db.data)
}

# Writes the data frame in a specific file
WriteData <- function(data.result, filename) {
     write.table(data.result, filename, quote = F, row.names = F)
}

GetTaskData <- function(input.dir.base, datacenter.type, simulation.date, preemption.policy) {
     input.file.tasks <- paste(input.dir.base, "/googletasks-", datacenter.type, "-", simulation.date, "-", preemption.policy, "-epoch1s.sqlite3", sep = "")
     tasks <- LoadDatabaseData(input.file.tasks, "googletasks")
     return(tasks)
}

GetCheckpointData <- function(input.dir.base, input.file.checkpoint) {
     input.file.checkpoint <- paste(input.dir.base, "/", input.file.checkpoint, sep = "")
     checkpoints <- LoadDatabaseData(input.file.checkpoint, "preemptivevms")
     return(checkpoints)
}

GetDatacenterData <- function(input.dir.base, datacenter.type, simulation.date, preemption.policy) {
     input.file.datacenter <- paste(input.dir.base, "/datacenter-", datacenter.type, "-", simulation.date, "-", preemption.policy, "-epoch1s.sqlite3", sep = "")
     datacenter <- LoadDatabaseData(input.file.datacenter, "datacenterusage")
     return(datacenter)
}

GetUsageData <- function(input.dir.base, datacenter.type, simulation.date, preemption.policy) {
     input.file.utilization <- paste(input.dir.base, "/utilization-", datacenter.type, "-", simulation.date, "-", preemption.policy, "-epoch1s.sqlite3", sep = "")
     usage <- LoadDatabaseData(input.file.utilization, "usage")
     return(usage)
}