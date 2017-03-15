library(dplyr)

# Read data from sqlite database
LoadDatabaseData <- function(filename, tablename) {
     db <- src_sqlite(filename)
     db.data <- tbl(db, tablename)
     return(db.data)
}

# Writes the data frame in a specific file
WriteData <- function(data.result, output.dir.path, filename, overwrite.data = T) {
     full.filename <-  paste(output.dir.path, "/", filename, sep = "")
     if (!file.exists(full.filename) | overwrite.data) {
          write.table(data.result, full.filename, quote = F, row.names = F)
     }
}

ReadData <- function(output.dir.path, filename) { 
     full.filename <-  paste(output.dir.path, "/", filename, sep = "")
     data.result <- NULL
     if (file.exists(full.filename)) {
          data.result <- read.table(full.filename, header = T)
     } else {
          warning("Data file not found")
     }
     return(data.result)
}

CreateOutputDir <- function(output.dir, simulation.date, datacenter.type, preemption.policy) {
     dir.name.data = paste(output.dir, "/", simulation.date, "-", datacenter.type, "-", preemption.policy, sep = "")
     dir.create(dir.name.data, showWarnings = F)
     
     dir.name.img = paste(output.dir, "/", simulation.date, "-", datacenter.type, "-", preemption.policy, "/img", sep = "")
     dir.create(dir.name.img, showWarnings = F)
     
     return(list("data" = dir.name.data, "img" = dir.name.img))
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