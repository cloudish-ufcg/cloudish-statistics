GenerateVmAvailability <- function(tasks, max.interested.submiTtime = Inf) {
     
     data.result <- tasks %>% filter(submitTime <= max.interested.submiTtime) %>% collect(n = Inf) %>%
          mutate(priorityStr = ifelse(priority == 0, "prod", ifelse(priority == 1, "batch", "free")), availability = runtime/(finishTime - submitTime)) 
     return(data.result)
}

CalcMeanVmAvailability <- function(tasks) {
     
     data.result <- tasks %>% group_by(priority) %>% summarise(av.mean = mean(availability), av.var = var(availability), av.sd = sd(availability))
     return(data.result)
}

GenerateVmAvailabilityOldScheme <- function(tasks, max.interested.submiTtime = Inf) {

     data.result <- tasks %>% filter(submitTime <= max.interested.submiTtime) %>% collect(n = Inf) %>%
          mutate(priorityStr = ifelse(priority == 0, "prod", ifelse(priority == 1, "batch", "free")), availability = runtime/(finish_time - submit_time))
     return(data.result)
}

CalcMeanVmAvailabilityOldScheme <- function(tasks) {
     
     data.result <- tasks %>% group_by(priority) %>% summarise(av.mean = mean(availability), av.var = var(availability), av.sd = sd(availability))
     return(data.result)
}

SummariseMigrationsPerHour <- function(tasks) {
     
     data.result <- tasks %>% group_by(priority) %>% mutate(migrations.per.hour = migrations/(runtime/(60 * 60))) %>% 
          summarise(runtime.hour.mean = mean(runtime/(60*60)), runtime.hour.max = max(runtime/(60*60)), 
                    runtime.hour.min = min(runtime/(60*60)), migr.hour.min = min(migrations.per.hour), 
                    migr.hour.max = max(migrations.per.hour), migr.hour.mean = mean(migrations.per.hour))
     return(data.result)
}

GetSLOFulfillment <- function(tasks, interested.priority, slo.target) {
     
     tasks <- tasks %>% filter(priority == interested.priority) %>% collect(n = Inf)

     total.tasks <- (tasks %>% summarise(total = n()))$total
     violations <- (tasks %>% mutate(availability = runtime/(finishTime - submitTime)) %>% filter(availability < slo.target) %>% summarise(violations = n()))$violations
     fulfillment.value <- 1 - (violations/total.tasks)
     return(fulfillment.value)
}

GetInterestedTasks <- function(tasks, checkpointed.tasks = NULL, max.interested.time = Inf) {
     
     tasks <- tasks %>% filter(finishTime <= max.interested.time) %>% collect(n = Inf)  
     
     if (!is.null(checkpointed.tasks)) {
          checkpoint <- checkpointed.tasks %>% summarise(taskId = vmId, cpuReq, submitTime, finishTime = max.interested.time, 
                                                         runtime = actualRuntime, priority, preemptions, 
                                                         backfillingChoices = backfillingChoice) %>% collect(n = Inf)
          
          tasks <- rbind(tasks, checkpoint)
     }
     
     return(tasks)
}

SummarisePreemptionsAndBackfilling <- function(tasks, checkpointed.tasks = NULL, max.interested.time = Inf) {
     
     tasks <- GetInterestedTasks(tasks, checkpointed.tasks, max.interested.time)
     
     data.result <- tasks %>% group_by(priority) %>% summarise(tasks = n(), preemptions.total = sum(preemptions), 
                                                               preemptions.mean = mean(preemptions), 
                                                               backfilling.total = sum(backfillingChoices), 
                                                               backfilling.mean = mean(backfillingChoices))
     return(data.result)

}

SummariseMigrations <- function(tasks, checkpointed.tasks = NULL, max.interested.time = Inf) {
     
     tasks <- GetInterestedTasks(tasks, checkpointed.tasks, max.interested.time)
     
     data.result <- tasks %>% mutate(migrations.per.minute = migrations/(runtime/60000000)) %>% group_by(priority) %>% 
          summarise(tasks = n(), migrations.total = sum(migrations), migrations.mean = mean(migrations), migrations.per.minute.mean = mean(migrations.per.minute))
     return(data.result)
}

SummariseMigrationsPerSecond <- function(tasks, checkpointed.tasks = NULL, max.interested.time = Inf) {
     
     tasks <- GetInterestedTasks(tasks, checkpointed.tasks, max.interested.time)
     
     data.result <- tasks %>% mutate(migrations.per.minute = migrations/(runtime/60)) %>% group_by(priority) %>% 
          summarise(tasks = n(), preemptions.total = sum(preemptions), preemptions.mean = mean(preemptions), 
                    backfilling.total = sum(backfillingChoices), backfilling.mean = mean(backfillingChoices), 
                    migrations.total = sum(migrations), migrations.mean = mean(migrations), 
                    migration.proportion = migrations.total/preemptions.total, migrations.per.minute.mean = mean(migrations.per.minute))
     
     return(data.result)
}
