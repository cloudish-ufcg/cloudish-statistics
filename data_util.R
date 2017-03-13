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