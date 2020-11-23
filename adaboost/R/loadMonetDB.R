#' Reads a tibble into a csv file and creates a table into MonetDBLite
#'
#' MonetDBLite is a columnar storage architecture SQL database that runs inside
#' R. This function reads a tibble and stores it into a csv file.
#' In order to create a temporary server for columnar storage MonetDBLite, it
#' create a DBI connection, then it reads the csv file and creates the table
#' with specified column name.
#'
#' It also queries MonetDBLite database and provides results of the following
#' commonly asked queries:
#' How many total records are in the table?
#' Print output of select columns from the table.
#'
#' This function closes the MonetDBLite connection after fecthing the data.
#'
#' @usage loadMonetDB(tibble, colName, tabName)
#'
#' @param tibble Tibble to be written into csv file
#' @param colName Column names for the table to load data
#' @param tabName Name of the table to load the data
#' @return Prints table name and output queries from MonetDB after loading the data
#'
#' @importFrom DBI dbConnect dbExistsTable dbRemoveTable dbDisconnect dbListTables
#' @importFrom MonetDBLite monetdb.read.csv monetdblite_shutdown src_monetdblite
#' @importFrom readr write_csv
#' @importFrom dplyr tbl %>% count select_all
#' @export
#'
#' @examples
#'#=======================================================================#
#'# Load into MonetDBLite and run queries
#'#=======================================================================#
#' MonetDBLite::monetdblite_shutdown()
#' dFile <- system.file("extdata", "lasc04765150.las", package = "adaboost")
#' lasOut <- readFile(dFile)
#' dataTibble <- lasSlotExtract(lasOut, dFile)$dataTibble
#' dataColName <- lasSlotExtract(lasOut, dFile)$dbColnamesData
#' loadMonetDB(dataTibble, dataColName, "slotdata" )
#' MonetDBLite::monetdblite_shutdown()
#'
loadMonetDB <- function(tibble, colName, tabName) {
  csvFile <- tempfile()
  readr::write_csv(tibble, csvFile)
  dbDir <- tempfile()
  MonetDBLite::monetdblite_shutdown()
  con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), dbDir )
  MonetDBLite::monetdb.read.csv(conn = con,
                                files = csvFile,
                                header = TRUE,
                                tablename = tabName,
                                col.names = colName)
  cat("Name of the Table loaded in the database: ",DBI::dbListTables(con), "\n")
  dbHandle <- MonetDBLite::src_monetdblite(dbDir)
  queryTbl <- dplyr::tbl(dbHandle, tabName)
  nstr1 <- sprintf('%s %s count()', 'queryTbl', "%>%", "()" )
  cat("Total no of records in table: ", tabName, "\n")
  print(base::eval(parse(text = nstr1)))
  cat("Select data from the table: ", tabName, "\n")
  nstr2 <- sprintf('%s %s select_all()', 'queryTbl', "%>%", "()" )
  print(base::eval(parse(text = nstr2)))
  MonetDBLite::monetdblite_shutdown()
  DBI::dbDisconnect(con, shutdown=TRUE)
}
