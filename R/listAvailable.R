# Created by E.Hughes to list objects and tables from a SQLite db

#' Get the names of the Objects in a table that is stored in a SQLite database
#'
#' @param tableName table to append to
#' @param database The Path to the database that stores the information
#'
#' @import RSQLite

#' @return A character vector of objects stored in the table
#' @family listAvailable
#'
#' @examples
#' \dontrun{
#' exampleVector<-c(1,2,3,4)
#' appendObjectToTable(exampleVector, "Example_Vector", "ListOfVectors","Example.DB")
#'
#' objectNameVector<-listExistingObjects("ListOfVectors","Example.DB")
#'
#' identical(objectNameVector,"Example_Vector")
#' }
#'
#' @export
listExistingObjects <- function( tableName , database ) {
  if(is.null(tableName)) stop("Must Provide a table to read from! 'tableName' cannot be left null")
  # Connect to the database and check if table exists.
  # If it does exist, read and decompile list to allow for appending
  # If not, create blank list
  if(file.exists(database)){
    ll <- lock( tempfile() )
    on.exit( unlock(ll))
    mydb <- dbConnect(RSQLite::SQLite(), dbname = database)
    on.exit(dbDisconnect(mydb))
    on.exit( unlock(ll), add=TRUE )

  }else{
    stop("Database does not exist!")
  }

  if(dbGetQuery(mydb,sprintf("SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = '%s'",tableName))[[1]]){
    objectList<-try(dbGetQuery(mydb,sprintf("SELECT OBJECTNAMES FROM %s",tableName))[[1]],silent = TRUE)
  }else{
    stop(sprintf("Table '%s' does not exist in the database.",tableName))
  }

  if(inherits(objectList,"try-error")){
    stop('Table is not an AxioSerializer Compatable table')
  }

  return(objectList)
}

#' Get the number of the Objects in a table that is stored in a SQLite database
#'
#' @param tableName table to append to
#' @param database The Path to the database that stores the information
#'
#' @import RSQLite

#' @return A numeric
#' @family listAvailable
#'
#' @examples
#' \dontrun{
#' exampleVector<-c(1,2,3,4)
#' appendObjectToTable(exampleVector, "Example_Vector", "ListOfVectors","Example.DB")
#'
#' nObjects<-nSerializedObjects("ListOfVectors","Example.DB")
#'
#' identical(nObjects,1)
#' }
#'
#' @export
nSerializedObjects <- function( tableName , database ) {
  if(is.null(tableName)) stop("Must Provide a table to read from! 'tableName' cannot be left null")
  # Connect to the database and check if table exists.
  # If it does exist, read and decompile list to allow for appending
  # If not, create blank list
  if(file.exists(database)){
    ll <- lock( tempfile() )
    on.exit( unlock(ll))
    mydb <- dbConnect(RSQLite::SQLite(), dbname = database)
    on.exit(dbDisconnect(mydb))
    on.exit( unlock(ll), add=TRUE )
  }else{
    stop("Database does not exist!")
  }

  if(dbGetQuery(mydb,sprintf("SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = '%s'",tableName))[[1]]){
    count<-try(dbGetQuery(mydb,sprintf("SELECT count(*) FROM %s",tableName))[[1]],silent = TRUE)
  }else{
    stop(sprintf("Table '%s' does not exist in the database.",tableName))
  }

  if(inherits(count,"try-error")){
    stop('Table is not an AxioSerializer Compatable table')
  }

  return(count)
}


#' Get the names of the Axioserializer compliant tables in a SQLite database
#'
#' @param database The Path to the database that stores the information
#'
#' @import RSQLite

#' @return A character vector of objects stored in the table
#' @family listAvailable
#'
#' @examples
#' \dontrun{
#' exampleVector<-c(1,2,3,4)
#' appendObjectToTable(exampleVector, "Example_Vector", "ListOfVectors","Example.DB")
#'
#' tableVector<-listExistingTables("Example.DB")
#'
#' identical(tableVector,"ListOfVectors")
#' }
#'
#' @export
listExistingTables <- function( database ) {
  # Connect to the database and check if table exists.
  # If it does exist, read and decompile list to allow for appending
  # If not, create blank list
  if(file.exists(database)){
    ll <- lock( tempfile() )
    on.exit( unlock(ll))
    mydb <- dbConnect(RSQLite::SQLite(), dbname = database)
    on.exit(dbDisconnect(mydb))
    on.exit( unlock(ll), add=TRUE )
  }else{
    stop("Database does not exist!")
  }

  tables<-dbGetQuery(mydb,"SELECT name FROM sqlite_master WHERE type = 'table'")[[1]]
  goodTables<-sapply(tables,function(tableName,mydb){
    objnames<-try(dbGetQuery(mydb,sprintf("SELECT OBJECTNAMES FROM %s",tableName))[[1]],silent=TRUE)
    if(!inherits(objnames,"try-error")){
      return(TRUE)
    }else{
      return(FALSE)
    }
  },mydb)

  tables[goodTables]
}

#' Get the names of the Objects by Axioserializer compliant tables in a SQLite database
#'
#' @param database The Path to the database that stores the information
#'
#' @import RSQLite

#' @return A data.frame with two columns: Table and Objects
#' @family listAvailable
#'
#' @examples
#' \dontrun{
#' exampleVector<-c(1,2,3,4)
#' appendObjectToTable(exampleVector, "Example_Vector", "ListOfVectors","Example.DB")
#'
#' ObjectTableDF<-listExistingObjectsByTable("Example.DB")
#'
#' identical(ObjectTableDF,data.frame(Table="ListOfVectors",ObjectName="Example_Vector")
#' }
#'
#' @export
listExistingObjectsByTable <- function( database ){
  possibleTables<-listExistingTables(database)

  AvailableObjects<-do.call('rbind',lapply(possibleTables,function(tableName,database){
    objects<-try(listExistingObjects(tableName,database),silent=TRUE)
    if(!inherits(objects,"try-error")){
      return(data.frame(Table=tableName,ObjectName=objects))
    }
  },database))
}
