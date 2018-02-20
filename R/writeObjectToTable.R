# Created by E.Hughes to add an object to a list of objects stored in a SQLite db

#' Read add an Object to a list of objects inside a SQLite database
#'
#' @param objectToSave Any R object to be stored inside the database
#' @param ObjectName The name the object will be saved as in table. If left blank, will be the name of the object
#' @param tableName table to append to
#' @param databaseToWriteTo The Path to the database that the object will read from, must end in '.DB'
#'
#' @import RSQLite
#' @import flock
#' @import lz4
#' @import stringr
#' @import stringi
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' exampleVector<-c(1,2,3,4)
#' appendObjectToTable(exampleVector, "Example_Vector", "ListOfVectors","Example.DB")
#'
#' exampleVector2<-readObjectFromTable("Example_Vector","ListOfVectors","Example.DB")
#'
#' identical(exampleVector,exampleVector2)
#' }
#'
#' @export
writeObjectToTable <- function( objectToSave, ObjectName , tableName , databaseToWriteTo , overwriteEntry = FALSE, lockFile = tempfile() )
{

  ll <- lock( lockFile )
  on.exit( unlock(ll))

  if(is.null(tableName)) stop("Must Provide a table to append to!, 'tableName' cannot be left null")

  # Create object name for storing --------------------------------------------------------------------
  if(is.null(ObjectName)){
    ObjectName <- deparse(substitute(objectToSave))
  }

  # Connect to the database  ------------------------------------------------
  mydb <- dbConnect(RSQLite::SQLite(), dbname = databaseToWriteTo)

  # Set failsafe for exiting on failure that db gets disconnected
  on.exit(  dbDisconnect( mydb ) )
  on.exit( unlock(ll), add=TRUE )

  # Check if table exists. If it does exist, Check to see if the objectname already exists------------------------------------
  CheckExistence(mydb,tableName,ObjectName,overwriteEntry)

  # Serialize and Compress Object. Append to list of objects
  serialized <- serialize(objectToSave, NULL)
  compressedserializedData<-rawToChar(lzCompress(serialized),TRUE)
  compressedserializedData[which(compressedserializedData=="")]<-'BLANK'
  compressedserializedData[which(compressedserializedData==" ")]<-"SPC"
  compressedserializedData[which(compressedserializedData=='"')]<-"DQ"
  compressedserializedData[which(compressedserializedData=="]")]<-"CSB"
  compressedserializedData[which(compressedserializedData=="[")]<-"OSB"
  compressedserializedData[which(compressedserializedData=="'")]<-"SQ"
  compressedserializedData<-stri_join(compressedserializedData, collapse=" ")

  databaseQuery <- stri_join("REPLACE INTO ", tableName, "(OBJECTNAMES,RAWDATA,COMPRESSIONTYPE) VALUES('",ObjectName, "','", compressedserializedData, "','LZ4')")

  repeat {
    rv <- try(dbExecute(mydb, databaseQuery))
    if (!is(rv, "try-error"))
      break

  }
}

CheckExistence<-function(mydb,tableName,ObjectName,overwriteEntry){
  tableList<-dbListTables(mydb)
  if(tableName%in%tableList){
    #check if the object exists already in the table. If so and overwriteEntry=FALSE, throw error
    preexistingObject<-dbGetQuery(mydb,sprintf("SELECT \"COMPRESSIONTYPE\" FROM %s WHERE OBJECTNAMES IN ('%s') ",tableName,ObjectName), row.names=FALSE)
    if(dim(preexistingObject)[1]>0){
      if(overwriteEntry==FALSE){
        stop("Cannot overwrite preexisting object. Either supply a new name, or set overwriteEntry = TRUE")
      }
      if(is.character(preexistingObject$COMPRESSIONTYPE) && identical(preexistingObject$COMPRESSIONTYPE,"COMPRESSIONTYPE")){
        try(dbExecute(mydb,sprintf("ALTER TABLE %s ADD COMPRESSIONTYPE varchar",tableName)),silent=TRUE)
      }
    }
  }else{
    createNewTable<-try(dbExecute(mydb,sprintf("CREATE TABLE %s (OBJECTNAMES varchar PRIMARY KEY,RAWDATA varchar,COMPRESSIONTYPE varchar)",tableName), row.names=FALSE),silent=TRUE)
    if (is(createNewTable, "try-error"))
      CheckExistence(mydb,tableName,ObjectName,overwriteEntry)

  }
}

# objectExists<-function(mydb,tableName,objectName,overwriteEntry=FALSE){
#   tableExists<-dbGetQuery(mydb,sprintf("SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = '%s'",tableName))[[1]]
#   if(!tableExists){
#     createNewTableResults<-try(dbExecute(mydb,sprintf("CREATE TABLE %s (OBJECTNAMES varchar PRIMARY KEY,RAWDATA varchar,COMPRESSIONTYPE varchar)",tableName)),silent=TRUE)
#     if (is(createNewTableResults, "try-error"))
#       objectExists(mydb,tableName,objectName,overwriteEntry)
#   }else{
#     objectExists<-dbGetQuery(mydb,sprintf("SELECT CASE (SELECT \"COMPRESSIONTYPE\" FROM %1$s WHERE OBJECTNAMES IN ('%2$s')) WHEN 'LZ4' THEN 1 WHEN 'COMPRESSIONTYPE' THEN 2 ELSE 0 END",tableName,objectName))[[1]]
#     if(objectExists){
#       if(objectExists==2){
#         try(dbExecute(mydb,sprintf("ALTER TABLE %s ADD COMPRESSIONTYPE varchar",tableName)),silent=TRUE)
#       }
#       if(!overwriteEntry){
#         stop("Cannot overwrite preexisting object. Either supply a new name, or set overwriteEntry = TRUE")
#       }
#     }else{
#     }
#   }
# }
