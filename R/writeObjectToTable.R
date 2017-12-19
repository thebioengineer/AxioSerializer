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
writeObjectToTable <- function( objectToSave, ObjectName , tableName , databaseToWriteTo , overwriteEntry = FALSE, compressionType = "bzip2" , lockFile = tempfile() )
{

  ll <- lock( lockFile )
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
  compressedserializedData<-rawToChar(memCompress(serialized,compressionType),TRUE)
  compressedserializedData[which(compressedserializedData=="")]<-'BLANK'
  compressedserializedData[which(compressedserializedData==" ")]<-"SPC"
  compressedserializedData[which(compressedserializedData=='"')]<-"DQ"
  compressedserializedData[which(compressedserializedData=="]")]<-"CSB"
  compressedserializedData[which(compressedserializedData=="[")]<-"OSB"
  compressedserializedData[which(compressedserializedData=="'")]<-"SQ"
  compressedserializedData<-paste(compressedserializedData, collapse=" " )

  databaseQuery <- paste0("REPLACE INTO ",tableName,"(OBJECTNAMES,RAWDATA) VALUES('",ObjectName,"','",compressedserializedData,"')")


  repeat {
    rv <- try(dbExecute(mydb, databaseQuery))
    if (!is(rv, "try-error"))
      break
  }


  unlock( ll )
}

CheckExistence<-function(mydb,tableName,ObjectName,overwriteEntry){
  tableList<-dbListTables(mydb)
  if(tableName%in%tableList){
    #check if the object exists already in the table. If so and overwriteEntry=FALSE, throw error
    preexistingObject<-dim(dbGetQuery(mydb,paste0("SELECT OBJECTNAMES FROM ",tableName," WHERE OBJECTNAMES IN ('",ObjectName,"')"), row.names=FALSE))[1]>0
    if(preexistingObject){
      if(overwriteEntry==FALSE){
        stop("Cannot overwrite preexisting object. Either supply a new name, or set overwriteEntry = TRUE")
      }
    }
  }else{
    createNewTable<-try(dbExecute(mydb,paste("CREATE TABLE",tableName,"(OBJECTNAMES varchar PRIMARY KEY,RAWDATA varchar)"), row.names=FALSE))
    if (is(createNewTable, "try-error"))
      CheckExistence(mydb,tableName,ObjectName,overwriteEntry)

  }
}
