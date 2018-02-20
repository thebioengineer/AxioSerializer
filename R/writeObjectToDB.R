# Created by E. Hughes to serialize objects and store in a SQLite Database

#' Store any Object inside a SQLite database
#'
#' Serializes any class of object and stores them in a database table, with the objects name as the table name
#'
#' @import RSQLite DBI blob
#'
#' @param objectToSave Any R object to be stored inside the database
#' @param databaseToWriteTo The Path to the database that the object will be put into, must end in '.DB'
#' @param tableName The name the object will be saved in. If left blank, will be the name of the object
#' @param overwriteEntry Do you overwrite the entry in the database? Defaults to FALSE. Will throw an error if object already exists.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' exampleVector<-c(1,2,3,4)
#' writeObjectToDB(exampleVector,"Example.DB")
#' }
#'
#' @export
writeObjectToDB <- function(objectToSave, databaseToWriteTo, tableName = NULL, overwriteEntry = FALSE, compressionType = "bzip2") {

    # Set up object for storing -----------------------------
    if(is.null(tableName)){
    objectToSaveName <- deparse(substitute(objectToSave))
    }else{objectToSaveName<-tableName}
    serialized <- serialize(objectToSave, NULL)
    # compressedserializedData<-as.factor(rawToChar(memCompress(serialized,compressionType),TRUE))
    compressedserializedData<-I(as.blob(memCompress(serialized,compressionType)))
    serializeddata <- data.frame(serializedObject = compressedserializedData)

    # Put object into database ------------------------------
    mydb <- dbConnect(RSQLite::SQLite(), dbname = databaseToWriteTo)
    on.exit(dbDisconnect(mydb))
    dbWriteTable(mydb, objectToSaveName, serializeddata, overwrite = overwriteEntry)

}
