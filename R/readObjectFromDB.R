# Created by E.Hughes to read serialized data from a SQLite db and return the original object

#' Read stored Object from inside a SQLite database
#'
#' Un-Serializes objects from the database, returning them in their original state
#'
#' @param objectToRead Any R object to be stored inside the database
#' @param databaseToReadFrom The Path to the database that the object will read from, must end in '.DB'
#'
#' @import RSQLite DBI blob
#'
#' @return an R object
#'
#' @examples
#' \dontrun{
#' exampleVector<-c(1,2,3,4)
#' writeObjectToDB(exampleVector,"Example.DB")
#'
#' exampleVector2<-readObjectFromDB("exampleVector","Example.DB")
#'
#' identical(exampleVector,exampleVector2)
#' }
#'
#' @export
readObjectFromDB <- function(objectToRead, databaseToReadFrom, compressionType = "bzip2") {

    # Connect to the database and read the serialized object from the database
    mydb <- dbConnect(RSQLite::SQLite(), dbname = databaseToReadFrom)
    tmpCompressedObject <- dbReadTable(mydb, objectToRead, row.names=FALSE)
    dbDisconnect(mydb)

    #Convert Chars to Raws, making sure to put in the empty raws --------------
    if(is.raw(tmpCompressedObject$serializedObject[[1]])){
      CompressedrawVector<-tmpCompressedObject$serializedObject[[1]]
    }else if(is.character(tmpCompressedObject$serializedObject[[1]])){

      CompressedemptyRaws<-which(tmpCompressedObject$serializedObject!="")
      CompressedNonEmptyRaws<-charToRaw(enc2native(paste(tmpCompressedObject$serializedObject[CompressedemptyRaws],collapse="")))
      CompressedrawVector <- raw(length(tmpCompressedObject$serializedObject))
      CompressedrawVector[CompressedemptyRaws]<-CompressedNonEmptyRaws

    }else{
      stop(paste("Cannot identify serialized object type, is type:",class(tmpCompressedObject$serializedObject[[1]]),", expected 'raw' or 'character'"))
    }

    #Decompress Raw vector
    rawVector<-memDecompress(CompressedrawVector, type = compressionType)

    #Unserialize the object and return it -------------------------------------
    unserializedObject<-unserialize(rawVector)
    return(unserializedObject)
}

