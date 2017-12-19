# Created by E.Hughes to read serialized object from a SQLite db and return the object to the Global Environment

#' Return stored Object(s) from inside a SQLite database to the Global Environment
#'
#' Un-Serializes object(s) from the database, returning them in their original state, and putting them back into a specified environment
#'
#' @import RSQLite DBI blob
#'
#' @param databaseToReadFrom The Path to the database that the object(s) will be read from, must end in '.DB'
#' @param objectNames Any R object to be stored inside the database. If null, returns All the objects stored in the database to the specified environment
#' @param environmentToReturnTo The name of an existing environment to return the objects to, must  be a string. Defaults to the Global Environment
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' exampleVector<-testVector<-c(1,2,3,4)
#' writeObjectToDB(exampleVector,"Example.DB")
#'
#' rm(exampleVector)
#'
#' returnMultipleObjectsFromDB("Example.DB")
#'
#' identical(exampleVector,testVector)
#' }
#'
#' @export
returnMultipleObjectsFromDB <- function(databaseToReadFrom, objectNames = NULL, environmentToReturnTo = .GlobalEnv, compressionType = "bzip2") {


    if(!is.environment(environmentToReturnTo)){stop("environmentToReturnTo must be the name of an existing environment")}

    # Connect to the database and figure out what objects to return -----------
    mydb <- dbConnect(RSQLite::SQLite(), dbname = databaseToReadFrom)
    if (is.null(objectNames)) {
        objectNames <- dbListTables(mydb)
    }

    #For each object, unserialize and put into the specified environment ---------
    lapply(objectNames, function(objectToRead, databaseConnection) {

        tmpCompressedObject <- dbReadTable(databaseConnection, objectToRead)
        # CompressedemptyRaws<-(tmpCompressedObject$serializedObject=="")
        # CompressedNonEmptyRaws<-charToRaw(paste(tmpCompressedObject$serializedObject[CompressedemptyRaws==FALSE],collapse=""))
        # CompressedrawVector <- raw(length(tmpCompressedObject$serializedObject))
        # CompressedrawVector[CompressedemptyRaws==FALSE]<-CompressedNonEmptyRaws
        CompressedrawVector<-tmpCompressedObject$serializedObject[[1]]

        #Decompress Raw vector
        rawVector<-memDecompress(CompressedrawVector, type = compressionType)

        #Unserialize the object and return it -------------------------------------
        unserializedObject<-unserialize(rawVector)

        eval(parse(text =
                  paste0("environmentToReturnTo$", objectToRead, "<-unserializedObject")
        ))

    }, mydb)
    dbDisconnect(mydb)
}
