# Created by E.Hughes to add an object to a list of objects stored in a SQLite db

#' Read add an Object to a list of objects inside a SQLite database
#'
#' @param objectToSave Any R object to be stored inside the database
#' @param ObjectName The name the object will be saved as in table. If left blank, will be the name of the object
#' @param tableName table to append to
#' @param databaseToWriteTo The Path to the database that the object will read from, must end in '.DB'
#'
#' @import RSQLite DBI Unicode
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
readObjectFromTable <- function( objectToRead, tableName , databaseToReadFrom, compressionType = "bzip2" ) {

  if(is.null(tableName)) stop("Must Provide a table to append to! 'tableName' cannot be left null")

  # Connect to the database and check if table exists.
  # If it does exist, read and decompile list to allow for appending
  # If not, create blank list
  mydb <- dbConnect(RSQLite::SQLite(), dbname = databaseToReadFrom)
  tableList<-dbListTables(mydb)
  if(tableName%in%tableList){
    #check if the object exists already in the table. If so and overwriteEntry=FALSE, throw error
    Object<-dbGetQuery(mydb,paste0("SELECT * FROM ",tableName," WHERE OBJECTNAMES IN ('",objectToRead,"')"), row.names=FALSE)

    if(dim(Object)[1]==0){
        stop("Cannot find object in the db. Check if the tableName is correct")
    }
  }else{  stop("Table does not exist")  }

  dbDisconnect(mydb)

    tmpCompressedObject<-Object$RAWDATA
    tmpCompressedObject<-strsplit(tmpCompressedObject," ")[[1]]

    tmpCompressedObject[which(tmpCompressedObject=="DQ")]<-'"'
    tmpCompressedObject[which(tmpCompressedObject=="CSB")]<-"]"
    tmpCompressedObject[which(tmpCompressedObject=="OSB")]<-"["
    tmpCompressedObject[which(tmpCompressedObject=="SQ")]<-"'"
    tmpCompressedObject[which(tmpCompressedObject=="SPC")]<-" "
    tmpCompressedObject[which(tmpCompressedObject=="BLANK")]<-''

    #Convert Chars to Raws, making sure to put in the empty raws --------------
    CompressedemptyRaws<-which(tmpCompressedObject!="")

    nonProperUTF8Index<-which(grepl("<..>",tmpCompressedObject))
    if(length(nonProperUTF8Index)>0){
      nonProperUTF8<-tmpCompressedObject[nonProperUTF8Index]
      nonProperUTF8<-gsub("<","",gsub(">","",nonProperUTF8))
      nonProperUTF8<-do.call('c',lapply(1:length(nonProperUTF8),function(index,nonProperUTF8){intToUtf8(as.u_char(nonProperUTF8[index]))},nonProperUTF8))
      tmpCompressedObject[nonProperUTF8Index]<-nonProperUTF8
    }

    Locale<-localeToCharset()
    if("UTF-8"%in%Locale){
      CompressedNonEmptyRaws<-charToRaw(iconv(enc2native(paste(tmpCompressedObject[CompressedemptyRaws],collapse="")),"UTF-8","l1"))
    }else{
      CompressedNonEmptyRaws<-charToRaw(enc2native(paste(tmpCompressedObject[CompressedemptyRaws],collapse="")))
    }
    CompressedrawVector <- raw(length(tmpCompressedObject))
    CompressedrawVector[CompressedemptyRaws]<-CompressedNonEmptyRaws

    #Decompress Raw vector
    rawVector<-memDecompress(CompressedrawVector, type = compressionType)

    #Unserialize the object and return it -------------------------------------
    unserializedObject<-unserialize(rawVector)
    return(unserializedObject)
}
