# Created by E.Hughes to add an object to a list of objects stored in a SQLite db

#' Read add an Object from inside a SQLite database table
#'
#' @param objectToRead The name of theR object to be read from inside the database
#' @param tableName table to read from
#' @param databaseToReadFrom The Path to the database that the object will read from
#' @param ... additional arguments to be passed to the decompression function \code{\link[base]{memDecompress}}, unlikely necessary
#'
#' @import RSQLite
#' @import DBI
#' @import Unicode
#' @import lz4
#' @import stringr
#' @import stringi
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' exampleVector<-c(1,2,3,4)
#' appendObjectToTable(exampleVector, "Example_Vector", "Example_Table","Example.DB")
#'
#' exampleVector2<-readObjectFromTable("Example_Vector","Example_Table","Example.DB")
#'
#' identical(exampleVector,exampleVector2)
#' }
#'
#' @export
readObjectFromTable <- function( objectToRead, tableName , databaseToReadFrom, ... ) {

  if(is.null(tableName)) stop("Must Provide a table to read from! 'tableName' cannot be left null")

  # Connect to the database and check if table exists.
  # If it does exist, read and decompile list to allow for appending
  # If not, create blank list
  if(file.exists(databaseToReadFrom)){
    mydb <- dbConnect(RSQLite::SQLite(), dbname = databaseToReadFrom)
    on.exit(dbDisconnect(mydb))
  }else{
    stop("Database does not exist!")
  }

  #check if the object exists already in the table
  Object<-try(dbGetQuery(mydb,paste0("SELECT RAWDATA,\"COMPRESSIONTYPE\" FROM ",tableName," WHERE OBJECTNAMES IN ('",objectToRead,"')"), row.names=FALSE),silent=TRUE)
  if(inherits(Object,"try-error") || length(Object$RAWDATA)==0){
      stop("Cannot find object in the Table. Check if the tableName or ObjectName is correct")
  }

    tmpCompressedObject<-Object$RAWDATA
    tmpCompressedObject<-stri_split_regex(tmpCompressedObject," ")[[1]]

    tmpCompressedObject[which(tmpCompressedObject=="DQ")]<-'"'
    tmpCompressedObject[which(tmpCompressedObject=="CSB")]<-"]"
    tmpCompressedObject[which(tmpCompressedObject=="OSB")]<-"["
    tmpCompressedObject[which(tmpCompressedObject=="SQ")]<-"'"
    tmpCompressedObject[which(tmpCompressedObject=="SPC")]<-" "
    tmpCompressedObject[which(tmpCompressedObject=="BLANK")]<-''

    #Convert Chars to Raws, making sure to put in the empty raws --------------
    CompressedemptyRaws<-tmpCompressedObject!=""

    nonProperUTF8Index<-startsWith(tmpCompressedObject,"<") & endsWith(tmpCompressedObject,">")
    if(any(nonProperUTF8Index)){
      nonProperUTF8<-tmpCompressedObject[which(nonProperUTF8Index)]
      nonProperUTF8<-gsub("<","",gsub(">","",nonProperUTF8))
      nonProperUTF8<-do.call('c',lapply(1:length(nonProperUTF8),function(index,nonProperUTF8){intToUtf8(as.u_char(nonProperUTF8[index]))},nonProperUTF8))
      tmpCompressedObject[which(nonProperUTF8Index)]<-nonProperUTF8
    }


    #fix encoding issues and convert to a raw vector
    CompressedNonEmptyRaws<-stri_enc_toutf8(stri_join(tmpCompressedObject[CompressedemptyRaws],collapse=""))
    CompressedNonEmptyRaws_char<-charToRaw(CompressedNonEmptyRaws)
    if(length(CompressedNonEmptyRaws_char)!=sum(CompressedemptyRaws)){
      CompressedNonEmptyRaws_char<-charToRaw(stri_enc_tonative(CompressedNonEmptyRaws))
    }
    if(length(CompressedNonEmptyRaws_char)!=sum(CompressedemptyRaws)){
      CompressedNonEmptyRaws_char<-charToRaw(iconv(CompressedNonEmptyRaws,"UTF-8","l1"))
    }

    CompressedrawVector <- raw(length(tmpCompressedObject))
    CompressedrawVector[CompressedemptyRaws]<-CompressedNonEmptyRaws_char

    #Decompress Raw vector
    if(is.character(Object$COMPRESSIONTYPE)){
      if(Object$COMPRESSIONTYPE=="LZ4"){
        rawVector<-lzDecompress(CompressedrawVector)
      }else{
        stop(paste("invalid Compression type entry:",Object$COMPRESSTIONTYPE))
      }
    }else{
      rawVector<-memDecompress(CompressedrawVector,...)
    }


    #Unserialize the object and return it -------------------------------------
    unserializedObject<-unserialize(rawVector)
    return(unserializedObject)
}
