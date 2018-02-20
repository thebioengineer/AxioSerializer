library(AxioSerializer)
library(testthat)
library(RSQLite)
context("Storing Serialize Objects in parallel inside a single table")

pathToDB <- file.path( path.package( "AxioSerializer" ) , "tests","testthatdata" ,"Test_Parallel.DB")
# pathToDB<-"tests/testthatdata/Test_Parallel.DB"

testwritingConn<-dbConnect(SQLite(),pathToDB)
tables<-dbListTables(testwritingConn)
for(table in tables){
  dbExecute(testwritingConn,paste("DROP TABLE",table))
}
dbDisconnect(testwritingConn)

test_that("Objects can be written in parallel into the Table",{

  canParallel<-require(snowfall,quietly = TRUE)

  if(canParallel){
    testObject1<-c(1,2,3)

    sfInit(parallel = TRUE, cpus = 3)
    on.exit(sfStop())
    suppressPackageStartupMessages(suppressMessages(sfLibrary( AxioSerializer , keep.source = TRUE , verbose = FALSE)))
    LockFile<-file.path( path.package( "AxioSerializer" ) , "tests","testthatdata" ,"Test_Parallel")
    #LockFile<-file.path( "tests","testthatdata" ,"Test_Parallel")
    if(file.exists(paste0(LockFile,".log")))
      file.remove(paste0(LockFile,".log"))

    writeObjectWrapper <- function(val,object,pathToDB,lockFile) {
      writeObjectToTable(object,paste0("testObject_",val),"Test_Parallel",pathToDB,lockFile = lockFile)
    }

    expect_silent(sfLapply(1:100, writeObjectWrapper,testObject1,pathToDB,LockFile))

    db<-dbConnect(SQLite(),pathToDB)
    writtenObjects<-dbGetQuery(db,"SELECT OBJECTNAMES FROM Test_Parallel")
    dbDisconnect(db)

    expect_true(!any(!(paste0("testObject_",1:100)%in%writtenObjects$OBJECTNAMES)))
    expect_identical(readObjectFromTable(paste0("testObject_1"),"Test_Parallel",pathToDB),testObject1)
  }
})


test_that("Objects can be written in parallel twice into the Table",{

  canParallel<-require(snowfall,quietly = TRUE)

  if(canParallel){
    testObject1<-c(1,2,3)

    sfInit(parallel = TRUE, cpus = 3)
    suppressPackageStartupMessages(sfLibrary( AxioSerializer , keep.source = TRUE ,verbose = FALSE))
    LockFile<-file.path( path.package( "AxioSerializer" ) , "tests","testthatdata" ,"Test_Parallel_WriteTwice")
    #LockFile<-file.path( "tests","testthatdata" ,"Test_Parallel_WriteTwice")
    if(file.exists(paste0(LockFile,".log")))
      file.remove(paste0(LockFile,".log"))

    writeObjectWrapper <- function(val,object,pathToDB,table,lockFile) {
      writeObjectToTable(object,paste0("testObject_",val),table, pathToDB,lockFile = lockFile)
    }
    sfExport("writeObjectWrapper")

    writeTwoTables<-function(val,object,pathToDB,lockFile){
      writeObjectWrapper(val,object,pathToDB,"Test_Parallel_WriteTwice_1",lockFile)
      writeObjectWrapper(val,object,pathToDB,"Test_Parallel_WriteTwice_2",lockFile)
    }

    expect_silent(sfLapply(1:100, writeTwoTables,testObject1,pathToDB,LockFile))

    db<-dbConnect(SQLite(),pathToDB)
    writtenObjects_table1<-dbGetQuery(db,"SELECT OBJECTNAMES FROM Test_Parallel_WriteTwice_1")
    writtenObjects_table2<-dbGetQuery(db,"SELECT OBJECTNAMES FROM Test_Parallel_WriteTwice_2")
    dbDisconnect(db)

    expect_true(!any(!(paste0("testObject_",1:100)%in%writtenObjects_table1$OBJECTNAMES)))
    expect_true(!any(!(paste0("testObject_",1:100)%in%writtenObjects_table2$OBJECTNAMES)))
    expect_true(!any(!(writtenObjects_table1$OBJECTNAMES%in%writtenObjects_table2$OBJECTNAMES)))


    sfStop()

  }
})


