library(AxioSerializer)
library(testthat)
library(RSQLite)
context("Storing Serialize Objects in parallel inside a single table")

pathToDB <- file.path( path.package( "AxioSerializer" ) , "tests","testthatdata" ,"Test_Parallel.DB")

testwritingConn<-dbConnect(SQLite(),pathToDB)
tables<-dbListTables(testwritingConn)
for(table in tables){
  dbSendQuery(testwritingConn,paste("DROP TABLE",table))
}
dbDisconnect(testwritingConn)

test_that("Objects can be written in parallel into the Table",{

  canParallel<-require(snowfall)

  if(canParallel){
    testObject1<-c(1,2,3)

    sfInit(parallel = TRUE, cpus = 3)
    sfLibrary( AxioSerializer , keep.source = FALSE )

    writeObjectWrapper <- function(val,object,pathToDB) {
      writeObjectToTable(object,paste0("testObject_",val),"Test_Parallel",pathToDB)
    }

    expect_silent(sfLapply(1:100, writeObjectWrapper,testObject1,pathToDB))

    sfStop()

  }
})



