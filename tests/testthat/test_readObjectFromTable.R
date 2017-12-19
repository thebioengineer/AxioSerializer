  library(AxioSerializer)
  library(testthat)
  library(RSQLite)
  context("Reading Multiple Serialized Objects Stored in a Single Table")

  pathToDB <- file.path( path.package( "AxioSerializer" ) , "tests","testthatdata" ,"Test_Reading_fromTable.DB")

  testwritingConn<-dbConnect(SQLite(),pathToDB)
  tables<-dbListTables(testwritingConn)
  for(table in tables){
    dbExecute(testwritingConn,paste("DROP TABLE",table))
  }
  dbDisconnect(testwritingConn)

  test_that("Normal Vectors can be read properly from the database",{
    testObject1<-c(1,2,3)
    testObject2<-c(1,2,3,4,5,6)
    writeObjectToTable(testObject1,"testObject1","Test_Read",pathToDB)
    writeObjectToTable(testObject2,"testObject2","Test_Read",pathToDB)

    expect_equal(readObjectFromTable("testObject1","Test_Read",pathToDB),testObject1)
    expect_equal(readObjectFromTable("testObject2","Test_Read",pathToDB),testObject2)
  })

  test_that("Muli-level objects Can be read properly from the database",{
    ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
    trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
    group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
    weight <- c(ctl, trt)
    lmD9 <- lm(weight ~ group)

    writeObjectToTable(lmD9,"testMultilevelObject","Test_Read",pathToDB)

    expect_equal(readObjectFromTable("testMultilevelObject","Test_Read",pathToDB),lmD9)
  })

