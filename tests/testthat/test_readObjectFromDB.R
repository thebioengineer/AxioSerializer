  library(AxioSerializer)
  library(testthat)
  library(RSQLite)
  context("Reading Serialize Objects")

  pathToDB <- file.path( path.package( "AxioSerializer" ) , "tests","testthatdata" ,"Test_Reading.DB")

  testwritingConn<-dbConnect(SQLite(),pathToDB)
  tables<-dbListTables(testwritingConn)
  for(table in tables){
    dbExecute(testwritingConn,paste("DROP TABLE",table))
  }
  dbDisconnect(testwritingConn)

  test_that("Normal Vectors can be read properly from the database",{
    testObject1<-c(1,2,3)
    testObject2<-c(1,2,3,4,5,6)
    writeObjectToDB(testObject1,pathToDB,overwriteEntry = TRUE)
    writeObjectToDB(testObject2,pathToDB,overwriteEntry = TRUE)

    expect_equal(readObjectFromDB("testObject1",pathToDB),testObject1)
    expect_equal(readObjectFromDB("testObject2",pathToDB),testObject2)
  })

  test_that("Muli-level objects Can be read properly from the database",{
    ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
    trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
    group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
    weight <- c(ctl, trt)
    lmD9 <- lm(weight ~ group)

    writeObjectToDB(lmD9,pathToDB,overwriteEntry = TRUE)

    expect_equal(readObjectFromDB("lmD9",pathToDB),lmD9)
  })

