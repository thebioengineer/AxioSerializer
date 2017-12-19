library(AxioSerializer)
library(testthat)
library(RSQLite)
context("Storing Serialize Objects")

pathToDB <- file.path( path.package( "AxioSerializer" ) , "tests","testthatdata" ,"Test_Writing.DB")

testwritingConn<-dbConnect(SQLite(),pathToDB)
tables<-dbListTables(testwritingConn)
for(table in tables){
  dbExecute(testwritingConn,paste("DROP TABLE",table))
}
dbDisconnect(testwritingConn)

test_that("Normal Vectors can be written into the database",{
  testObject1<-c(1,2,3)
  testObject2<-c(testObject1,4,5,6)

  expect_silent(writeObjectToDB(testObject1,pathToDB,overwriteEntry = TRUE))
  expect_silent(writeObjectToDB(testObject2,pathToDB,overwriteEntry = TRUE))
})

test_that("Muli-level objects can be written into the database",{
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
  weight <- c(ctl, trt)
  lmD9 <- lm(weight ~ group)

  expect_silent(writeObjectToDB(lmD9,pathToDB,overwriteEntry = TRUE))
})

test_that("Will not overwrite another object unless told to",{
  overwritingTestObject<-c(1,2,3)

  expect_silent(writeObjectToDB(overwritingTestObject,pathToDB))
  expect_silent(writeObjectToDB(overwritingTestObject,pathToDB,overwriteEntry = TRUE))
  expect_error(writeObjectToDB(overwritingTestObject,pathToDB))

})


