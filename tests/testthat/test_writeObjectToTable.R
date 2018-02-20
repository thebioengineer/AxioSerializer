library(AxioSerializer)
library(testthat)
library(RSQLite)

context("Storing Serialize Objects inside a single table to be smaller")

pathToDB <- file.path( path.package( "AxioSerializer" ) , "tests","testthatdata" ,"Test_Appending.DB")

testwritingConn<-dbConnect(SQLite(),pathToDB)
tables<-dbListTables(testwritingConn)
for(table in tables){
  dbExecute(testwritingConn,paste("DROP TABLE",table))
}
dbDisconnect(testwritingConn)

test_that("Normal Vectors can be written into the Table",{
  testObject1<-c(1,2,3)
  testObject2<-c(testObject1,4,5,6)

  expect_silent(writeObjectToTable(testObject1,"testObject1","Test_Append",pathToDB))
  expect_silent(writeObjectToTable(testObject2,"testObject2","Test_Append",pathToDB))
})

test_that("Muli-level objects can be written into the Table",{
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
  weight <- c(ctl, trt)
  lmD9 <- lm(weight ~ group)

  expect_silent(writeObjectToTable(lmD9,"lmD9","Test_Append",pathToDB))
})

test_that("Will not overwrite another object unless told to",{
  overwritingTestObject<-c(1,2,3)

  expect_silent(writeObjectToTable(overwritingTestObject,"overwritingTestObject","Test_Append",pathToDB))
  expect_silent(writeObjectToTable(overwritingTestObject,"overwritingTestObject","Test_Append",pathToDB,overwriteEntry = TRUE))
  expect_error(writeObjectToTable(overwritingTestObject,"overwritingTestObject","Test_Append",pathToDB))

})

test_that("Overwrite another object correctly",{
  overwritingTestObject<-c(1,2,3)
  newValue<-c(4,5,6)

  writeObjectToTable(overwritingTestObject,"overwritingAnotherTestObject","Test_Append",pathToDB)
  expect_equal(readObjectFromTable("overwritingAnotherTestObject","Test_Append",pathToDB),overwritingTestObject)

  writeObjectToTable(newValue,"overwritingAnotherTestObject","Test_Append",pathToDB,overwriteEntry = TRUE)
  expect_equal(readObjectFromTable("overwritingAnotherTestObject","Test_Append",pathToDB),newValue)
})
