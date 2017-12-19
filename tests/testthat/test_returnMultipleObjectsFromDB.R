library(AxioSerializer)
library(testthat)
library(RSQLite)
context("Reading Serialize Objects and putting them in an environment")

pathToDB <- file.path( path.package( "AxioSerializer" ) , "tests","testthatdata" ,"Test_ReadingEnv.DB")

testwritingConn<-dbConnect(SQLite(),pathToDB)
tables<-dbListTables(testwritingConn)
for(table in tables){
  dbExecute(testwritingConn,paste("DROP TABLE",table))
}
dbDisconnect(testwritingConn)

test_that("Normal Vectors can be read properly from the database",{
  testObject1<-testObject1copy<-c(1,2,3)
  testObject2<-testObject2copy<-c(testObject1,4,5,6)
  writeObjectToDB(testObject1,pathToDB,overwriteEntry = TRUE)
  writeObjectToDB(testObject2,pathToDB,overwriteEntry = TRUE)

  rm("testObject1","testObject2")

  returnMultipleObjectsFromDB(pathToDB)

  expect_equal(testObject1,testObject1copy)
  expect_equal(testObject2,testObject2copy)
})

test_that("Muli-level objects Can be read properly from the database",{
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
  weight <- c(ctl, trt)
  lmD9 <- lmD92<- lm(weight ~ group)

  writeObjectToDB(lmD9,pathToDB,overwriteEntry = TRUE)
  #remove lmD9 from global environment
  rm(lmD9)

  #Return lmD9 from database to global Env
  returnMultipleObjectsFromDB(pathToDB,"lmD9")

  expect_equal(lmD9,lmD92)
})

test_that("Normal Vectors can be read properly into a specified environment",{
  testObject1<-c(1,2,3)
  testObject2<-c(testObject1,4,5,6)
  writeObjectToDB(testObject1,pathToDB,overwriteEntry = TRUE)
  writeObjectToDB(testObject2,pathToDB,overwriteEntry = TRUE)

  tempEnvironment<-new.env()
  returnMultipleObjectsFromDB(pathToDB,environmentToReturnTo = tempEnvironment)

  expect_equal(tempEnvironment$testObject1,testObject1)
  expect_equal(tempEnvironment$testObject2,testObject2)
})

test_that("Ensure environmentToReturnTo is an environment",{

  fakeEnvironment<-data.frame(c(0,0),c(1,1))
  expect_error(returnMultipleObjectsFromDB(pathToDB,environmentToReturnTo = fakeEnvironment))

})
