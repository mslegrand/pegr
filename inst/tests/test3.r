library(testthat)
library(stringr)

context("Test3")


pegE<-new.env()
pegE$.RECORD.NODE<-FALSE
include.sComponents(pegE)
include.literal(pegE)

test_that("SPACE",
{
  SPACEX<-literal(' ')
  SPACEX(" ")->res
  expect_true(res$ok)
  expect_equal(res$pos,1)
  expect_equal(paste(res$val,collapse=""), " ")
}
)

test_that("leftarrow",
{
  LA<-literal('<-')
  LA("<-")->res
  #print(res)
  expect_true(res$ok)
  expect_equal(res$pos,2)
  expect_equal(paste(res$val,collapse=""), "<-")
}
)
