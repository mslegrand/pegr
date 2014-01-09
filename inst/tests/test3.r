library(testthat)
library(stringr)
# print("test 3")
#source("sComponents.r")
#source("literal.r")
pegE<-new.env()
pegE$.DEBUG.NODE<-FALSE
DEVEL.DEBUG<-FALSE
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
