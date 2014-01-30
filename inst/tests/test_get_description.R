library(testthat)

 

context("get_description")

 

test_that("GET_DESCRIPTION",
{
   pegR<-new.parser()
   add_rule(pegR, "A<-'a'")
   txt<-"recognizes 'a'"
   set_description(pegR, "A", txt)
   txt2<-get_description(pegR, "A")
   expect_equal(txt, txt2)
})

 

