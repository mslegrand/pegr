library(testthat)

context("test_delete_rule") 

test_that("DELETE_RULE",
{
   peg<-new.parser()
   peg<-add_rule(peg,  "A<-'a'")
   peg<-add_rule(peg,  "B<-'b'")
   ids<-rule_ids(peg)
   expect_equal(length(ids),2)
   peg<-delete_rule(peg, "A")
   ids<-rule_ids(peg)
   expect_equal(length(ids),1)
   expect_equal(ids,"B")
})

 

