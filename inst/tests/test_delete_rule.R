library(testthat)

context("test_delete_rule") 

test_that("DELETE_RULE",
{
   peg<-new.parser()
   add_rule(peg,  "A<-'a'")
   add_rule(peg,  "B<-'b'")
   ids<-rule_ids(peg)
   expect_equal(length(ids),2)
   delete_rule(peg, "A")
   ids<-rule_ids(peg)
   expect_equal(length(ids),1)
   expect_equal(ids,"B")
})

 

