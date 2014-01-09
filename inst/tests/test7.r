library(testthat)


test_that("ADD_RULE",
{
  peg<-new.parser()
  add_rule(peg, "X<-x", des="A bad rule (I forgot quotes)")
  inspect_rule(peg, "X")->s1 
  answ1<-ruleStruct("X", "X<-x","A bad rule (I forgot quotes)" , NULL)
  expect_equal(s1,answ1)
  add_rule(peg, "X<-'x'", act="list('X')")
  inspect_rule(peg, "X")->s2
  answ2<-ruleStruct("X", "X<-'x'", NULL, c("Inline:", "list('X')") )
  expect_equal(s2,answ2)
})

test_that("SET_RULE",
{
  peg<-new.parser()
  # Add rule A to recognize 'a' and return list('A') as it's value
  add_rule(peg, "A<-'a'", act="list('A')")
  value(apply_rule(peg, 'A', 'a', exe=TRUE))->v
  expect_equal(length(v),1)
  expect_equal(v[[1]], list("A")[[1]] )
  
  set_rule(peg, "A<-'b'")
  # Now A will only recognize 'b', so it will now fail on input 'a'
  status(apply_rule(peg, 'A', 'a', exe=TRUE))->v
  expect_equal(v,FALSE)
  # However, A not recognizes b, but still returs list('A') as it's value
  value(apply_rule(peg, 'A', 'b', exe=TRUE))->v
  expect_equal(length(v),1)
  expect_equal(v[[1]], list("A")[[1]])
  
})