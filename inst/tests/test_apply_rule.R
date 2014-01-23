library(testthat)

context("test_apply_rule")


test_that("APPLY_RULE",
{
   # The simplest example: a parser that only accepts a single character 'a'
   peg<-new.parser()
   add_rule(peg, "A<-'a'")
   res<-apply_rule(peg, 'A', "a")
   expect_equal(res$ok, TRUE)
  expect_equal(res$pos, 1)
  expect_equal(length(res$val), 1)
  expect_equal(res$val[[1]], 'a')

   # the results
   # A more complex example using actions:
   # A number parser: Extracts number
   peg<-new.parser()
   add_rule(peg, "NUM<-[0-9]+ ('.' [0-9]*)?)", act = "paste1(v)", des = "numbers")
   apply_rule(peg, "NUM", "12.3", exe=T)->res
  expect_equal(res$pos, 4)
  expect_equal(length(res$val), 1)
  expect_equal(res$val[[1]], "12.3")
})

 

test_that('[.PEGR"',
{
  # The simplest example: a parser that only accepts a single character 'a'
  # First create a new pegR
  peg<-new.parser()
  # Next we add the rule to the peg
  peg + "A<-'a'"
  # Next apply the rule to the string "a"
  peg[['A']]("a")->res 
  # to see the result  print(res)
  expect_equal(res$ok, TRUE)
  expect_equal(res$pos, 1)
  expect_equal(length(res$val), 1)
  expect_equal(res$val[[1]], 'a')
})

 
 

