library(testthat)

context("test_set_rule")

 

# test_that("SET_RULE",
# {
# expect_equal(0,1)
#    peg<-new.parser()
#    # Add rule A to recognize 'a' and return list('A') as it's value
#    add_rule(peg, "A<-'a'", act="list('A')")
#    value(apply_rule(peg, 'A', 'a', exe=TRUE))
#    set_rule(peg, "A<-'b'")
#    # Now A will only recognize 'b', so it will now fail on input 'a'
#    status(apply_rule(peg, 'A', 'a', exe=TRUE))
#    # However, A not recognizes b, but still returs list('A') as it's value
#    value(apply_rule(peg, 'A', 'b', exe=TRUE))
# })

 

test_that("[<-.PEGR",
{
   peg<-new.parser()
   invisible(peg + "A<-'a'")
   inspect_rule(peg,'A')->rs
  expect_equal(rs$name,"A")
  expect_equal(rs$def,"A<-'a'")
  expect_equal(rs$com,NULL)
  expect_equal(rs$act, NULL )

   peg[["A"]]<-"A<-'ab'"
inspect_rule(peg,'A')->rs
expect_equal(rs$name,"A")
expect_equal(rs$def,"A<-'ab'")
expect_equal(rs$com,NULL)
expect_equal(rs$act, NULL )

   peg[["A"]]<-c("A<-'xx'", des="replace xx by a", act="list('a')")
inspect_rule(peg,'A')->rs
expect_equal(rs$name,"A")
expect_equal(rs$def,"A<-'xx'")
expect_equal(rs$com,"replace xx by a")
expect_equal(rs$act[2], "list('a')" )
})

 

