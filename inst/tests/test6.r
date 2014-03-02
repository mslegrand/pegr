library(testthat)

context("Test6")

test_that("DEFINITION",
{
  peg<-new.parser()
  peg<-add_rule(peg,"X<-[a-z]")
  #expect_equal(res$parsed, "X<-[a-z]" )
  expect_equal(peg$pegE$.SOURCE.RULES[["X"]], "X<-[a-z]" )
  rules<-rule_ids(peg)
  expect_equal(rules[1],"X")
  apply_rule(peg,"X","b")->res2
  expect_equal(res2$val$atom,"b")
})


test_that("SET ACTION: TEXT",
{
  peg<-new.parser()
  peg<-add_rule(peg,"X<-.")
  #expect_equal(res$parsed, "X<-." )
  peg<-set_action(peg,"X", "n<-length(v); v<-c(v,n); v<-paste(v,collapse='');list(v)")
  rules<-rule_ids(peg)
  expect_equal(rules[1],"X")
  apply_rule(peg,"X","b", exe=TRUE)->res2
  expect_equal(res2$val[[1]],"b1")
})


test_that("SET ACTION: FUNCTION 1",
{
  peg<-new.parser()
  peg<-add_rule(peg,"XX<-.")
  #expect_equal(res$parsed, "XX<-." )
  fn<-function(v){n<-length(v)+1; v<-c(v,n); v<-paste(v,collapse='');list(v)}
  peg<-set_action(peg,"XX", "fn<-get('fn'); fn(v)")
  rules<-rule_ids(peg)
  expect_equal(rules[1],"XX")
  #browser()
  apply_rule(peg,"XX","b", exe=TRUE)->res2
  expect_equal(res2$val[[1]],"b2")
})

test_that("SET RULE DESCRIPTION",
{
  peg<-new.parser()
  peg<-add_rule(peg,"X<-.")
  #expect_equal(res$parsed, "X<-." )
  rules<-rule_ids(peg)
  expect_equal(rules[1],"X")
  txt<-"any char"
  peg<-set_description(peg,"X",txt)
  txt2<-get_description(peg,"X")
  expect_equal(txt,txt2)
})

test_that("SET RULE DESCRIPTION",
{
  peg<-new.parser()
  peg<-add_rule(peg,"X<-.")
  #expect_equal(res$parsed, "X<-." )
  expect_equal(peg$pegE$.SOURCE.RULES[["X"]], "X<-." )
  rules<-rule_ids(peg)
  expect_equal(rules[1],"X")
  txt<-"any char"
  peg<-set_description(peg,"X",txt)
  fn<-function(v){ n<-length(v)+1; v<-c(v,n); v<-paste(v,collapse='');list(v)}
  peg<-set_action(peg,"X", "fn(v)") 
  txt2<-get_description(peg,"X")
  expect_equal(txt,txt2)
  expect_equal(length(peg$pegE$.SOURCE.RULES),1)
  expect_equal(length(peg$pegE$.ACTION),1)
  expect_equal(length(peg$pegE$.RULE_DESCRIPT),1)
  peg<-delete_rule(peg,"X")
  expect_equal(length(peg$pegE$.SOURCE.RULES),0)
  expect_equal(length(peg$pegE$.ACTION),0)
  expect_equal(length(peg$pegE$.RULE_DESCRIPT),0)
})

test_that("DISPLAY TREE C<-A B",
{
  peg<-new.parser()
  peg<-add_rule(peg, "A<-'a'")
  peg<-add_rule(peg, "B<-'b'")
  peg<-add_rule(peg,"C<-A B")
  #peg$pegE$.RECORD.NODE<-T
  apply_rule(peg,"C","ab", record=TRUE)->res
  s<-c("____C(ab) = list(a, b )",  "    |____A(a) = list(a )", "    |____B(b) = list(b )" )
  capture.output(tree(res))->out.s
  expect_equal(out.s, s)
})

test_that("PLOT TREE C<-A B",
{
  peg<-new.parser()
  peg<-add_rule(peg, "A<-'a'")
  peg<-add_rule(peg, "B<-'b'") 
  peg<-add_rule(peg, "D<-'d'")
  peg<-add_rule(peg, "C<-'c'")
  peg<-add_rule(peg,"ROOT<-A B C D")
  apply_rule(peg,"ROOT","abcd", record=TRUE)->res
  plot(res)
})

