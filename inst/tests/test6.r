library(testthat)

#source("generator.r")

#test  true for
#def literal true
test_that("DEFINITION",
{
  genE<-new.parser()
  add_rule(genE,"X<-[a-z]")->res
  expect_equal(res$parsed, "X<-[a-z]" )
  expect_equal(genE$pegE$.SOURCE.RULES[["X"]], "X<-[a-z]" )
  rules<-rule_ids(genE)
  expect_equal(rules[1],"X")
  apply_rule(genE,"X","b")->res2
  expect_equal(res2$val$atom,"b")
})


test_that("SET ACTION: TEXT",
{
  genE<-new.parser()
  add_rule(genE,"X<-.")->res
  expect_equal(res$parsed, "X<-." )
  set_action(genE,"X", "n<-length(v); v<-c(v,n); v<-paste(v,collapse='');list(v)")
  rules<-rule_ids(genE)
  expect_equal(rules[1],"X")
  apply_rule(genE,"X","b", exe=TRUE)->res2
  expect_equal(res2$val[[1]],"b1")
})


test_that("SET ACTION: FUNCTION",
{
  genE<-new.parser()
  add_rule(genE,"XX<-.")->res
  expect_equal(res$parsed, "XX<-." )
  fn<-function(v){n<-length(v)+1; v<-c(v,n); v<-paste(v,collapse='');list(v)}
  set_action(genE,"XX", fn)
  rules<-rule_ids(genE)
  expect_equal(rules[1],"XX")
  apply_rule(genE,"XX","b", exe=TRUE)->res2
  expect_equal(res2$val[[1]],"b2")
})

test_that("SET RULE DESCRIPTION",
{
  genE<-new.parser()
  add_rule(genE,"X<-.")->res
  expect_equal(res$parsed, "X<-." )
  rules<-rule_ids(genE)
  expect_equal(rules[1],"X")
  txt<-"any char"
  set_description(genE,"X",txt)
  txt2<-get_description(genE,"X")
  expect_equal(txt,txt2)
})

test_that("SET RULE DESCRIPTION",
{
  genE<-new.parser()
  add_rule(genE,"X<-.")->res
  expect_equal(res$parsed, "X<-." )
  expect_equal(genE$pegE$.SOURCE.RULES[["X"]], "X<-." )
  rules<-rule_ids(genE)
  expect_equal(rules[1],"X")
  txt<-"any char"
  set_description(genE,"X",txt)
  fn<-function(v){ n<-length(v)+1; v<-c(v,n); v<-paste(v,collapse='');list(v)}
  set_action(genE,"X", fn) 
  txt2<-get_description(genE,"X")
  expect_equal(txt,txt2)
  expect_equal(length(genE$pegE$.SOURCE.RULES),1)
  expect_equal(length(genE$pegE$.ACTION),1)
  expect_equal(length(genE$pegE$.RULE_DESCRIPT),1)
  delete_rule(genE,"X")
  expect_equal(length(genE$pegE$.SOURCE.RULES),0)
  expect_equal(length(genE$pegE$.ACTION),0)
  expect_equal(length(genE$pegE$.RULE_DESCRIPT),0)
})

test_that("DISPLAY TREE C<-A B",
{
  gen<-new.parser()
  add_rule(gen, "A<-'a'")
  add_rule(gen, "B<-'b'")
  add_rule(gen,"C<-A B")
  #gen$pegE$.DEBUG.NODE<-T
  apply_rule(gen,"C","ab", debugTree=TRUE)->res
  print("\n")
  tree(res)
})

test_that("PLOT TREE C<-A B",
{
  gen<-new.parser()
  add_rule(gen, "A<-'a'")
  add_rule(gen, "B<-'b'") 
  add_rule(gen, "D<-'d'")
  add_rule(gen, "C<-'c'")
  add_rule(gen,"ROOT<-A B C D")
  apply_rule(gen,"ROOT","abcd", debugTree=TRUE)->res
  plot(res)
})

