library(testthat)

#source("generator.r")

#test  true for
#def literal true
test_that("DEFINITION",
{
  genE<-new.generator()
  AddRule(genE,"X<-[a-z]")->res
  expect_equal(res$parsed, "X<-[a-z]" )
  expect_equal(genE$pegE$.SOURCE.RULES[["X"]], "X<-[a-z]" )
  rules<-RuleIds(genE)
  expect_equal(rules[1],"X")
  Parse(genE,"X","b")->res2
  expect_equal(res2$val$atom,"b")
})


test_that("SET ACTION: TEXT",
{
  genE<-new.generator()
  AddRule(genE,"X<-.")->res
  expect_equal(res$parsed, "X<-." )
  SetAction(genE,"X", "n<-length(v); v<-c(v,n); v<-paste(v,collapse='');list(v)")
  rules<-RuleIds(genE)
  expect_equal(rules[1],"X")
  Parse(genE,"X","b")->res2
  expect_equal(res2$val[[1]],"b1")
})


test_that("SET ACTION: FUNCTION",
{
  genE<-new.generator()
  AddRule(genE,"XX<-.")->res
  expect_equal(res$parsed, "XX<-." )
  fn<-function(v){n<-length(v)+1; v<-c(v,n); v<-paste(v,collapse='');list(v)}
  SetAction(genE,"XX", fn)
  rules<-RuleIds(genE)
  expect_equal(rules[1],"XX")
  Parse(genE,"XX","b")->res2
  expect_equal(res2$val[[1]],"b2")
})

test_that("SET RULE DESCRIPTION",
{
  genE<-new.generator()
  AddRule(genE,"X<-.")->res
  expect_equal(res$parsed, "X<-." )
  rules<-RuleIds(genE)
  expect_equal(rules[1],"X")
  txt<-"any char"
  SetDescription(genE,"X",txt)
  txt2<-GetDescription(genE,"X")
  expect_equal(txt,txt2)
})

test_that("SET RULE DESCRIPTION",
{
  genE<-new.generator()
  AddRule(genE,"X<-.")->res
  expect_equal(res$parsed, "X<-." )
  expect_equal(genE$pegE$.SOURCE.RULES[["X"]], "X<-." )
  rules<-RuleIds(genE)
  expect_equal(rules[1],"X")
  txt<-"any char"
  SetDescription(genE,"X",txt)
  fn<-function(v){ n<-length(v)+1; v<-c(v,n); v<-paste(v,collapse='');list(v)}
  SetAction(genE,"X", fn) 
  txt2<-GetDescription(genE,"X")
  expect_equal(txt,txt2)
  expect_equal(length(genE$pegE$.SOURCE.RULES),1)
  expect_equal(length(genE$pegE$.ACTION),1)
  expect_equal(length(genE$pegE$.RULE_DESCRIPT),1)
  DeleteRule(genE,"X")
  expect_equal(length(genE$pegE$.SOURCE.RULES),0)
  expect_equal(length(genE$pegE$.ACTION),0)
  expect_equal(length(genE$pegE$.RULE_DESCRIPT),0)
})

test_that("DISPLAY TREE C<-A B",
{
  gen<-new.generator()
  AddRule(gen, "A<-'a'")
  AddRule(gen, "B<-'b'")
  AddRule(gen,"C<-A B")
  #gen$pegE$.DEBUG.NODE<-T
  Parse(gen,"C","ab", debugTree=TRUE)->res
  DisplayTree(res)
})

test_that("PLOT TREE C<-A B",
{
  gen<-new.generator()
  AddRule(gen, "A<-'a'")
  AddRule(gen, "B<-'b'") 
  AddRule(gen, "D<-'d'")
  AddRule(gen, "C<-'c'")
  AddRule(gen,"ROOT<-A B C D")
  #gen$pegE$.DEBUG.NODE<-T
  Parse(gen,"ROOT","abcd", debugTree=TRUE)->res
  plot(res)
})

