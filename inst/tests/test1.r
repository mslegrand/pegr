#Unit tests
library(testthat)
#source("../../R/pegs.r")
pegE<-new.env()
pegE$.DEBUG.NODE<-F
DEVEL.DEBUG<-FALSE
pegE$DEVEL.DEBUG<-FALSE

#source("sComponents.r")
include.sComponents(pegE)
include.gComponents(pegE)
include.literal(pegE)
#set up
library(testthat)
library(stringr)


# sapply(letters, literal)->tmp
# sapply(LETTERS, mk.atom)->tmp
# rm(tmp)

sapply(letters, mk.atom)->tmp
sapply(LETTERS, mk.atom)->tmp
rm(tmp)


# DEVEL.DEBUG=FALSE),
# 
# sapply(letters, mk.atom)->tmp,
# sapply(LETTERS, mk.atom)->tmp,
# rm(tmp)

# 
# 
# 
rule.X<-(atom.x + opt.0x(rule.X) )

rule.A <- (rule.X + (atom.a / atom.b))

rule.B<- (rule.X + atom.b)

rule.S <- (rule.A + atom.c) / (rule.B + atom.d)


#regTest<-function(){
  #regression tests
  #atom.a("a") #expect ok,1

test_that("atom.a('a')",
{
  res<-atom.a("a")
  expect_that(res$pos, equals(1) )
  expect_that(res$ok, equals(TRUE) )  
})
  
test_that("atom.a+atom.b",
{
  res<-(atom.a + atom.b)("ab") #expect T 1
  expect_that(res$pos, equals(2) )
  expect_that(res$ok, equals(TRUE) )
})

test_that("atom.a + atom.b + atom.c + atom.d",
{
  res<-(atom.a + atom.b + atom.c + atom.d)("abcd")
  expect_that(res$pos, equals(4) )
  expect_that(res$ok, equals(TRUE) )
})

test_that("atom.a + atom.b + atom.c + atom.d (2)",
{
  res<-(atom.a + atom.b +atom.c +atom.d)("abce")
  expect_that(res$pos, equals(0) )
  expect_that(res$ok, equals(FALSE) )
})

test_that("atom.a / atom.b",
{
  res<-(atom.a / atom.b)("bx") # expect T 2
  expect_that(res$pos, equals(1) )
  expect_that(res$ok, equals(TRUE) )
})

test_that("atom.a/atom.b (2)",
{
  res<-(atom.a / atom.b)("xb") # expect F, 0
  expect_that(res$pos, equals(0) )
  expect_that(res$ok, equals(FALSE) )
})

test_that("atom.a/atom.b (3)",
{
  res<-(atom.a / atom.b)("bx") #expect T 1
  expect_that(res$pos, equals(1) )
  expect_that(res$ok, equals(TRUE) )
})

test_that("atom.a/atom.b (4)",
{
  res<-(atom.a / atom.b)("xb") # expect F, 0
  expect_that(res$pos, equals(0) )
  expect_that(res$ok, equals(FALSE) )
})

test_that("atom.a / atom.b /atom.c /atom.d",
{
  res<-(atom.a / atom.b /atom.c /atom.d)("cb")
  expect_that(res$pos, equals(1) )
  expect_that(res$ok, equals(TRUE) )
})

test_that("opt.01(atom.a)",
{
  res<-opt.01(atom.a)("xa",FALSE,2) #expect T, 1
  expect_that(res$pos, equals(1) )
  expect_that(res$ok, equals(TRUE) )
})

test_that("s.not(atom.b)",
{
  res<-s.not(atom.b)("ab",FALSE,2)
  expect_that(res$ok, equals(FALSE))
  expect_that(res$pos, equals(0))
})

test_that("s.not(atom.b) (2)",
{
  res<-s.not(atom.b)("ac",FALSE,2)
  expect_that(res$ok, equals(TRUE))
  expect_that(res$pos, equals(0))
})

test_that("atom.a + s.and(atom.b)",
{
  res<-(atom.a + s.and(atom.b)) ("ab")
  expect_that(res$ok, equals(TRUE))
  expect_that(res$pos, equals(1))
})

test_that("atom.a + s.not(s.and(atom.b))",
{
  res<-(atom.a + s.not(s.and(atom.b))) ("ab")
  expect_that(res$ok, equals(FALSE))
  expect_that(res$pos, equals(0))
})

test_that("atom.a + !s.and(atom.b) (2)",
{
  res<-(atom.a + s.not(s.and(atom.b))) ("ac")
  expect_that(res$ok, equals(TRUE))
  expect_that(res$pos, equals(1))  
})

test_that("rule.B('xb')",
{
  res<-rule.B("xb")
  expect_that(res$pos, equals(2) )
  expect_that(res$ok, equals(TRUE) )
})

test_that("rule.X('xxx')",
{
  res<-rule.X("xxx")
  expect_that(res$pos, equals(3) )
  expect_that(res$ok, equals(TRUE) )
})

test_that("rule.B('xxxb')",
{
  res<-rule.B("xxxb")
  expect_that(res$pos, equals(4) )
  expect_that(res$ok, equals(TRUE) )
})

test_that("rule.A('xxxa')",
{
  res<-rule.A("xxxa")
  expect_that(res$pos, equals(4) )
  expect_that(res$ok, equals(TRUE) )
})

test_that("rule.A('xxxb')",
{
  res<-rule.A("xxxb")
  expect_that(res$pos, equals(4) )
  expect_that(res$ok, equals(TRUE) )
})

# test_that("peg.parse",
# {
#   expect_that(peg.parse("xxx"), equals(FALSE) )
#   
#   expect_that(peg.parse("xxxbc"), equals(TRUE) )
#   
#   expect_that(peg.parse("xxxxbd"), equals(TRUE) )
# })
# 

test_that("s.sequence (1)",
{
  s.sequence(atom.a, atom.b, atom.c, atom.d)("abcd")->res
  expect_true(res$ok)
  expect_equal(res$pos,4)
})


test_that("s.sequence (2)",
{
  s.sequence(atom.a, atom.b, atom.c, atom.d)("abce")->res
  expect_false(res$ok)
  expect_equal(res$pos,0)
})

test_that("s.first (1)",
{
  s.first(atom.a, atom.b, atom.c, atom.d)("c")->res
  expect_true(res$ok)
  expect_equal(res$pos,1)
  expect_equal(res$val[[1]], "c")
})

test_that("s.first (2)",
{
  s.first(atom.a, atom.b, atom.c, atom.d)("e")->res
  expect_false(res$ok)
  expect_equal(res$pos,0)
})




#}

 #regTest()
# > "%_%" <- function(a,b) paste(a,b,sep="")
# > "Hello," %_% " " %_% "world"
# [1] "Hello, world"

# walk.rule<-function(s){
#   rtv<-list(start=list())
#   start
# }
# 
# h<-function(y){
#   if(!exists("x")){
#     x<-y
#   }
#   x
# }
#(atom.a +atom.b + atom.c)("abc")$val->tree
# l<-leaves(tree)
#tree
