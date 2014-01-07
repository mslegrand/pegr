library(testthat)
library(stringr)
pegE<-new.env()
pegE$.DEBUG.NODE<-FALSE
DEVEL.DEBUG<-FALSE

include.sComponents()
include.sComponents(pegE)
include.sConnectives(pegE)
include.gConnectives(pegE)

context("Test quote capabilities")

s.quote1<-s.atom("\'")
s.quote2<-s.atom("\"")
mk.atom('c')

special.characters<-c(
  quote.2="\"", quote.1="\'", plus="+", star="*", question="?", ampersand="&", paren.left="(", 
  paren.right=")", bracket.left="[", bracket.right="]", bang="!", slash.forward="/", space=" ", dot=".",
  EOL="\n", arrow.left="<-", arrow.right="->", colon=":", equals="=", minus="-", 
  dollar="$", at="@", pound="#", tilde="~", comma=",", less.than="<", greater.than=">", currly.left="{",
  curley.right="}", vertical="|", percent="%", slash.back="\\", hat="^", underscore="_",
  zero="0", one="1", two="2",three="3", four="4", five="5", six="6", seven="7", eight="8", nine="9"
  )

mk.atom.from.quote<-function(x){
  y<-x[c(-1,-length(x))]
  if(y[[1]] %in% special.characters){ #handle special names
     v<-names(special.characters)[match(y[[1]], special.characters)]
  } else{
     v<-paste(y,collapse="")
  } 
  mk.atom(v)
}



s.quote<- s.quote1 / s.quote2 < list("s.quote",NULL)
R.1Quote<-s.quote1 + opt.0x(s.not(s.quote1) + s.dot) +s.quote1 < list("R.1Quote", function(v){ mk.atom.from.quote(v);  v})
R.2Quote<-s.quote2 + opt.0x(s.not(s.quote2) + s.dot) + s.quote2 < list("R.2Quote", function(v){   mk.atom.from.quote(v);  v})
R.Quote<- R.1Quote / R.2Quote            < list("R.Quote",NULL)
R.NotQuote <- opt.1x( s.not(R.Quote) + s.dot) < list("R.NotQuote",NULL)
#R.NotQuote <- opt.0x( s.dot) 
R.String1<- ( R.NotQuote + R.Quote )  < list("R.String1",NULL)
R.String<- opt.1x( R.NotQuote / R.Quote )   < list("R.String2",NULL)


#setName<-function(rName){ obj<-get(rName); attr(obj,"rName")<-rName; rName}

#rules<-c("R.1Quote",   "R.2Quote",         "R.NotQuote", "R.Quote",    "R.String1",  "R.String2")

#sapply(rules, setName)

test_that("dot and opt.0x play together",{
  opt.0x(s.dot)("abcde")->res
  expect_that(res$ok, equals(TRUE) )
  expect_that(res$pos, equals(5) )
  expect_that(paste(res$val, collapse=""), equals("abcde"))
})

            
# test_that("dot, opt.0x and not play together",{
#   opt.0x( (!atom.c) + s.dot )("abc")->res
#   expect_true(res$ok )
#   expect_equal(res$pos, 2 )
#   expect_equal(paste(res$val, collapse=""), "ab")
# })


test_that("dot, opt.0x and not play together (1.5)",{
  (opt.0x( s.not(atom.c) + s.dot ) )("abcd")->res
  expect_true(res$ok )
  expect_equal(res$pos, 2 )
  expect_equal(paste(res$val, collapse=""), "ab")
})

test_that("dot, opt.0x and not play together (2)",{
  (opt.0x( s.not(atom.c) + s.dot ) + atom.c)("abcd")->res
  expect_true(res$ok )
  expect_equal(res$pos, 3 )
  expect_equal(paste(res$val, collapse=""), "abc")
})


test_that("dot, opt.0x and not play together with double quote",{
  opt.0x( s.not(s.quote) + s.dot )("ab\"xx")->res
  expect_true(res$ok )
  expect_equal(res$pos, 2 )
  expect_equal(paste(res$val, collapse=""), "ab")
})

test_that("double quote alone",{
  s.quote("\"",TRUE)->res
  expect_true(res$ok)
  expect_equal(res$pos,1)
  expect_equal(res$val[[1]],"\"")
})

test_that("a single double quote",{
(opt.0x(s.not(s.quote) + s.dot) + s.quote )("ab\"xx")->res
expect_true(res$ok )
expect_equal(res$pos, 3 )
expect_equal(paste(res$val, collapse=""), "ab\"")
})


test_that("R.2Quote",{
  R.2Quote("\"hello\"",TRUE)->res
  expect_true(res$ok)
  expect_equal(res$pos, 7)
  expect_equal(paste(res$val, collapse=""), "\"hello\"")
  expect_true(exists("atom.hello"))
  #delete.all.atoms()
})

test_that("R.Quote",{
  R.Quote("\'hi\'",TRUE)->res
  expect_true(res$ok)
  expect_equal(res$pos, 4)
  expect_equal(paste(res$val, collapse=""), "\'hi\'")
  expect_true(exists("atom.hi"))
  #delete.all.atoms()
})


test_that("R.NotQuote",{
  R.NotQuote("abc",TRUE)->res
  expect_true(res$ok)
  expect_equal(res$pos, 3)
  expect_equal(paste(res$val, collapse=""), "abc")
  #delete.all.atoms()  
})


test_that("R.NotQuote (2)",{
  R.NotQuote("abcd\"lovely\"",TRUE)->res
  expect_true(res$ok)
  expect_equal(res$pos, 4)
  expect_equal(paste(res$val, collapse=""), "abcd")
  #delete.all.atoms()
})

#DEBUG.PEG=TRUE

test_that("R.String ", {
R.String("bird \"cat\"",TRUE)->res
expect_true(res$ok)
expect_equal(res$pos, 10)
expect_equal(paste(res$val, collapse=""), "bird \"cat\"")
expect_true(exists("atom.cat"))
#rm("atom.cat", envir=globalenv())
})


test_that("R.String (2) ", {
  R.String("bird \"cat\" \"dog\" pig ",TRUE)->res
  expect_true(res$ok)
  expect_equal(res$pos, 21)
  expect_equal(paste(res$val, collapse=""), "bird \"cat\" \"dog\" pig ")
  expect_true(exists("atom.cat"))
  expect_true(exists("atom.dog"))  
#   rm("atom.cat", envir=globalenv())
#   rm("atom.dog", envir=globalenv())
})




          
# test_that("R.String",{
#   R.String("bird \"cat\"")->res
#   expect_true(res$ok)
#   expect_equal(res$pos, 7)
#   expect_equal(paste(res$val, collapse=""), "abc")
#   #delete.all.atoms()  
# })



# ll<-ls(pattern="^atom\\.*")
# rm(ll)

# R.String("bird \"cat\"")->res
# expect_true(res$ok)
# expect_equal(res$pos, 7)
# expect_equal(paste(res$val, collapse=""), "\"cat\"")
# expect_true(exists("atom.cat"))
# #delete.all.atoms()
# res



# s.quote("\"")->v
# print(v)



# R.2Quote("\"hello\"")->res
# print(res)

# rm( pattern="$atom.* ")

# R.String("hello \"xxx\" for all")->v
# print(v)