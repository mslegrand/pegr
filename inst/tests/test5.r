library(testthat)

#source("generator.r")


as.ID=function(v){
  class(v)<-c("peg.name",class(v))
  v
}

#test  true for
#def literal true
test_that("DEFINITION TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "xxx<-'y'")->res
  expect_true(res$ok )
  expect_equal(res$val$nodeName, as.ID("xxx"))
  expect_equal(res$pos, 8 )
  expect_true(exists("atom.y", envir=gen$pegE))
  # expect_equal(paste(res$val, collapse=""), "x<-y<<myfunction")  
  rm("atom.y",envir=gen$pegE)
  rm("xxx",envir=gen$pegE)
})


#def dot true
test_that("DOT TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "xxx<-.")->res
  expect_true(res$ok )
  expect_equal(res$pos, 6 )
  res2<-gen$pegE$xxx("z")
  expect_equal(res2$val[[1]],"z")
  rm(xxx, envir=gen$pegE)
})

#def sequence true
#def class true
#def dot true
test_that("CLASS 1 TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-[a-z]")->res
  expect_true(res$ok )
  expect_equal(res$pos, 8 )
  res2<-gen$pegE$X("z")
  #res2
  expect_equal(res2$val[[1]],"z")
  rm(X, envir=gen$pegE)
})

test_that("CLASS 2 TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-[z]")->res
  expect_true(res$ok )
  expect_equal(res$pos, 6 )
  res2<-gen$pegE$X("z")
  #res2
  expect_equal(res2$val[[1]],"z")
  rm(X, envir=gen$pegE)
})

test_that("SEQUENCE TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' 'b'")->res
  expect_true(res$ok )
  expect_equal(res$pos, 10 )
  res2<-gen$pegE$X("ab")
  #res2
  expect_equal(paste(res2$val,collapse=""),"ab")
  rm(X, envir=gen$pegE)
})

test_that("SEQUENCE TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' 'b'")->res
  expect_true(res$ok )
  expect_equal(res$pos, 10 )
  res2<-gen$pegE$X("ab")
  #res2
  expect_equal(paste(res2$val,collapse=""),"ab")
  rm(X, envir=gen$pegE)
})

test_that("SEQUENCE TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' 'b' 'c'")->res
  expect_true(res$ok )
  expect_equal(res$pos, 14 )
  res2<-gen$pegE$X("abc")
  #res2
  expect_equal(paste(res2$val,collapse=""),"abc")
  rm(X, envir=gen$pegE)
})

#def question true---------------------------------
test_that("QUESTION 1 TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' 'b'?")->res
  expect_true(res$ok )
  expect_equal(res$pos, 11 )
  res2<-gen$pegE$X("ab")
  # res2
  expect_equal(paste(res2$val,collapse=""),"ab")
  rm(X, envir=gen$pegE)
})

test_that("QUESTION 0 TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' 'b'?")->res
  expect_true(res$ok )
  expect_equal(res$pos, 11 )
  res2<-gen$pegE$X("a")
  # res2
  expect_equal(paste(res2$val,collapse=""),"a")
  rm(X, envir=gen$pegE)
})

#def star true-------------------------------------------------------------

test_that("STAR 1 TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' 'b'*")->res
  expect_true(res$ok )
  expect_equal(res$pos, 11 )
  res2<-gen$pegE$X("abb")
  # res2
  expect_equal(paste(res2$val,collapse=""),"abb")
  rm(X, envir=gen$pegE)
})

test_that("STAR 0 TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' 'b'*")->res
  expect_true(res$ok )
  expect_equal(res$pos, 11 )
  res2<-gen$pegE$X("a")
  # res2
  expect_equal(paste(res2$val,collapse=""),"a")
  rm(X, envir=gen$pegE)
})

#def plus true -------------------------------------------------------

test_that("PLUS 1 TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' 'b'+")->res
  expect_true(res$ok )
  expect_equal(res$pos, 11 )
  res2<-gen$pegE$X("abb")
  # res2
  expect_equal(paste(res2$val,collapse=""),"abb")
  rm(X, envir=gen$pegE)
})

test_that("PLUS 0 TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' 'b'+")->res
  expect_true(res$ok )
  expect_equal(res$pos, 11 )
  res2<-gen$pegE$X("a")
  #  res2
  expect_false(res2$ok)
  #expect_equal(paste(res2$val,collapse=""),"a")
  rm(X, envir=gen$pegE)
})

# def NOT TRUE ---------------------------------------------------

test_that("NOT 0 TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a'* !'b' .")->res
  expect_true(res$ok )
  expect_equal(res$pos, 14 )
  res2<-gen$pegE$X("aac")
  expect_equal(paste(res2$val,collapse=""),"aac")
  rm(X, envir=gen$pegE)
})

test_that("NOT 1 TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a'* !'b' .")->res
  expect_true(res$ok )
  expect_equal(res$pos, 14 )
  res2<-gen$pegE$X("abc")
  expect_false(res2$ok)
  rm(X, envir=gen$pegE)
})

#def  EXPRESSION---------------------------------------------------

test_that("EXPRESSION TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' / 'b' / 'c' .")->res
  expect_true(res$ok )
  expect_equal(res$pos, 20 )
  res2<-gen$pegE$X("acd")
  expect_equal(paste(res2$val,collapse=""),"a")
  rm(X, envir=gen$pegE)
})

test_that("EXPRESSION TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' / 'b' / 'c'")->res
  expect_true(res$ok )
  #expect_equal(res$pos, 20 )
  res2<-gen$pegE$X("c")
  res2
  expect_equal(paste(res2$val,collapse=""),"c")
  rm(X, envir=gen$pegE)
})

test_that("EXPRESSION TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' / 'b' / 'c' .")->res
  expect_true(res$ok )
  expect_equal(res$pos, 20 )
  res2<-gen$pegE$X("bd")
  expect_equal(paste(res2$val,collapse=""),"b")
  rm(X, envir=gen$pegE)
})

test_that("EXPRESSION TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' / 'b' / 'c' .")->res
  expect_true(res$ok )
  expect_equal(res$pos, 20 )
  res2<-gen$pegE$X("bd")
  expect_equal(paste(res2$val,collapse=""),"b")
  rm(X, envir=gen$pegE)
})

test_that("EXPRESSION TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' / 'b' / 'c' .")->res
  expect_true(res$ok )
  expect_equal(res$pos, 20 )
  res2<-gen$pegE$X("cd")
  expect_equal(paste(res2$val,collapse=""),"cd")
  rm(X, envir=gen$pegE)
})

test_that("OPEN CLOSE TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' / 'b' 'd' / 'c' .")->res
  expect_true(res$ok)
  expect_equal(res$pos, 24 )
  res2<-gen$pegE$X("bd")
  expect_equal(paste(res2$val,collapse=""),"bd")
  rm(X, envir=gen$pegE)
})

test_that("OPEN CLOSE TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a' / ('b' 'd') / 'c' .")->res
  expect_true(res$ok )
  expect_equal(res$pos, 26 )
  res2<-gen$pegE$X("bd")
  expect_equal(paste(res2$val,collapse=""),"bd")
  rm(X, envir=gen$pegE)
})

test_that("OPEN CLOSE TRUE",
{          
  gen<-new.parser()
  gen$DEFINITION(  "X<-('a' / 'b') ('d' / 'c' .)")->res
  expect_true(res$ok )
  expect_equal(res$pos, 28 )
  res2<-gen$pegE$X("bce")
  expect_equal(paste(res2$val,collapse=""),"bce")
  rm(X, envir=gen$pegE)
}
)

# def AND TRUE ---------------------------------------------------

test_that("AND 1 TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a'* &'b'")->res
  expect_true(res$ok )
  expect_equal(res$pos, 12 )
  res2<-gen$pegE$X("aab")
  expect_equal(res2$pos,2)
  expect_equal(paste(res2$val,collapse=""),"aa")
  rm(X, envir=gen$pegE)
})

test_that("AND 0 TRUE",
{
  gen<-new.parser()
  gen$DEFINITION(  "X<-'a'* &'b'")->res
  expect_true(res$ok )
  expect_equal(res$pos, 12 )
  expect_equal(res$val$nodeName,as.ID("X"))
  res2<-gen$pegE$X("aac")
  expect_false(res2$ok)
  rm(X, envir=gen$pegE)
})

#def ACTION AND TRUE ----------------------------------------------------
test_that("ACTION 1 TRUE",
{
  gen<-new.parser()
  gen$pegE$.AUTO_ACTION<-TRUE
  ff<<-function(v){  n<-length(v); v<-c(v,n); v }
  gen$DEFINITION(  "X<-'a'* { ff } ")->res
  expect_true(res$ok )
  expect_equal(res$pos, 14 )
  res2<-gen$pegE$X("aaaa",TRUE)
  expect_equal(res2$pos,4)
  expect_equal(paste(res2$val,collapse=""),"aaaa4")
  rm(X, envir=gen$pegE)
  rm(ff,pos=1)
})

test_that("SUBSTITUTE true",
{ 
  gen<-new.parser()
  gen$pegE$.AUTO_ACTION<-TRUE
  gen$DEFINITION(  "X<-'a'")->res
  gen$DEFINITION(  "Y<-X")->res
  gen$pegE$Y('a',TRUE)->res
  expect_true(res$ok)
  expect_equal(res$pos,1)
  expect_equal(res$val[[1]],"a")
  rm(X,Y, envir=gen$pegE)
})

#--------------------------------------------------------------------------------------------
test_that("ACTION 2 TRUE",
{
  gen<-new.parser()
  gen$pegE$.AUTO_ACTION<-TRUE
  ff<<-function(v){ n<-length(v); v<-c(v,n); v }
  gg<<-function(v){ v<-paste(v,collapse="_"); v}
  gen$DEFINITION( "X<-'a'* { ff }" )->res1
  gen$DEFINITION( "Y<-X 'b'{gg}")->res2
  gen$DEFINITION(  "Y<-X 'b' { gg}")->ress  
  gen$pegE$Y("aaaab")->res2
  expect_true(res2$ok)
  expect_equal(res2$pos,5)
  expect_equal(res2$val, "a_a_a_a_4_b")
  rm(X,  Y,  envir=gen$pegE)
  rm(ff,gg,pos=1)
})

test_that("ACTION 3 TRUE",
{
  gen<-new.parser()
  gen$pegE$.AUTO_ACTION<-TRUE
  ff<<-function(v){ n<-length(v); v<-c(v,n); v }
  gg<<-function(v){ v<-paste(v,collapse="*"); v}
  gen$DEFINITION( "X<-'a'* {ff}")->res1
  gen$DEFINITION(  "Y<-X 'b' {gg} ")->ress
  local({
    Y("aaaab",TRUE)->res2
    expect_true(res2$ok)
    expect_equal(res2$pos,5)
    expect_equal(res2$val, "a*a*a*a*4*b")    
  }, envir=gen$pegE)
  rm(X,  Y,  envir=gen$pegE)
  rm(ff,gg, pos=1)
})

test_that("DEBUG NODE X<-'a' ; Y<-X 'b' ",
{
  gen<-new.parser()
  #gen$GRAMMAR("X<-'a'*  \nY<-X 'b'")->res
  gen$DEFINITION(  "X<-'a'* ")->res1
  gen$DEFINITION(  "Y<-X 'b'")->res2
  #assign(".DEBUG.NODE", TRUE, envir=gen$pegE)
  gen$pegE$.DEBUG.NODE<-T
  gen$pegE$Y("aaaab",TRUE)->res
  dbNode<-res$debugNode[[1]]
  expect_equal(as.character(dbNode$name),"Y")
  dbChildren<-dbNode$children
  expect_equal(length(dbChildren),1)
  dbChild<-dbChildren[[1]]
  expect_equal(as.character(dbChild$name),"X")
  expect_equal(length(dbChild$children),0)
  rm("Y",envir=gen$pegE)
  rm("X",envir=gen$pegE)
  #rm(".DEBUG.NODE",envir=gen$pegE)
  gen$pegE$.DEBUG.NODE<-F
})

test_that("DEBUG NODE C<-A B",
{
  gen<-new.parser()
 #gen$GRAMMAR("A<-'a'\nB<-'b'\n C<-A B")->res
 gen$DEFINITION(  "A<-'a'")->res
 gen$DEFINITION(  "B<-'b'")->res
 gen$DEFINITION(  "C<-A B")->res
 #assign(".DEBUG.NODE", TRUE, envir=gen$pegE)
  gen$pegE$.DEBUG.NODE<-T
  gen$pegE$C("ab",TRUE)->res
  #expect_equal(length(res$debugNode),4) #replace with expect is of class node
  expect_true("node" %in% class( res$debugNode[[1]] ))
  dbNodeC<-res$debugNode[[1]]
  expect_equal(as.character(dbNodeC$name),"C")
  dbChildrenC<-dbNodeC$children
  expect_equal(length(dbChildrenC),2)
  dbChildA<-dbChildrenC[[1]]
  expect_equal(as.character(dbChildA$name),"A")
  expect_equal(length(dbChildA$children),0)
  dbChildB<-dbChildrenC[[2]]
  expect_equal(as.character(dbChildB$name),"B")
  expect_equal(length(dbChildB$children),0)
  rm("A",envir=gen$pegE)
  rm("B",envir=gen$pegE) 
  rm("C",envir=gen$pegE)
  #rm(".DEBUG.NODE",envir=gen$pegE) 
  gen$pegE$.DEBUG.NODE<-F
})

test_that("DEBUG NODE C<-A / B",
{
  gen<-new.parser()
  #gen$GRAMMAR("A<-'a'\nB<-'b'\n C<-A / B")->res
  gen$DEFINITION(  "A<-'a'")->res
  gen$DEFINITION(  "B<-'b'")->res
  gen$DEFINITION(  "C<-A / B")->res
  #assign(".DEBUG.NODE", TRUE, envir=gen$pegE)
  gen$pegE$.DEBUG.NODE<-T
  gen$pegE$C("a",TRUE)->res
  expect_true("node" %in% class(res$debugNode[[1]]))
  dbNodeC<-res$debugNode[[1]]
  expect_equal(as.character(dbNodeC$name),"C")
  dbChildrenC<-dbNodeC$children
  expect_equal(length(dbChildrenC),1)
  dbChild<-dbChildrenC[[1]]
  expect_equal(as.character(dbChild$name),"A")
  expect_equal(length(dbChild$children),0)
  
  gen$pegE$C("b",TRUE)->res
  #expect_equal(length(res$debugNode),1)
  expect_true("node" %in% class(res$debugNode[[1]]))
  
  dbNodeC<-res$debugNode[[1]]
  expect_equal(as.character(dbNodeC$name),"C")
  dbChildrenC<-dbNodeC$children
  expect_equal(length(dbChildrenC),1)
  dbChild<-dbChildrenC[[1]]
  expect_equal(as.character(dbChild$name),"B")
  expect_equal(length(dbChild$children),0)
  rm("A",envir=gen$pegE)
  rm("B",envir=gen$pegE) 
  rm("C",envir=gen$pegE)
  #rm(".DEBUG.NODE",envir=gen$pegE) 
  gen$pegE$.DEBUG.NODE<-F
})

