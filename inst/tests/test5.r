library(testthat)

#source("generator.r")


#test  true for
#def literal true
test_that("DEFINITION TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("xxx<-'y'", TRUE)->res
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
  gen<-new.generator()
  gen$DEFINITION("xxx<-.", TRUE)->res
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
  gen<-new.generator()
  gen$DEFINITION("X<-[a-z]", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 8 )
  res2<-gen$pegE$X("z")
  #res2
  expect_equal(res2$val[[1]],"z")
  rm(X, envir=gen$pegE)
})

test_that("CLASS 2 TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-[z]", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 6 )
  res2<-gen$pegE$X("z")
  #res2
  expect_equal(res2$val[[1]],"z")
  rm(X, envir=gen$pegE)
})

test_that("SEQUENCE TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-'a' 'b'", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 10 )
  res2<-gen$pegE$X("ab")
  #res2
  expect_equal(paste(res2$val,collapse=""),"ab")
  rm(X, envir=gen$pegE)
})

test_that("SEQUENCE TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-'a' 'b'", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 10 )
  res2<-gen$pegE$X("ab")
  #res2
  expect_equal(paste(res2$val,collapse=""),"ab")
  rm(X, envir=gen$pegE)
})

test_that("SEQUENCE TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-'a' 'b' 'c'", TRUE)->res
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
  gen<-new.generator()
  gen$DEFINITION("X<-'a' 'b'?", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 11 )
  res2<-gen$pegE$X("ab")
  # res2
  expect_equal(paste(res2$val,collapse=""),"ab")
  rm(X, envir=gen$pegE)
})

test_that("QUESTION 0 TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-'a' 'b'?", TRUE)->res
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
  gen<-new.generator()
  gen$DEFINITION("X<-'a' 'b'*", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 11 )
  res2<-gen$pegE$X("abb")
  # res2
  expect_equal(paste(res2$val,collapse=""),"abb")
  rm(X, envir=gen$pegE)
})

test_that("STAR 0 TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-'a' 'b'*", TRUE)->res
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
  gen<-new.generator()
  gen$DEFINITION("X<-'a' 'b'+", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 11 )
  res2<-gen$pegE$X("abb")
  # res2
  expect_equal(paste(res2$val,collapse=""),"abb")
  rm(X, envir=gen$pegE)
})

test_that("PLUS 0 TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-'a' 'b'+", TRUE)->res
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
  gen<-new.generator()
  gen$DEFINITION("X<-'a'* !'b' .", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 14 )
  res2<-gen$pegE$X("aac")
  expect_equal(paste(res2$val,collapse=""),"aac")
  rm(X, envir=gen$pegE)
})

test_that("NOT 1 TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-'a'* !'b' .", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 14 )
  res2<-gen$pegE$X("abc")
  expect_false(res2$ok)
  rm(X, envir=gen$pegE)
})

#def  EXPRESSION---------------------------------------------------

test_that("EXPRESSION TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-'a' / 'b' / 'c' .", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 20 )
  res2<-gen$pegE$X("acd")
  expect_equal(paste(res2$val,collapse=""),"a")
  rm(X, envir=gen$pegE)
})

test_that("EXPRESSION TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-'a' / 'b' / 'c'", TRUE)->res
  expect_true(res$ok )
  #expect_equal(res$pos, 20 )
  res2<-gen$pegE$X("c")
  res2
  expect_equal(paste(res2$val,collapse=""),"c")
  rm(X, envir=gen$pegE)
})

test_that("EXPRESSION TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-'a' / 'b' / 'c' .", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 20 )
  res2<-gen$pegE$X("bd")
  expect_equal(paste(res2$val,collapse=""),"b")
  rm(X, envir=gen$pegE)
})

test_that("EXPRESSION TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-'a' / 'b' / 'c' .", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 20 )
  res2<-gen$pegE$X("bd")
  expect_equal(paste(res2$val,collapse=""),"b")
  rm(X, envir=gen$pegE)
})

test_that("EXPRESSION TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-'a' / 'b' / 'c' .", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 20 )
  res2<-gen$pegE$X("cd")
  expect_equal(paste(res2$val,collapse=""),"cd")
  rm(X, envir=gen$pegE)
})

test_that("OPEN CLOSE TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-'a' / 'b' 'd' / 'c' .", TRUE)->res
  expect_true(res$ok)
  expect_equal(res$pos, 24 )
  res2<-gen$pegE$X("bd")
  expect_equal(paste(res2$val,collapse=""),"bd")
  rm(X, envir=gen$pegE)
})

test_that("OPEN CLOSE TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-'a' / ('b' 'd') / 'c' .", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 26 )
  res2<-gen$pegE$X("bd")
  expect_equal(paste(res2$val,collapse=""),"bd")
  rm(X, envir=gen$pegE)
})

test_that("OPEN CLOSE TRUE",
{          
  gen<-new.generator()
  gen$DEFINITION("X<-('a' / 'b') ('d' / 'c' .)", TRUE)->res
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
  gen<-new.generator()
  gen$DEFINITION("X<-'a'* &'b'", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 12 )
  res2<-gen$pegE$X("aab")
  expect_equal(res2$pos,2)
  expect_equal(paste(res2$val,collapse=""),"aa")
  rm(X, envir=gen$pegE)
})

test_that("AND 0 TRUE",
{
  gen<-new.generator()
  gen$DEFINITION("X<-'a'* &'b'", TRUE)->res
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
  gen<-new.generator()
  ff<<-function(v){  n<-length(v); v<-c(v,n); v }
  gen$DEFINITION("X<-'a'* { ff } ", TRUE)->res
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
  gen<-new.generator()
  gen$DEFINITION("X<-'a'",TRUE)->res
  gen$DEFINITION("Y<-X",TRUE)->res
  gen$pegE$Y('a',TRUE)->res
  expect_true(res$ok)
  expect_equal(res$pos,1)
  expect_equal(res$val[[1]],"a")
  rm(X,Y, envir=gen$pegE)
})

test_that("ACTION 2 TRUE",
{
  gen<-new.generator()
  ff<<-function(v){ n<-length(v); v<-c(v,n); v }
  gg<<-function(v){ v<-paste(v,collapse="_"); v}
  gen$GRAMMAR("X<-'a'* { ff } \nY<-X 'b'{gg} ", TRUE)->res
  gen$DEFINITION("Y<-X 'b' { gg}",TRUE)->ress  
  gen$pegE$Y("aaaab",TRUE)->res2
  expect_true(res2$ok)
  expect_equal(res2$pos,5)
  expect_equal(res2$val, "a_a_a_a_4_b")
  rm(X,  Y,  envir=gen$pegE)
  rm(ff,gg,pos=1)
})

test_that("ACTION 3 TRUE",
{
  gen<-new.generator()
  ff<<-function(v){ n<-length(v); v<-c(v,n); v }
  gg<<-function(v){ v<-paste(v,collapse="*"); v}
  gen$GRAMMAR("X<-'a'* {ff}  \nY<-X 'b' {gg}", TRUE)->res
  gen$DEFINITION("Y<-X 'b' {gg} ",TRUE)->ress
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
  gen<-new.generator()
  gen$GRAMMAR("X<-'a'*  \nY<-X 'b'", TRUE)->res
  #assign(".DEBUG.NODE", TRUE, envir=gen$pegE)
  gen$pegE$.DEBUG.NODE<-T
  gen$pegE$Y("aaaab",TRUE)->res
  dbNode<-res$debugNode
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
  gen<-new.generator()
  gen$GRAMMAR("A<-'a'\nB<-'b'\n C<-A B", TRUE)->res
  #assign(".DEBUG.NODE", TRUE, envir=gen$pegE)
  gen$pegE$.DEBUG.NODE<-T
  gen$pegE$C("ab",TRUE)->res
  #expect_equal(length(res$debugNode),4) #replace with expect is of class node
  expect_true("node" %in% class(res$debugNode))
  dbNodeC<-res$debugNode
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
  gen<-new.generator()
  gen$GRAMMAR("A<-'a'\nB<-'b'\n C<-A / B", TRUE)->res
  #assign(".DEBUG.NODE", TRUE, envir=gen$pegE)
  gen$pegE$.DEBUG.NODE<-T
  gen$pegE$C("a",TRUE)->res
  expect_true("node" %in% class(res$debugNode))
  dbNodeC<-res$debugNode
  expect_equal(as.character(dbNodeC$name),"C")
  dbChildrenC<-dbNodeC$children
  expect_equal(length(dbChildrenC),1)
  dbChild<-dbChildrenC[[1]]
  expect_equal(as.character(dbChild$name),"A")
  expect_equal(length(dbChild$children),0)
  
  gen$pegE$C("b",TRUE)->res
  #expect_equal(length(res$debugNode),1)
  expect_true("node" %in% class(res$debugNode))
  
  dbNodeC<-res$debugNode
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
