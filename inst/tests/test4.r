library(testthat)

#source("generator.r")

pegR<-new.parser()
gen<-pegR$getGenE() #slimmly way to keep unit test working

# used in  unittest4 unitTest4 
as.ID=function(v){
  class(v)<-c("peg.name",class(v))
  v
}

#we need to do some tests on this grammer,maybe first echo the input
test_that("SPACE",
{
  gen$SPACE(" ",FALSE)->res
  expect_true(res$ok)
  expect_equal(res$pos,1)
  expect_equal(paste(res$val,collapse=""), " ")
}
)

test_that("SPACE",
{
  gen$SPACE(" ",TRUE)->res
  expect_true(res$ok)
  expect_equal(res$pos,1)
  expect_equal(paste(res$val,collapse=""), "")
}
)

test_that("STAR",
{
  gen$STAR("* x")->res
  expect_true(res$ok)
  expect_equal(res$pos,2)
  expect_equal(paste(res$val,collapse=""), "*")
}
)


#test range
test_that("range(a-z)",
{
  gen$s.range("a","b")("a1")->res
  expect_true(res$ok)
  expect_equal(res$pos,1)
  expect_equal(paste(res$val,collapse=""), "a")
}
)

#test IDENTSTART
test_that("IDENTSTART('B1')",
{
  gen$IDENTSTART("B1",FALSE)->res
  expect_true(res$ok)
  expect_equal(res$pos,1)
  expect_equal(paste(res$val,collapse=""), "B")
}
)


#test IDENTIFIER
test_that("IDENTCONT('B1')",
{
  gen$IDENTCONT("B1",p=2 )->res
  expect_true(res$ok)
  expect_equal(res$pos,1)
  expect_equal(paste(res$val,collapse=""), "1")
}
)

#test IDENTIFIER
test_that("IDENTIFIER('B1') FALSE",
{
  gen$IDENTIFIER("B1",FALSE)->res
  #print(res)
  expect_true(res$ok)
  expect_equal(res$pos,2)
  expect_equal(paste(res$val,collapse=""), "B1")
}
)

#test IDENTIFIER
test_that("IDENTIFIER('B1') TRUE",
{
  gen$IDENTIFIER("B1",TRUE)->res
  expect_true(res$ok)
  expect_equal(res$pos,2)
  expect_equal(res$val$ID, as.ID("B1"))
}
)

#test primary
test_that("identifier('abc')",{
  gen$IDENTIFIER("abc",FALSE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 3 )
  expect_equal(paste(res$val, collapse=""), "abc")
})

#test PRIMARY
test_that("PRIMARY('B1',FALSE)",
{
  gen$PRIMARY("B1",FALSE)->res
  expect_true(res$ok)
  expect_equal(res$pos,2)
  expect_equal(paste(res$val,collapse=""), "B1")
}
)


#test leftarrow
test_that("x<-y",{
  gen$DEFINITION("x<-y",FALSE)->res
  #print(res)
  expect_true(res$ok )
  expect_equal(res$pos, 4 )
  expect_equal(paste(res$val, collapse=""), "x<-y")
})


#test suffix
test_that("suffix primary",{
  gen$SUFFIX("xy",FALSE)->res
  #print(res)
  expect_true(res$ok )
  expect_equal(res$pos, 2 )
  expect_equal(paste(res$val, collapse=""), "xy")
})

test_that("suffix ?",{
  gen$SUFFIX("xy?",FALSE)->res
 # print(res)
  expect_true(res$ok )
  expect_equal(res$pos, 3 )
  expect_equal(paste(res$val, collapse=""), "xy?")
})

test_that("suffix *",{
  gen$SUFFIX("xy*",FALSE)->res
  #print(res)
  expect_true(res$ok )
  expect_equal(res$pos, 3 )
  expect_equal(paste(res$val, collapse=""), "xy*")
})

test_that("suffix +",{
  gen$SUFFIX("xy+",FALSE)->res
 # print(res)
  expect_true(res$ok )
  expect_equal(res$pos, 3 )
  expect_equal(paste(res$val, collapse=""), "xy+")
})

#test prefix
test_that("prefix primary",{
  gen$PREFIX("xy",FALSE)->res
 #print(res)
  expect_true(res$ok )
  expect_equal(res$pos, 2 )
  expect_equal(paste(res$val, collapse=""), "xy")
})

test_that("!PREFIX",{
  gen$PREFIX("!xy",FALSE)->res
  # print(res)
  expect_true(res$ok )
  expect_equal(res$pos, 3 )
  expect_equal(paste(res$val, collapse=""), "!xy")
})

test_that("&PREFIX *",{
  gen$PREFIX("&xy",FALSE)->res
  #print(res)
  expect_true(res$ok )
  expect_equal(res$pos, 3 )
  expect_equal(paste(res$val, collapse=""), "&xy")
})

test_that("SEQUENCE",{
  gen$SEQUENCE("&x y",FALSE)->res
 # print(res)
  expect_true(res$ok )
  expect_equal(res$pos, 4 )
  expect_equal(paste(res$val, collapse=""), "&x y")
})

test_that("EXPRESSION",{
  gen$EXPRESSION("&xx/yy",FALSE)->res
#  print(res)
  expect_true(res$ok )
  expect_equal(res$pos, 6 )
  expect_equal(paste(res$val, collapse=""), "&xx/yy")
})

test_that("PRIMARY",{
  gen$EXPRESSION("(&xx/yy)",FALSE)->res
  #  print(res)
  expect_true(res$ok )
  expect_equal(res$pos, 8 )
  expect_equal(paste(res$val, collapse=""), "(&xx/yy)")
})

test_that("LITERAL (TRUE)",{
  gen$LITERAL("\'x\'",TRUE)->res
  #print(res)
  expect_true(res$ok )
  expect_equal(res$pos, 3 )
  expect_equal(names(res$val), "leaf")
  tmp<-res$val$leaf
  expect_true(tmp("x")$ok)
  #expect_equal(res$val$atom, "atom.x")
})

test_that("DEFINITION",
{
  gen$DEFINITION("z<-&xx/yy",FALSE)->res
  #print(res)
  expect_true(res$ok )
  expect_equal(res$pos, 9 )
  expect_equal(paste(res$val, collapse=""), "z<-&xx/yy")
})

test_that("COMMENT",
{
  gen$COMMENT("#xx\nab", FALSE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 4 )
  expect_equal(paste(res$val, collapse=""), "#xx\n")  
})

test_that("COMMENT",
{
  gen$COMMENT("#xx\nab", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 4 )
  expect_equal(paste(res$val, collapse=""), "")  
})


test_that("SPACING 2",
{
  gen$SPACING(" #xx\nab",TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 5 )
  expect_equal(paste(res$val, collapse=""), "")  
})

test_that("EXEC",
{
  gen$EXEC("{myfunction}",FALSE)->res
  expect_true(res$ok )
 # res
  expect_equal(res$pos, 12 )
  expect_equal(paste(res$val, collapse=""), "{myfunction}")  
})

test_that("DEFINITION 2",
{
  gen$DEFINITION("x<-y {myfunction}", FALSE)->res
  expect_true(res$ok )
 # res
  expect_equal(res$pos, 17 )
  expect_equal(paste(res$val, collapse=""), "x<-y {myfunction}")  
})



test_that("GRAMMAR 1",
{
  gen$GRAMMAR("x<-y { myfunction }",FALSE)->res
  expect_true(res$ok )
  #res
  expect_equal(res$pos, 19 )
  expect_equal(paste(res$val, collapse=""), "x<-y { myfunction }")  
})

#test  true for
#def literal true
test_that("DEFINITION TRUE",
{
  gen$DEFINITION("xxx<-'y'", TRUE)->res
  expect_true(res$ok )
  # res
  expect_equal(res$pos, 8 )
  expect_true(exists("atom.y", envir=gen$pegE))
  # expect_equal(paste(res$val, collapse=""), "x<-y<<myfunction")  
  rm("atom.y", "xxx",envir=gen$pegE)
})

#def dot true
test_that("DOT TRUE",
{
  gen$DEFINITION("xxx<-.", TRUE)->res
  expect_true(res$ok )
  expect_equal(res$pos, 6 )
  res2<-gen$pegE$xxx("z")
  res2
  expect_equal(res2$val[[1]],"z")
  #expect_true(exists("atom.y", envir=.GlobalEnv))
  # expect_equal(paste(res$val, collapse=""), "x<-y<<myfunction")  
  #rm("atom.y",envir=.GlobalEnv)
})

#def sequence true
#def class true
#def open expression close true
#def question true
#def star true
#def plus true
#def and true
#def not true

#test prefix
#test sequence
#test expression
#test definition
#test grammer

#input<-'term<-identifier'

