library(testthat)

context("test_add_rule")

test_that("ADD_RULE1",
{
#expect_equal(0,1)
   peg<-new.parser()
   peg<-add_rule(peg, "X<-x", des="A bad rule (I forgot quotes)")
  
   
   # Next we inspect the rule
   inspect_rule(peg, "X")->rs
    expect_equal(rs$name,"X")
    expect_equal(rs$def,"X<-x")
    expect_equal(rs$com,"A bad rule (I forgot quotes)")
    expect_equal(rs$act, NULL )

   # Inspect rule returns the following
   # Rule: X
   # Def: X<-x
   # Com: A bad rule (needs quotes)
   # Act:
   
   # Now we replace the rule by overwriting
   peg<-add_rule(peg, "X<-'x'", act="list('X')")
   # When again inspect, we see
   # the definition was fixed (x now is quoted), the description was removed, an action was added.
   inspect_rule(peg,"X")->rs
   expect_equal(rs$name,"X")
   expect_equal(rs$def,"X<-'x'")
   expect_equal(rs$com,NULL)
   expect_equal(rs$act, "list('X')")
})

 

test_that('+.PEGR"',
{
#expect_equal(0,1)
   peg<- new.parser()
   peg<- peg + "A<-'a'" + "B<-'b'" + "C<-'c'"  
   rule_ids(peg)->res
   expect_equal(res, c("A","B","C"))
   inspect_rule(peg,"B")->res
   expect_equal(res$def, "B<-'b'")
   #now add rule D with action and comment
   peg<- peg + c("D<-'d'", des="capitalize D", act="list(atom='D')")
   inspect_rule(peg,"D")->rs
   expect_equal(rs$name,"D")
   expect_equal(rs$def,"D<-'d'")
   expect_equal(rs$com,"capitalize D")
   expect_equal(rs$act, "list(atom='D')" )
  
   peg<- peg + c( "E<-'e'", "#double E", "{list('EE')}" )
   inspect_rule(peg,"E")->rs
   expect_equal(rs$name,"E")
   expect_equal(rs$def,"E<-'e'")
   expect_equal(rs$com,"double E")
   expect_equal(rs$act, "list('EE')" )
           
})

 

