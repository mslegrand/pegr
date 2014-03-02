library(testthat)

context("test_new_parser")

test_that("NEW_PARSER_NO_ARG",
{
  #Create an empty parser
  parser<-new.parser() 
  parser<-add_rule(parser, "Any<-.")  
  ids<-rule_ids(parser)  # returns "Any"
  expect_equal(ids,"Any")
})


 test_that("NEW_PARSER_DATAFRAME_ARG",
{
  #Create a parser from a data.frame
  #fn<-function(x){list()}
  df<-data.frame(
  rule.id=c('A','B'), 
  rule.definition=c("A<-'a'", "B<-'b'"), 
  rule.description=c("aaa",NA),  
  action.specification=c("list()", "c(v,v)"), 
  stringsAsFactors=FALSE
  )
  peg<-new.parser(df)
  as.data.frame(peg)->df2
  expect_true(all(df2==df, na.rm=T)) #matches on the non-na portion
  expect_true(all(is.na(df2)==is.na(df)))  #and na-portions match
})

 test_that("NEW_PARSER_DATAFRAME_ARG",
{
  peg<-new.parser()
  peg<- peg +
    c("A<-'a'", des="a 2 A", action="'A'") +
    c("B<-'b'", des="b does nothtng")
  as.data.frame(peg)->df1
  new.parser(df1)->peg1
  as.data.frame(peg1)->df2
  expect_true(identical(df1,df2))
})
