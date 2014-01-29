library(testthat)

context("test_new_parser")

test_that("NEW_PARSER_NO_ARG",
{
  #Create an empty parser
  parser<-new.parser() 
  add_rule(parser, "Any<-.")  
  ids<-rule_ids(parser)  # returns "Any"
  expect_equal(ids,"Any")
})


test_that("NEW_PARSER_DATAFRAME_ARG",
{
  #Create a parser from a data.frame
  fn<-function(x){list()}
  df<-data.frame(
  rule.id=c('A','B'), 
  rule.source=c("A<-'a'", "B<-'b'"), 
  rule.description=c("aaa",NA),  
  action.type=c("Inline","External"), 
  action.specification=c("list()", "fn"), 
  stringsAsFactors=FALSE)
  peg<-new.parser(df)
  as.data.frame(peg)->df2
  expect_true(all(df2==df, na.rm=T)) #matches on the non-na portion
  expect_true(all(is.na(df2)==is.na(df)))  #and na-portions match
})