# Palindromes
   
#print(with just the lettersp 'a,b,c')
# parser<-new.parser()
# add_rule(parser,"S<-('a' S 'a') / ('b' S 'b') / ('c' S 'c') / ''")
# apply_rule(parser, "S", "baccab", debugTree=T)->res
# res
# tree(res)            

pal<-paste(c(sapply(letters, function(x){paste0("('",x,"' PAL '",x,"')")}),"''"),collapse=" / ")
rule<-paste0("PAL<-",pal)
add_rule(parser, rule)

words<-c("bob", "civic", "deified", "level", "madam", "nun", "peep", "pop", "racecar", "radar", "refer", "rotator", "rotor", "sagas", "sas", "siris", "solos", "sexes","stats", "testset", "wow")
for(word in words){
  apply_rule(parser, "P", word)->res
  print(paste(word, "is a palindrome:", status(res) ))
}
 

