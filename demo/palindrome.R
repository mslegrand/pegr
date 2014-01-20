# Palindromes
   
print("A simple palidrone parser for just the lettersp 'a,b,c'")
palSimple<-new.parser()
add_rule(palSimple,"S<-('a' S 'a') / ('b' S 'b') / ('c' S 'c') / ''")
apply_rule(palSimple, "S", "baccab", record=T)->res
res
tree(res)            

print("Expanding the simple palindrone for all lowercase letters")
palLower<-new.parser()
palRule<-paste(c(sapply(letters, function(x){paste0("('",x,"' PAL '",x,"')")}),"''"),collapse=" / ")
rule<-paste0("PAL<-",palRule)
add_rule(palLower, rule)

words<-c("bob", "civic", "deified", "level", "madam", "nun", "peep", "pop", "racecar", "radar", "refer", "rotator", "rotor", "sagas", "sas", "siris", "solos", "sexes","stats", "testset", "wow")
for(word in words){
  apply_rule(palLower, "PAL", word)->res
  print(paste(word, "is a palindrome:", status(res) ))
}
 

