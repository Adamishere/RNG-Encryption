#ROT_RNG
#Substitution Cypher using a random number generator
#Encypter:
#Takes two arguments
#   plain.text - which is a string text you wish to encrypt
#   Key - A numeric seed that will serve as your code, accept 1- 10 numeric characters

#alpha-numeric table
num<-as.data.frame(1:31)
let<-as.data.frame(c(as.character(letters),' ','.',"'",'!','?'))
rot0<-cbind(let,num)
names(rot0)<-c('let','num')

#returns the numeric equivelent of the letter
converter<-function(x){
  z<-rot0[rot0$let==x,2]
  return(z)
}
#returns the string equivelent of the number
converter2<-function(x){
  z<-rot0[rot0$num==x,1]
  return(z)
}

encrypter<-function(plain.text,key1,key2){
  
  #input
  message<-unlist(strsplit(tolower(plain.text),''))
  n<-length(message)
  message1<-as.data.frame(message,stringsAsFactors = FALSE)
  message1$message

  #creates new list of numbers
  raw.text<-unlist(lapply((message1$message),converter))
  set.seed(key1)
  cypher1<-round(runif(n,1,100))
  set.seed(key2)
  cypher2<-round(runif(n,1,100))
  
  encrypted.text<-raw.text+cypher1+cypher2
  return((encrypted.text))
}

#Decypter:
#Takes two arguments
#   coded.text - Numeric list of the coded text
#   Key - A numeric seed that will serve as your code, accept 1- 10 numeric characters

decrypter<-function(coded.text,key1,key2){
  
  message<-as.data.frame(coded.text)
  n<-nrow(message)
  set.seed(key1)
  cypher1<-as.data.frame(round(runif(n,1,100)))
  set.seed(key2)
  cypher2<-as.data.frame(round(runif(n,1,100)))
  
  plain.num<- message-cypher1-cypher2

  #creates new list of numbers
  plain.text<-unlist(lapply((plain.num$coded.text),converter2))
  
  #output
  return(paste(as.character(plain.text),collapse=''))
}


#Example "Hello World' encryption
coded.text<-encrypter("This is a coded message!",0123456789,123)
coded.text

plain.text<-decrypter(coded.text, 0123456789,12)
plain.text




