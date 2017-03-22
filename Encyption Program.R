#RNG Additive Stream Cypher using a random number generator

#initialize tables and functions
#alpha-numeric table
num<-as.data.frame(1:41)
let<-as.data.frame(c(as.character(letters),' ','.',"'",'!','?','1','2','3','4','5','6','7','8','9','0'))
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

#Encypter:
#Takes two arguments
#   plain.text - which is a string text you wish to encrypt
#   Key1 - A numeric seed that will serve as your code, accepts numeric values, max 10 digits
#   Key2 - A numeric seed that will serve as your code, accepts numeric values, max 10 digits

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
#   Key1 - A numeric seed that will serve as your code, accepts numeric values, max 10 digits
#   Key2 - A numeric seed that will serve as your code, accepts numeric values, max 10 digits

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
coded.text<-encrypter("This is a coded message!",0123456789,0123456789)
coded.text

plain.text<-decrypter(coded.text, 0123456789,0123456789)
plain.text

#Interactive encryption/decryption prompts
#No stored vars
interactive_prompt<-function(){
  gate1<-readline("[E]ncrypt or [D]ecrypt?:")
  
  if(gate1=="E"|gate1=='e'){
  
    plain.text<-readline("Enter text you wish to encrypt:")
    key1<-readline("Enter encyption key1:")
    key2<-readline("Enter encyption key2:")
    print(encrypter(plain.text,key1,key2))
    
    plain.text<-0
    plain.text<-1
    plain.text<-NULL
    
    key1<-0
    key1<-1
    key1<-NULL
    
    key2<-0
    key2<-1
    key2<-NULL
  }  else if(gate1=="d"|gate1=='d'){
    
    coded.text0<-readline("Enter text you wish to decrypt (space delimited):")
    coded.text<-as.numeric(unlist(strsplit(tolower(coded.text0),' ')))
    key1<-readline("Enter encyption key1:")
    key2<-readline("Enter encyption key2:")
    print(decrypter(coded.text,key1,key2))
    coded.text0<-0
    coded.text0<-1
    coded.text0<-NULL
    
    coded.text<-0
    coded.text<-1
    coded.text<-NULL
    
    key1<-0
    key1<-1
    key1<-NULL
    
    key2<-0
    key2<-1
    key2<-NULL
  }
}

#activate prompt
interactive_prompt()

