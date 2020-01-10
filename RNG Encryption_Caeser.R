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
#Takes three arguments
#   plain.text - which is a string text you wish to encrypt
#   Key1 - A numeric seed that will serve as your code, accepts numeric values, max 10 digits
#   Key2 - A numeric seed that will serve as your code, accepts numeric values, max 10 digits
#   Key3 - A numeric seed that will obscure the rare upper/lower limit of message and cypher

encrypter<-function(plain.text,key1,key2,key3){
  
  #input
  message<-unlist(strsplit(tolower(plain.text),''))
  n<-length(message)
  message1<-as.data.frame(message,stringsAsFactors = FALSE)
  message1$message
  
  #creates new list of numbers
  raw.text<-unlist(lapply((message1$message),converter))
  
  #Modifies cypher's upper/lower bound, hiding bounds that may be apparent 
  #through min/max random number and min/max coded number
  set.seed(key3)
  m_bound<-runif(1,50,100)  
  #two keys
  set.seed(key1)
  cypher1<-round(runif(n,1+m_bound,100+m_bound))
  set.seed(key2)
  cypher2<-round(runif(n,1+m_bound,100+m_bound))
  
  encrypted.num<-raw.text+cypher1+cypher2
  
  mod.num<-unlist(encrypted.num%%nrow(rot0))
  return(mod.num)
}


#Decypter:
#Takes three arguments
#   coded.text - Numeric list of the coded text
#   Key1 - A numeric seed that will serve as your code, accepts numeric values, max 10 digits
#   Key2 - A numeric seed that will serve as your code, accepts numeric values, max 10 digits
#   Key3 - A numeric seed that will obscure the rare upper/lower limit of message and cypher
decrypter<-function(coded.text,key1,key2,key3){
  
  message<-as.data.frame(coded.text)
  n<-nrow(message)
  
  #through min/max random number and min/max coded number
  set.seed(key3)
  m_bound<-runif(1,50,100)  
  #two keys
  set.seed(key1)
  cypher1<-round(runif(n,1+m_bound,100+m_bound))
  set.seed(key2)
  cypher2<-round(runif(n,1+m_bound,100+m_bound))
  
  plain.num<- message-cypher1-cypher2
  
  mod.num<-unlist(plain.num%%nrow(rot0))
  
  #creates new list of numbers
  plain.text<-sapply(mod.num,converter2)
  
  #output
  return(paste(as.character(plain.text),collapse=''))
  #return(cypher1+cypher2)
}
out<-decrypter(coded.text,0123456789,0123456789,4)
out

9999999999*9999999999*9999999999
#Example "Hello World' encryption
coded.text<-encrypter("This is a coded message!",0123456789,0123456789,4)
coded.text

plain.text<-decrypter(coded.text,0123456789,0123456789,4)
plain.text


#Interactive encryption/decryption prompts

interactive_prompt<-function(){
  gate1<-readline("[E]ncrypt or [D]ecrypt?:")
  
  if(gate1=="E"|gate1=='e'){
    
    plain.text<-readline("Enter text you wish to encrypt:")
    key1<-readline("Enter encyption key1:")
    key2<-readline("Enter encyption key2:")
    max <-readline("Enter encyption key3:")
    print(encrypter(plain.text,key1,key2,max))

  }  else if(gate1=="D"|gate1=='d'){
    
    coded.text0<-readline("Enter text you wish to decrypt (space delimited):")
    coded.text<-as.numeric(unlist(strsplit(tolower(coded.text0),' ')))
    key1<-readline("Enter encyption key1:")
    key2<-readline("Enter encyption key2:")
    max <-readline("Enter encyption key3:")
    print(decrypter(coded.text,key1,key2,max))

  }
}

#activate prompt
interactive_prompt()


