#RNG Additive Stream Cypher using a random number generator

#initialize tables and functions
#alpha-numeric table
let<-as.data.frame(c(toupper(as.character(letters)),
                     as.character(letters),
                     "'",' ','.','!','@','#','$','%','^','&','*','(',')',
                     '_','+','~','[',']','{','}','-','=',';','<','>','?',':','|',
                     '1','2','3','4','5','6','7','8','9','0'))

num<-as.data.frame(1:nrow(let))
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


#Generate key stream based on seed and length of message
#  seed - numeric - serves as part of password
#  n - numeric - length of message. Produces a key stream just as long
keystreamrng<- function(seed,n){
  set.seed(seed)
  keystream<-round(runif(n,1,100))
  return(keystream)
}

#Encypter:
#Takes three arguments
#   plain.text - string - which is a string text you wish to encrypt
#   password - string - Password to generate keystream
#   pin - numeric - 10 digits, additional key, default 1
plain.text<-"hh"
password<-"z"
pin<-1

encrypter<-function(plain.text,password, pin=1){
  #parse password
  pwd<-unlist(strsplit(password, split=""))
  pwd_num<-unlist(lapply(pwd,converter))
  pwd_num<-c(pwd_num,pin)
  pwd_length<-length(pwd_num)
  
  #Convert text to dataframe
  message<-unlist(strsplit(tolower(plain.text),''))
  n<-length(message)
  msg.df<-as.data.frame(message,stringsAsFactors = FALSE)
  msg.df$message
  
  msg.df$num_message<-(sapply((msg.df$message),converter))
  
  #generate key stream for each character in password
  keystream<-sapply(pwd_num,keystreamrng, n=n)
  #Prevents cbind from transposing a single row
  if(n==1){
    keystream<-t(as.data.frame(keystream))
  }
  #attach to message dataframe
  msg.df<-cbind(msg.df,keystream)

  #Combined all keystreams and message
  msg.df$raw_coded<-rowSums(msg.df[,2:(2+pwd_length)])
  
  #Code back to original possible values
  mod.num<-unlist(msg.df$raw_coded%%nrow(rot0))
  mod.num
  return(mod.num)
}

#Decypter:
#Takes three arguments
#   coded.text - Numeric list of the coded text
#   password - string - password to decode message
#   pin - numeric - 10 digits, additional key, default 1

decrypter<-function(coded.text, password, pin=1){
  
  #parse password
  pwd<-unlist(strsplit(password, split=""))
  pwd_num<-unlist(lapply(pwd,converter))
  pwd_num<-c(pwd_num,pin)
  pwd_length<-length(pwd_num)
  
  #parse coded text as df
  message<-as.data.frame(coded.text)
  n<-nrow(message)
  
  #generate keystream
  keystream<-sapply(pwd_num,keystreamrng, n=n)
  
  #Prevents cbind from transposing a single row
  if(n==1){
    keystream<-t(as.data.frame(keystream))
  }
  
  #merge key onto message frame
  message1<-cbind(message,keystream)
  
  message1$raw_uncoded<-message1$coded.text - rowSums(message1[,2:(1+pwd_length)])
  
  decoded.num<-unlist(message1$raw_uncoded%%nrow(rot0))
  decoded.rawtxt<-sapply(decoded.num,converter2)
    
  decoded.text<-paste(as.character(decoded.rawtxt),collapse='')
  return(decoded.text)
}

#Example "Hello World' encryption

coded.text<-encrypter("The max pin 999999999","z",999999999)
coded.text

plain.text<-decrypter(coded.text,"z",999999999)
plain.text


#Interactive encryption/decryption prompts
interactive_prompt<-function(){
  gate1<-readline("[E]ncrypt or [D]ecrypt?:")
  
  if(gate1=="E"|gate1=='e'){
    
    plain.text<-readline("Enter text you wish to encrypt:")
    pass<-readline("Enter password:")
    pin<-readline("Enter pin (optional):")
    
    if(pin==""){pin<-1}
    
    print(encrypter(plain.text,pass,pin))

  }  else if(gate1=="D"|gate1=='d'){
    
    coded.text0<-readline("Enter text you wish to decrypt (space delimited):")
    coded.text<-as.numeric(unlist(strsplit(tolower(coded.text0),' ')))
    
    pass<-readline("Enter password:")
    pin<-readline("Enter pin (optional):")
    
    if(pin==""){pin<-1}
    
    print(decrypter(coded.text,pass,pin))

  }
}


#activate prompt
interactive_prompt()

print(encrypter("zz","z",21))
print(decrypter("58 85","z",21))
