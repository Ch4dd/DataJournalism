###BASED TWITTER
library(devtools)
library(streamR)
library(ROAuth)
library(quanteda)
library(smappR)
library(reshape2)

library(dplyr)
setwd("C:/Users/Chadd/Desktop/based/")
load("C:/Users/Chadd/Desktop/based/1_30.RData")
accounts<-c("arzE", "diplo", "cher", "markhoppus", "officialjaden", "rihanna",
             "grimezsz", "amandapalmer", "wavves", "perfumegenius",
            "drake",   "warondrugsjams", "KillerMikeGTO", 
            "therealelp",   "myszkaway", "taylorswift13",
            "YG",  "iLoveMakonnen5D",
             "DeJLoaf", "Interpol", "BandofNOTHING", 
            "flyinglotus",  "st_vincent",  
            "SchoolBoyQ", "FreddieGibbs", "Pharrell",
            "futureislands", "BobbyShmurdaGS9", "OGMaco",  "yungleann", "theroots", 
             "jodyhighroller", "1future", "RaeSremmurd", "50cent", "youngjeezy", 
            "djquik", "RichHomieQuan", "eminem", "LilTunechi", 
            "NICKIMINAJ", "LanaDelRey", "ArianaGrande", "azealiabanks", "PaulMcCartney",
            "actionbronson", "LAKUTIS",  "rickyrozay", "souljaboy", "xdannyxbrownx", 
            "LILBTHEBASEDGOD", "fucktyler", "Skrillex", "avicii", "QuavoStuntin")
k<-length(accounts)

for (i in 32:k){
  
  getTimeline(filename=paste0("C:/Users/Chadd/Desktop/based/tweets1_30/",accounts[i], ".json"), screen_name=accounts[i], 
              n=1000, oauth_folder="C:/Users/Chadd/Dropbox/credentials/")
  
}
###Let's make sure ppl have enough non-RT's
count<-1:k
for (i in 1:k){
  tweets<-parseTweets(paste0("C:/Users/Chadd/Desktop/based/tweets/",accounts[i], ".json"))
  tweets<-filter(tweets, grepl("RT ", tweets$text)==FALSE)
  count[i]<-length(tweets$text)

}

twtz<-cbind(as.numeric(count), accounts)

###full corpus
texts<-1:k
for (i in 1:k){
  tweets<-parseTweets(paste0("C:/Users/Chadd/Desktop/based/tweets1_30/",accounts[i], ".json"))
  tweets<-filter(tweets, grepl("RT ", tweets$text)==FALSE)
  tweets<-tweets[1:250,]
  xx <- iconv(tweets$text, "UTF-8", "ASCII",  sub="")
  txt<-paste(xx, sep="", collapse="") 
  texts[i]<-txt
}
full<-texts
names(texts)<-accounts

corp<-corpus(texts)
the<-stopwordsGet(kind = "english")
ddfm<-dfm(corp, ignoreFeatures=the)
diverse<-statLexdiv(ddfm)
?statLexdiv
####offensiveness
badDict<-list(racism=c("beaner","chinc", "chink","coon","dego","gook","guido","heeb","kike","kyke","jigaboo","mick","negro",
                       "nigger*","niglet","paki","porchmonkey","pollock","ruski","sandnigger",
                       "spic", "wop", "jap","junglebunny", "spick","wetback"),
              sexism=c("bitch","cunt","dyke","skank*","slut*",
                       "whore*", "ho"),
              offensive=c("*bastard*", "*shit*",  "*fuck*", "carpetmuncher","*cock*",                "cum",
                          "*douche*", "*fag*", "fudgepacker","blowjob","handjob","homo","jizz",                "lesbo",
                          "lezzie","pussy", "queerbait", "rimjob","skeet","tard", "fuk" ))
texts<-1:k
scores<-1:k
for (i in 1:k){
  tweets<-parseTweets(paste0("C:/Users/Chadd/Desktop/based/tweets/",accounts[i], ".json"))
  tweets<-tweets[1:1000,]
  
  tweets$text <- iconv(tweets$text, "UTF-8", "ASCII",  sub="")
  tweets<-filter(tweets, grepl("RT ", tweets$text)==FALSE)
  tweets<-filter(tweets, tweets$text!="", tweets$text!="  " )
  tweets<-filter(tweets,  tweets$text!=" ", tweets$text!="\n") 
  tweets<-filter(tweets,  tweets$text!="   ", tweets$text!="    ")
  tweets<-filter(tweets,  tweets$text!="     ", tweets$text!="      ")
  tweets<-filter(tweets, grepl("RT ", tweets$text)==FALSE)
  tweets<-tweets[1:250,]
  x<-tweets$text
  mycorpus<-corpus(x)
  myDfm<-dfm(mycorpus, dict=badDict, dictionary_regex=FALSE)
  df<-as.data.frame(topfeatures(myDfm, 4))
  
  score<-(df["sexism",1] + df["racism",1] + df["offensive",1])/df["Non_Dictionary",1]
  scores[i]<-score
  }

thissss<-scores






###sentiment
LIWCdict <- readWStatDict("C:/Users/Chadd/Desktop/based/WordStat Sentiments.cat", enc="WINDOWS-1252")
posi<-LIWCdict[[8]]
posi1<-posi[1:1000]
posi2<-posi[1001:2000]
posi3<-posi[2001:3000]
posi4<-posi[3001:4000]
posi5<-posi[4001:5000]


neg<-LIWCdict[[7]]
neg1<-neg[1:1000]
neg2<-neg[1001:2000]
neg3<-neg[2001:3000]
neg4<-neg[3001:4000]
neg5<-neg[4001:5000]


sentDict<-list(b1=posi1, b2=posi2, b3=posi3, b4=posi4, b5=posi5, u1=neg1, u2=neg2, u3=neg3, u4=neg4, u5=neg5)



####Positivity
ratio<-1:k
based<-1:k
unbased<-1:k
needs<-1:3
diction<-sentDict
j<-length(diction)+1
for (i in 1:k){
  tweets<-parseTweets(paste0("C:/Users/Chadd/Desktop/based/tweets1_30/",accounts[i], ".json"))
  tweets<-tweets[1:1000,]
  
  tweets$text <- iconv(tweets$text, "UTF-8", "ASCII",  sub="")
  tweets<-filter(tweets, grepl("RT ", tweets$text)==FALSE)
  tweets<-filter(tweets, tweets$text!="", tweets$text!="  " )
  tweets<-filter(tweets,  tweets$text!=" ", tweets$text!="\n") 
  tweets<-filter(tweets,  tweets$text!="   ", tweets$text!="    ")
  tweets<-filter(tweets,  tweets$text!="     ", tweets$text!="      ")
  tweets<-filter(tweets, grepl("RT ", tweets$text)==FALSE)
  tweets<-tweets[1:250,]
  x<-tweets$text
  mycorpus<-corpus(x)
  myDfm<-dfm(mycorpus, dict=diction, dictionary_regex=TRUE)
  df<-topfeatures(myDfm, j)
  df<-df[2:j]
  ratio[i]=(df['b1']+df['b2']+df['b3']+df['b4']+df['b5'])/(df['u1']+(df['u2'])+(df['u3'])+(df['u4'])+(df['u5']))
  based[i]=df['b1']+df['b2']+df['b3']+df['b4']+df['b5']
  unbased[i]=df['u1']+(df['u2'])+(df['u3'])+(df['u4'])+(df['u5'])
}




#####Similarity
basedcos<-similarity(ddfm, n=55, "LILBTHEBASEDGOD", margin="documents", method="cosine")

?similarity
