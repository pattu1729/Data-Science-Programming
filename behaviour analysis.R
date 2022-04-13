# Exploratory data analysis, Data manupulation, 
# String manupulaion, etc.
library("tidyverse")

# Classification package
library("kernlab")

# Association rule
library("arules")

# Time Series package
library("astsa")

#OTHERS
# Data to csv or latex format
library("xtable")

# To draw sample by SRSWOR
library("sampling")

# Package management, error handling
library("R.oo")

EC=read.csv("C:/Users/veniniyan/Downloads/data.csv")
N=nrow(EC)
head(EC)
summary(EC)

#Convert to Date and time class
EC$InvoiceDate=as.POSIXct(EC$InvoiceDate,tryFormats = c("%m/%d/%Y %H:%M"))

#Remove defective descriptions, missing customer ID etc
U.Description=unique(EC$Description)
LC=str_split("qwertyuiopasdfghjklzxcbnm?","")

# Checking if the item description contains lowercase letters
auto_defects=str_subset(U.Description,"[qwertyuiopasdfghjklzxcvbnm?]")
auto_defects_index=which(str_detect(U.Description,"[qwertyuiopasdfghjklzxcvbnm?]")==TRUE)

print("#### Suspecious defective items")
auto_defects
print("#### Suspecious defective items index")
auto_defects_index

not_defects<-c(2,3,4,5,6,7,8,10,11,13,15,17,18,21,22,23,27,28,89,94,121)
print("#### Misclassified as Defective Items")
auto_defects[not_defects]

# Clean Description
Clean.Description=U.Description[-append(auto_defects_index[-not_defects],396)]

# Defective Description
Defective.Description=auto_defects[-not_defects]
print("#### Defective Items")
Defective.Description

#Useful variables
original_loc="/kaggle/working"
Loc<-function(x) return(paste("C:/Users/veniniyan/Downloads",x,sep=""))

dir.create(Loc("Countries"))
dir.create(Loc("Arules"))
dir.create(Loc("Time_Series"))
dir.create(Loc("Arules/Apriori"))
dir.create(Loc("Arules/Eclat"))
dir.create(Loc("Time_Series/TS_Data"))
dir.create(Loc("Time_Series/TS_Graphs"))

# List of Countries
Countries=unique(EC$Country)

# Some customers ID are still missing
Clean.EC=EC %>% filter(Description %in% Clean.Description)
items=as.character(unique(Clean.EC$Description))

#Data Visualisation
EC%>%count(Country=="United Kingdom")
#We observe that most of the transactions are done in UK
EC1=EC%>%filter(Country!="United Kingdom")
ggplot(data = EC1) +geom_bar(mapping=aes(x=Country))+coord_flip()
EC%>%count(Country)%>%arrange(desc(n))

#Number of transactions in different weeks
Days=cut(EC$InvoiceDate,breaks="week")
ggplot(data=as.data.frame(Days))+geom_bar(aes(x=Days))+coord_flip()

datum=as.POSIXct("2010-12-01 00:00", format="%Y-%m-%d %H:%M")
Time=rep(0,N)
for (i in (1:N)){
  Time[i]=floor((as.numeric(difftime(EC$InvoiceDate[i],datum,units = "secs"))%%(3600*24))/3600)
}

Time=as.data.frame(Time)
ggplot(Time,aes(x=Time))+geom_histogram(binwidth = 1)

Time%>%count(Time) #Flip this table later

#Time Series Analysis of different items
Popular_Items <- function(minvalue) {
  ItemCount = Clean.EC %>% count(Description) %>% arrange(desc(n)) %>% filter(n > minvalue)
  ItemIndex <- function(x) return(paste("Item", x, sep = ""))
  ItemCode = 1:nrow(ItemCount) %>% map_chr(ItemIndex)
  return(tibble(Code = ItemCode, Name = ItemCount$Description, Count = ItemCount$n))
}
Quantity.Sold <- function(item) {
  D = Clean.EC %>% filter(Description == item)
  start = as.POSIXct("2010-12-01 00:00:00")
  end = as.POSIXct("2011-12-09 00:00:00")
  Timestamp = seq(start, end, "week")
  Timestamp1 = Timestamp
  Quantity = rep(0, length(Timestamp) - 1)
  j = 1
  k = 1
  for (i in (1:(length(Timestamp) - 1))) {
    while (as.numeric(D$InvoiceDate[j] - Timestamp[i]) < 0 & j < nrow(D)) {
      j = j + 1
    }
    k = j
    while (as.numeric(D$InvoiceDate[k] - Timestamp[i + 1]) < 0 & k < nrow(D)) {
      k = k + 1
    }
    Quantity[i] = sum(D$Quantity[j:k])
    j = k
  }
  D = tibble(Week = Timestamp[2:length(Timestamp)], Quantity)
  D$Week = as.factor(D$Week)
  return(D)
}

Sold.Items.Graphs <- function(minvalue) {
  setwd(original_loc)
  setwd("Time_Series/TS_Graphs")
  D = Popular_Items(minvalue)
  for (i in 1:nrow(D)) {
    assign(paste("TSitem", i, sep = ""), ggplot(data = Quantity.Sold(D$Name[i]), 
                                                aes(x = Week, y = Quantity)) + geom_bar(stat = "identity") + coord_flip())
    png(paste("TSitem", i, ".png", sep = ""))
    print(get(paste("TSitem", i, sep = "")))
    dev.off()
  }
  setwd(original_loc)
  
  Sold.Items.TS <- function(minvalue) {
    setwd(original_loc)
    setwd("Time_Series/TS_Data")
    D = Popular_Items(minvalue)
    for (i in 1:nrow(D)) {
      assign(paste("TSitem", i, "data", sep = ""), Quantity.Sold(D$Name[i]))
      write_csv(get(paste("TSitem", i, "data", sep = "")), paste("TSitem", i, sep = ""))
    }
    setwd(original_loc)
  }
  
  write_csv(Popular_Items(500),"Popular_ItemsID.csv")
  Sold.Items.Graphs(500)
  Sold.Items.TS(500)
  
  list.files(Loc("Time_Series/TS_Graphs"))
  
#Association Rules Analysis
  Country.Data <- function(k) return(paste(Countries[k], ".data", sep = ""))
  
  Country.File <- function(k) {
    pathname = Loc("Countries/")
    return(paste(pathname,Countries[k], ".csv", sep = ""))
  }
  
  Country.Transactions <- function(k) return(paste("Transactions", Countries[k], sep = ""))
  
# Define datasets for the different countries.
  Country.Datasets <- function() {
    for (i in (1:length(Countries))) {
      assign(Country.Data(i), Clean.EC %>% filter(Country == Countries[i]))
      write_csv(get(Country.Data(i)), Country.File(i), col_names = TRUE)
    }
    setwd(original_loc)
  }
}

Country.Datasets()
for (i in (2:length(Countries)))
  assign(Country.Transactions(i),
         read.transactions(Country.File(i),format = "single",cols = c(1,3),header = TRUE,sep = ",",quote = ""))

Country.Ar.Write<-function(k,support=0.075,confidence=0.65,max){
  A=apriori(get(Country.Transactions(k)),parameter = list("support"=support,"confidence"=confidence))
  E=eclat(get(Country.Transactions(k)),parameter = list("support"=support))
  setwd(Loc("Arules/Apriori"))
  save(A,file=paste(Countries[k],"Aprules"))
  setwd(Loc("Arules/Eclat"))
  save(E,file=paste(Countries[k],"Ecrules"))
  setwd(original_loc)
}

# 14,35,15,16 Blacklisted

2:10%>% map_chr(Country.Ar.Write)

list.files("/kaggle/working/Arules/Apriori")
Countries

#Analysis of Items
## Returns a corpus of words using the items' names present in vector V 

WORDS<-function(V){
  Words.corpus<-c()
  for (i in 1:length(V)){
    B=strsplit(V[i],split=" ")
    B=B[[1]]
    Words.corpus=c(Words.corpus,B)
  }
  return (Words.corpus)
}
Words=WORDS(items[1:10])
length(Words)

unlink("/kaggle/working/Nitin",recursive=TRUE)
getwd()
# for (k in (:30)){
#     string=Country.Transactions(k)
#     rm(string)
# }
# ls()
# Country.Transactions(1)








