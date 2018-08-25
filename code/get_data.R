#new file https://elibrary.ru/org_profile.asp?id=2238

library(RSelenium)
library(docker)
library(rvest)
library(XML)
library(readxl)
library(httr)
library(htmltools)
library(dplyr)
library(xlsx)
library(stringr)
library(reshape2)
proxy <- read_excel("data/proxies.xlsx")
id_all <- scan("data\\id_uni.txt", what="", sep="\n")
id <- as.list(id_all)

error_code=TRUE
df <-NULL


j=1
N=length(id)
for (i in 1:N)
{
  
  
  str_uni_rubrics <- paste("https://elibrary.ru/org_profile.asp?id=",toString(id[i]),sep = "")
  
  while (error_code==TRUE && j!=(length(proxy$ips)))
  {
    error_message<-tryCatch(html_session(str_uni_rubrics, use_proxy(proxy$ips[j], proxy$ports[j])),
                            error = function(e) {"error"},
                            warning = function(w) {"error"})
    print(error_message)
    error<-str_detect(error_message,"error")
    if (sum(error)!=0) 
    {
      error_code=TRUE
      j=j+1
    }
    
    if (sum(error)==0 && (str_detect(error_message$url,"ip_restricted")||str_detect(error_message$url,"page_404")))
    {
      error_code=TRUE
      j=j+1
    }
    else if(sum(error)==0 )
    {
      error_code=FALSE
    }
    if (i!=length(id) && j==(length(proxy$ips))) j=1
    print(paste("error_code=",error_code))
    print(paste("j=",j))
  }  
  
  session<-error_message
  uni_profile <- read_html(session)
  
  main_inf <-uni_profile %>% html_nodes("table") %>% .[[22]] 
  
  name <- main_inf %>% html_nodes("font") %>% .[1] %>% html_text()
  place <- main_inf %>% html_nodes("font") %>% .[2] %>% html_text()
  
  main_indicators <- uni_profile %>% html_nodes("table") %>% .[[23]]  %>% html_table(fill=TRUE)
  main_indicators[-c(1:3,7,11,15),-1] ->main_indicators
  main_indicators<-t(main_indicators)
  colnames( main_indicators) = main_indicators[1, ]
  main_indicators = main_indicators[-1, ]
  
  dtest<-as.data.frame(cbind(id=id[i],name,place,t(main_indicators)))
  
  five_indicators <- uni_profile %>% html_nodes("table") %>% .[[24]]  %>% html_table(fill=TRUE)
  five_indicators[-c(1:3,10,20,28,35),-1] ->five_indicators
  five_indicators<-t(five_indicators)
  colnames(five_indicators) = five_indicators[1, ]
  five_indicators = t(five_indicators[-1, ])
  colnames(five_indicators) <- paste(colnames(five_indicators),"5_лет",sep = "_")
  
  dtest2<-as.data.frame(cbind(dtest,five_indicators))
  
  pub_indicators <- uni_profile %>% html_nodes("table") %>% .[[25]]  %>% html_table(fill=TRUE)
  pub_indicators %>% slice(-2:-3) %>% select(-X1)->pub_indicators
  colnames(pub_indicators) = pub_indicators[1, ]
  pub_indicators = pub_indicators[-1, ]
  pub_indicators<-cbind(id[i],pub_indicators)
  colnames(pub_indicators)[1] <- "id"
  pub_indicators$`Область знаний`<-as.factor(pub_indicators$`Область знаний`)
  pub_indicators<-dcast(melt(pub_indicators, id.vars=c("id","Область знаний")), id~variable+`Область знаний`)
  colnames(pub_indicators) <- paste("пуб",colnames(pub_indicators),sep = "_")
  
  dtest3<-as.data.frame(cbind(dtest2,pub_indicators[,-1]))
  
  cit_indicators <- uni_profile %>% html_nodes("table") %>% .[[26]]  %>% html_table(fill=TRUE)
  cit_indicators %>% slice(-2:-3) %>% select(-X1)->cit_indicators
  colnames(cit_indicators) = cit_indicators[1, ]
  cit_indicators = cit_indicators[-1, ]
  cit_indicators<-cbind(id[i],cit_indicators)
  colnames(cit_indicators)[1] <- "id"
  cit_indicators$`Область знаний`<-as.factor(cit_indicators$`Область знаний`)
  cit_indicators<-dcast(melt(cit_indicators, id.vars=c("id","Область знаний")), id~variable+`Область знаний`)
  colnames(cit_indicators) <- paste("цит",colnames(cit_indicators),sep = "_")
  
  dtest4<-as.data.frame(cbind(dtest3,cit_indicators[,-1]))
  
  by_year_indicators <- uni_profile %>% html_nodes("table") %>% .[[27]]  %>% html_table(fill=TRUE)
  by_year_indicators[-c(2,3,14,21,31,34),-1] -> by_year_indicators
  colnames(by_year_indicators) = by_year_indicators[1, ]
  by_year_indicators = by_year_indicators[-1, ]
  by_year_indicators<-cbind(id[i],by_year_indicators)
  colnames(by_year_indicators)[1] <- "id"
  by_year_indicators$`Название показателя`<-as.factor(by_year_indicators$`Название показателя`)
  by_year_indicators<-dcast(melt(by_year_indicators, id.vars=c("id","Название показателя")), id~`Название показателя`+variable)
  
  dtest5<-as.data.frame(cbind(dtest4,by_year_indicators[,-1]))

  df <- rbind(df,dtest5)
  
  if ((i%%3)==0) Sys.sleep(5)
  if ((i%%20)==0) Sys.sleep(100)
  print(paste("i=",i))
  error_code=TRUE
  if (((i%%250)==0))
  {
    
    date1<-str_replace_all(Sys.time(), " ", "-")
    date<-str_replace_all(date1, ":", "-")
    file_name<-paste("uni_rince_",date,"_",i,".xlsx",sep="")
    write.xlsx(df[(i-249):i,],file_name)
    Sys.sleep(500)
  }
  
}

date1<-str_replace_all(Sys.time(), " ", "-")
date<-str_replace_all(date1, ":", "-")
file_name<-paste("uni_rince_",date,".xlsx",sep="")

write.xlsx(df,file_name)

