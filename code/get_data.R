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
proxy <- read_excel("data/proxies.xlsx")
id_all <- scan("data\\id_uni.txt", what="", sep="\n")
id <- as.list(id_all)

error_code=TRUE
df <-NULL
de<-NULL

df <- data.frame(ID=numeric(),
                 Name=character(),
                 Place=character())
df <- setNames(df, c("ID",
                     "Name",
                     "Place"))

j=1
#N=length(id)
N=1
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
  
  de<-data.frame(c(id[i],
                   name,
                   place))
  de <- setNames(de, c("ID",
                       "Name",
                       "Place"))
  
  df <- rbind(df,de)
  
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
date<-str_replace(date1, ":", "-")
file_name<-paste_all("uni_rince_",date,".xlsx",sep="")

write.xlsx(df,file_name)

