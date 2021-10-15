library(rvest)
library(dplyr)
library(data.table)
library(pdftools)
library(stringr)
repeat{
  while(format(Sys.time(),"%H")%in%c("17","18","19","20")){
    path<-"https://www.city.hakodate.hokkaido.jp/docs/2020050300019/"
    # data<-
    #   fread("https://www.harp.lg.jp/opendata/dataset/1369/resource/3132/010006_hokkaido_covid19_patients.csv")%>%
    #   select(No,公表_年月日,患者_居住地)%>%
    #   mutate(公表_年月日=as.Date(公表_年月日))
    path2<-
      read_html(path)%>%
      html_nodes("a")%>%
      html_attr("href")%>%
      data.frame()%>%
      filter(str_detect(.,"../202[0-9]+/"))%>%
      mutate(pdf=paste0("https://www.city.hakodate.hokkaido.jp/docs",str_remove(.,"..")))
    i=1
    TA2<-data.frame()
    RH2<-data.frame()
    for (i in 1:nrow(path2)) {
      table1<-read_html(path2[i,2])%>%
        html_table()
      RH<-
        read_html(path2[i,2])%>%
        html_nodes("a")%>%
        html_attr("href")%>%
        data.frame()%>%
        rename("pdf"=".")
        #filter(str_detect(.,"pdf"))
      RH2<-
        cbind(RH,read_html(path2[i,2])%>%
                html_nodes("a")%>%
                html_text()%>%
                data.frame())%>%
        filter(str_detect(.,"[0-9]+/[0-9]+"))%>%
        rename("D"=".")%>%
        filter(!str_detect(pdf,"henikabu"))
      TA<-
        table1[[1]]%>%
        data.frame()
      
      colnames(TA)<-TA[1,]
      TA<-TA%>%
        filter(No.!="No.")%>%
        filter(str_detect(No.,"[0-9]"))
      for (k in 1:nrow(RH2)) {
        pdf<-pdf_text(paste0(path2[i,2],RH2[k,1]))[1]
        S<-
          pdf%>%
          str_locate("令和.+日")
        Date<-pdf%>%
          str_sub(S[1],S[2])%>%
          stringi::stri_trans_nfkc()
        TA[k,8]<-Date
        print(k)
      }
      
      TA2<-rbind(TA2,TA[,c(1:6,8)])
      print(i)
      
    }
    TA3<-
      TA2%>%
      mutate(No=as.numeric(No.))%>%
      rename("Date"="V8")%>%
      rename("北海道発表No"="北海道\r\n発表No.")%>%
      mutate(北海道発表No=as.integer(北海道発表No))%>%
      mutate(Date=gsub("令和3年","2021年",Date)) %>%
      mutate(Date=gsub("令和2年","2020年",Date))%>%
      mutate(Date=as.Date(Date,format="%Y年%m月%d日"))
    

    data2<-
      left_join(TA3,data,by=c("北海道発表No"="No"))%>%
      mutate(Date=ifelse(is.na(Date),公表_年月日,Date))%>%
      mutate(Date=as.Date(Date,origin="1970-1-1"))
    write.csv(data2%>%
                select(No,北海道発表No,判明日,Date,年代,性別,居住地),
              "hakodate202108.csv",fileEncoding="UTF-8",row.names = F)
    table1<-read_html(path)%>%
      html_nodes("table")
    #html_table()
    RH<-
      read_html(path)%>%
      html_nodes("a")%>%
      html_attr("href")%>%
      data.frame()%>%
      rename("pdf"=".")
    #filter(str_detect(.,"pdf"))
    RH2<-
      cbind(RH,read_html(path)%>%
              html_nodes("a")%>%
              html_text()%>%
              data.frame())%>%
      filter(str_detect(.,"[0-9]+/[0-9]+"))%>%
      rename("D"=".")%>%
      filter(!str_detect(pdf,"henikabu"))
    TA<-
      table1[[2]]%>%
      html_table()%>%
      data.frame()
    
    colnames(TA)<-TA[1,]
    TA<-TA%>%
      filter(No.!="No.")%>%
      filter(str_detect(No.,"[0-9]"))
    for (k in 1:nrow(RH2)) {
      pdf<-pdf_text(paste0(path,RH2[k,1]))[1]
      S<-
        pdf%>%
        str_locate("令和.+日")
      Date<-pdf%>%
        str_sub(S[1],S[2])%>%
        stringi::stri_trans_nfkc()
      TA[k,8]<-Date
      print(k)
    }
    
    TA2<-TA[,c(1:6,8)]
    TA3<-
      TA2%>%
      mutate(No=as.numeric(No.))%>%
      rename("Date"="V8")%>%
      rename("北海道発表No"="北海道\r\n発表No.")%>%
      mutate(北海道発表No=as.integer(北海道発表No))%>%
      mutate(Date=gsub("令和3年","2021年",Date)) %>%
      mutate(Date=gsub("令和2年","2020年",Date))%>%
      mutate(Date=as.Date(Date,format="%Y年%m月%d日"))
    write.csv(TA3%>%
                select(No,北海道発表No,判明日,Date,年代,性別,居住地),
              "hakodate202109.csv",fileEncoding="UTF-8",row.names = F)
    data1<-
      read.csv("hakodate202108.csv",encoding = "UTF-8")
    data2<-
      read.csv("hakodate202109.csv",encoding = "UTF-8")
    data3<-
      rbind(data1,data2)%>%
      mutate(No=as.numeric(No))%>%
      arrange(desc(No))
    write.csv(data3,
              "hakodate.csv",
              fileEncoding="UTF-8",row.names = F)
    print("出力しました")
    Sys.sleep(900)
    
  }}