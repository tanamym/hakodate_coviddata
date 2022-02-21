library(rvest)
library(dplyr)
library(data.table)
library(pdftools)
library(stringr)
library(tidyr)
repeat{
  while(format(Sys.time(),"%H")%in%c("17","18","19","20")){
     path<-"https://www.city.hakodate.hokkaido.jp/docs/2020050300019/"
    # # data<-
    # #   fread("https://www.harp.lg.jp/opendata/dataset/1369/resource/3132/010006_hokkaido_covid19_patients.csv")%>%
    # #   select(No,公表_年月日,患者_居住地)%>%
    # #   mutate(公表_年月日=as.Date(公表_年月日))
    # path2<-
    #   read_html(path)%>%
    #   html_nodes("a")%>%
    #   html_attr("href")%>%
    #   data.frame()%>%
    #   filter(str_detect(.,"../202[0-9]+/"))%>%
    #   mutate(pdf=paste0("https://www.city.hakodate.hokkaido.jp/docs",str_remove(.,"..")))
   #  i=1
   #  path2="https://www.city.hakodate.hokkaido.jp/docs/2022012000029/"
   #  TA2<-data.frame()
   #  RH2<-data.frame()
   #  #for (i in 1:nrow(path2)) {
   #    # table1<-read_html(path2[i,2])%>%
   #    #   html_table()
   #    RH<-
   #      read_html(path2)%>%
   #      html_nodes("a")%>%
   #      html_attr("href")%>%
   #      data.frame()%>%
   #      rename("pdf"=".")%>%
   #      mutate(pdf=str_remove(pdf,".."))
   #      #filter(str_detect(.,"pdf"))
   #    RH2<-
   #      cbind(RH,read_html(path2)%>%
   #              html_nodes("a")%>%
   #              html_text()%>%
   #              data.frame())%>%
   #      #filter(str_detect(.,"[0-9]+/[0-9]+"))%>%
   #      filter(str_detect(.,"判明分"))%>%
   #      rename("D"=".")%>%
   #      filter(!str_detect(pdf,"henikabu"))
   #    # TA<-
   #    #   table1[[1]]%>%
   #    #   data.frame()
   #    # 
   #    # colnames(TA)<-TA[1,]
   #    # TA<-TA%>%
   #    #   filter(No.!="No.")%>%
   #    #   filter(str_detect(No.,"[0-9]"))
   #    for (k in 1:nrow(RH2)) {
   #      print(k)
   #      table1<-read_html(paste0("https://www.city.hakodate.hokkaido.jp/docs",RH2[k,1]))%>%
   #        html_table()
   #      TA<-
   #        table1[[1]]%>%
   #        data.frame()
   #      
   #      colnames(TA)<-TA[1,]
   #      TA<-TA%>%
   #        filter(No.!="No.")%>%
   #        filter(str_detect(No.,"[0-9]"))
   #      RH3<-
   #        read_html(paste0("https://www.city.hakodate.hokkaido.jp/docs",RH2[k,1]))%>%
   #        html_nodes("a")%>%
   #        html_attr("href")%>%
   #        data.frame()%>%
   #        rename("pdf"=".")
   #      #filter(str_detect(.,"pdf"))
   #      RH4<-
   #        cbind(RH3,read_html(paste0("https://www.city.hakodate.hokkaido.jp/docs",RH2[k,1]))%>%
   #                html_nodes("a")%>%
   #                html_text()%>%
   #                data.frame())%>%
   #        filter(str_detect(.,"[0-9]+/[0-9]+"))%>%
   #        rename("D"=".")%>%
   #        filter(!str_detect(pdf,"henikabu"))
   #      n=1
   #      for (n in 1:nrow(RH4)) {
   #        #if(n>=63&k==9)next
   #        pdf<-try(pdf_text(paste0("https://www.city.hakodate.hokkaido.jp/docs",RH2[k,1],RH4[n,1]))[1])
   #        if(pdf=="try-error")next
   #      S<-
   #        pdf%>%
   #        str_locate("令和.+日")
   #      Date<-pdf%>%
   #        str_sub(S[1],S[2])%>%
   #        stringi::stri_trans_nfkc()
   #      TA[n,8]<-Date
   #      
   #      }
   #      TA2<-rbind(TA2,TA[,c(1:6,8)])
   #    }
   #    
   #    
   #   # print(i)
   #    
   # # }
   #  TA3<-
   #    TA2%>%
   #    mutate(No=as.numeric(No.))%>%
   #    rename("Date"="V8")%>%
   #    rename("北海道発表No"="北海道\r\n発表No.")%>%
   #    mutate(北海道発表No=as.integer(北海道発表No))%>%
   #    mutate(Date=gsub("令和5年","2023年",Date)) %>%
   #    mutate(Date=gsub("令和4年","2022年",Date)) %>%
   #    mutate(Date=gsub("令和3年","2021年",Date)) %>%
   #    mutate(Date=gsub("令和2年","2020年",Date))%>%
   #    mutate(Date=as.Date(Date,format="%Y年%m月%d日"))
   #  
   # 
   #  data2<-
   #    left_join(TA3,data,by=c("北海道発表No"="No"))%>%
   #    mutate(Date=ifelse(is.na(Date),公表_年月日,Date))%>%
   #    mutate(Date=as.Date(Date,origin="1970-1-1"))
   #  write.csv(data2%>%
   #              select(No,北海道発表No,判明日,Date,年代,性別,居住地),
   #            "hakodate202108.csv",fileEncoding="UTF-8",row.names = F)
    table1<-read_html(path,options="HUGE")%>%
      html_nodes("table")
    #html_table()
    RH<-
      read_html(path,options="HUGE")%>%
      html_nodes("a")%>%
      html_attr("href")%>%
      data.frame()%>%
      rename("pdf"=".")%>%
      filter(str_detect(pdf,"hasseijyoukyou"))
    # RH2<-
    #   cbind(RH,read_html(path,options="HUGE")%>%
    #           html_nodes("a")%>%
    #           html_text()%>%
    #           data.frame())%>%
    #   filter(str_detect(.,"[0-9]+/[0-9]+"))%>%
    #   rename("D"=".")%>%
    #   filter(!str_detect(pdf,"henikabu"))
    TA<-
      table1[[2]]%>%
      html_table()%>%
      data.frame()
    
    colnames(TA)<-TA[1,]
    TA<-TA%>%
      filter(No.!="No.")%>%
      filter(str_detect(No.,"[0-9]"))
    DF<-data.frame(Date="",n=1:nrow(RH),函館市内="",北海道内="",北海道外="")
    for (k in 1:nrow(RH)) {
      pdf<-pdf_text(paste0(path,RH[k,1]))[1]
      S<-
        pdf%>%
        str_locate("令和.+日")
      Date<-pdf%>%
        str_sub(S[1],S[2])%>%
        stringi::stri_trans_nfkc()

      lo<-str_locate(Date,"令和.+年")
      s=lo[1,1]
      e=lo[1,2]
      S<-as.numeric(str_sub(Date,s+2,e-1))+2018
      seireki<-paste0(S,str_sub(Date,e,-1))
      DF$Date[k]<-seireki
      if(as.Date(seireki,"%Y年%m月%d日")>="2022-01-26"){
         #pd<-pdf_data(paste0(path,RH[k,1]))[[1]]
         pd<-pdf_text(paste0(path,RH[k,1]))%>%
            base::strsplit("\n\n")%>%
            data.frame()
         colnames(pd)<-"text"
         pd2<-pd%>%
            filter(str_detect(text,"居住地別"))%>%
            mutate(text2=str_replace_all(text," +","_"))
         re<-regexpr("函館市_.{1,3}_",pd2$text2)
         at<-attr(re,"match.length")
         DF$函館市内[k]<-substring(pd2$text2,re+4,re+at-2)
         re<-regexpr("函館市外（道内）_.{1,3}_",pd2$text2)
         at<-attr(re,"match.length")
         DF$北海道内[k]<-substring(pd2$text2,re+9,re+at-2)
         re<-regexpr("函館市外（道外）_.{1,3}_??",pd2$text2)
         at<-attr(re,"match.length")
         DF$北海道外[k]<-substring(pd2$text2,re+9,re+at)%>%str_remove("_")
        # pd<-pdf_data(paste0(path,RH[k,1]))[[1]]$text
        # TD<-pd%>%data.frame()%>%rename("text"=".")
        # wh1<-which(str_detect(TD$text,"居住地別"))
        # DF$函館市内[k]<-TD$text[wh1+2]
        # 
        # wh2<-which(str_detect(TD$text,"函館市.+道内"))
        # DF$北海道内[k]<-TD$text[wh2+1]
        # wh3<-which(str_detect(TD$text,"函館市.+道外"))
        # DF$北海道外[k]<-TD$text[wh3+1]
      }else{
      if(as.Date(seireki,"%Y年%m月%d日")=="2022-01-25"){
        DF$函館市内[k]<-98
        DF$北海道内[k]<-0
        DF$北海道外[k]<-0
      }
        if(as.Date(seireki,"%Y年%m月%d日")=="2022-01-24"){
          DF$函館市内[k]<-38
          DF$北海道内[k]<-0
          DF$北海道外[k]<-0
        }
      }
      }
    write.csv(DF,
              "hakodate2.csv",
              fileEncoding="UTF-8",row.names = F)
    # TA2<-TA[,c(1:6,8)]
    # TA3<-
    #   TA2%>%
    #   mutate(No=as.numeric(No.))%>%
    #   rename("Date"="V8")%>%
    #   rename("北海道発表No"="北海道\r\n発表No.")%>%
    #   mutate(北海道発表No=as.integer(北海道発表No))%>%
    #   mutate(Date=gsub("令和5年","2023年",Date)) %>%
    #   mutate(Date=gsub("令和4年","2022年",Date)) %>%
    #   mutate(Date=gsub("令和3年","2021年",Date)) %>%
    #   mutate(Date=gsub("令和2年","2020年",Date))%>%
    #   mutate(Date=as.Date(Date,format="%Y年%m月%d日"))
    # write.csv(TA3%>%
    #             select(No,北海道発表No,判明日,Date,年代,性別,居住地),
    #           "hakodate202109.csv",fileEncoding="UTF-8",row.names = F)
    # data1<-
    #   read.csv("hakodate202108.csv",encoding = "UTF-8")
    # # data2<-
    # #   read.csv("hakodate202109.csv",encoding = "UTF-8")
    # # data3<-
    # #   rbind(data1,data2)%>%
    # #   mutate(No=as.numeric(No))%>%
    # #   arrange(desc(No))
    # write.csv(data1,
    #           "hakodate.csv",
    #           fileEncoding="UTF-8",row.names = F)
    print("出力しました")
    
    print(Sys.time())
    Sys.sleep(3600)
    
    }}
  