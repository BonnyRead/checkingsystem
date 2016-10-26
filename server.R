
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(magrittr)
library(stringr)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(httr)
library(xml2)
library(mlr)


shinyServer(function(input, output) {
  
  options(shiny.maxRequestSize=30*1024^2)
  
  #依前端送來的路徑將資料讀入
  
  datafile <- eventReactive(input$act,{
    if (is.null(input$file1) | is.null(input$file2)  ) {return(NULL)}
    tmpdata1 <- read.csv(input$file1$datapath,stringsAsFactors = FALSE)
    tmpdata2 <- read.csv(input$file2$datapath,stringsAsFactors = FALSE)
    tmpdata2[!tmpdata2$訂單日期=="",] -> rawraw
    mapvalues(tmpdata2$訂單號碼,rawraw$訂單號碼,rawraw$訂單日期) -> tmpdata2$訂單日期
    
  # 排除預購字眼避免重複計算商品
    
    str_replace_all(tmpdata1$商品名稱,"(\\[.*?\\])|\\【.*?\\】","") %>% trimws -> tmpdata1$商品名稱
    str_replace_all(tmpdata2$商品名稱,"(\\[.*?\\])|\\【.*?\\】","") %>% trimws -> tmpdata2$商品名稱
    
  # 替換變數名稱以利後續的操作
    
    colnames(tmpdata2)[colnames(tmpdata2)=="商品名稱"] <- "itemname"
    colnames(tmpdata2)[colnames(tmpdata2)=="選項"] <- "spec"
    colnames(tmpdata2)[colnames(tmpdata2)=="訂單日期"] <- "orderingdate"
    colnames(tmpdata1)[colnames(tmpdata1)=="商品名稱"] <- "itemname"
    colnames(tmpdata1)[colnames(tmpdata1)=="選項"] <- "spec"
    colnames(tmpdata1)[grep("庫存",colnames(tmpdata1))] <- "Remain"
    colnames(tmpdata1)[grep("商店貨號",colnames(tmpdata1))] <- "itemid"
    colnames(tmpdata1)[grep("已確認",colnames(tmpdata1))] <- "Salespermitted"
    
  # 合併資料
    
    tmpdata2 %>% group_by(orderingdate,itemname,spec) %>% summarise(itemsales = sum(數量) ) -> Salesdata
    spread(Salesdata,key = "orderingdate",value  = "itemsales") -> Salesmerge
    tmpdata1 %>% select(itemid,itemname,spec,Remain,Salespermitted) -> Stockingmerge
    merge(Stockingmerge,Salesmerge,by=c("itemname","spec")) -> AlarmingBase
    apply( AlarmingBase,1,function(k) { k %>% .[-1:-4] %>% as.numeric } ) %>% t -> buyingmatrix
    buyingmatrix[is.na(buyingmatrix)] <- 0
    AlarmingBase %>% select( 1 : 5 ) -> AlarmingBase
    
    
    # 將產品做分類
    
    AlarmingBase[grepl("戒指|對戒|戒組|尾戒|關節戒|連指戒|情侶戒|三件戒|開口戒",AlarmingBase$itemname),"category"]<-"戒指"
    AlarmingBase[grepl("耳環|耳針|耳扣|耳夾",AlarmingBase$itemname),"category"]<-"耳環"
    AlarmingBase[grepl("項鍊|鎖骨鍊|頸鍊|頸圈",AlarmingBase$itemname),"category"]<-"項鍊"
    AlarmingBase[grepl("手鍊|手環|手鐲",AlarmingBase$itemname),"category"]<-"手鍊"
    AlarmingBase[grepl("髮飾|髮帶|髮圈|髮夾|髮箍",AlarmingBase$itemname),"category"]<-"髮飾"
    AlarmingBase[grepl("手錶",AlarmingBase$itemname),"category"]<-"手錶"
    AlarmingBase[grepl("刺青貼紙",AlarmingBase$itemname),"category"]<-"刺青貼紙"
    AlarmingBase[grepl("墨鏡",AlarmingBase$itemname),"category"]<-"墨鏡"
    AlarmingBase[grepl("腳鍊",AlarmingBase$itemname),"category"]<-"腳鍊"
    AlarmingBase[is.na(AlarmingBase$category),"category"]<-"其它"
    
    AlarmingBase[grepl("Gold",AlarmingBase$spec),"color"] <- "Gold"
    AlarmingBase[grepl("Black",AlarmingBase$spec),"color"] <- "Black"
    AlarmingBase[grepl("Pink",AlarmingBase$spec),"color"] <- "Pink"
    AlarmingBase[grepl("Yellow",AlarmingBase$spec),"color"] <- "Yellow"
    AlarmingBase[grepl("Blue",AlarmingBase$spec),"color"] <- "Blue"
    AlarmingBase[grepl("Red",AlarmingBase$spec),"color"] <- "Red"
    AlarmingBase[grepl("White",AlarmingBase$spec),"color"] <- "White"
    AlarmingBase[grepl("Brown",AlarmingBase$spec),"color"] <- "Brown"
    AlarmingBase[grepl("Purple",AlarmingBase$spec),"color"] <- "Purple"
    AlarmingBase[grepl("Orange",AlarmingBase$spec),"color"] <- "Orange"
    AlarmingBase[grepl("Rose Gold",AlarmingBase$spec),"color"] <- "Rose Gold"
    AlarmingBase[grepl("Gray",AlarmingBase$spec),"color"] <- "Gray"
    AlarmingBase[grepl("Green",AlarmingBase$spec),"color"] <- "Green"
    AlarmingBase[grepl("Silver",AlarmingBase$spec),"color"] <- "Silver"
    AlarmingBase[grepl("Gold",AlarmingBase$spec),"color"] <- "Gold"
    AlarmingBase[is.na(AlarmingBase$color),"color"] <- "No Show or rare color"
    
    
    # 對要進行隨機森林的變數進行調整
    
    data.frame () -> ramdata
    for (i in 14 : 31) {
      data.frame(
        category = factor (AlarmingBase$category ),
        color = factor (AlarmingBase$color ),
        recent3 = apply(buyingmatrix,1,function(k) { k[ ( i - 9) : ( i - 7 ) ] %>% sum } ),
        recent7 = apply(buyingmatrix,1,function(k) { k[ ( i - 13 ) :( i - 7 ) ] %>% sum } ),
        target = apply(buyingmatrix,1,function(k) { k[ ( i - 6) : i  ] %>% sum } )
      ) -> shorttermdata
      rbind(ramdata,shorttermdata) -> ramdata
    }
    
    data.frame(
      category = factor (AlarmingBase$category),
      color = factor (AlarmingBase$color),
      recent3 = apply(buyingmatrix,1,function(k) { k[ (length(k) - 2) : (length(k) ) ] %>% sum } ),
      recent7 = apply(buyingmatrix,1,function(k) { k[ (length(k) - 6) : (length(k) ) ] %>% sum } )
    ) -> predata
    
    # 進行Bayesian Regularization for Feed-Forward Neural Networks模型訓練與預測
    
    makeRegrTask("bobochacha",ramdata,"target") -> issac
    makeLearner("regr.brnn",par.vals = list(neurons = 3) ) -> lrn
    train(lrn,issac) -> mod
    predictLearner(lrn,mod,predata) -> pooh
    pooh + (pooh * input$conservativepara) -> AlarmingBase$future14
    AlarmingBase[AlarmingBase$future14 < 0,"future14"] <- 0
    ifelse(AlarmingBase$Remain < AlarmingBase$future14, "Run out" , "Stayed") -> AlarmingBase$Replenishment
    
    
    
    # 排除官網上沒有的品項
    
    paste0("http://www.bonnyread.com.tw/products?page=",1:90) -> klist
    sapply(klist,function(k) { k %>% GET %>% content(encoding = "UTF-8") %>%
        xml_find_all("//div[@class='title text-primary-color']") %>% xml_text(trim = TRUE)}) %>% unlist -> namelist
    str_replace_all(namelist,"(\\[.*?\\])|\\【.*?\\】","") -> namelist
    AlarmingBase[AlarmingBase$itemname %in% namelist,] -> AlarmingBase
    
    # 輸出報表
    
    AlarmingBase %>% select (-color,-category) %>% filter( !Remain == 0 ) -> AlarmingBase
    data.frame(AlarmingBase)
     } )
  
    # 針對輸入的資料做進貨推薦
  
  datafile2 <- eventReactive(input$act2,{
    data.frame(datafile)
  })
 
    # 輸出進貨表
  
  output$contents <- renderDataTable({
      datafile() 
  })
  
  # 製作輸出按鈕
  
  output$downloadthis <- downloadHandler(
    filename = function() {
      paste('replenishment','csv',sep = ".")
    },
    content = function(file) {
      write.csv(datafile(),file,row.names = FALSE)
    }
  )
  
  output$recommendations <- renderDataTable({
    data.frame(datafile2)
  })
 

}) 
