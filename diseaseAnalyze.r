ui <- fluidPage(

  navbarPage(":D",tabPanel("巨量資料分析",h1("傳染病（登革熱、急性病毒性Ｂ型肝炎、類流感）、地區及年齡層之關係")),

             navbarMenu("登革熱",
                        tabPanel("Plot1_1",imageOutput("Plot1_1")),
                        tabPanel("Plot1_2",imageOutput("Plot1_2")),
                        tabPanel("Plot1_3",imageOutput("Plot1_3")),
                        tabPanel("Dataframe",dataTableOutput('data1'))),

             navbarMenu("急性病毒性Ｂ型肝炎",
                        tabPanel("Plot2_1",imageOutput("Plot2_1")),
                        tabPanel("Plot2_2",imageOutput("Plot2_2")),
                        tabPanel("Plot2_3",imageOutput("Plot2_3")),
                        tabPanel("Dataframe",dataTableOutput('data2'))),

             navbarMenu("類流感",
                        tabPanel("Plot3_1",imageOutput("Plot3_1")),
                        tabPanel("Plot3_2",imageOutput("Plot3_2")),
                        tabPanel("Dataframe",dataTableOutput('data3')))


  ))

server <- function(input, output) {

  data1 <- read.csv("D:/R/dataset/dengue.csv")  #登革熱
  data2 <- read.csv("D:/R/dataset/B_liver.csv")  #急性病毒性Ｂ型肝炎
  data3 <- read.csv("D:/R/dataset/Influenza.csv")  #類流感

  output$data1 = renderDataTable({
    data1
  })
  output$data2 = renderDataTable({
    data2
  })
  output$data3 = renderDataTable({
    data3
  })

  #將data1年齡層中的日期格式資料去除
  #data1 <- data1[-which(substr(data1$年齡層,3,3)=="月"),]
  #data1 <- data1[-which(substr(data1$年齡層,2,2)=="月"),]
  colnames(data1) <- c("病名","年份","週別","縣市","鄉鎮","性別","是否境外移入","年齡層","人數")
  rownames(data1) <- NULL
  data1$年齡層 <- as.character(data1$年齡層)
  data1$縣市 <- as.character(data1$縣市)
  #為了在ggplot畫圖時 x軸會照著0 1 15-19 2 2X-2X 3 … => 0 01 02 03 04 15-19 …
  data1[which(data1$年齡層=="1"),]$年齡層 <- paste(0,data1[which(data1$年齡層=="1"),]$年齡層,sep="")
  data1[which(data1$年齡層=="2"),]$年齡層 <- paste(0,data1[which(data1$年齡層=="2"),]$年齡層,sep="")
  data1[which(data1$年齡層=="3"),]$年齡層 <- paste(0,data1[which(data1$年齡層=="3"),]$年齡層,sep="")
  data1[which(data1$年齡層=="4"),]$年齡層 <- paste(0,data1[which(data1$年齡層=="4"),]$年齡層,sep="")
  data1[which(substr(data1$年齡層,1,2)=="5-"),]$年齡層 <- paste(0,data1[which(substr(data1$年齡層,1,2)=="5-"),]$年齡層,sep="")
  data1$年份 <- as.factor(data1$年份)

  #將data2年齡層中的日期格式資料去除
  #data2 <- data2[-which(substr(data2$年齡層,3,3)=="月"),]
  #data2 <- data2[-which(substr(data2$年齡層,2,2)=="月"),]
  colnames(data2) <- c("病名","年份","月份","縣市","鄉鎮","性別","是否境外移入","年齡層","人數")
  rownames(data2) <- NULL
  data2$年齡層 <- as.character(data2$年齡層)
  data2$縣市 <- as.character(data2$縣市)
  #為了在ggplot畫圖時 x軸會照著0 1 15-19 2 2X-2X 3 … => 0 01 02 03 04 15-19 …
  data2[which(data2$年齡層=="1"),]$年齡層 <- paste(0,data2[which(data2$年齡層=="1"),]$年齡層,sep="")
  data2[which(data2$年齡層=="2"),]$年齡層 <- paste(0,data2[which(data2$年齡層=="2"),]$年齡層,sep="")
  data2[which(substr(data2$年齡層,1,2)=="5-"),]$年齡層 <- paste(0,data2[which(substr(data2$年齡層,1,2)=="5-"),]$年齡層,sep="")
  data2$年份 <- as.factor(data2$年份)

  colnames(data3) <- c("年份","週","年齡層","縣市","人數")
  data3$年齡層 <- as.character(data3$年齡層)
  #data3 <- data3[-which(substr(data3$年齡層,3,3)=="月"),]  在data3$年齡層中月份沒到兩位數
  #data3 <- data3[-which(substr(data3$年齡層,2,2)=="月"),]
  data3[which(substr(data3$年齡層,1,2)=="7-"),]$年齡層 <- paste(0,data3[which(substr(data3$年齡層,1,2)=="7-"),]$年齡層,sep="")
  data3$年份 <- as.factor(data3$年份)
  #開始繪圖
  #-------------------dengue----------------------------------------
  plot1_1 <- ggplot(data1, aes(年齡層, 人數,fill=縣市)) + geom_col()
  plot1_1 <- plot1_1 + labs(x = "年齡層", y = "人數", color = "縣市", title = "不同年齡層在各縣市得登革熱之情形", subtitle = "", caption = "", tag = "")
  #為了讓x軸的字不要互相檔到 因此讓他們有角度去顯示
  plot1_1 <- plot1_1 + theme(axis.text.x = element_text(face="bold", color="#993333", size=14, angle=45),
                             axis.text.y = element_text(face="bold", color="#993333", size=14))
  #print(plot1_1)
  data1 <- ddply(.data = data1 , c("縣市","年份"))  #排序過後 圖片顯示出來才正常
  plot1_2 <- ggplot(data1, aes(縣市, 人數,fill=年份)) + geom_col()
  plot1_2 <- plot1_2 + labs(x = "縣市", y = "人數", color = "年份", title = "不同縣市在各年份區間得登革熱之情形", subtitle = "", caption = "", tag = "")
  plot1_2 <- plot1_2 + theme(axis.text.x = element_text(face="bold", color="#993333", size=14, angle=90),
                             axis.text.y = element_text(face="bold", color="#993333", size=14))
  #print(plot1_2)
  plot1_3 <- ggplot(data1, aes(性別 ,fill=縣市)) + geom_histogram(stat="count",position="dodge")
  plot1_3 <- plot1_3 + labs(x = "性別", y = "人數", color = "縣市", title = "不同性別在各縣市得登革熱之情形", subtitle = "", caption = "", tag = "")
  #print(plot1_3)

  #------------------B_liver----------------------------------------
  plot2_1 <- ggplot(data2, aes(年齡層, 人數,fill=縣市)) + geom_col()
  plot2_1 <- plot2_1 + labs(x = "年齡層", y = "人數", color = "縣市", title = "不同年齡層在各縣市得急性病毒性Ｂ型肝炎之情形", subtitle = "", caption = "", tag = "")
  plot2_1 <- plot2_1 + theme(axis.text.x = element_text(face="bold", color="#993333", size=14, angle=45),
                             axis.text.y = element_text(face="bold", color="#993333", size=14))
  #print(plot2_1)

  data2 <- ddply(.data = data2 , c("縣市","年份"))
  plot2_2 <- ggplot(data2, aes(縣市, 人數,fill=年份)) + geom_col()
  plot2_2 <- plot2_2 + labs(x = "縣市", y = "人數", color = "年份", title = "不同縣市在各年份區間得急性病毒性Ｂ型肝炎之情形", subtitle = "", caption = "", tag = "")
  plot2_2 <- plot2_2 + theme(axis.text.x = element_text(face="bold", color="#993333", size=14, angle=90),
                             axis.text.y = element_text(face="bold", color="#993333", size=14))
  #print(plot2_2)

  plot2_3 <- ggplot(data2, aes(性別 ,fill=縣市)) + geom_histogram(stat="count",position="dodge")
  plot2_3 <- plot2_3 + labs(x = "性別", y = "人數", color = "縣市", title = "不同性別在各縣市得急性病毒性Ｂ型肝炎之情形", subtitle = "", caption = "", tag = "")
  #print(plot2_3)

  #-------------------Influenza-------------------------------------
  plot3_1 <- ggplot(data3, aes(年齡層, 人數,fill=縣市)) + geom_col()
  plot3_1 <- plot3_1 + labs(x = "年齡層", y = "人數", color = "縣市", title = "不同年齡層在各縣市得類流感之情形", subtitle = "", caption = "", tag = "")
  plot3_1 <- plot3_1 + theme(axis.text.x = element_text(face="bold", color="#993333", size=14, angle=45),
                             axis.text.y = element_text(face="bold", color="#993333", size=14))
  #print(plot3_1)

  data3 <- ddply(.data = data3 , c("縣市","年份"))
  plot3_2 <- ggplot(data3, aes(縣市, 人數,fill=年份)) + geom_col()
  plot3_2 <- plot3_2 + labs(x = "縣市", y = "人數", color = "年份", title = "不同縣市在各年份區間得類流感之情形", subtitle = "", caption = "", tag = "")
  plot3_2 <- plot3_2 + theme(axis.text.x = element_text(face="bold", color="#993333", size=14, angle=90),
                             axis.text.y = element_text(face="bold", color="#993333", size=14))
  #print(plot3_2)

  #-----------------------迴歸分析------------------------
  data1.lm <- lm(人數 ~ 性別, data = data1)
  cat("---------Summary(data1.lm): dengue-------------")
  print(summary(data1.lm))
  cat("\n\n")

  data2.lm <- lm(人數 ~ 性別, data = data2)
  cat("---------Summary(data2.lm):  B_liver-----------")
  print(summary(data2.lm))
  cat("\n\n")

  data3.lm <- lm(人數 ~ 年份, data = data3)
  cat("---------Summary(data3.lm):  Influenza---------")
  print(summary(data3.lm))
  cat("\n\n")
  #-------------------dengue----------------------------------------
  output$Plot1_1 <- renderPlot({
    plot1_1
  })
  output$Plot1_2 <- renderPlot({
    plot1_2
  })
  output$Plot1_3 <- renderPlot({
    plot1_3
  })
  #-------------------B_liver----------------------------------------
  output$Plot2_1 <- renderPlot({
    plot2_1
  })
  output$Plot2_2 <- renderPlot({
    plot2_2
  })
  output$Plot2_3 <- renderPlot({
    plot2_3
  })
  #-------------------Influenza----------------------------------------
  output$Plot3_1 <- renderPlot({
    plot3_1
  })
  output$Plot3_2 <- renderPlot({
    plot3_2
  })

  #output$text1 <- renderText({
  #})



}
shinyApp(ui, server)
