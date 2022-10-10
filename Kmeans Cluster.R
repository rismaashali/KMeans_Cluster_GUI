library(shiny)
library(shinythemes)

#membuat UI
myui<-fluidPage(
  theme = shinytheme("united"),
  titlePanel(h1("Analisis Cluster Non-Hirarki (K-Means Cluster)")),
  titlePanel(h4(strong("by: Risma Ashali Fauziah"))),
  navbarPage("",
             tabPanel("Data", icon = icon("table"),
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("data","Input file di 
sini:",accept = ".txt")
                        ),
                        mainPanel(br(strong("Berikut merupakan data yang digunakan untuk analisis cluster")),br(),dataTableOutput("tabel"),h5(strong("Tabel Statistika Deskriptif")),verbatimTextOutput("cluster"),br()
                        )
                      )),
             tabPanel("Jarak",icon = icon("chess-board"),
                      mainPanel(br("Di bawah ini adalah visualisasi dan nilai dari jarak antar data"),h5("Keterangan:"),h5("Hijau = Dekat ; Putih = Sedang ; Merah = Jauh"),br(),plotOutput("plot1"),br(),verbatimTextOutput("cluster1"),br()
                      )
             ),
             tabPanel("Cluster Optimal",icon = icon("chart-bar"),
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("Metode","Pilih Metode:",choices = c("Elbow"="wss","Silhouette"="silhouette")),
                        ),
                        mainPanel(plotOutput("plot2"))
                      )),
             tabPanel("K-Means Cluster",icon = icon("list-alt"),
                      sidebarLayout(
                        sidebarPanel(
                          textInput("Jml","Masukkan Jumlah Cluster"),
                          actionButton("cekbutton","HITUNG",icon = icon("redo"),class="btn-success"),
                        ),
                        mainPanel(plotOutput("plot3"),br(),verbatimTextOutput("cekklaster"),h5(strong("Total Withinss:")),verbatimTextOutput("tw"),h5(strong("Betweenss:")),verbatimTextOutput("b"),h5(strong("Note:")),h5("Cluster dinyatakan baik apabila nilai total withinss kecil dan nilai beetweenss besar"),br())
                      ))
  )
)

#membuat server
myserver<-function(input,output,session){
  output$tabel<-renderDataTable({
    datainput=input$data
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    datapakai
  })
  output$cluster<-renderPrint({
    datainput=input$data
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    summary(datapakai)
  })
  output$plot1<-renderPlot({
    datainput=input$data
    if (is.null(datainput)){return()}
    library(factoextra)
    library(gridExtra)
    datapakai=read.delim(datainput$datapath,header = T)
    distance<-get_dist(datapakai)
    plotfix<-fviz_dist(distance, gradient = list(low = "green", mid = "white", high = "red"))
    cat("Berikut merupakan jarak korelasi antar data:\n")
    plot(plotfix)
  })
  output$cluster1<-renderPrint({
    datainput=input$data
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    distance<-get_dist(datapakai)
    distance
  })
  output$plot2<-renderPlot({
    datainput=input$data
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    datafix=datapakai[,2:4]
    plotfix2<-fviz_nbclust(datafix, kmeans, method = input$Metode, print.summary = F)
    plot(plotfix2)
  })
  observeEvent(input$cekbutton,{
    datainput=input$data
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    datafix=datapakai[,2:4]
    jumlah<-input$Jml
    jumlah=as.numeric(jumlah)
    klaster <- kmeans(datafix,centers=jumlah, nstart = 25)
    TW=klaster$tot.withinss
    B=klaster$betweenss
    p <- fviz_cluster(klaster, geom = "point", data = datafix)
    output$plot3<-renderPlot(p)
    output$cekklaster<-renderPrint(klaster)
    output$tw<-renderPrint(TW)
    output$b<-renderPrint(B)
  })
}

shinyApp(ui=myui,server=myserver)