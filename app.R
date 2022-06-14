#import library
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)

#read dataset
jpp <- read.csv("https://raw.githubusercontent.com/Dinjuro/tetris/main/Jumlah%20Pengangguran%20Menurut%20Provinsi.csv")
jps <- read.csv("https://raw.githubusercontent.com/Dinjuro/tetris/main/Jumlah%20Pengangguran%20Menurut%20Pendidikan%20Terakhir%20Yang%20Ditamatkan.csv")
jpu <- read.csv("https://raw.githubusercontent.com/Dinjuro/tetris/main/Jumlah%20Pengangguran%20Menurut%20Kelompok%20Umur.csv")

#inspeksi dataset
head(jpp)
tail(jpp)
str(jpp)
dim(jpp)
summary(jpp)
names(jpp)

head(jps)
tail(jps)
str(jps)
dim(jps)
summary(jps)
names(jps)

head(jpu)
tail(jpu)
str(jpu)
dim(jpu)
summary(jpu)
names(jpu)


#visualisasi shiny
Provinsi <- c(unique(jpp$Provinsi))
Pendidikan.Terakhir <- c(unique(jps$Pendidikan.Terakhir.Yang.Ditamatkan))
Golongan.Umur <- c(unique(jpu$Golongan.Umur))

ui <- fluidPage(
  h1("Pandemi Covid-19 Meningkatkan Jumlah Pengangguran di Indonesia"),
  br(),
  h4("Pada awal tahun 2020, Pandemi Covid-19 mulai memasuki Indonesia.
     Banyak sekali sektor yang terdampak akibat datanganya pandemi tersebut. 
     Salah satu sektor yang terdampak adalah sektor ekonomi, 
     yang berujung dengan meningkatnya jumlah pengangguran di Indonesia.
     Berikut adalah data yang diambil dari laman bps.go.id mengenai pengangguran di Indonesia selama 6 tahun terakhir."),
 
   #Plot1
  plotOutput("indonesia"),
  br(),
  h4("Dapat dilihat dari grafik, bahwa tingkat pengangguran di Indonesia pada Tahun 2016 - 2019 cenderung konstan. 
    Kemudian, pada Tahun 2020 terjadi kenaikan jumlah pengangguran yang sangat signifikan. 
    Hal ini disebabkan oleh pandemi covid-19 yang mengakibatkan perekonomian Indonesia menurun. 
    Banyak perusahaan yang merugi bahkan sampai harus gulung tikar. 
    Akibatnya, perusahaan dengan terpaksa harus mem-PHK pekerjanya demi meringankan beban perusahaan."),
  br(),
  
  h4("Untuk melihat lebih detail tentang peningkatan jumlah jumlah pengangguran di Indonesia. 
     Kita bisa membagi peningkatan jumlah pengangguran di Indonesia menurut 3 indikator. Yaitu, menurut provinsi, 
     menurut pendidikan terakhir yang ditamatkan, dan menurut golongan umur."),
  br(),
  
  #Plot2
  h4("Berikut adalah peningkatan jumlah pengangguran di indonesia menurut Provinsi."),
  selectInput("pilih_prov", "Pilih Provinsi:", Provinsi),
  plotOutput("prov"),
  br(),
  
  #Plot3
  h4("Berikut adalah peningkatan jumlah pengangguran di indonesia menurut pendidikan terakhir yang ditamatkan."),
  selectInput("pilih_pend", "Pilih Pendidikan:", Pendidikan.Terakhir),
  plotOutput("pend"),
  br(),
  
  #Plot4
  h4("Berikut adalah peningkatan jumlah pengangguran di indonesia menurut golongan umur."),
  selectInput("pilih_umur", "Pilih Golongan Umur:", Golongan.Umur),
  plotOutput("umur")
)

server <- function (input, output, session) {
  #Plot1
  output$indonesia <- renderPlot(
    ggplot(data = jpp %>%
                  group_by(Tahun) %>%
                  summarise(Jumlah.Pengangguran=sum(Jumlah.Pengangguran)),
            mapping = aes(x = Tahun, y = Jumlah.Pengangguran)) +
    geom_point() +
    geom_line(color="red") + 
    geom_text(aes(label=Jumlah.Pengangguran),hjust=0.5, vjust=-0.5) + 
    ylim(0,10000000) +
    ggtitle("Jumlah Pengangguran di Indonesia Tahun 2016 - 2021")
  )
  
  #Plot2
  output$prov <- renderPlot({
    ggplot(data = jpp %>%
                    filter(Provinsi==input$pilih_prov) %>%
                    group_by(Tahun) %>%
                    summarise(Jumlah.Pengangguran=sum(Jumlah.Pengangguran)),
            mapping = aes(x=Tahun,y=Jumlah.Pengangguran)) +
    geom_point() +
    geom_line(color = "darkblue") + 
    geom_text(aes(label=Jumlah.Pengangguran),hjust=0.5, vjust=-0.5) +
    ggtitle("PROVINSI", input$pilih_prov)
  })
  
  #plot3
  output$pend <- renderPlot({
    ggplot(data = jps %>%
             filter(Pendidikan.Terakhir.Yang.Ditamatkan==input$pilih_pend) %>%
             group_by(Tahun) %>%
             summarise(Jumlah.Pengangguran=sum(Jumlah.Pengangguran)),
           mapping = aes(x=Tahun,y=Jumlah.Pengangguran)) +
    geom_point() +
    geom_line(color = "gold") + 
    geom_text(aes(label=Jumlah.Pengangguran),hjust=0.5, vjust=-0.5) +
    ggtitle("PENDIDIKAN TERAKHIR", input$pilih_pend)
  })
  
  #Plot4
  output$umur <- renderPlot({
    ggplot(data = jpu %>%
             filter(Golongan.Umur==input$pilih_umur) %>%
             group_by(Tahun) %>%
             summarise(Jumlah.Pengangguran=sum(Jumlah.Pengangguran)),
           mapping = aes(x=Tahun,y=Jumlah.Pengangguran)) +
    geom_point() +
    geom_line(color = "dark green") + 
    geom_text(aes(label=Jumlah.Pengangguran),hjust=0.5, vjust=-0.5) +
    ggtitle("GOLONGAN UMUR", input$pilih_umur)
  })
}

shinyApp(ui, server)