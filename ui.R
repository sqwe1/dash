#Getting the required files for descriptives and charts
file1<-read.csv("./eo final senti.csv",stringsAsFactors = F,header = T)
file2<-read.csv("./OO final senti.csv",stringsAsFactors = F,header = T)
file3<-read.csv("./Te final senti.csv",stringsAsFactors = F,header = T)
file4<-read.csv("./Th final senti.csv",stringsAsFactors = F,header = T)
file5<-read.csv("./Vi final senti.csv",stringsAsFactors = F,header = T)
file6<-read.csv("./Vo final senti.csv",stringsAsFactors = F,header = T)

#File import for word cloud
w1<-read.csv("./nee.csv")
w1<-as.data.frame(w1)
w2<-read.csv("./noo.csv")
w2<-as.data.frame(w2)
w3<-read.csv("./nte.csv")
w3<-as.data.frame(w3)
w4<-read.csv("./nth.csv")
w4<-as.data.frame(w4)
w5<-read.csv("./nvi.csv")
w5<-as.data.frame(w5)
w6<-read.csv("./nvo.csv")
w6<-as.data.frame(w6)

#Getting the required packages

# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("ECharts2Shiny")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("plotly")

library(shiny)
library(shinydashboard)
library(plotly)
# library(ECharts2Shiny)
 library(wordcloud)
 library(RColorBrewer)

title1 <- tags$a(href='https://www.google.com',
               tags$img(src="radio-tower.png", height = '50', width = '50'),
               'Telecom Dashboard', target="_blank")




shinyUI(
  dashboardPage(title=title1, skin = "purple",
    dashboardHeader(title = title1),
    dashboardSidebar(
      sliderInput("bins","Number of breaks",1,20,5),
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("Th","Vo","Te","Vi","Eo","Oo")),
      selectInput(inputId="emotion",
                  label = "select an emotion",
                  choices = c("Positive","Negative")),
      selectInput("inField1","Select a field to create a Histogram", choices = names(file4[,3:12])),
      selectInput("inField2","Choose a month",choices=names(file4[,18:36]))),
    dashboardBody(
      fluidPage(
      dateRangeInput("dates", 
                     "Date range",
                     start = "2015-01-01", 
                     end = as.character(Sys.Date())),
      textOutput("DateRange")
    ),
      
                fluidRow(
                  infoBoxOutput("No.ofreviews", width = 3),
                  infoBoxOutput("Positive", width = 3),
                  infoBoxOutput("Neutral", width = 3),
                  infoBoxOutput("Negative", width = 3),
                  box(plotOutput("histogram1")),
                  box(plotOutput("histogram2")),
                  box(plotOutput("line1")),
                  box(plotOutput("bar1")),
                  box(plotOutput("bar2")),
                  box(plotOutput("line2")),
                  box(plotOutput("pie1")),
                  box(plotOutput("word1"))
                  
      )
      
      
    )
  )
)

