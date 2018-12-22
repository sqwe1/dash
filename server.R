shinyServer(function(input,output,session) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Th" = file4,
           "Vo" = file6,
           "Te"=file3,
           "Vi"=file5,
           "Eo"=file1,
           "Oo"=file2)
  })
  
  emotionInput <- reactive({
    switch(input$emotion,
           c("Positive","Negative") )
  })
  
  output$DateRange <- renderText({
    # make sure end date later than start date
    validate(
      need(input$dates[2] > input$dates[1], "end date is earlier than start date"
      )
    )
    
    # make sure greater than 2 week difference
    validate(
      need(difftime(input$dates[2], input$dates[1], "days") > 14, "date range less the 14 days"
      )
    )
    
    paste("Your date range is", 
          difftime(input$dates[2], input$dates[1], units="days"),
          "days")
  })

  output$No.ofreviews<- renderInfoBox({
    
    if(input$dataset=="Th"){
      infoBox(title = "No. of reviews", 
              value = nrow(file4)
      ) }
    else if (input$dataset=="Vo"){
      infoBox(title = "No. of reviews", 
              value = nrow(file6)
            
      )}
    else if (input$dataset=="Te"){
      infoBox(title = "No. of reviews", 
              value = nrow(file3)
      )}
    else if (input$dataset=="Vi"){
      infoBox(title = "No. of reviews", 
              value = nrow(file5)
      )}
    else if (input$dataset=="Eo"){
      infoBox(title = "No. of reviews", 
              value = nrow(file1)
      )}
    else {infoBox(title = "No. of reviews", 
                  value = nrow(file2))}
    
  })

    output$Positive <- renderInfoBox({
    
    if(input$dataset=="Th"){
      infoBox(title = "", 
              value = as.data.frame(table(file4$category_senti1))[3,],
              icon("arrow-up"),
              subtitle = "",
              color = "green"
      ) }
    else if (input$dataset=="Vo"){
      infoBox(title = "", 
              value = as.data.frame(table(file6$category_senti1))[3,],
              icon("arrow-up"),
              subtitle = "",
              color = "green"
      )}
    else if (input$dataset=="Te"){
      infoBox(title = "", 
              value = as.data.frame(table(file3$category_senti1))[3,],
              icon("arrow-up"),
              subtitle = "",
              color = "green"
      )}
    else if (input$dataset=="Vi"){
      infoBox(title = "", 
              value = as.data.frame(table(file5$category_senti1))[3,],
              icon("arrow-up"),
              subtitle = "",
              color = "green"
      )}
    else if (input$dataset=="Eo"){
      infoBox(title = "", 
              value = as.data.frame(table(file1$category_senti1))[3,],
              icon("arrow-up"),
              subtitle = "",
              color = "green"
      )}
    else {infoBox(title = "", 
                  value = as.data.frame(table(file2$category_senti1))[3,],
                  icon("arrow-up"),
                  subtitle = "",
                  color = "green")}
    
  })
  output$Neutral <- renderInfoBox({if(input$dataset=="Th"){
    infoBox(title = "", 
            value = as.data.frame(table(file4$category_senti1))[2,],
            icon("arrows-h"),
            subtitle = "",
            color = "yellow"
    ) }
    else if (input$dataset=="Vo"){
      infoBox(title = "", 
              value = as.data.frame(table(file6$category_senti1))[2,],
              icon("arrows-h"),
              subtitle = "",
              color = "yellow"
      )}
    else if (input$dataset=="Te"){
      infoBox(title = "", 
              value = as.data.frame(table(file3$category_senti1))[2,],
              icon("arrows-h"),
              subtitle = "",
              color = "yellow"
      )}
    else if (input$dataset=="Vi"){
      infoBox(title = "", 
              value = as.data.frame(table(file5$category_senti1))[2,],
              icon("arrows-h"),
              subtitle = "",
              color = "yellow"
      )}
    else if (input$dataset=="Eo"){
      infoBox(title = "", 
              value = as.data.frame(table(file1$category_senti1))[2,],
              icon("arrows-h"),
              subtitle = "",
              color = "yellow"
      )}
    else {infoBox(title = "", 
                  value = as.data.frame(table(file2$category_senti1))[2,],
                  icon("arrows-h"),
                  subtitle = "",
                  color = "yellow")}
    
  })
  
  
  output$Negative <- renderInfoBox({
    if(input$dataset=="Th"){
      infoBox(title = "", 
              value = as.data.frame(table(file4$category_senti1))[1,],
              icon("arrow-down"),
              subtitle = "",
              color = "red"
      ) }
    else if (input$dataset=="Vo"){
      infoBox(title = "", 
              value = as.data.frame(table(file6$category_senti1))[1,],
              icon("arrow-down"),
              subtitle = "",
              color = "red"
      )}
    else if (input$dataset=="Te"){
      infoBox(title = "", 
              value = as.data.frame(table(file3$category_senti1))[1,],
              icon("arrow-down"),
              subtitle = "",
              color = "red"
      )}
    else if (input$dataset=="Vi"){
      infoBox(title = "", 
              value = as.data.frame(table(file5$category_senti1))[1,],
              icon("arrow-down"),
              subtitle = "",
              color = "red"
      )}
    else if (input$dataset=="Eo"){
      infoBox(title = "", 
              value = as.data.frame(table(file1$category_senti1))[1,],
              icon("arrow-down"),
              subtitle = "",
              color = "red"
      )}
    else {infoBox(title = "", 
                  value = as.data.frame(table(file2$category_senti1))[1,],
                  icon("arrow-down"),
                  subtitle = "",
                  color = "red")}
    
  })
  
  
  
  output$histogram1<-renderPlot({ifelse(input$dataset=="Th",hist(file4[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of Three"),xlab = paste("Emotion Score"),ylab = paste("Frequency")),
                                        ifelse(input$dataset=="Vo",hist(file6[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of Vodafone"),xlab = paste("Emotion Score"),ylab = paste("Frequency")),
                                               ifelse(input$dataset=="Te",hist(file3[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of Tesco"),xlab = paste("Emotion Score"),ylab = paste("Frequency")),
                                                      ifelse(input$dataset=="Vi",hist(file5[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of Virgin mobile"),xlab = paste("Emotion Score"),ylab = paste("Frequency")),
                                                             ifelse(input$dataset=="Eo",hist(file1[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of EE"),xlab = paste("Emotion Score"),ylab = paste("Frequency")),
                                                                    hist(file2[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of O2"),xlab = paste("Emotion Score"),ylab = paste("Frequency")))))))
    
  })
  output$histogram2<-renderPlot({ifelse(input$dataset=="Th",hist(file4$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of Three"),xlab = paste("Sentiment values"),ylab = paste("Frequency")),
                                        ifelse(input$dataset=="Vo",hist(file6$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of Vodafone"),xlab = paste("Sentiment values"),ylab = paste("Frequency")),
                                               ifelse(input$dataset=="Te",hist(file3$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of Tesco"),xlab = paste("Sentiment values"),ylab = paste("Frequency")),
                                                      ifelse(input$dataset=="Vi",hist(file5$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of Virgin mobile"),xlab = paste("Sentiment values"),ylab = paste("Frequency")),
                                                             ifelse(input$dataset=="Eo",hist(file1$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of EE"),xlab = paste("Sentiment values"),ylab = paste("Frequency")),
                                                                    hist(file2$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of O2"),xlab = paste("Sentiment values"),ylab = paste("Frequency")))))))


})
  
 output$line1<-renderPlot({if(input$dataset=="Th"){
   plot(file4$sent.val1, type="line",main = "Line Chart of sentiment values of Three",xlab = "Reviews",ylab = "Sentiment values")
   }
                                else if(input$dataset=="Vo"){
                                  plot(file6$sent.val1, type="line",main = "Line Chart of sentiment values of Vodafone",xlab = "Reviews",ylab = "Sentiment values")
                                }
                                         else if(input$dataset=="Te"){plot(file3$sent.val1, type="line",main = "Line Chart of sentiment values of Tesco",xlab = "Reviews",ylab = "Sentiment values")}
                                                else if(input$dataset=="Vi"){plot(file5$sent.val1, type="line",main = "Line Chart of sentiment values of Virgin mobile",xlab = "Reviews",ylab = "Sentiment values")}
                                                       else if(input$dataset=="Eo"){plot(file1$sent.val1, type="line",main = "Line Chart of sentiment values of EE",xlab = "Reviews",ylab = "Sentiment values")}
                                                              else{plot(file2$sent.val1, type="line",main = "Line Chart of sentiment values of O2",xlab = "Reviews",ylab = "Sentiment values")}
})

 output$bar1<-renderPlot({if(input$dataset=="Th"){
   barplot(table(file4$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Three",xlab = "Months",ylab = "Count")
 }
   else if(input$dataset=="Vo"){
     barplot(table(file6$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Vodafone",xlab = "Months",ylab = "Count")
   }
   else if(input$dataset=="Te"){barplot(table(file3$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Tesco",xlab = "Months",ylab = "Count")}
   else if(input$dataset=="Vi"){barplot(table(file5$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Virgin mobile",xlab = "Months",ylab = "Count")}
   else if(input$dataset=="Eo"){barplot(table(file1$Month.and.Year),main = "Bar Chart of no. of monthly reviews of EE",xlab = "Months",ylab = "Count")}
   else{barplot(table(file2$Month.and.Year),main = "Bar Chart of no. of monthly reviews of O2",xlab = "Months",ylab = "Count")}})
 
 output$bar2<-renderPlot({if(input$dataset=="Th"){
   barplot(table(file4$category_senti1,file4$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Three",xlab = "Months",ylab = "Count",col=c("Red","Blue","Dark Green"))
 }
   else if(input$dataset=="Vo"){
     barplot(table(file6$category_senti1,file6$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Vodafone",xlab = "Months",ylab = "Count",col=c("Red","Blue","Dark Green"))
   }
   else if(input$dataset=="Te"){barplot(table(file3$category_senti1,file3$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Tesco",xlab = "Months",ylab = "Count",col=c("Red","Blue","Dark Green"))}
   else if(input$dataset=="Vi"){barplot(table(file5$category_senti1,file5$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Virgin mobile",xlab = "Months",ylab = "Count",col=c("Red","Blue","Dark Green"))}
   else if(input$dataset=="Eo"){barplot(table(file1$category_senti1,file1$Month.and.Year),main = "Bar Chart of no. of monthly reviews of EE",xlab = "Months",ylab = "Count",col=c("Red","Blue","Dark Green"))}
   else{barplot(table(file2$category_senti1,file2$Month.and.Year),main = "Bar Chart of no. of monthly reviews of O2",xlab = "Months",ylab = "Count",col=c("Red","Blue","Dark Green"))}})
 
 output$line2<-renderPlot({if(input$dataset=="Th"){plot(file4[,input$inField2], type="line", xlim = c(0,200),xlab = "Reviews",ylab = "Sentiment values",main = "Line Chart of monthly sentiment values of Three")}
     else if(input$dataset=="Vo"){plot(file6[,input$inField2], type="line", xlim = c(0,180),main = "Line Chart of monthly sentiment values of Vodafone",xlab = "Reviews",ylab = "Sentiment values")}
     else if(input$dataset=="Te"){plot(file3[,input$inField2], type="line", xlim = c(0,10),main = "Line Chart of monthly sentiment values of Tesco",xlab = "Reviews",ylab = "Sentiment values")}
     else if(input$dataset=="Vi"){plot(file5[,input$inField2], type="line", xlim = c(0,75),main = "Line Chart of monthly sentiment values of Virgin mobile",xlab = "Reviews",ylab = "Sentiment values")}
     else if(input$dataset=="Eo"){plot(file1[,input$inField2], type="line", xlim = c(0,110),main = "Line Chart of monthly sentiment values of EE",xlab = "Reviews",ylab = "Sentiment values")}
     else{plot(file2[,input$inField2], type="line", xlim = c(0,75),main = "Line Chart of monthly sentiment values of O2",xlab = "Reviews",ylab = "Sentiment values")}
 })
 
 output$pie1<-renderPlot({if(input$dataset=="Th"){
   pie(x=table(file4$category_senti1),
       density = 50,
       labels = paste(names(table(file4$category_senti1))," ",round(table(file4$category_senti1)/sum(table(file4$category_senti1))*100,2),"%",sep=''),
       col=c("Red","Blue","Dark Green"),
       main = "Pie Chart of sentiment categories of Three")
   
 }
   else if(input$dataset=="Vo"){
     pie(x=table(file6$category_senti1),
         density = 50,
         labels = paste(names(table(file6$category_senti1))," ",round(table(file6$category_senti1)/sum(table(file6$category_senti1))*100,2),"%",sep=''),
         col=c("Red","Blue","Dark Green"),
         main = "Pie Chart of sentiment categories of Vodafone")
     
   }
   else if(input$dataset=="Te"){pie(x=table(file3$category_senti1),
                                    density = 50,
                                    labels = paste(names(table(file3$category_senti1))," ",round(table(file3$category_senti1)/sum(table(file3$category_senti1))*100,2),"%",sep=''),
                                    col=c("Red","Blue","Dark Green"),
                                    main = "Pie Chart of sentiment categories of Tesco")
   }
   else if(input$dataset=="Vi"){pie(x=table(file5$category_senti1),
                                    density = 50,
                                    labels = paste(names(table(file5$category_senti1))," ",round(table(file5$category_senti1)/sum(table(file5$category_senti1))*100,2),"%",sep=''),
                                    col=c("Red","Blue","Dark Green"),
                                    main = "Pie Chart of sentiment categories of Virgin mobile")
   }
   else if(input$dataset=="Eo"){pie(x=table(file1$category_senti1),
                                    density = 50,
                                    labels = paste(names(table(file1$category_senti1))," ",round(table(file1$category_senti1)/sum(table(file1$category_senti1))*100,2),"%",sep=''),
                                    col=c("Red","Blue","Dark Green"),
                                    main = "Pie Chart of sentiment categories of EE")
   }
   else{pie(x=table(file2$category_senti1),
            density = 50,
            labels = paste(names(table(file2$category_senti1))," ",round(table(file2$category_senti1)/sum(table(file2$category_senti1))*100,2),"%",sep=''),
            col=c("Red","Blue","Dark Green"),
            main = "Pie Chart of sentiment categories of O2")
   }
 })
 
  output$word1<-renderPlot({if(input$dataset=="Th" & input$emotion=="Positive"){
    p11_th<-subset(w4,cat_sent_th=="Positive")
    p11_th<-as.data.frame(p11_th)
    wordcloud(words=p11_th$`wordcloud_bigram_th.ngrams`,freq=p11_th$`wordcloud_bigram_th.freq`,
              min.freq = 1,
              max.words=500, random.order=FALSE, 
              color="Dark Green")
    
  }
    else if(input$dataset=="Th" & input$emotion=="Negative"){
      b11_th<-subset(w4,cat_sent_th=="Negative")
      b11_th<-as.data.frame(b11_th)
      wordcloud(words=b11_th$`wordcloud_bigram_th.ngrams`,freq=b11_th$`wordcloud_bigram_th.freq`,
               min.freq = 2,
                max.words=200, random.order=FALSE, 
                colors="Red")
      
    }
    else if(input$dataset=="Vo" & input$emotion=="Positive"){
      p11_vo<-subset(w6,cat_sent_vo=="Positive")
      p11_vo<-as.data.frame(p11_vo)
      wordcloud(words=p11_vo$`wordcloud_bigram_vo.ngrams`,freq=p11_vo$`wordcloud_bigram_vo.freq`,
                min.freq = 2,
                max.words=500, random.order=FALSE, 
                color="Dark Green")
    }
    else if(input$dataset=="Vo" & input$emotion=="Negative"){
      b11_vo<-subset(w6,cat_sent_vo=="Negative")
      b11_vo<-as.data.frame(b11_vo)
      wordcloud(words=b11_vo$`wordcloud_bigram_vo.ngrams`,freq=b11_vo$`wordcloud_bigram_vo.freq`,
                min.freq = 2,
                max.words=500, random.order=FALSE, 
                colors="Red")
      
    }
    else if(input$dataset=="Oo" & input$emotion=="Positive"){
      p11_oo<-subset(w2,cat_sent_oo=="Positive")
      p11_oo<-as.data.frame(p11_oo)
      wordcloud(words=p11_oo$`wordcloud_bigram_oo.ngrams`,freq=p11_oo$`wordcloud_bigram_oo.freq`,
                min.freq = 2,
                max.words=500, random.order=FALSE, 
                color="Dark Green")
      
    }
    else if(input$dataset=="Oo" & input$emotion=="Negative"){
      b11_oo<-subset(w2,cat_sent_oo=="Negative")
      b11_oo<-as.data.frame(b11_oo)
      wordcloud(words=b11_oo$`wordcloud_bigram_oo.ngrams`,freq=b11_oo$`wordcloud_bigram_oo.freq`,
                min.freq = 2,
                max.words=500, random.order=FALSE, 
                colors="Red")
      }
    else if(input$dataset=="Vi" & input$emotion=="Positive"){
      p11_vi<-subset(w5,cat_sent_vi=="Positive")
      p11_vi<-as.data.frame(p11_vi)
      wordcloud(words=p11_vi$`wordcloud_bigram_vi.ngrams`,freq=p11_vi$`wordcloud_bigram_vi.freq`,
                min.freq = 2,
                max.words=500, random.order=FALSE, 
                color="Dark Green")
      }  
    else if(input$dataset=="Vi" & input$emotion=="Negative"){
      b11_vi<-subset(w5,cat_sent_vi=="Negative")
      b11_vi<-as.data.frame(b11_vi)
      wordcloud(words=b11_vi$`wordcloud_bigram_vi.ngrams`,freq=b11_vi$`wordcloud_bigram_vi.freq`,
                min.freq = 2,
                max.words=500, random.order=FALSE, 
                colors="Red")
      }
      else if(input$dataset=="Eo" & input$emotion=="Positive"){
        p11_ee<-subset(w1,cat_sent_ee=="Positive")
        p11_ee<-as.data.frame(p11_ee)
        wordcloud(words=p11_ee$`wordcloud_bigram_ee.ngrams`,freq=p11_ee$`wordcloud_bigram_ee.freq`,
                  min.freq = 2,
                  max.words=500, random.order=FALSE, 
                  color="Dark Green")
      }
    else if(input$dataset=="Eo" & input$emotion=="Negative"){
      b11_ee<-subset(w1,cat_sent_ee=="Negative")
      b11_ee<-as.data.frame(b11_ee)
      wordcloud(words=b11_ee$`wordcloud_bigram_ee.ngrams`,freq=b11_ee$`wordcloud_bigram_ee.freq`,
                min.freq = 2,
                max.words=500, random.order=FALSE, 
                colors="Red")
      
    }
    else if(input$dataset=="Te" & input$emotion=="Positive"){
      p11_te<-subset(w3,cat_sent_te=="Positive")
      p11_te<-as.data.frame(p11_te)
      wordcloud(words=p11_te$`wordcloud_bigram_te.ngrams`,freq=p11_te$`wordcloud_bigram_te.freq`,
                min.freq = 2,
                max.words=500, random.order=FALSE, 
                color="Dark Green")
    }  
    else{
      b11_te<-subset(w3,cat_sent_te=="Negative")
      
      b11_te<-as.data.frame(b11_te)
      wordcloud(words=b11_te$`wordcloud_bigram_te.ngrams`,freq=b11_te$`wordcloud_bigram_te.freq`,
                min.freq = 2,
                max.words=500, random.order=FALSE, 
                colors="Red")
      }
      
    
    })
 

})



