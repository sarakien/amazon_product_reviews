
shinyServer(function(input, output){

  output$hist <- renderGvis({
    gvisHistogram(star_rating_data, options = list(
      title="Star Rating Frequency", legend="none", 
      hAxis="{title:'Star Rating', titleTextStyle:{color:'blue', fontSize:16},
      ticks: [1,2,3,4,5]}",
      width=700, height=400,
      titleTextStyle="{color:'red', 
                      fontName:'Courier', 
                      fontSize:16}",
      vAxis="{title:'Frequency', titleTextStyle:{color:'blue', fontSize:16}, 
      gridlines:{count:0}}"
      )
    )
  })
  
  output$bar1 <- renderGvis({
    gvisColumnChart(helpful_data, options = list(
      title="Helpful Review Votes", legend="none", 
      hAxis="{title:'Star Rating', titleTextStyle:{color:'blue', fontSize:16},
      ticks: [1,2,3,4,5], gridlines:{color: 'white', count:2}}",
      width=700, height=400,
      titleTextStyle="{color:'red', 
                      fontName:'Courier', 
                      fontSize:16}",
      vAxis="{title:'Total Votes', titleTextStyle:{color:'blue', fontSize:16}, 
      gridlines:{count:0}}",
      bar = "{groupWidth: '20%'}"
      )
    )
  })
  
  output$bar2 <- renderGvis({
    gvisColumnChart(Sentiment_Data, options = list(
      title="Positive and Negative Sentiments", legend="none",
      hAxis="{title:'Sentiment', titleTextStyle:{color:'blue', fontSize:16}}",
      width=700, height=400,
      titleTextStyle="{color:'red', 
                      fontName:'Courier', 
                      fontSize:16}",
      vAxis="{title:'Frequency', titleTextStyle:{color:'blue', fontSize:16}, 
      gridlines:{count:0}}",
      bar = "{groupWidth: '10%'}"
      )
    )
  })
  
  
  output$table1 <- DT::renderDataTable({
    datatable(word_frequency_exp, rownames=FALSE)
  })
  
  output$table2 <- DT::renderDataTable({
    datatable(word_frequency_emo, rownames=FALSE) 
  })
  
  output$table3 <- DT::renderDataTable({
    datatable(word_frequency_func, rownames=FALSE) 
  })
  
  filtered_data = reactive({
    my_data_gather %>%   
    filter(alexa == input$alexa & word_type == input$word_type) %>%
      group_by(.,sum) %>% 
      summarise(.,mean_rating = mean(reviews.rating), 
                mean_helpful = mean(reviews.numHelpful)) 
  })
  
  output$plot1 = renderPlot({
    ggplot(data = filtered_data(), aes(x=sum,y=mean_rating)) + 
      geom_col(fill = "royalblue4") + ylim(c(0,5)) +
      labs(x="Number of Words", y = "Average Star Rating") +
      theme(panel.background = element_rect(fill = "gray97")) +
      theme(
        axis.title.x = element_text(color="red2", size=20,vjust=-0.40),
        axis.title.y = element_text(color="red2" , size=20,vjust=0.40)) +
      theme(axis.text.x=element_text(size=15)) +
      theme(axis.text.y=element_text(size=15))
  }, height = 300, width = 450)
  
  output$plot2 = renderPlot({
    ggplot(data = filtered_data(), aes(x=sum,y=mean_helpful)) + 
      geom_col(fill = "royalblue4") + ylim(c(0,1.25)) +
      labs(x="Number of Words", y = "Average Helpful Vote") +
      theme(panel.background = element_rect(fill = 'gray97')) +
      theme(
        axis.title.x = element_text(color="red2", size=20,vjust=-0.35),
        axis.title.y = element_text(color="red2" , size=20,vjust=0.35)) +
      theme(axis.text.x=element_text(size=15)) +
      theme(axis.text.y=element_text(size=15))
  }, height = 300, width = 450)
 
  filtered_date_data = reactive({
    my_data_gather %>%   
      filter(word_type == input$wordtype) %>%
      group_by(.,month_year) %>% 
      summarise(.,mean_sum_word = mean(sum)) 
  }) 
  
  output$plot3 = renderPlot({
    ggplot(data = filtered_date_data(), aes(x=month_year,y=mean_sum_word)) + 
      geom_line() +
      labs(x="Year", y = "Average Review Words") +
      theme(panel.background = element_rect(fill = 'gray97')) +
      theme(
        axis.title.x = element_text(color="red2", size=20,vjust=-0.35),
        axis.title.y = element_text(color="red2" , size=20,vjust=0.35)) +
      theme(axis.text.x=element_text(size=15)) +
      theme(axis.text.y=element_text(size=15))
  }, height = 300, width = 450)

  output$plot4 = renderPlot({
    ggplot(data = google_trends, aes(x=week,y=search_index)) + 
      geom_line() + 
      labs(x="Year", y = "Google Search Trends") +
      theme(panel.background = element_rect(fill = 'gray97')) +
      theme(
        axis.title.x = element_text(color="red2", size=20,vjust=-0.35),
        axis.title.y = element_text(color="red2" , size=20,vjust=0.35)) +
      theme(axis.text.x=element_text(size=15)) +
      theme(axis.text.y=element_text(size=15))
  }, height = 300, width = 450)

  filtered_google_analysis = reactive({
    final_date_data_gather %>%   
      filter(word_type == input$wordtype3)  
  }) 

  output$plot5 = renderPlot({
    ggplot(data = filtered_google_analysis(), aes(x=mean_index,y=mean_sum)) + 
      geom_smooth(method='lm', formula= y~x) +
      labs(x="Google Search Trends", y = "Word Type Trends") +
      theme(panel.background = element_rect(fill = 'gray97')) +
      theme(
        axis.title.x = element_text(color="red2", size=20,vjust=-0.35),
        axis.title.y = element_text(color="red2" , size=20,vjust=0.35)) +
      theme(axis.text.x=element_text(size=15)) +
      theme(axis.text.y=element_text(size=15))
  })
    
})