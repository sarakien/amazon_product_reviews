
shinyUI(dashboardPage(
    dashboardHeader(title = "Dashboard"),
        dashboardSidebar(
            sidebarUserPanel("Amazon Product Reviews", image = "https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcR3txb5KxXhupZHn2MaOxupWl6hlRnv3JgFFA&usqp=CAU"),
            sidebarMenu(
                menuItem("Background", tabName = "summary1", icon = icon("list-alt")),
                menuItem("Overall Review Analysis", tabName = "summary2", icon = icon("star")),
                menuItem("Word Type Examples", tabName = "data", icon = icon("database")),
                menuItem("Word Type Review Trends", tabName = "plots", icon = icon("bar-chart-o")),
                menuItem("Annual Trends", tabName = "plot3", icon = icon("bar-chart-o")),
                menuItem("Google Trend Analysis", tabName = "plot5", icon = icon("chart-line")),
                menuItem("Article Links", tabName = "articles", icon = icon("external-link"))
                )
            ),
            dashboardBody(
                tabItems(
                    tabItem(tabName = "summary1",
                            h1("Background"),
                            img(src = "product_review.png"),
                            h2("The Importance of Product Reviews and Experiential Framing"),
                            h4("While only 25% of consumers trust print and digital ads, 84% trust online customer reviews.*"),
                            h4("The number of positive reviews can increase purchase rates and consumer confidence, making consumer reviews a powerful sales strategy."),
                            h4("But are all positive reviews treated equally? Are there specific types of positive reviews that possibly have different effects on prospective consumers?"),
                            h4("In the Journal of Business Research, Gallo et al., (2019) reported that experiential framing of a product improves consumers' evaluation of the product, improves brand loyalty and brand satisfaction, and increases the likelihood that a consumer will review the product."),
                            h4("Experiential framing typically includes a description or portrayal of a product that highlights the action or occasion in which a product is used. This type of framing often elicits emotional responses and is associated with an increase in happiness. Experiential framing is contrasted with framing that focuses on a product's functionality."),
                            h4("The purpose of the present analysis is to compare experiential, emotional, and functional framing of Alexa enabled vs. non-Alexa enabled Amazon products in consumer reviews. Analyses include the following:"),
                            h4("1) An overall analysis of star ratings and \"helpful review\" votes, as well as an overall sentiment analysis of reviews**"),
                            h4("2) Exploration of specific Experience, Emotion, and Functionality words in Amazon product reviews, including frequencies and examples"),
                            h4("3) Examination of Experience, Emotion, and Functionality word types in Amazon product reviews for Alexa enabled vs. non-Alexa enabled products"),
                            h4("4) Investigation of trends for using specific word types in Alexa-enabled product reviews throughout the 2016-2018 time period"),
                            h4("5) Visualization of Google trends for searches related to Alexa-enabled products throughout the 2016-2018 time period"),
                            h4("6) Analysis of the relationship between the use of specific word types and Google search trends for Alexa-enabled products"),
                            h6("*For more information regarding the research findings reported in this summary, please see the sources provided under Article Links."),
                            h6("**Source for Sentiment and Emotion Lexicon: \"Crowdsourcing a Word-Emotion Association Lexicon, Saif Mohammad and Peter Turney, Computational Intelligence, 29 (3), 436-465, 2013\"")
                    ),
                    tabItem(tabName = "summary2",
                        fluidRow(
                            h1("Overall Review Analysis: Consumers love Amazon Products"),
                            img(src = "stars.jpg"),
                            tabsetPanel(
                                tabPanel("Star Ratings", 
                                         htmlOutput("hist")),
                                tabPanel("Helpful Review Votes", 
                                         htmlOutput("bar1")),
                                tabPanel("Sentiment Analysis", 
                                         htmlOutput("bar2"))
                                )
                        )
                    ),    
                    tabItem(tabName = "data",
                        fluidRow(
                            h1("Word Type Examples"),
                            h2(div(HTML("<em>\"Like having another person in the house, I talk to Alexa more [than] my wife</em>\""))),
                            h3("Select Word Type"),
                            tabsetPanel(
                                tabPanel("Experience", DT::dataTableOutput("table1")),
                                tabPanel("Emotion", DT::dataTableOutput("table2")),
                                tabPanel("Functionality", DT::dataTableOutput("table3"))
                            )
                        )
                    ),
                    tabItem(tabName = "plots",
                        fluidRow(
                            sidebarLayout(
                                sidebarPanel(
                                    selectizeInput(inputId = "alexa",
                                                   label = "Select Product Type",
                                                   choices = my_data_gather$alexa
                                    ),
                                    selectizeInput(inputId = "word_type",
                                                   label = "Select Word Type",
                                                   choices = my_data_gather$word_type
                                    )
                                ),
                                mainPanel(
                                    h4("Select Product Type and Word Type to View Charts"),
                                    plotOutput("plot1", height = "100%", width = "100%"),
                                    plotOutput("plot2", height = "100%", width = "100%"),
                                    h6("*Multiple Regressions were statistically significant."),
                                    h6("*For Alexa Products:"),
                                    h6("--Both Emotion and Experience words accounted for a significant"),
                                    h6("proportion of the variance in star ratings above and beyond Functionality."),
                                    h6("--Only Emotion words accounted for a significant proportion of the variance"),
                                    h6("in Helpful Votes."),
                                    h6("*For non-Alexa Products:"),
                                    h6("--Both Emotion and Functionality words accounted for a significant proportion"),
                                    h6("of the variance in star ratings."),
                                    h6("--Only Functionality words accounted for a significant proportion of the variance"),
                                    h6("in Helpful Votes.")
                                )
                            )
                        )
                    ),
                    tabItem(tabName = "plot3",
                            fluidRow(
                                sidebarLayout(
                                    sidebarPanel(
                                        selectizeInput(inputId = "wordtype",
                                                       label = "Select Word Type",
                                                       choices = my_data_gather$word_type
                                        )
                                    ),
                                    mainPanel(
                                        h4("Select Word Type to view Review Trends for Alexa Products from 2016-2018"),
                                        plotOutput("plot3", height = "100%", width = "100%"),
                                        h4("Google Search Trends for Alexa Products from 2016-2018"),
                                        plotOutput("plot4", height = "100%", width = "100%")
                                    )
                                )
                            )
                    ),
                    tabItem(tabName = "plot5",
                            fluidRow(
                                sidebarLayout(
                                    sidebarPanel(
                                        selectizeInput(inputId = "wordtype3",
                                                       label = "Select Word Type",
                                                       choices = final_date_data_gather$word_type
                                        )
                                    ),
                                    mainPanel(
                                        h3("Select Word Type to view the Relationship between Google Search Trends and Word Type Trends for Alexa Products"),
                                        plotOutput("plot5"),
                                        h6("*For Experience Words: r = .17"),
                                        h6("*For Emotion Words: r = .07"),
                                        h6("*For Functionality Words: r = -.14")
                                    )
                                )
                            )
                    ),
                    tabItem(tabName = "articles",
                            img(src = "tnw.png"),
                            h3(tags$a(href="https://thenextweb.com/contributors/2018/07/19/backed-by-psychology-reviews-are-big-for-your-business-heres-why/%7B%7B%20linkUrl/",
                                  "Click here to access this article^^ from thenextweb.com")),
                            h3(""),
                            img(src = "business_journal.png"),
                            h3(tags$a(href="https://www.sciencedirect.com/science/article/abs/pii/S0148296319300074", 
                                "Click here to access this article^^ from the Journal of Business Research")))
                        
            )
        )
    )
)

