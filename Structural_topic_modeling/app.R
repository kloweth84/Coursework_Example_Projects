#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(tm)
library(NLP)
library(tidyverse)
library(stm)
library(quanteda)
library(plotly)
library(wordcloud)
library(igraph)

movies = read.csv("movies_tagline_sentiment.csv")
movieplot = movies %>% select(id, vote_average, title, runtime, revenue_budget_ratio, release_year, Oscar_nominee, tagline, imdb_plot, impt_words, fps_pov, sp_pov, tps_pov, fpp_pov, tpp_pov, anger, fear,sadness,trust, anticipation,joy,surprise,disgust)
movieplot_bin = movies %>% select(id, vote_average, title, runtime, revenue_budget_ratio, release_year, Oscar_nominee, tagline, imdb_plot, impt_words, fps_pov_bin, sp_pov_bin, tps_pov_bin, fpp_pov_bin, tpp_pov_bin, anger_bin, fear_bin,sadness_bin,trust_bin, anticipation_bin,joy_bin,surprise_bin,disgust_bin)

varnames = c("Release Year" = "release_year",
             "Movie Runtime (minutes)" = "runtime",
             "First Person Singular" = "fps_pov_bin",
             "Second Person" = "sp_pov_bin",
             "Third Person Singular" = "tps_pov_bin",
             "First Person Plural" = "fpp_pov_bin",
             "Third Person Plural" = "tpp_pov_bin",
             "Anger" = "anger_bin", 
             "Fear" = "fear_bin",
             "Sadness" = "sadness_bin",
             "Trust" = "trust_bin", 
             "Anticipation" ="anticipation_bin",
             "Joy" = "joy_bin",
             "Surprise" = "surprise_bin",
             "Disgust" = "disgust_bin",
             "Average Audience Rating" = "vote_average",
             "Film Revenue-to-Budget Ratio" ='revenue_budget_ratio')

stop_words_2 = c('what', 'which', 'who', 'whom', 'this', 'that', "that'll", 'these', 'those', 'am', 'is', 'are', 'was', 'were', 'be', 
                 'been', 'being', 'have', 'has', 'had', 'having', 'does', 'did', 'doing', 'a', 'an', 'the', 'and', 'but', 'if', 
                 'or', 'because', 'as', 'until', 'while', 'of', 'at', 'by', 'for', 'with', 'about', 'between', 'into', 
                 'through', 'during', 'before', 'after', 'above', 'below', 'to', 'from', 'up', 'down', 'in', 'out', 'on', 'off', 
                 'over', 'under', 'again', 'further', 'then', 'once', 'here', 'there', 'when', 'where', 'why', 'how', 
                 'other', 'some', 'such', 'only', 'so', 's', 't', 'can', 'just', 'don', 'should', "should've", 'd', 'll', 
                 'm', 'o', 're', 've', 'y', 'll', 'ain', 'aren', "aren't", 'couldn', "couldn't", 'didn', "didn't", 'doesn', "doesn't", 
                 'hadn', "hadn't", 'hasn', "hasn't", 'haven', "haven't", 'isn', "isn't", 'ma', 'mightn', "mightn't", 'mustn', 
                 "mustn't", 'needn', "needn't", 'shan', "shan't", 'shouldn', "shouldn't", 'wasn', "wasn't", 'weren', "weren't", 
                 'won', 'wouldn', "wouldn't")


ui <- dashboardPage(
  
  # title ----
  dashboardHeader(title = "PPOL 5205 Project Showcase - by Katharyn Loweth"),
  
  # sidebar ----
  dashboardSidebar(
    sidebarMenu(id = "sidebarid",
                menuItem("Overview of STM", tabName = "page1"),
                menuItem("Step 1: Review Features", tabName = "page2"),
                menuItem("Step 2: Prep Corpus & Select K", tabName = "page3"),
                menuItem("Step 3: Create Model", tabName = "page4"),
                menuItem("Step 4: Review Topics", tabName = "page5"),
                conditionalPanel(
                  'input.sidebarid == "page2"',
                  sliderInput("bins", "Number of bins for Histogram:", min = 1, max = 50, value = 30),
                  selectInput("variable", "Select variable to visualize in Histogram:", 
                              choices = c("Film Runtime (minutes)" = "runtime", 
                                          "Average Audience Rating" = "vote_average", 
                                          "Film Release Year" = "release_year", 
                                          "Film Revenue-to-Budget Ratio" ='revenue_budget_ratio')),
                  numericInput("sample_num", "# of Documents to Include in Sample", value = 4227, min = 1000, max = 4227)
                ),
                conditionalPanel(
                  'input.sidebarid == "page3"',
                  numericInput("sample_num_prep", "# of Documents to Include in Sample", value = 2000, min = 1000, max = 4227),
                  numericInput("numeric_mindf", "Minimum Document Frequency for Tokens", value = 4, min = 0, max = 30),
                  selectizeInput("modelvar", "Select variable(s) to include in model:", 
                                 varnames,
                                 selected = c("anger_bin", "fear_bin", "sadness_bin","trust_bin", "anticipation_bin", "joy_bin","surprise_bin","disgust_bin", "fps_pov_bin", "sp_pov_bin", "tps_pov_bin", "fpp_pov_bin", "tpp_pov_bin"),
                                 multiple = TRUE) #,
                  #actionButton("selectK", "Explore Number of Topics")
                ),
                conditionalPanel(
                  'input.sidebarid == "page4"',
                  numericInput("sample_num_model", "# of Documents to Include in Sample", value = 2000, min = 1000, max = 4227),
                  numericInput("numeric_mindf", "Minimum Document Frequency for Tokens", value = 4, min = 0, max = 30),
                  numericInput("numeric_k", "# of Topics (K)", value = 8, min = 4, max = 30),
                  selectizeInput("modelvar", "Select variable(s) to include in model:", 
                              varnames,
                              selected = c("anger_bin", "fear_bin", "sadness_bin","trust_bin", "anticipation_bin", "joy_bin","surprise_bin","disgust_bin", "fps_pov_bin", "sp_pov_bin", "tps_pov_bin", "fpp_pov_bin", "tpp_pov_bin"),
                              multiple = TRUE),
                  actionButton("update", "Run Structural Topic Model"),
                  actionButton("CTM", "Run Correlated Topic Model")
                ) #,
               # conditionalPanel(
               #   'input.sidebarid == "page5"',
               #  numericInput("sample_num_topic", "# of Documents to Include in Sample", value = 2000, min = 1000, max = 4227),
               #   numericInput("numeric_mindf", "Minimum Document Frequency for Tokens", value = 4, min = 0, max = 30),
               #    numericInput("numeric_k", "# of Topics (K)", value = 8, min = 4, max = 30),
               #   selectizeInput("modelvar", "Select variable(s) to include in model:", 
               #                   varnames,
               #                  selected = c("anger_bin", "fear_bin", "sadness_bin","trust_bin", "anticipation_bin", "joy_bin","surprise_bin","disgust_bin"),
               #                  multiple = TRUE),
               #   actionButton("update", "Run Model")
               #)
    )
  ),
  
  # body ----
  dashboardBody(
    tabItems(
      # page 1 ----
      tabItem(tabName = "page1", span("PPOL 5205 Project Showcase - by Katharyn Loweth", style = "font-size: 18px; font-weight: bold"),
              br(), br(),
              span("Instructions for Dashboard", style = "font-size: 18px; font-weight: bold"),
              br(),br(),
              "This dashboard provides step-by-step instructions for a structural topic model. 
              Using the left sidebar menu, click through the different steps of the structural topic model process. 
              Additionally, on the tabs for steps 1-3, there are filters on the sidebar menu that control the graphs and implemented models.
              You can investigate the different features that can inform the structural topic model and determine what parameters--token minimum document frequency, number of topics, and the topical prevalence covariates--are appropriate given the sample.
              On the last tab, step 4, you can review the different topics that result from the topic model you create in step 3.",
              br(),br(),
              "NOTES: The results under step 4 only appear if you initialize the model in step 3. Additionally, sometimes running the structural topic model will cause the app to disconnect from the server, depending on the parameters. 
              If this occurs, reload the app and lower the sample size.",
              br(),br(),
              span("Overview of Structural Topic Modeling (STM)", style = "font-size: 18px; font-weight: bold"),
              br(), br(),
              
              "Text is an important source of data that can help us learn about the patterns in language and importance of word choice.
              Topic modeling is a process that allows for us to understand hidden patterns in the structure of documents from a collection of documents (corpus). 
              This unstructured modeling technique allows for us to review and analyze thousands of documents together, which would be difficult to do manually.
              The most common of these topic modeling methods is Latent Dirichlet Allocation (LDA), which is a generative probabilistic model. 
              However, LDA has its limitations. LDA assumes that each topic within a document is independent of one another and that topics can be based solely on the words included in the text.
              It does not take into account other information we have on the documents, such as year it was written.",
              
              br(),br(),
              
              "Structural Topic Modeling (STM) is one solution for LDA's limitations. STM, like LDA, is a generative model of word counts.
              While LDA topic modeling uses hyperparameters to determine topic prevalence, STM allows for the use of document metadata to inform topic prevalence within a document. 
              The use of document metadata can also help improve the assignment of words to the topics within the corpus.
              Additionally, STM does not assume that topics are independent of one another, and allows us to examine the possible correlation between topics.
              A visual of the structural topic model process is included below, where we can see that the metadata is used to inform the theta parameter of the model.",
              
              br(), br(),
              img(src='STM_model_2.png', height="75%", width="75%", align = "center"),
              br(), br(),
              
              "In the demonstration of structural topic modeling, you can implement a structural topic model of movie plots. The dataset contains data on english language films from the last 50 years. 
              Example of metadata in this case include aspects like the year the film was released, but we are also going to use data from other text features to help inform the model.
              An example observation is included below.",
              br(),br(),
              fluidRow(box(title = "Example Movie Plot", width = 12, status = "primary", tableOutput("plotex"))),
              ),
      # page 2 ----
      tabItem(tabName = "page2", 
              span("Step 1: Review Features in Dataset to determine which ones serve as metadata in STM", style = "font-size: 18px; font-weight: bold"),
              
              br(), br(),
              
              span("Decision Point 1: Determine the number of documents to include in the corpus for text analysis.", style = "font-weight: bold"),
              br(),br(),
              "In this dataset there are 4,227 documents (movies) in total that have text plot information and also have information for all potential relevant metadata.
              By using the filter on the sidebar panel, you can choose to randomly sample n number of documents from the sample to serve as the text for the topic model.",
              br(), br(),
              span("Decision Point 2: Determine the variables that will serve as topical prevalence covariates in STM model.", style = "font-weight: bold"),
              br(), br(),
              
              "One key component in Structural Topic Modeling (STM) is determining which features in the dataset may affect topic prevalence and thus should be included in the model.
              While there are many variables included in the dataset, not all of them may be relevant to the topic prevalence in our corpus. For the STM, these variables that explain topical prevalence are called topical prevalence covariates.
              These variables can be continuous, categorical, or dichotomous. Below you can explore the different features that could be considered as topical prevalence covariates.",
              
              br(),br(),
              span("Continuous Features", style = "font-size: 16px"),
              br(),br(),
              "The first section highlights the different continuous features and their distribution within the sample based on the number of documents specified in the sidebar menu.
              On the sidebar you can also select which variable is visualized. Examples of films and the selected column values are included in the right table.",
              br(),
              fluidRow(box(title = "Histogram", status= "primary", width = 8, plotOutput("distPlot")), box(title = "Sample of 10 Films", width = 4, tableOutput("data"))),
              br(), 
              span("Text Features", style = "font-size: 16px"),
              br(),br(),
              "The second section highlights the features pulled from a text analysis of the movie taglines. The taglines were stemmed and tokenized using a bag-of-words approach. The most common unigram tokens (excluding common english stopwords) for the sample are listed in the bottom left table.
              The words in the taglines were analyzed to determine if they indicated a certain emotional sentiment (using the NRC Emotion Lexicon) or point of view. 
              In the table below, these words are noted under the important words (impt words) column.
              The count and percentages for the different emotion and perspective categories based on the specified sample size are included in the charts below.",
              br(),
              fluidRow(box(title = "Example Taglines", width = 12, status = "primary", tableOutput("taglines"))),
              
              fluidRow(box(title = "Most common tokens", width = 3, tableOutput("wordfreq")),
                       tabBox(side = "right", width = 9, height = "250px",
                              selected = "Emotion1",
                              tabPanel("Perspective2", "Percentage of Taglines with Perspective, by Grammatical Person", plotlyOutput("barchart4")),
                              tabPanel("Perspective1", "Count of Perspective Across Sample Taglines, by Grammatical Person", plotlyOutput("barchart3")),
                              tabPanel("Emotion2", "Percentage of Taglines with Emotion, Based on NRC Lexicon", plotlyOutput("barchart2")),
                              tabPanel("Emotion1", "Count of Sentiment Across Sample Taglines, Based on NRC Lexicon", plotlyOutput("barchart1"))))
              ),
      # page 3 ----

      tabItem(tabName = "page3", span("Step 2: Prepping the Corpus & Deciding Parameters for Structural Topic Model", style = "font-size: 18px; font-weight: bold"),
              br(), br(),
              span("Decision Point 3: Determine the minimum document frequency for tokens considered in the model.", style = "font-weight: bold"),
              br(), br(),
              
              "As part of text analysis, it is important to consider the size of the dictionary and the tokens to include in modeling. 
              Tokens that are unique to a single document or very few documents can increase the computational load of the analysis without providing meaningful insight into patterns across the corpus.
              However, removing too many tokens could result in nuances of the patterns across documents being lost.
              In the figure below, we can see how the number of documents, number of unique tokens, number of tokens in corpus varies based on setting a minimum document frequency for the tokens included in the analysis.
              While varying min_df does not remove any documents from the corpus, it significantly reduces the number of unique tokens included in the text analysis.
              Use the graph to determine what is the most suitable min_df for the analysis and select it in the filter on the sidebar.",
              br(), br(),
              fluidRow(box(title = "Minimum Document Frequency", status= "primary", width = 9, plotOutput("prepplots")), box(title = "Plot Tokens", width = 3, tableOutput("plotwordfreq"))),
               
              span("Decision Point 4: Determine the number of topics to specify in the model.", style = "font-weight: bold"),
              br(),br(),
              
              "With our knowledge of the other features in the dataset and the prepped corpus, we now have to consider the appropriate number of topics (k) in our model.
              The STM package has a function, searchK(), that allows you to compare the results of multiple fitted models with different number of topics (k).
              An example of the results for a model, using a sample of 2000, a minimum document frequency of 4, and containing the emotion and perspective variables as prevalence covariates, are below.  
              While there may not be a true value for number of topics and it may vary based on the topical prevalence covariates we include in the model, 
              we can use these metrics, like the semantic coherence plot, to discern which may be most appropriate for the analysis.",
              
              br(),br(),
              img(src='diag_plots.jpg', height="75%", width="75%", align = "center"),
              br(),br() #,
              #fluidRow(box(title = "Reviewing Possible Number of Topics (Takes a few minutes to load)", status = "warning", width = 12, plotOutput("kplot")))
              ),
     
      #page 4 -------
     tabItem(tabName = "page4", span("Step 3: Creating a Structural Topic Model and Comparing Against Topic Model without Metadata", style = "font-size: 18px; font-weight: bold"),
             br(), br(),
             
             "With all of the parameters considered, it is now time to run the STM. 
             Specify the different parameters in the sidebar panel and click the Run Structural Topic Model button at the bottom to initiate the model.",
             br(),br(),
             "NOTE: Running the structural topic model on the shiny server can sometimes cause the app to disconnect from the server, depending on the parameters. If this occurs, reload the app and lower the sample size.",
             br(),br(),
             span("Decision Point 5: Review results and consider whether any parameters need to be changed to improve results.", style = "font-weight: bold"),
             br(), br(),
             "The results below are the document level and look at the topic proportions across the corpus and the correlation between topics. 
             The last tab includes the results of the STM's estimateeffect() command, which allows you to better understand the effect that the topical prevalence covariates have on the model.
             By looking at these results, you can see if there is any significant overlap or relationship between topics or if there are topics with few documents, suggesting that you may need to reduce the number of topics included in the model.",
             br(),
              fluidRow(
               tabBox(side = "right", width = 10,
                      selected = "STM1",
                      tabPanel("STM4", "Estimated Effects of Metadata - STM", verbatimTextOutput("metaeffects")),
                      tabPanel("STM3", "Correlation Between Topics - STM", plotOutput("topicplot3")),
                      tabPanel("STM2", "Exclusivity and Semantic Coherence Plot - STM", plotOutput("topicplot2")),
                      tabPanel("STM1", "Topic Proportions - STM", plotOutput("topicplot1"))),
               box(title = "STM Topic Distribution", width = 2, tableOutput("STMdomtop"))),
             br(), 
             span("Decision Point 6: Validate results against models that do not use document metadata to discern the effect it has on model results.", style = "font-weight: bold"),
             br(),br(),
             
             "While the results of the STM may show that topical prevalence covariates have an effect on our topic proportion, you can also compare the result to a topic model that does not consider any covariates in determining topic prevalence.
             With the STM package, when no topical prevalence covariates are selected, the model reverts to a correlated topic model (CTM). A CTM is similar to the standard LDA topic model and uses the same methodological approach, but does not assume that topics have to be independent of one another.
             To compare the results, click the Run Correlated Topic model button, which will run a model using the same sample, token min_df, and number of topics as the STM.",
             
             br(),br(),
             fluidRow(
               tabBox(side = "right", width = 10,
                      selected = "CTM1",
                      tabPanel("CTM3", "Correlation Between Topics - No Metadata", plotOutput("topicplot3_CTM")),
                      tabPanel("CTM2", "Exclusivity and Semantic Coherence Plot - No Metadata", plotOutput("topicplot2_CTM")),
                      tabPanel("CTM1", "Topic Proportions - No Metadata", plotOutput("topicplot1_CTM"))),
               box(title = "CTM Topic Distribution", width = 2, tableOutput("CTMdomtop")))
               
     ),
     
     #page 5 -------
     
     tabItem(tabName = "page5", span("Step 4: Reviewing Results and Topics", style = "font-size: 18px; font-weight: bold"),
             br(),br(),
             span("Structural Topic Model Parameters:", style = "font-size: 18px"),
             br(),
             fluidRow(
               valueBoxOutput("sampleBox", width =3),
               valueBoxOutput("minDFBox", width =3),
               valueBoxOutput("KtopicBox", width =3),
               valueBoxOutput("PrevCovBox",width =3)
             ),
             br(), 
             "Now having looked at the topics at the corpus-level, you can also begin to explore the topics themselves and how they are different. 
             In the visuals on this page, you view the most frequent and defining tokens for each topic, both as a word cloud and in table form, some of the most representative films for each topic, and a representative plot.
             Using the filters in the box on the right side, you can toggle through the topics and determine the number of words included in the topic word cloud.",
             br(), br(),
             fluidRow(
               tabBox(side = "right", width = 8, selected = "WordCloud", 
                      tabPanel("Plot", "Example Plot Representative of Topic", plotOutput("topicplot")),
                      tabPanel("Films", "5 Representative Films of Topic", plotOutput("topicmovies")),
                      tabPanel("Tokens", "Defining Tokens of Topic", plotOutput("topicwords")),
                      tabPanel("WordCloud", "Topic Word Cloud", plotOutput("wordcloud", height = "40em"))),
               box(title = "Inputs for Topic Visual",sliderInput("topicnum", "Topic to visualize:", min = 1, max = 10, value = 1), 
                   br(), sliderInput("wordcloudwords", "No. of Words in Wordcloud", min = 10, max = 50, value = 30), width = 4))
             
             
             )
  )
)
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #page 1 outputs
  output$plotex <- renderTable({
    movieplot %>% select(title, release_year, runtime, tagline, imdb_plot) %>% rename_with(~ tolower(gsub("_", " ", .x, fixed = TRUE))) %>% slice_head(n=1)
  })
  
  #page 2 outputs ------
  df <- reactive({
    movieplot %>% select("title", input$variable) %>% slice_sample(n = input$sample_num)})
  
  
  output$distPlot <- renderPlot({
    var    <- df()[, input$variable]
    varname = str_to_title(str_replace(input$variable, "_", " "))
    ggplot(df(), aes(x = df()[,input$variable])) + geom_histogram(bins = input$bins, fill = "lightblue", color = "white") + theme_minimal() + ggtitle(sprintf("Distribution of %s", varname)) + xlab(varname)
  })
  
  output$data <- renderTable({
    
    subset = df() %>% rename_with(~ tolower(gsub("_", " ", .x, fixed = TRUE))) %>% slice_sample(n = 10)
    subset
  })
  
  set.seed(47)
  text_sample <- reactive({
    movieplot %>% slice_sample(n = input$sample_num)})
  
  
  text_sample_reshaped = reactive({text_sample() %>% pivot_longer(cols = c("anger", "fear", 'sadness', 'trust', 'anticipation', 'joy', 'surprise', 'disgust'), names_to = "Emotion", values_to = "Word_Count")})
  
  text_sample_reshaped_per = reactive({text_sample() %>% pivot_longer(cols = c("fps_pov", "sp_pov", 'tps_pov', 'fpp_pov', 'tpp_pov'), names_to = "Perspective", values_to = "Word_Count") %>% mutate(Perspective = recode(Perspective, "fps_pov" = "First Person Singular", "sp_pov" = "Second Person", "tps_pov" = "Third Person Singular", "fpp_pov" = "First Person Plural", "tpp_pov" = "Third Person Plural"))})
  
  output$taglines <- renderTable({
    text_sample() %>% select(title, tagline, impt_words) %>% rename_with(~ tolower(gsub("_", " ", .x, fixed = TRUE))) %>% slice_head(n=5)
  })
  
  output$wordfreq <- renderTable({
    tags = text_sample() %>% select(tagline)
    token_tagline = tokens(text_sample()$tagline, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) |> tokens_select(stop_words_2, selection = "remove", padding = FALSE) |> tokens_wordstem() |> dfm()
    top_n = topfeatures(token_tagline, n = 13)
    data.frame(Term = names(top_n), Frequency = round(top_n,0), row.names = NULL)
  })
  
  output$barchart1 <- renderPlotly({
    bc1 = ggplot(data = text_sample_reshaped() %>% group_by(Emotion) %>% summarise(Emotion_Count = sum(Word_Count)), aes(x=Emotion, y=Emotion_Count, fill = Emotion, text = sprintf("Emotion: %s<br>Count: %d", Emotion, Emotion_Count))) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position = "none") + xlab("Emotion") + ylab("Count of Emotional Words, by Emotion")
    ggplotly(bc1, tooltip = c("text"))
    })
  
  output$barchart2 <- renderPlotly({
    bc2 = ggplot(data = text_sample_reshaped() %>% group_by(Emotion) %>% summarise(Emotion_Found = sum(Word_Count > 0)) %>% mutate(Perc = Emotion_Found/nrow(text_sample())), aes(x=Emotion, y=Perc, fill = Emotion, text = sprintf("Emotion: %s<br>Percent of Sample: %.2f", Emotion, Perc))) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position = "none") + xlab("Emotion") + ylab("Percent of Sample Containing Emotional Words")
    ggplotly(bc2, tooltip = c("text"))
    })
  
  output$barchart3 <- renderPlotly({
    bc3 = ggplot(data = text_sample_reshaped_per() %>% group_by(Perspective) %>% summarise(Pers_Count = sum(Word_Count)), aes(x=str_wrap(Perspective, width = 10), y=Pers_Count, fill = Perspective, text = sprintf("Perspective: %s<br>Count: %d", Perspective, Pers_Count))) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position = "none") + xlab("Point of View Category") + ylab("Count of Perspective Words")
    ggplotly(bc3, tooltip = c("text"))
  })
  
  output$barchart4 <- renderPlotly({
    bc4 = ggplot(data = text_sample_reshaped_per() %>% group_by(Perspective) %>% summarise(Pers_Found = sum(Word_Count > 0)) %>% mutate(Perc = Pers_Found/nrow(text_sample())), aes(x=str_wrap(Perspective, width = 10), y=Perc, fill = Perspective, text = sprintf("Perspective: %s<br>Percent of Sample: %.2f", Perspective, Perc))) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position = "none") + xlab("Point of View Category") + ylab("Percent of Sample Containing POV Words")
    ggplotly(bc4, tooltip = c("text"))
  })
  
  
  
  #page 3 outputs -----
  
  set.seed(47)
  text_sample_2 <- reactive({
    movieplot %>% slice_sample(n = input$sample_num_prep)})
  
 
  
  output$plotwordfreq <- renderTable({
    token_plot = tokens(text_sample_2()$imdb_plot, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) |> tokens_remove(stopwords("en")) |> tokens_wordstem() |> dfm()
    top_n_plot = topfeatures(token_plot, n = 10)
    data.frame(Term = names(top_n_plot), Frequency = round(top_n_plot,0), row.names = NULL)
  })
  
  processed <- reactive({
    textProcessor(text_sample_2()$imdb_plot, metadata = text_sample_2())})
  
  out <- reactive({prepDocuments(processed()$documents, processed()$vocab, processed()$meta)})
  
  
  output$prepplots <- renderPlot({
    plotRemoved(processed()$documents, lower.thresh = seq(1, 30, by = 2))
    })
  
  varlist <- reactive({
    selected_vars <- input$modelvar
    if (length(selected_vars) == 0) {
      return(NULL) 
    }
    paste(selected_vars, collapse = " + ")
    
  })
  
  text_sample_bin <- reactive({
    movieplot_bin %>% slice_sample(n = input$sample_num_prep)})
  
  processed_2 <- reactive({
    textProcessor(text_sample_bin()$imdb_plot, metadata = text_sample_bin())})
  
  #modelk <- reactive({prepDocuments(processed_2()$documents, processed_2()$vocab, processed_2()$meta, lower.thresh = input$numeric_mindf)})
  
  #kresult <- reactive({searchK(modelk()$documents, modelk()$vocab,  K= seq(5, 30,5),  
  #                  prevalence=as.formula(paste("~", varlist())), data=modelk()$meta)}) |> bindEvent(input$selectK) 
  
  #output$kplot <- renderPlot({
  #  plot(kresult())        
  #})
  
  #page 4 output ----
  
  observeEvent(input$sample_num_prep, {
    updateSliderInput(session = session, inputId = "sample_num_model", value = input$sample_num_prep)
  })
  
  set.seed(47)
  text_sample_bin_2 <- reactive({
    movieplot_bin %>% slice_sample(n = input$sample_num_model)})
  
  processed_bin_2 <- reactive({
    textProcessor(text_sample_bin_2()$imdb_plot, metadata = text_sample_bin_2())})
  
  modeldata <- reactive({prepDocuments(processed_bin_2()$documents, processed_bin_2()$vocab, processed_bin_2()$meta, lower.thresh = input$numeric_mindf)})
  

  movietopics = reactive({stm(documents = modeldata()$documents, vocab = modeldata()$vocab, 
                    prevalence= as.formula(paste("~", varlist())),
                    K = input$numeric_k, max.em.its = 40, 
                    data = modeldata()$meta, init.type = "Spectral", seed = 47)}) |> bindEvent(input$update) 
  
  
  output$topicplot1 <- renderPlot({
    plot(movietopics(), type="summary") 
  })
  
  output$topicplot2 <- renderPlot({
    topicQuality(model=movietopics(), documents=modeldata()$documents) 
  })
  
  output$topicplot3 <- renderPlot({
    mod.out.corr <- topicCorr(movietopics())
    plot(mod.out.corr) 
  })
  
  
  output$metaeffects <- renderPrint({
    
    clusternum = reactive({
      1:input$numeric_k
    })
    
    prep <- estimateEffect(as.formula(paste(clusternum(), "~", varlist())), 
                           movietopics(), 
                           meta=modeldata()$meta, 
                           uncertainty="Global")
    
    summary(prep, topics = 1)
  })
  
  output$STMdomtop = renderTable({
    theta <- movietopics()$theta 
    topic <- apply(theta, 1, which.max)
    as.data.frame(table(topic), row.names = NULL)
  })
  
  movietopicsCTM = reactive({stm(documents = modeldata()$documents, vocab = modeldata()$vocab, 
                              prevalence= NULL,
                              K = input$numeric_k, max.em.its = 40, 
                              data = modeldata()$meta, init.type = "Spectral", seed = 47)}) |> bindEvent(input$CTM) 
  
  
  output$topicplot1_CTM <- renderPlot({
    plot(movietopicsCTM(), type="summary") 
  })
  
  output$topicplot2_CTM <- renderPlot({
    topicQuality(model=movietopicsCTM(), documents=modeldata()$documents) 
  })
  
  output$topicplot3_CTM <- renderPlot({
    mod.out.corr <- topicCorr(movietopicsCTM())
    plot(mod.out.corr) 
  })
  
  output$CTMdomtop = renderTable({
    thetaCTM <- movietopicsCTM()$theta 
    topic <- apply(thetaCTM, 1, which.max)
    as.data.frame(table(topic), row.names = NULL)
  })
  
  #page 5 output --------
  
  output$sampleBox <- renderValueBox({
    valueBox(
      input$sample_num_model, "Sample Size",
      color = "purple"
    )
  })
  
  output$minDFBox <- renderValueBox({
    valueBox(
      input$numeric_mindf, "Token Min Doc Freq",
      color = "blue"
    )
  })
  
  output$KtopicBox <- renderValueBox({
    valueBox(
      input$numeric_k, "# of Topics",
      color = "yellow"
    )
  })
  
  output$PrevCovBox <- renderValueBox({
    valueBox(
      length(input$modelvar), "# of Topical Prevalence Covariates",
      color = "green"
    )
  })
  
  observeEvent(input$numeric_k, {
    updateSliderInput(session = session, inputId = "topicnum", max = input$numeric_k)
  })
  
  observeEvent(input$sample_num_model, {
    updateSliderInput(session = session, inputId = "sample_num_topic", value = input$sample_num_model)
  })
  
  output$wordcloud <- renderPlot({
    cloud(movietopics(), topic = input$topicnum, max.words = input$wordcloudwords)
  })

  output$topicwords <- renderPlot({
    plot(movietopics(), type="labels", width = 60, topics=input$topicnum)
  })
  
  output$topicmovies <- renderPlot({
    topic = findThoughts(movietopics(), texts = modeldata()$meta$title, n = 5, topics = input$topicnum)
    plot(topic)
  })
  
  output$topicplot <- renderPlot({
    movieplot = findThoughts(movietopics(), texts = modeldata()$meta$imdb_plot, n = 1, topics = input$topicnum)
    movietitle = findThoughts(movietopics(), texts = modeldata()$meta$title, n = 1, topics = input$topicnum)
    title = unlist(movietitle)
    
    plot(movieplot, width = 90, main = title)
  })
  
  
  
    
}


# Run the application 
shinyApp(ui = ui, server = server)
