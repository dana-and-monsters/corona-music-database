library(ggplot2)
library(plotly)
library(shiny)
library("readr")
library(qdapTools)
library(dplyr)
library(tidyr)
library("ggridges")
library("lubridate")
library(stringr)
library("tidytext")
library(wordcloud2)
library(quanteda)

source("app-pre-processing.R") # scripts for preprocessing are located here

# load data
media<-read_delim("Coronamusic_MEDIA_v20210107.csv", delim = ";")
video<-read_delim("Coronamusic_VIDEO_v20201214.csv", delim = ";")
titles<-read_delim("Titles.csv", delim = ";")

# pre-process (date and category)
media<-preprocess_data(media)
video<-preprocess_data(video)

# factor vars
video<-factor_video_variables(video)

# create country column
media<-make_country_and_continent_columns(media)
video<-make_country_and_continent_columns(video)

# create setting code columns
video<-make_setting_code(video) 
media<-make_setting_code(media)

# create genre code column
video<-make_genre_code(video) 

# create emotions code column
video<-make_emotions_code(video)
media<-make_emotions_code(media)

# Show only certain variables depending on which plot is selected
# VIDEO
video_varListWordCloud<-c("Title", "Social Media Platform", "Country", "Setting", "Genre", "Emotions","Features")
video_booleanList = c("Joint Musicking", "Original Covid Song","Original Covid Lyrics","Movement", "Health Information", "Conflict")
video_characteristicList = c("Setting Code", "Genre Code", "Social Media Platform", "Emotions Code")
# MEDIA
media_varListWordCloud<-c("Title", "Media Platform", "Country", "Language", "Setting", "Emotions","Features")
media_booleanList = c("Media Written Online", "Media Audio","Media Video","Media Written Print", "Embedded External Video", "Movement", "Health Information", "Conflict")
media_characteristicList = c("Setting Code", "Emotions Code", "Music Maker Professionalism")

## UI

ui<-fluidPage(
  sidebarLayout(
    
### INPUT
    
    sidebarPanel = sidebarPanel(
      h3("Select Options"),
      selectInput("database","Database:",
                  list("Video" = "video",
                       "Media" = "media")
      ),
      selectInput("plottype","Plot Type:",
                  list("Histogram" = "histogram",
                       "Frequency Polygon" = "frequencyPolygon",
                       "Wordcloud" = "wordcloud",
                       "Plotly" = "plotly")
      ),
      selectInput("variableName", "Variable:",
                  NULL
                  ),
      # create filtering options
      checkboxInput("filterbox", "Filter"),
      conditionalPanel(
        condition = "input.filterbox == true",
        selectInput("filter", "Filter By:",
                    NULL),
        selectInput("filterVar", "",
                    NULL
          )
      )
      ),
### OUTPUT 

    mainPanel = mainPanel(
      h3(textOutput("caption")),
      
      ## tabsets
      
      tabsetPanel(type = "tabs",
                  tabPanel("Plot",
                          conditionalPanel(
                            condition = 'input.plottype == "histogram"',
                            plotOutput('histogram')
                          ),
                          conditionalPanel(
                            condition = 'input.plottype == "frequencyPolygon"',
                            plotOutput('freqPoly')
                          ),
                          conditionalPanel(
                            condition = 'input.plottype == "wordcloud"',
                            wordcloud2Output('wordcloud', width = "100%", height= "400px")

                          ),
                          conditionalPanel(
                            condition = 'input.plottype == "plotly"',
                            plotlyOutput('plotly')
                          )
                        ),
                  #tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      )
    ),
    position = c("left", "right"),
    fluid = TRUE
  )
)

## SERVER

server<-function(input, output, session){
  check<-function(x){is.null(x) || x==""}
  
  ### Update available options in the variable list
  
  options<-c()
  
  observe({
    if(check(input$database)) return() # if no database
    if(!check(input$database)){
      if(input$database =="video"){
        booleanList<-video_booleanList
        characteristicList<-video_characteristicList
        varListWordCloud<-video_varListWordCloud
      } else if(input$database =="media"){
        booleanList<-media_booleanList
        characteristicList<-media_characteristicList
        varListWordCloud<-media_varListWordCloud
      }
      frequencyOptions<-c(booleanList, characteristicList) # same for the frequency polygon and the histogram
      plotlyOptions<-c(booleanList, characteristicList)
      wordcloudOptions<-varListWordCloud
    
      if("histogram" %in% input$plottype|| "frequencyPolygon" %in% input$plottype){
        options<-c(options,frequencyOptions)
      }
      if("plotly" %in% input$plottype){
        options<-c(options,plotlyOptions)
      }
      if("wordcloud" %in% input$plottype){
        options<-c(options,wordcloudOptions)
      }
      updateSelectInput(session, "variableName", choices = options,selected = NULL)
    }
  })
  
  ### Update available options in the filtering list
  filt_options<-c("Country", "Continent")
  
  observe({
    if(check(input$database)) return() # if no database
    if(!check(input$database)){
      if(input$database =="video"){
        booleanList<-video_booleanList
        characteristicList<-video_characteristicList
        varListWordCloud<-video_varListWordCloud
      } else if(input$database =="media"){
        booleanList<-media_booleanList
        characteristicList<-media_characteristicList
        varListWordCloud<-media_varListWordCloud
      }
      frequencyOptions<-c(booleanList, characteristicList) # same for the frequency polygon and the histogram
      plotlyOptions<-c(booleanList, characteristicList)
      wordcloudOptions<-varListWordCloud
    
      if("histogram" %in% input$plottype|| "frequencyPolygon" %in% input$plottype){
        options<-c(filt_options,frequencyOptions)
      }
      if("plotly" %in% input$plottype){
        options<-c(filt_options,plotlyOptions)
      }
      if("wordcloud" %in% input$plottype){
        options<-c(filt_options,wordcloudOptions)
      }
      updateSelectInput(session, "filter", choices = options,selected = NULL)
    }
  })
  
  ### Update available options in the filtering by list

  observe({
    check<-function(x){is.null(x) || x==""}
    if(check(input$filter)) return() # if no variable name
    
    if(!check(input$filter)){
      data=get(input$database)
      filt_var<-filter_variable()
      txt = as.character(data[[filt_var]])
      response.split<-str_split(txt, " |,")
      # ID levels
      lev<-unique(unlist(response.split))
      filter_list<- lev[!(is.na(lev))] # remove NA
      updateSelectInput(session, "filterVar", choices = filter_list,selected = "")
    }
  })
  
  
  ### Plot title
  
  output$caption<-renderText({
    switch(input$plottype,
           "wordcloud" = "Wordcloud",
           "histogram" 	= 	"Histogram",
           "frequencyPolygon" 	= 	"Frequency Polygon",
           "plotly" 	= 	"Plotly", 
           )
  })
  
  ### Get variable name
  
  variable<-reactive({
    check<-function(x){is.null(x) || x==""}
    if(check(input$variableName)) return() # if no variable name
    
    if(!check(input$variableName)){ # if it exists
      var<-titles[[input$variableName]]
    }
    var
  })
  
  ### Get filtering variable name
  
  filter_variable<-reactive({
    check<-function(x){is.null(x) || x==""}
    if(check(input$filter)) return() # if no variable name
    
    if(!check(input$filter)){
      var<-titles[[input$filter]]
    }
    var
  })
  
  ### Get data object
  
  plotData<-reactive({
    
    variable<-variable()
    
    if(!exists(input$database)) return() # if no database
    
    check<-function(x){is.null(x) || x==""}
    
    if(check(input$database)||check(variable)) return()
    
    data=get(input$database)
    
    check<-function(obj){
      !all(variable %in% colnames(obj))
    }
    
    if(check(data)) return()
    
    # PRE-PROCESS
    
    if (input$variableName %in% video_booleanList||input$variableName %in% media_booleanList){
      
      # filter data
      filt_var<-filter_variable()
      filter_by<-input$filterVar
      if (filter_by != "" && input$filterbox == TRUE){
        data<-data%>%
          filter(grepl(filter_by, get(filt_var)))
      }
      
      # make count data
      data_count<-data %>% 
        group_by(date, Country1, Continent)%>%
        summarize(Present = sum(get(variable)==1),
                  Absent = sum(get(variable)==0))
      
      # convert to long form
      df_long<-gather(data_count, variable, count, Present:Absent, factor_key = TRUE)
      
      # PROCESS VARS IN CHARACTERISTIC LIST
    } else if (input$variableName %in% video_characteristicList||input$variableName %in% media_characteristicList){
      
      # filter data
      filt_var<-filter_variable()
      filter_by<-input$filterVar
      if (filter_by != "" && input$filterbox == TRUE){
        data<-data%>%
          filter(grepl(filter_by, get(filt_var)))
      }
      
      # split on space
      resp.split<-str_split(as.character(data[[variable]]), " |,")
      
      # ID levels
      lev<-unique(unlist(resp.split))
      lev<-lev[lev !=""] # remove "" level
      lev<-lev[!is.na(lev)] # remove  NA level
      first<-lev[1]
      last<-lev[length(lev)]
      
      # convert to tabulated version
      resp.dummy<-lapply(resp.split, function(x) table(factor(x, levels = lev)))
      variable_tabs<-with(data, data.frame(ID, do.call(rbind, resp.dummy), get(variable)))
      
      # change name of the column
      names(variable_tabs)[length(variable_tabs)]<-variable
      
      # merge with data
      variable2<-merge(variable_tabs, data, by = c("ID", variable))
      
      # convert into a frequency table
      data_count<-variable2%>%
        gather(variable, count, first:last, factor_key = TRUE)
      
      # convert to long form
      df_long<-data_count %>% 
        group_by(date,variable, Country1, Continent)%>%
        summarize(count = sum(count ==1))
      
    }
    
    obj<-list(data = df_long,
              variable = variable)
    
    obj
    
  })
  
  
## WORDCLOUD
  
  # Get words function
    
    words<- reactive({
      variable<-variable()
      
      if(!exists(input$database)) return() # if no database
      
      check<-function(x){is.null(x) || x==""}
      
      if(check(input$database)||check(variable)) return()
      
      data=get(input$database)
      
      # filter data
      filt_var<-filter_variable()
      filter_by<-input$filterVar
      if (filter_by != "" && input$filterbox == TRUE){
        data<-data%>%
          filter(grepl(filter_by, get(filt_var)))
      }
      
      if (input$variableName %in% video_varListWordCloud||input$variableName %in% media_varListWordCloud){
        wordlist <- data%>%
          select(variable)%>%
          filter(!is.na(get(variable)))
        names(wordlist)[1]<-"word"
        
        # get tokens
        var_tokens<-tokens(unlist(as.character(wordlist)),remove_punct = TRUE,remove_symbols = TRUE, padding = FALSE)
        # remove stopwords
        var_tokens_stop<-tokens_remove(var_tokens, stopwords("en"), padding = FALSE)
        # use keyword phrases to replace some tokens
        phrases = c("covid-19","COVID-19", "corona virus","being moved", "wash hands", "living room", "soundtrack/theme song", "dance/electronica", "rap/hip-hop", "news report", "lyric video", "music video", "grief/sadness")
        toks_comp<-tokens_compound(var_tokens_stop, pattern = phrase(phrases))
        # convert to dataframe
        var_DFM<-dfm(toks_comp, remove = stopwords(source = "smart"))
        # prepare for wordcloud by counting
        f<-colSums(dfm_trim(var_DFM, min_termfreq = 1)) # could update mintermfreq if you want only words mentioned multiple times. 
        # create dataframe
        word_df<-data.frame(names(f), f)
        names(word_df)<-c("word", "n")
        word_df<-arrange(word_df, desc(n))%>%
          filter(!is.na(word))
        
        check<-function(obj){
          !all(c("word", "n") %in% colnames(obj))
        }
        
        if(check(word_df)) return()
        
        word_df
      }
      else {
        return()
      }
    })
    
    output$wordcloud<-renderWordcloud2({
      
      wordlist<-words()
      
      # conditions for plotting
      if(is.null(wordlist)) return()
      
      #make sure variable and group have loaded
      if(wordlist$word[1] == "" | wordlist$n[1] =="") return()
      
      set.seed(1234)
      colorpalette<-c("darkviolet","violet", "aquamarine", "turquoise","gold", "darkturquoise", "lightseagreen", "darkblue")
      colorVec<-rep(colorpalette, length.out=nrow(wordlist))
      
      wordcloud2(wordlist,rotateRatio = 0.3, color = colorVec, size = 0.4)
      
    })
    
    ## HISTOGRAM
    
    output$histogram<- renderPlot({
      plot.obj<-plotData()
      #conditions for plotting
      if(is.null(plot.obj)) return()
      
      #make sure variable and group have loaded
      if(plot.obj$variable == "") return()
      
      df_long<-plot.obj$data
      
      df<-df_long%>%
        select(variable, count)%>%
        group_by(variable)%>%
        summarise(count =sum(count, na.rm = TRUE)) # note without the na.rm = TRUE any instances of NA result in NAs for the sum
        
      ggplot(df,
             aes_string(
               x = "variable", 
               fill = "variable",
               y = "count"))+
        geom_col()+
        ggtitle(paste("Histogram of", input$variableName))+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45))
    })
    
    
    ## FREQUENCY POLYGON
    
    output$freqPoly<- renderPlot({
      plot.obj<-plotData()
      #conditions for plotting
      if(is.null(plot.obj)) return()
      
      #make sure variable has loaded
      if(plot.obj$variable == "") return()
      
      df_long<-plot.obj$data%>%
        filter(count>0)
        
      ggplot(df_long, aes_string(x = "date", colour = "variable"))+
        geom_freqpoly(binwidth=2)+
        ggtitle(paste("Frequency of", input$variableName))+
        theme_minimal()
    })
    
    ## PLOTLY
    
    output$plotly<-renderPlotly({
      plot.obj<-plotData()
      
      #conditions for plotting
      if(is.null(plot.obj)) return()
      
      #make sure variable has loaded
      if(plot.obj$variable == "") return()
      
      df_long<-plot.obj$data
      
      plotdat<-df_long %>%
        filter(count > 0)%>%
        filter(!is.na(Country1))%>%
        filter(!is.na(Continent))
      
      plot<-ggplot(plotdat, aes_string(x = "date", y = "variable", size = "count", color = "Continent"))+
        geom_jitter(alpha = 0.5, aes(text = paste0("Country = ", Country1,"\n", "Date = ", date, "\n", "Count = ", count)))+
        ggtitle(paste("Prevalence of",input$variableName))+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45),
              legend.title = element_blank()) 
      ggplotly(plot, tooltip = "text")
    })
    
    
    # ## CREATE SUMMARY
    # output$summary<-renderPrint({
    #   if(input$plottype == "histogram"||input$plottype == "plotly"||input$plottype == "frequencyPolygon"){
    #     plot.obj<-plotData()
    #     summary(plot.obj)
    #   } else if(input$plottype == "wordcloud"){
    #     wordList<-words()
    #     summary(wordList)
    #   }
    # })
  
    ## CREATE TABLE
    output$table <- renderTable({
      if (input$plottype == "histogram"||input$plottype == "plotly"||input$plottype == "frequencyPolygon"){
        table<-plotData()
        t<-table$data%>%
          filter(count>0)
        t$date<-as.character(t$date)
        t
        
      } else if (input$plottype == "wordcloud")
        wordList<-words()
      })
  }

# Create Shiny app ----
shinyApp(ui, server, options = list(height = 1000))