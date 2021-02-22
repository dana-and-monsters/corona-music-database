library(ggplot2)
library(plotly)
library(shiny)
library(readr)
library(qdapTools)
library(dplyr)
library(tidyr)
library(ggridges)
library(lubridate)
library(stringr)
library(tidytext)
library(wordcloud2)
library(quanteda)
library(shinythemes)
library(fastDummies)
library(data.table)

source("app-pre-processing.R") # scripts for preprocessing are located here along with the black theme (theme_black())

# load data
media<-read_delim("Coronamusic_MEDIA_v20210218_utf8.csv", delim = ";") # these utf-8 files were created by taking the file sent by Niels (ANSI encoding) opening in Notepad, save as and changing encoding to utf-8. There are still at least 2 special characters that are not working (named )
video<-read_delim("Coronamusic_VIDEO_v20210220_utf8.csv", delim = ";")
titles<-read_delim("Titles.csv", delim = ";")

#re-code dummy variables into the media file. The media file was changed after the app was created. These variables were originally dummy-coded/Boolean-coded. 
media <- dummy_cols(media,select_columns="MediaFormat",split=",") # Remake old dummy variables
setnames(media,# Rename dummy variables to be consistent with previous format
         old=c("MediaFormat_audio","MediaFormat_video","MediaFormat_writtenDigital","MediaFormat_writtenPrint"),
         new=c("MediaAudio","MediaVideo","MediaWrittenOnline","MediaWrittenPrint"))
media<-media%>%
  select(-MediaFormat)

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
  theme = shinytheme("cyborg"),# other options: darkly, slate, cyborg
  # # CSS
  tags$head(
    tags$style(HTML("
        h3 {
        font-size: 26px;
        }
        .well {
         color: white;
         background-color: #45CDD1;
        }
                    "))
  ),
  
  # Title
  titlePanel(title=div(img(src="Title-w-logo-transparent-2.png", width = "100%"))),
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
                       "Plotly" = "plotly",
                       "Wordcloud" = "wordcloud")
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
      ),
      # change y-axis range
      checkboxInput("y_range", "Change y-axis range")
      ),
### OUTPUT 

    mainPanel = mainPanel(
      tags$style(HTML("
          .nav.nav-tabs>li.active>a{
          background-color: #45CDD1;
          }
       
                  ")),
      h3(textOutput("caption")),
      
      ## tabsets
      
      tabsetPanel(
          type = "tabs",
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
                            wordcloud2Output('wordcloud', width = "auto") #, width = "100%", height= "400px"

                          ),
                          conditionalPanel(
                            condition = 'input.plottype == "plotly"',
                            plotlyOutput('plotly')
                          )
                        ),
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
      updateSelectInput(session, "variableName", choices = options,selected = input$variableName)
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
      response.split<-str_split(txt, ",")
      response.split.no.ws<-str_trim(unlist(response.split))
      # ID levels
      lev<-unique(response.split.no.ws)
      filter_list<- lev[!(is.na(lev))] # remove NA
      filter_list<-filter_list[filter_list !="?"]# remove "?"
      updateSelectInput(session, "filterVar", choices = filter_list[str_order(filter_list)],selected = "")
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
      filter_by = ""
      if (input$filterVar!=""){
        filter_by<-input$filterVar  
      }
      data_f<-data
      if (filter_by != "" && input$filterbox == TRUE){
        data_f<-data%>%
          filter(grepl(filter_by, get(filt_var)))
      }
      
      # make count data
      data_count<-data_f %>% 
        group_by(date, Country1, Continent)%>%
        summarize(Present = sum(get(variable)==1),
                  Absent = sum(get(variable)==0))
      
      # convert to long form
      df_long<-gather(data_count, variable, count, Present:Absent, factor_key = TRUE)
      
      obj<-list(data = df_long,
                variable = variable,
                ylim = nrow(data))
      
      obj
      
      # PROCESS VARS IN CHARACTERISTIC LIST
    } else if (input$variableName %in% video_characteristicList||input$variableName %in% media_characteristicList){
      
      # filter data
      filt_var<-filter_variable()
      filter_by<-input$filterVar
      data_f<-data
      if (filter_by != "" && input$filterbox == TRUE){
        data_f<-data%>%
          filter(grepl(filter_by, get(filt_var)))
      }
      
      # split on space
      resp.split<-str_split(as.character(data_f[[variable]]), " |,")
      
      # ID levels
      lev<-unique(unlist(resp.split))
      lev<-lev[lev !=""] # remove "" level
      lev<-lev[!is.na(lev)] # remove  NA level
      first<-lev[1]
      last<-lev[length(lev)]
      
      # convert to tabulated version
      resp.dummy<-lapply(resp.split, function(x) table(factor(x, levels = lev)))
      variable_tabs<-with(data_f, data.frame(ID, do.call(rbind, resp.dummy), get(variable)))
      
      # change name of the column
      names(variable_tabs)[length(variable_tabs)]<-variable
      
      # merge with data
      variable2<-merge(variable_tabs, data_f, by = c("ID", variable))
      
      # convert into a frequency table
      data_count<-variable2%>%
        gather(variable, count, first:last, factor_key = TRUE)
      
      # convert to long form
      df_long<-data_count %>% 
        group_by(date,variable, Country1, Continent)%>%
        summarize(count = sum(count ==1))
      
      obj<-list(data = df_long,
                variable = variable,
                ylim = nrow(data))
      
      obj
      
    } else {
      obj<-NULL
      
      obj
    }
    
    
    
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
      data_f<-data
      if (filter_by != "" && input$filterbox == TRUE){
        data_f<-data%>%
          filter(grepl(filter_by, get(filt_var)))
      }
      
      if (input$variableName %in% video_varListWordCloud||input$variableName %in% media_varListWordCloud){
        wordlist <- data_f%>%
          select(variable)%>%
          filter(!is.na(get(variable)))
        names(wordlist)[1]<-"word"
        
        # get tokens
        var_tokens<-tokens(unlist(as.character(wordlist)),remove_punct = TRUE,remove_symbols = TRUE, padding = FALSE)
        # remove stopwords
        var_tokens_stop<-tokens_remove(var_tokens, stopwords("en"), padding = FALSE)
        # use keyword phrases to replace some tokens
        phrases = c("covid-19","COVID-19", "corona virus","being moved", "wash hands", "living room", "soundtrack/theme song", "dance/electronica", "rap/hip-hop", "news report", "lyric video", "music video", "grief/sadness", "theme song", "brass band", "nursery rhyme", "heavy metal")
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
      colorpalette<-c("darkviolet", "cyan", "violet","gold", "mediumspringgreen") # , darkturquoise", "aquamarine" turquoise
      colorVec<-rep(colorpalette, length.out=nrow(wordlist))
      
      sizeValue = 0.4#0.4
      wordcloud2(wordlist,rotateRatio = 0.3, color = colorVec, backgroundColor = "black", size = sizeValue, widgetsize =c("1000","1000")) #, widgetsize = "200%"
      
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
        
      ylimit<-plot.obj$ylim
      if(input$y_range == TRUE){
        ylimit = max(df$count)
      }
      
      names(df)<-c("Variable", "Count")
      
      ggplot(df,
             aes_string(
               x = "Variable", 
               fill = "Variable",
               y = "Count"))+
        geom_col()+
        scale_y_continuous(name = "Count", limits = c(0,ylimit))+
        ggtitle(paste("Histogram of", input$variableName))+
        theme_black()+
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
        scale_x_date(name = "Date", date_breaks = "1 week",date_labels = "%b %d", limits = c(as.Date("2020-02-09"),as.Date("2020-07-27")))+
        ggtitle(paste("Frequency of", input$variableName))+
        labs(y = "Count", colour = "Variable")+
        theme_black()+
        theme(axis.text.x = element_text(angle = 45))
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
        geom_jitter(alpha = 0.7, aes(text = paste0("Country = ", Country1,"\n", "Date = ", date, "\n", "Count = ", count)))+
        scale_x_date(name = "Date", date_breaks = "1 week",date_labels = "%b %d", limits = c(as.Date("2020-02-09"),as.Date("2020-07-27")))+
        ggtitle(paste("Prevalence of",input$variableName))+
        labs(y = "Variable")+
        theme_black()+
        theme(axis.text.x = element_text(angle = 45),
              legend.title = element_blank()) 
      ggplotly(plot, tooltip = "text")
    })
  
    ## CREATE TABLE
    output$table <- renderTable({
      
      if (input$plottype == "histogram"||input$plottype == "plotly"||input$plottype == "frequencyPolygon"){
        table<-plotData()
        if(is.null(table)) return()
        t<-table$data%>%
          filter(count>0)
        t$date<-as.character(t$date)
        names(t)<-c("Date", "Country", "Continent", input$variableName, "Count")
        t
        
      } else if (input$plottype == "wordcloud"){
          wordList<-words()
          if(is.null(wordList)) return()
          wordList$n<-as.integer(wordList$n)
          names(wordList)<-c("Word", "Count")
          wordList
      }
      })
  }

# Create Shiny app ----
shinyApp(ui, server, options = list(height = 1000))