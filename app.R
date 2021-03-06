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

source("app-pre-processing-2.R") # scripts for preprocessing are located here along with the black theme (theme_black())

# load data
media<-read_delim("Coronamusic_MEDIA_v20210519-utf8.csv", delim = ";") # these utf-8 files were created by taking the file sent by Niels (ANSI encoding) opening in Notepad, save as and changing encoding to utf-8. 
video<-read_delim("Coronamusic_VIDEO_v20210519-utf8.csv", delim = ";")
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

# Show only certain variables depending on which plot is selected
# VIDEO
video_varListWordCloud<-c("Title", "Social Media Platform", "Country", "Setting", "Genre", "Emotions","Features")
video_booleanList = c("Joint Musicking", "Original Covid Song","Original Covid Lyrics","Movement", "Health Information", "Conflict")
video_characteristicList = c("Setting", "Genre", "Social Media Platform", "Emotions", "Features")
# MEDIA
media_varListWordCloud<-c("Title", "Media Platform", "Country", "Language", "Setting", "Emotions","Features")
media_booleanList = c("Media Written Online", "Media Audio","Media Video","Media Written Print", "Embedded External Video", "Movement", "Health Information", "Conflict")
media_characteristicList = c("Setting", "Emotions", "Music Maker Professionalism", "Features")

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
  titlePanel(title=div(img(src="Title-w-logo-transparent-2.png",alt = "Coronamusic Database",  width = "100%")), windowTitle = "Coronamusic Database"),
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
                    choices = ""),
        selectInput("filterVar", "Category:",
                    choices = "")
      ),
      # change y-axis range
      checkboxInput("y_range", "Change y-axis range of histogram")
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
      ),
      "Hansen NC, Treider JMG, Swarbrick D, Bamford JS, Wilson J and Vuoskoski JK (2021)",
      tags$a(href="https://www.frontiersin.org/articles/10.3389/fpsyg.2021.684083/full", "A Crowd-Sourced Database of Coronamusic: Documenting Online Making and Sharing of Music During the COVID-19 Pandemic."),
      "Front. Psychol. 12:684083. doi: 10.3389/fpsyg.2021.684083",
      br(),
      br(),
      "Logo includes images of headphones provided by Mozilla, cute coronavirus by Manuela Molina (@mindheart.kids), and coronaviral font, all licensed under CC BY 4.0."
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
  
  ### Get variable name
  
  variable<-reactive({
    check<-function(x){is.null(x) || x==""}
    if(check(input$variableName)) return() # if no variable name
    
    if(!check(input$variableName)){ # if it exists
      var<-titles[[input$variableName]]
    }
    var
  })
  
  ### Update available options in the filtering list
  
  
  
  options2<-c()
  observeEvent(input$filterbox, {
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
        options2<-c(options2, "Country", "Continent",frequencyOptions)
      }
      if("plotly" %in% input$plottype){
        options2<-c(options2, "Country", "Continent",plotlyOptions)
      }
      if("wordcloud" %in% input$plottype){
        options<-c(options2, "Continent",wordcloudOptions)
        options2<- options[options!="Title"]
      }
      updateSelectInput(session, "filter", choices = options2,selected = input$filter)
    }
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
  
  ### Get filtering variable options
  
  
  filter_variable_options<-reactive({ # 
    filteroptions<-character(0)
    check<-function(x){is.null(x) || x==""}
    if(check(input$filter)) return() # if no variable name
    
    if(!check(input$filter)){
      data=get(input$database)
      filt_var<-filter_variable()
      txt = as.character(data[[filt_var]])
      response.split<-str_split(txt, ",")
      cats<-str_trim(unlist(response.split))
      filt_opt<-data.frame(cats)
      filt_opt2<-filt_opt%>%
        group_by(cats)%>%
        summarize(count = n())%>%
        filter(count>5)%>% # display only options with greater than 5 instances
        filter(!is.na(cats)) #remove na
      filteroptions<-filt_opt2$cats
    }
    filteroptions
  })

  
  ### Update available options in the filtering by list
  observeEvent(input$filter,{ #observeEvent(input$filter, {
      filterchoices<-character(0)
      filterchoices<-filter_variable_options()
      select= head(filterchoices,1)
      updateSelectInput(session = session,inputId =  "filterVar", choices = filterchoices, selected = select) #     }
  })
  
#   observe({ #observeEvent(filter_variable(),{...}
#     if(check(input$database)||check(input$filter)||input$filterbox == FALSE) return() # if no database, filter, or filtering box
#     if(!check(input$database)){
#       filterchoices<-c()
#       
#       if(input$filterbox == TRUE && !check(input$filter)){
# 
# #        filt_var<-input$filter
#         
#       #if(check(filVar)) return() # if no filtering variable name
#   
#       #if(!check(filVar)){ # if there is a filtering variable name
#         
#         # data=get(input$database)
#         # # filt_var<-filter_variable()
#         # txt = as.character(data[[filt_var]])
#         # response.split<-str_split(txt, ",")
#         # cats<-str_trim(unlist(response.split))
#         # filt_opt<-data.frame(cats)
#         # filt_opt2<-filt_opt%>%
#         #   group_by(cats)%>%
#         #   summarize(count = n())%>%
#         #   filter(count>5)%>% # display only options with greater than 5 instances
#         #   filter(!is.na(cats)) #remove na
#         # filterchoices<-filt_opt2$cats
#         filterchoices<-filter_variable_options()
#       }
#       else if (check(input$filter)){
#         filterchoices <- character(0)
#       }
#         
#         updateSelectInput(session = session,inputId =  "filterVar", choices = filterchoices, selected = input$filterVar) # 
#     }
#     #}
#   })
  
  ### Get filtering category name
  
  filter_variable_cat<-reactive({
    var<-character(0)
    check<-function(x){is.null(x) || x==""}
    if(check(input$filterVar)) return() # if no variable name
    
    if(!check(input$filterVar)){
      var<-input$filterVar
    }
    var
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
  
  ### Get data object
  
  plotData<-reactive({
    
    variable<-variable()
    
    if(!exists(input$database)) return() # if no database
    
    check<-function(x){is.null(x) || x==""}
    
    if(check(input$database)||check(variable)) return()
    
    data=get(input$database)
    
    check2<-function(obj){
      !all(variable %in% colnames(obj))
    }
    
    if(check2(data)) return()
    
    # PRE-PROCESS
    
    if (input$variableName %in% video_booleanList||input$variableName %in% media_booleanList){
      
      # filter data
      filt_var<-filter_variable()
      #filter_by = "" # commented out May 20th trying to repair filtering in wordcloud 
      filter_by = filter_variable_cat()
      # if (input$filterVar!=""){
      #   #filter_by<-input$filterVar  
      #   filter_by<-filter_variable_cat()
      # }
      data_f<-data
      if (!check(filter_by) && input$filterbox == TRUE){
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
      filter_by<-filter_variable_cat()#input$filterVar
      
      
      data_f<-data
      if (!check(filter_by) && input$filterbox == TRUE){ 
        data_f<-data%>%
          filter(grepl(filter_by, get(filt_var)))
      }
      
      # split on comma
      resp.split<-str_split(as.character(data_f[[variable]]), ",")
      
      #remove whitespace before first word
      resp.split_no_ws<-lapply(resp.split, str_trim)
      
      # ID levels
      lev<-unique(unlist(resp.split_no_ws))
      lev<-lev[!is.na(lev)] # remove  NA level
      first<-lev[1]
      last<-lev[length(lev)]
      
      # convert to tabulated version
      resp.dummy<-lapply(resp.split_no_ws, function(x) table(factor(x, levels = lev)))
      dummy.table<-do.call(rbind, resp.dummy)
      
      # check.names = FALSE should keep the var names the same punctuation
      variable_tabs<-with(data_f, data.frame(ID, dummy.table, get(variable), check.names=FALSE))
      
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
      
      check2<-function(obj){
        !all(variable %in% colnames(obj))
      }
      
      if(check2(data)) return()
      
      # filter data
      filt_var<-filter_variable()
      filter_by<-filter_variable_cat()#input$filterVar
      opt<-filter_variable_options()
      data_f<-data
      
      if (check(filter_by) && input$filterbox == TRUE) return()
      
      
      if (!check(filter_by) && input$filterbox == TRUE){
        if (!filter_by %in% opt) return() # THIS ALLOWS THE APP TO UPDATE THE FILTERING VARIABLE! VERY IMPORTNAT solved May 21, 2021
        
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
        var_tokens_stop<-tokens_remove(var_tokens, c(stopwords("en"), stopwords("fr"), stopwords("it"), stopwords("da"), stopwords("es"), stopwords("no")), padding = FALSE)
        # use keyword phrases to replace some tokens
        phrases = c("covid-19","COVID-19", "corona virus","being moved", "wash hands", "living room", "soundtrack/theme song", "dance/electronica", "rap/hip-hop", "news report", "lyric video", "music video", "grief/sadness", "theme song", "brass band",  "marching band","nursery rhyme", "heavy metal", "classic rock", "bossa nova", "2019", "2020", "Puerto Rico", "Costa Rica", "Dominican Republic", "Hong Kong", "Sri Lanka", "parking lot", "suburban neighbourhood", "bull pin", "deserted plaza", "cruise ship", "South African", "black and white","computer generated", "toilet paper", "spoken word", "Los Angeles", "World Economic Forum", "Saudi Arabia", "military base", "fire station", "live streaming", "original song", "government silencing", "participatory music making", "fake news", "joint singing", "live concert", "virtual concert", "virtual performance", "virtual dance", "virtual choir", "virtual festival", "virtual reality", "music industry", "music making")
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
      
      #Filter to only include categories with >5 instances
      df2<-df%>%
        filter(count>5)
        
      ylimit<-plot.obj$ylim
      if(input$y_range == TRUE){
        ylimit = max(df2$count)
      }
      
      names(df2)<-c("Variable", "Count")
      validate(
        need(dim(df2)[1]>0, "There are less than 5 instances of the categories of this variable with these filtering options. Please change the variable and/or filtering.")
      )
      ggplot(df2,
             aes_string(
               x = "Variable", 
               fill = "Variable",
               y = "Count"))+
        geom_col()+
        scale_y_continuous(name = "Count", limits = c(0,ylimit))+
        ggtitle(paste("Histogram of", input$variableName))+
        theme_black()+
        theme(axis.text.x = element_text(angle = 45))
    },bg = "black")
    
    
    ## FREQUENCY POLYGON
    
    output$freqPoly<- renderPlot({
      plot.obj<-plotData()
      #conditions for plotting
      if(is.null(plot.obj)) return()
      
      #make sure variable has loaded
      if(plot.obj$variable == "") return()
      
      #Filter to only include categories with >5 instances
      df_long<-plot.obj$data%>%
        filter(count>0)
      
      # check which items have greater than 5 instances regardless of location so know which vars to plot
      
      vars_to_plot<-df_long%>%
        select(variable, count)%>%
        group_by(variable)%>%
        summarise(count =sum(count, na.rm = TRUE))%>% # note without the na.rm = TRUE any instances of NA result in NAs for the sum
        filter(count>5)%>%
        select(variable)
      
      df2<-df_long[(which(df_long$variable %in% unlist(vars_to_plot))), ]
      validate(
        need(dim(df2)[1]>0, "There are less than 5 instances of the categories of this variable with these filtering options. Please change the variable and/or filtering.")
      )
      ggplot(df2, aes_string(x = "date", colour = "variable"))+
        geom_freqpoly(binwidth=2)+
        scale_x_date(name = "Date", date_breaks = "1 week",date_labels = "%b %d", limits = c(as.Date("2020-02-09"),as.Date("2020-07-27")))+
        ggtitle(paste("Frequency of", input$variableName))+
        labs(y = "Count", colour = "Variable")+
        theme_black()+
        theme(axis.text.x = element_text(angle = 45))
    },bg = "black")
    
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
      
      # check which items have greater than 5 instances regardless of location so know which vars to plot
      
      vars_to_plot<-plotdat%>%
        select(variable, count)%>%
        group_by(variable)%>%
        summarise(count =sum(count, na.rm = TRUE))%>% # note without the na.rm = TRUE any instances of NA result in NAs for the sum
        filter(count>5)%>%
        select(variable)
      
      df2<-plotdat[(which(plotdat$variable %in% unlist(vars_to_plot))), ]
      validate(
        need(dim(df2)[1]>0, "There are less than 5 instances of the categories of this variable with these filtering options. Please change the variable and/or filtering.")
      )
        plot<-ggplot(df2, aes_string(x = "date", y = "variable", size = "count", color = "Continent"))+
          geom_jitter(alpha = 0.7, aes(text = paste0("Variable = ", variable, "\n", "Country = ", Country1,"\n", "Date = ", date, "\n", "Count = ", count)))+
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
      if(input$plottype == "histogram"){
        table<-plotData()
        if(is.null(table)) return()
        t<-table$data%>%
          filter(count>0)
        t$date<-as.character(t$date)
        
        df<-t%>%
          select(variable, count)%>%
          group_by(variable)%>%
          summarise(count =sum(count, na.rm = TRUE)) # note without the na.rm = TRUE any instances of NA result in NAs for the sum
        names(df)<-c(input$variableName, "Count")
        df
      }
      else if (input$plottype == "plotly"||input$plottype == "frequencyPolygon"){
        table<-plotData()
        if(is.null(table)) return()
        t<-table$data%>%
          filter(count>0)
        t$date<-as.character(t$date)
        names(t)<-c("Date", input$variableName, "Country", "Continent", "Count")
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