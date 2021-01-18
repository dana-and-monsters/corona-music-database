preprocess_data<-function(data) {

  # factor category
  data$Category<-factor(data$Category)
  summary(data$Category)
  
  # convert date 
  data$date<-dmy(data$PublicationDate)#, format = "%d-%m-%Y")
   
  return(data)
}

factor_video_variables<-function(data){
  # factor boolean variables
  data$JointMusicking<-factor(data$JointMusicking)
  data$OrigCovidSong<-factor(data$OrigCovidSong)
  data$OrigCovidLyrics<-factor(data$OrigCovidLyrics)
  data$Movement<-factor(data$Movement)
  data$HealthInfo<-factor(data$HealthInfo)
  data$Conflict<-factor(data$Conflict)
  
  return(data)
}
  
# Create dummy coded tabulated data for the variables containing lists
  
make_country_and_continent_columns<-function(data){
  
  # Countries
  # Convert to character class
  data$Country<-as.character(data$Country)
  # split on comma
  resp.split<-strsplit(data$Country, ", ")
  # ID levels
  lev<-unique(unlist(resp.split))
  # convert to tabulated version
  resp.dummy<-lapply(resp.split, function(x) table(factor(x, levels = lev)))
  countries<-with(data, data.frame(ID, do.call(rbind, resp.dummy), Country))
  
  # create a var that contains only one country or international
  data$Country1<-data$Country
  data$Country1[str_detect(data$Country1, ",")]<-"International"
  summary(as.factor(data$Country1))
  
  # Add continent
  # Create sums by Continent
  europe<-c("Austria", "Belgium", "Croatia", "Denmark", "France", "Finland", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Lithuania","Norway", "Netherlands", "Poland", "Portugal", "Scotland", "Serbia","Slovenia","Slovakia", "Switzerland", "Spain", "Sweden", "Turkey", "UK","Ukraine")
  pattern<- paste(europe, collapse = "|")
  data$Continent[grepl(pattern, data$Country1)]<- "Europe"
  
  NorthAmerica<-c("USA", "Canada", "Mexico", "Costa Rica", "Dominican Republic", "Panama", "Puerto Rico", "Jamaica", "Federation of Saint Christopher and Nevis")
  pattern<- paste(NorthAmerica, collapse = "|")
  data$Continent[grepl(pattern, data$Country1)]<- "North_America"
  
  SouthAmerica<-c("Chile", "Brazil", "Colombia")
  pattern<- paste(SouthAmerica, collapse = "|")
  data$Continent[grepl(pattern, data$Country1)]<- "South_America"
  
  asia<-c("Malaysia", "India", "Singapore","Saudi Arabia", "Indonesia", "Japan", "China", "Vietnam","Korea", "Lebanon", "Iran", "The Phillipines","Phillipines", "Pakistan", "Thailand", "Israel", "Sri Lanka", "United Arab Emirates")
  pattern<- paste(asia, collapse = "|")
  data$Continent[grepl(pattern, data$Country1)]<- "Asia"
  
  oceania<-c("Australia", "New Zealand")
  pattern<- paste(oceania, collapse = "|")
  data$Continent[grepl(pattern, data$Country1)]<- "Oceania"
  
  africa<-c("South Africa", "Uganda","Senegal", "Algeria", "Kenya")
  pattern<- paste(africa, collapse = "|")
  data$Continent[grepl(pattern, data$Country1)]<- "Africa"
  
  int<-c("International")
  pattern<- paste(int, collapse = "|")
  data$Continent[grepl(pattern, data$Country1)]<- "International"
  
  return(data)
}

# Setting

## TRY SETTING FUNCTION:
make_setting_code<-function(data) {
  
  #unique(data$Setting)
  
  # Convert to character class
  data$Setting<-as.character(data$Setting)
  # replace words with other words to categorize them into meaningful categories
  data$Setting_code<-""
  # home words
  myValues<-c("home", "living room", "kitchen","indoors")
  pattern<- paste(myValues, collapse = "|")
  data$Setting_code[grepl(pattern, data$Setting)]<-"home"
  # outside words
  myValues<-c("outdoors", "outdoor", "outside", "street")
  pattern<- paste(myValues, collapse = "|")
  data$Setting_code[grepl(pattern, data$Setting)]<-paste(data$Setting_code[grepl(pattern, data$Setting)], "outside", sep = " ")
  # balcony words
  myValues<-c("balcony", "window")
  pattern<- paste(myValues, collapse = "|")
  data$Setting_code[grepl(pattern, data$Setting)]<-paste(data$Setting_code[grepl(pattern, data$Setting)], "balcony", sep = " ")
  # digital words
  myValues<-c("lyrics", "animation", "green screen", "album art")
  pattern<- paste(myValues, collapse = "|")
  data$Setting_code[grepl(pattern, data$Setting)]<-paste(data$Setting_code[grepl(pattern, data$Setting)], "virtual", sep = " ")
  # professional words
  myValues<-c("studio", "stage", "club", "concert hall", "venue", "museum")
  pattern<- paste(myValues, collapse = "|")
  data$Setting_code[grepl(pattern, data$Setting)]<-paste(data$Setting_code[grepl(pattern, data$Setting)], "professional", sep = " ")
  # healthcare words
  myValues<-c("hospital", "icu", "ambulance", "retirement")
  pattern<- paste(myValues, collapse = "|")
  data$Setting_code[grepl(pattern, data$Setting)]<-paste(data$Setting_code[grepl(pattern, data$Setting)], "healthcare", sep = " ")
  # workplace
  myValues<-c("workplace", "office", "warehouse", "classroom", "bus")
  pattern<- paste(myValues, collapse = "|")
  data$Setting_code[grepl(pattern, data$Setting)]<-paste(data$Setting_code[grepl(pattern, data$Setting)], "workplace", sep = " ")
  
  return(data)
}

# Genre 

# Make genre function

make_genre_code<-function(data){

  # Convert to character class
  data$Genre<-as.character(data$Genre)
  # replace words with other words to categorize them into meaningful categories
  data$Genre_code<-""
  
  myValues<-c("lternative")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-"alternative"
  myValues<-c("lues")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-"blues"
  myValues<-c("lassical")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)], "classical", sep = " ")
  myValues<-c("ountry")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)], "country", sep = " ")
  myValues<-c("ance", "lectronica")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"dance/electronica", sep = " ")
  myValues<-c("olk")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"folk", sep = " ")
  myValues<-c("funk", "Funk")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"funk", sep = " ")
  myValues<-c("ospel")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"gospel", sep = " ")
  myValues<-c("etal")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"metal", sep = " ")
  myValues<-c("orld")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"world", sep = " ")
  myValues<-c("azz")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"jazz", sep = " ")
  myValues<-c("new age")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"new age", sep = " ")
  myValues<-c("ldies")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"oldies", sep = " ")
  myValues<-c("pera")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"opera", sep = " ")
  myValues<-c("pop","Pop")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"pop", sep = " ")
  myValues<-c("punk", "Punk")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"punk", sep = " ")
  myValues<-c("hip", "hop", "rap", "Rap", "Hip", "Hop")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"rap/hip-hop", sep = " ")
  myValues<-c("eggae")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"reggae", sep = " ")
  myValues<-c("eligious", "hristian")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"religious", sep = " ")
  myValues<-c("ock")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"rock", sep = " ")
  myValues<-c("soul", "R&B", "Soul")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"soul/r&b", sep = " ")
  myValues<-c("oundtrack", "heme song")
  pattern<- paste(myValues, collapse = "|")
  data$Genre_code[grepl(pattern, data$Genre)]<-paste(data$Genre_code[grepl(pattern, data$Genre)],"soundtracks/theme song", sep = " ")
  
  return(data)
}

# Emotions

## TRY EMOTIONS FUNCTION:
make_emotions_code<-function(data) {
  
  # Convert to character class
  data$Emotions<-as.character(data$Emotions)
  # replace words with other words to categorize them into meaningful categories
  data$Emotion_code<-""
  # humour words
  myValues<-c("humour")
  pattern<- paste(myValues, collapse = "|")
  data$Emotion_code[grepl(pattern, data$Emotions)]<-"humour"
  # gratitude words
  myValues<-c("gratitude")
  pattern<- paste(myValues, collapse = "|")
  data$Emotion_code[grepl(pattern, data$Emotions)]<-paste(data$Emotion_code[grepl(pattern, data$Emotions)], "gratitude", sep = " ")
  # loneliness words
  myValues<-c("loneliness")
  pattern<- paste(myValues, collapse = "|")
  data$Emotion_code[grepl(pattern, data$Emotions)]<-paste(data$Emotion_code[grepl(pattern, data$Emotions)], "loneliness", sep = " ")
  # grief/sadness words
  myValues<-c("grief/sadness")
  pattern<- paste(myValues, collapse = "|")
  data$Emotion_code[grepl(pattern, data$Emotions)]<-paste(data$Emotion_code[grepl(pattern, data$Emotions)], "grief_sadness", sep = " ")
  # togetherness words
  myValues<-c("togetherness")
  pattern<- paste(myValues, collapse = "|")
  data$Emotion_code[grepl(pattern, data$Emotions)]<-paste(data$Emotion_code[grepl(pattern, data$Emotions)], "togetherness", sep = " ")
  # anger words
  myValues<-c("anger")
  pattern<- paste(myValues, collapse = "|")
  data$Emotion_code[grepl(pattern, data$Emotions)]<-paste(data$Emotion_code[grepl(pattern, data$Emotions)], "anger", sep = " ")
  # happiness words
  myValues<-c("happiness")
  pattern<- paste(myValues, collapse = "|")
  data$Emotion_code[grepl(pattern, data$Emotions)]<-paste(data$Emotion_code[grepl(pattern, data$Emotions)], "happiness", sep = " ")
  # being moved words
  myValues<-c("being moved")
  pattern<- paste(myValues, collapse = "|")
  data$Emotion_code[grepl(pattern, data$Emotions)]<-paste(data$Emotion_code[grepl(pattern, data$Emotions)], "being_moved", sep = " ")
  
  
  return(data)
}