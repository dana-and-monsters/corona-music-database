# try black theme
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size, color = "white"),  
      legend.title = element_text(size = base_size, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.spacing = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size, color = "white"),  
      strip.text.y = element_text(size = base_size, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white", vjust = 2),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}

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
  data$Continent[grepl(pattern, data$Country1)]<- "North America"
  
  SouthAmerica<-c("Chile", "Brazil", "Colombia")
  pattern<- paste(SouthAmerica, collapse = "|")
  data$Continent[grepl(pattern, data$Country1)]<- "South America"
  
  asia<-c("Malaysia", "India", "Singapore","Saudi Arabia", "Indonesia", "Japan", "China", "Vietnam","Korea", "Lebanon", "Iran", "The Philippines","Philippines", "Pakistan", "Thailand", "Israel", "Sri Lanka", "United Arab Emirates")
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

## TRY SETTING FUNCTION:
make_setting_code<-function(data) {
  
  # Convert to character class
  data$Setting<-as.character(data$Setting)
  # Identify the supercategory
  data$Setting_code<-""
  # home
  data$Setting_code["home" %in% data$Setting]<-paste(data$Setting_code, "home", sep = " ")
  # outside
  data$Setting_code["outside" %in% data$Setting]<-paste(data$Setting_code, "outside", sep = " ")
  # 
  # maybe add "Studio"
  
  return(data)
}