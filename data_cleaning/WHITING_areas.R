#libraries
library(readr)
library(tidyverse)
library(dplyr)

load("species_weight.Rdata")

###############################################################################
################################     ØRESUND.  ################################
###############################################################################

øresund_data <- read_csv("DATRAS_data/øresund_data.csv")#udtræk fra GIS
øresund_data <-filter(øresund_data ,Year>=2000)
øresund_data<-subset(øresund_data, Quarter=="1" |Quarter=="4")
øresund_data$haul<-as.numeric(as.factor(with(øresund_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

øresund_data2<- øresund_data%>% 
  group_by(haul) %>%
  complete(Species = "Merlangius merlangus", fill = list(n = 0)) #FILL IN Merlangius merlangus IN ALL HAULS

øresund_data2<-øresund_data2 %>%
  group_by(haul) %>%
  fill(Gear, .direction = 'downup')%>%
  fill(Quarter,.direction = 'downup')%>%
  fill(SedimentDK,.direction = 'downup')%>%
  fill(Year,.direction = 'downup')%>%
  fill(Species,.direction = 'downup')%>%
  fill(Depth,.direction = 'downup')%>%
  fill(Survey,.direction = 'downup')%>%
  fill(Ship,.direction = 'downup')%>%
  fill(Country,.direction = 'downup')%>%
  fill(DateTime,.direction = 'downup')

øresund_whiting<-subset(øresund_data2,Species==  "Merlangius merlangus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
øresund_whiting1<-øresund_whiting %>% select('Year','Quarter','Species','LngtClass','Ship','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
øresund_whiting2<- øresund_whiting1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_whiting$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

øresund_whiting_sum<-øresund_whiting2 %>%
  group_by(haul) %>%
  fill(Gear, .direction = 'downup')%>%
  fill(Quarter,.direction = 'downup')%>%
  fill(SedimentDK,.direction = 'downup')%>%
  fill(Year,.direction = 'downup')%>%
  fill(Species,.direction = 'downup')%>%
  fill(Depth,.direction = 'downup')%>%
  fill(Survey,.direction = 'downup')%>%
  fill(Ship,.direction = 'downup')%>%
  fill(Country,.direction = 'downup')%>%
  fill(DateTime,.direction = 'downup')

size_function<-function(sizeclass,length_min,length_max){
  if (sizeclass==1){
    return(øresund_whitingX<-subset(øresund_whiting_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(øresund_whitingX<-subset(øresund_whiting_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(øresund_whitingX<-subset(øresund_whiting_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  øresund_whitingX<-size_function(size_class,length_min,length_max)
  
  øresund_whitingX1<-øresund_whitingX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  øresund_whitingX<-merge(øresund_whitingX1,øresund_whitingX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  øresund_whitingX_occ<-øresund_whitingX[order(øresund_whitingX$haul,øresund_whitingX$Country,decreasing = FALSE),] 
  øresund_whitingX <-øresund_whitingX_occ[!duplicated(øresund_whitingX_occ$haul), ]
  
  return(øresund_whitingX)
}

#------------------JUVENILES------------------#
øresund_whitingJ<-Size_classes_function(1,200,300)
øresund_whitingJ$source<-"øresund_whitingJ" #add source so it can be subsetted in GAM
count(øresund_whitingJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
øresund_whitingM<-Size_classes_function(2,200,300)
øresund_whitingM$source<-"øresund_whitingM"
count(øresund_whitingM) 

#------------------ADULTS------------------#
øresund_whitingA<-Size_classes_function(3,200,300)
øresund_whitingA$source<-"øresund_whitingA" 
count(øresund_whitingA)

###############################################################################
################################     kattegatnord.  ################################
###############################################################################

kattegatnord_data <- read_csv("DATRAS_data/kattegatnord_data.csv")#udtræk fra GIS
kattegatnord_data <-filter(kattegatnord_data ,Year>=2000)
kattegatnord_data<-subset(kattegatnord_data, Quarter=="1" |Quarter=="4")
kattegatnord_data$haul<-as.numeric(as.factor(with(kattegatnord_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatnord_data2<- kattegatnord_data%>% 
  group_by(haul) %>%
  complete(Species = "Merlangius merlangus", fill = list(n = 0)) #FILL IN Merlangius merlangus IN ALL HAULS

kattegatnord_data2<-kattegatnord_data2 %>%
  group_by(haul) %>%
  fill(Gear, .direction = 'downup')%>%
  fill(Quarter,.direction = 'downup')%>%
  fill(SedimentDK,.direction = 'downup')%>%
  fill(Year,.direction = 'downup')%>%
  fill(Species,.direction = 'downup')%>%
  fill(Depth,.direction = 'downup')%>%
  fill(Survey,.direction = 'downup')%>%
  fill(Ship,.direction = 'downup')%>%
  fill(Country,.direction = 'downup')%>%
  fill(DateTime,.direction = 'downup')

kattegatnord_whiting<-subset(kattegatnord_data2,Species==  "Merlangius merlangus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatnord_whiting1<-kattegatnord_whiting %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatnord_whiting2<- kattegatnord_whiting1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_whiting$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatnord_whiting_sum<-kattegatnord_whiting2 %>%
  group_by(haul) %>%
  fill(Gear, .direction = 'downup')%>%
  fill(Quarter,.direction = 'downup')%>%
  fill(SedimentDK,.direction = 'downup')%>%
  fill(Year,.direction = 'downup')%>%
  fill(Species,.direction = 'downup')%>%
  fill(Depth,.direction = 'downup')%>%
  fill(Survey,.direction = 'downup')%>%
  fill(Ship,.direction = 'downup')%>%
  fill(Country,.direction = 'downup')%>%
  fill(DateTime,.direction = 'downup')

size_function<-function(sizeclass,length_min,length_max){
  if (sizeclass==1){
    return(kattegatnord_whitingX<-subset(kattegatnord_whiting_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatnord_whitingX<-subset(kattegatnord_whiting_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatnord_whitingX<-subset(kattegatnord_whiting_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatnord_whitingX<-size_function(size_class,length_min,length_max)
  
  kattegatnord_whitingX1<-kattegatnord_whitingX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatnord_whitingX<-merge(kattegatnord_whitingX1,kattegatnord_whitingX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatnord_whitingX_occ<-kattegatnord_whitingX[order(kattegatnord_whitingX$haul,kattegatnord_whitingX$Country,decreasing = FALSE),] 
  kattegatnord_whitingX <-kattegatnord_whitingX_occ[!duplicated(kattegatnord_whitingX_occ$haul), ]
  
  return(kattegatnord_whitingX)
}

#------------------JUVENILES------------------#
kattegatnord_whitingJ<-Size_classes_function(1,200,300)
kattegatnord_whitingJ$source<-"kattegatnord_whitingJ" #add source so it can be subsetted in GAM
count(kattegatnord_whitingJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatnord_whitingM<-Size_classes_function(2,200,300)
kattegatnord_whitingM$source<-"kattegatnord_whitingM"
count(kattegatnord_whitingM) 

#------------------ADULTS------------------#
kattegatnord_whitingA<-Size_classes_function(3,200,300)
kattegatnord_whitingA$source<-"kattegatnord_whitingA" 
count(kattegatnord_whitingA)

###############################################################################
################################     kattegatmidt.  ################################
###############################################################################

kattegatmidt_data <- read_csv("DATRAS_data/kattegatmidt_data.csv")#udtræk fra GIS
kattegatmidt_data <-filter(kattegatmidt_data ,Year>=2000)
kattegatmidt_data<-subset(kattegatmidt_data, Quarter=="1" |Quarter=="4")
kattegatmidt_data$haul<-as.numeric(as.factor(with(kattegatmidt_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatmidt_data2<- kattegatmidt_data%>% 
  group_by(haul) %>%
  complete(Species = "Merlangius merlangus", fill = list(n = 0)) #FILL IN Merlangius merlangus IN ALL HAULS

kattegatmidt_data2<-kattegatmidt_data2 %>%
  group_by(haul) %>%
  fill(Gear, .direction = 'downup')%>%
  fill(Quarter,.direction = 'downup')%>%
  fill(SedimentDK,.direction = 'downup')%>%
  fill(Year,.direction = 'downup')%>%
  fill(Species,.direction = 'downup')%>%
  fill(Depth,.direction = 'downup')%>%
  fill(Survey,.direction = 'downup')%>%
  fill(Ship,.direction = 'downup')%>%
  fill(Country,.direction = 'downup')%>%
  fill(DateTime,.direction = 'downup')

kattegatmidt_whiting<-subset(kattegatmidt_data2,Species==  "Merlangius merlangus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatmidt_whiting1<-kattegatmidt_whiting %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatmidt_whiting2<- kattegatmidt_whiting1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_whiting$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatmidt_whiting_sum<-kattegatmidt_whiting2 %>%
  group_by(haul) %>%
  group_by(haul) %>%
  fill(Gear, .direction = 'downup')%>%
  fill(Quarter,.direction = 'downup')%>%
  fill(SedimentDK,.direction = 'downup')%>%
  fill(Year,.direction = 'downup')%>%
  fill(Species,.direction = 'downup')%>%
  fill(Depth,.direction = 'downup')%>%
  fill(Survey,.direction = 'downup')%>%
  fill(Ship,.direction = 'downup')%>%
  fill(Country,.direction = 'downup')%>%
  fill(DateTime,.direction = 'downup')

size_function<-function(sizeclass,length_min,length_max){
  if (sizeclass==1){
    return(kattegatmidt_whitingX<-subset(kattegatmidt_whiting_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatmidt_whitingX<-subset(kattegatmidt_whiting_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatmidt_whitingX<-subset(kattegatmidt_whiting_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatmidt_whitingX<-size_function(size_class,length_min,length_max)
  
  kattegatmidt_whitingX1<-kattegatmidt_whitingX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatmidt_whitingX<-merge(kattegatmidt_whitingX1,kattegatmidt_whitingX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatmidt_whitingX_occ<-kattegatmidt_whitingX[order(kattegatmidt_whitingX$haul,kattegatmidt_whitingX$Country,decreasing = FALSE),] 
  kattegatmidt_whitingX <-kattegatmidt_whitingX_occ[!duplicated(kattegatmidt_whitingX_occ$haul), ]
  
  return(kattegatmidt_whitingX)
}

#------------------JUVENILES------------------#
kattegatmidt_whitingJ<-Size_classes_function(1,200,300)
kattegatmidt_whitingJ$source<-"kattegatmidt_whitingJ" #add source so it can be subsetted in GAM
count(kattegatmidt_whitingJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatmidt_whitingM<-Size_classes_function(2,200,300)
kattegatmidt_whitingM$source<-"kattegatmidt_whitingM"
count(kattegatmidt_whitingM) 

#------------------ADULTS------------------#
kattegatmidt_whitingA<-Size_classes_function(3,200,300)
kattegatmidt_whitingA$source<-"kattegatmidt_whitingA" 
count(kattegatmidt_whitingA)


###############################################################################
################################     kattegatsyd.  ################################
###############################################################################

kattegatsyd_data <- read_csv("DATRAS_data/kattegatsyd_data.csv")#udtræk fra GIS
kattegatsyd_data <-filter(kattegatsyd_data ,Year>=2000)
kattegatsyd_data<-subset(kattegatsyd_data, Quarter=="1" |Quarter=="4")
kattegatsyd_data$haul<-as.numeric(as.factor(with(kattegatsyd_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatsyd_data2<- kattegatsyd_data%>% 
  group_by(haul) %>%
  complete(Species = "Merlangius merlangus", fill = list(n = 0)) #FILL IN Merlangius merlangus IN ALL HAULS

kattegatsyd_data2<-kattegatsyd_data2 %>%
  group_by(haul) %>%
  fill(Gear, .direction = 'downup')%>%
  fill(Quarter,.direction = 'downup')%>%
  fill(SedimentDK,.direction = 'downup')%>%
  fill(Year,.direction = 'downup')%>%
  fill(Species,.direction = 'downup')%>%
  fill(Depth,.direction = 'downup')%>%
  fill(Survey,.direction = 'downup')%>%
  fill(Ship,.direction = 'downup')%>%
  fill(Country,.direction = 'downup')%>%
  fill(DateTime,.direction = 'downup')

kattegatsyd_whiting<-subset(kattegatsyd_data2,Species==  "Merlangius merlangus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatsyd_whiting1<-kattegatsyd_whiting %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatsyd_whiting2<- kattegatsyd_whiting1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_whiting$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatsyd_whiting_sum<-kattegatsyd_whiting2 %>%
  group_by(haul) %>%
  fill(Gear, .direction = 'downup')%>%
  fill(Quarter,.direction = 'downup')%>%
  fill(SedimentDK,.direction = 'downup')%>%
  fill(Year,.direction = 'downup')%>%
  fill(Species,.direction = 'downup')%>%
  fill(Depth,.direction = 'downup')%>%
  fill(Survey,.direction = 'downup')%>%
  fill(Ship,.direction = 'downup')%>%
  fill(Country,.direction = 'downup')%>%
  fill(DateTime,.direction = 'downup')

size_function<-function(sizeclass,length_min,length_max){
  if (sizeclass==1){
    return(kattegatsyd_whitingX<-subset(kattegatsyd_whiting_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatsyd_whitingX<-subset(kattegatsyd_whiting_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatsyd_whitingX<-subset(kattegatsyd_whiting_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatsyd_whitingX<-size_function(size_class,length_min,length_max)
  
  kattegatsyd_whitingX1<-kattegatsyd_whitingX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatsyd_whitingX<-merge(kattegatsyd_whitingX1,kattegatsyd_whitingX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatsyd_whitingX_occ<-kattegatsyd_whitingX[order(kattegatsyd_whitingX$haul,kattegatsyd_whitingX$Country,decreasing = FALSE),] 
  kattegatsyd_whitingX <-kattegatsyd_whitingX_occ[!duplicated(kattegatsyd_whitingX_occ$haul), ]
  
  return(kattegatsyd_whitingX)
}

#------------------JUVENILES------------------#
kattegatsyd_whitingJ<-Size_classes_function(1,200,300)
kattegatsyd_whitingJ$source<-"kattegatsyd_whitingJ" #add source so it can be subsetted in GAM
count(kattegatsyd_whitingJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatsyd_whitingM<-Size_classes_function(2,200,300)
kattegatsyd_whitingM$source<-"kattegatsyd_whitingM"
count(kattegatsyd_whitingM) 

#------------------ADULTS------------------#
kattegatsyd_whitingA<-Size_classes_function(3,200,300)
kattegatsyd_whitingA$source<-"kattegatsyd_whitingA" 
count(kattegatsyd_whitingA)

###############################################################################
################################     storebælt.  ################################
###############################################################################

storebælt_data <- read_csv("DATRAS_data/storebælt_data.csv")#udtræk fra GIS
storebælt_data <-filter(storebælt_data ,Year>=2000)
storebælt_data<-subset(storebælt_data, Quarter=="1" |Quarter=="4")
storebælt_data$haul<-as.numeric(as.factor(with(storebælt_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

storebælt_data2<- storebælt_data%>% 
  group_by(haul) %>%
  complete(Species = "Merlangius merlangus", fill = list(n = 0)) #FILL IN Merlangius merlangus IN ALL HAULS

storebælt_data2<-storebælt_data2 %>%
  group_by(haul) %>%
  fill(Gear, .direction = 'downup')%>%
  fill(Quarter,.direction = 'downup')%>%
  fill(SedimentDK,.direction = 'downup')%>%
  fill(Year,.direction = 'downup')%>%
  fill(Species,.direction = 'downup')%>%
  fill(Depth,.direction = 'downup')%>%
  fill(Survey,.direction = 'downup')%>%
  fill(Ship,.direction = 'downup')%>%
  fill(Country,.direction = 'downup')%>%
  fill(DateTime,.direction = 'downup')

storebælt_whiting<-subset(storebælt_data2,Species==  "Merlangius merlangus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
storebælt_whiting1<-storebælt_whiting %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
storebælt_whiting2<- storebælt_whiting1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_whiting$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

storebælt_whiting_sum<-storebælt_whiting2 %>%
  group_by(haul) %>%
  fill(Gear, .direction = 'downup')%>%
  fill(Quarter,.direction = 'downup')%>%
  fill(SedimentDK,.direction = 'downup')%>%
  fill(Year,.direction = 'downup')%>%
  fill(Species,.direction = 'downup')%>%
  fill(Depth,.direction = 'downup')%>%
  fill(Survey,.direction = 'downup')%>%
  fill(Ship,.direction = 'downup')%>%
  fill(Country,.direction = 'downup')%>%
  fill(DateTime,.direction = 'downup')

size_function<-function(sizeclass,length_min,length_max){
  if (sizeclass==1){
    return(storebælt_whitingX<-subset(storebælt_whiting_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(storebælt_whitingX<-subset(storebælt_whiting_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(storebælt_whitingX<-subset(storebælt_whiting_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  storebælt_whitingX<-size_function(size_class,length_min,length_max)
  
  storebælt_whitingX1<-storebælt_whitingX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  storebælt_whitingX<-merge(storebælt_whitingX1,storebælt_whitingX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  storebælt_whitingX_occ<-storebælt_whitingX[order(storebælt_whitingX$haul,storebælt_whitingX$Country,decreasing = FALSE),] 
  storebælt_whitingX <-storebælt_whitingX_occ[!duplicated(storebælt_whitingX_occ$haul), ]
  
  return(storebælt_whitingX)
}

#------------------JUVENILES------------------#
storebælt_whitingJ<-Size_classes_function(1,200,300)
storebælt_whitingJ$source<-"storebælt_whitingJ" #add source so it can be subsetted in GAM
count(storebælt_whitingJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
storebælt_whitingM<-Size_classes_function(2,200,300)
storebælt_whitingM$source<-"storebælt_whitingM"
count(storebælt_whitingM) 

#------------------ADULTS------------------#
storebælt_whitingA<-Size_classes_function(3,200,300)
storebælt_whitingA$source<-"storebælt_whitingA" 
count(storebælt_whitingA)

###############################################################################
################################     femeren.  ################################
###############################################################################

femeren_data <- read_csv("DATRAS_data/femeren_data.csv")#udtræk fra GIS
femeren_data <-filter(femeren_data ,Year>=2000)
femeren_data<-subset(femeren_data, Quarter=="1" |Quarter=="4")
femeren_data$haul<-as.numeric(as.factor(with(femeren_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID
femeren_data$CPUE_number_per_hour<-femeren_data$CPUE_numbe
femeren_data2<- femeren_data%>% 
  group_by(haul) %>%
  complete(Species = "Merlangius merlangus", fill = list(n = 0)) #FILL IN Merlangius merlangus IN ALL HAULS

femeren_data2<-femeren_data2 %>%
  group_by(haul) %>%
  fill(Gear, .direction = 'downup')%>%
  fill(Quarter,.direction = 'downup')%>%
  fill(SedimentDK,.direction = 'downup')%>%
  fill(Year,.direction = 'downup')%>%
  fill(Species,.direction = 'downup')%>%
  fill(Depth,.direction = 'downup')%>%
  fill(Survey,.direction = 'downup')%>%
  fill(Ship,.direction = 'downup')%>%
  fill(Country,.direction = 'downup')%>%
  fill(DateTime,.direction = 'downup')

femeren_whiting<-subset(femeren_data2,Species==  "Merlangius merlangus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
femeren_whiting1<-femeren_whiting %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
femeren_whiting2<- femeren_whiting1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_whiting$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

femeren_whiting_sum<-femeren_whiting2 %>%
  group_by(haul) %>%
  fill(Gear, .direction = 'downup')%>%
  fill(Quarter,.direction = 'downup')%>%
  fill(SedimentDK,.direction = 'downup')%>%
  fill(Year,.direction = 'downup')%>%
  fill(Species,.direction = 'downup')%>%
  fill(Depth,.direction = 'downup')%>%
  fill(Survey,.direction = 'downup')%>%
  fill(Ship,.direction = 'downup')%>%
  fill(Country,.direction = 'downup')%>%
  fill(DateTime,.direction = 'downup')

size_function<-function(sizeclass,length_min,length_max){
  if (sizeclass==1){
    return(femeren_whitingX<-subset(femeren_whiting_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(femeren_whitingX<-subset(femeren_whiting_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(femeren_whitingX<-subset(femeren_whiting_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  femeren_whitingX<-size_function(size_class,length_min,length_max)
  
  femeren_whitingX1<-femeren_whitingX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  femeren_whitingX<-merge(femeren_whitingX1,femeren_whitingX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  femeren_whitingX_occ<-femeren_whitingX[order(femeren_whitingX$haul,femeren_whitingX$Country,decreasing = FALSE),] 
  femeren_whitingX <-femeren_whitingX_occ[!duplicated(femeren_whitingX_occ$haul), ]
  
  return(femeren_whitingX)
}

#------------------JUVENILES------------------#
femeren_whitingJ<-Size_classes_function(1,200,300)
femeren_whitingJ$source<-"femeren_whitingJ" #add source so it can be subsetted in GAM
count(femeren_whitingJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
femeren_whitingM<-Size_classes_function(2,200,300)
femeren_whitingM$source<-"femeren_whitingM"
count(femeren_whitingM) 

#------------------ADULTS------------------#
femeren_whitingA<-Size_classes_function(3,200,300)
femeren_whitingA$source<-"femeren_whitingA" 
count(femeren_whitingA)

###############################################################################
################################     østersøen.  ################################
###############################################################################

østersøen_data <- read_csv("DATRAS_data/østersøen_data.csv")#udtræk fra GIS
østersøen_data <-filter(østersøen_data ,Year>=2000)
østersøen_data<-subset(østersøen_data, Quarter=="1" |Quarter=="4")
østersøen_data$haul<-as.numeric(as.factor(with(østersøen_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

østersøen_data2<- østersøen_data%>% 
  group_by(haul) %>%
  complete(Species = "Merlangius merlangus", fill = list(n = 0)) #FILL IN Merlangius merlangus IN ALL HAULS

østersøen_data2<-østersøen_data2 %>%
  group_by(haul) %>%
  fill(Gear, .direction = 'downup')%>%
  fill(Quarter,.direction = 'downup')%>%
  fill(SedimentDK,.direction = 'downup')%>%
  fill(Year,.direction = 'downup')%>%
  fill(Species,.direction = 'downup')%>%
  fill(Depth,.direction = 'downup')%>%
  fill(Survey,.direction = 'downup')%>%
  fill(Ship,.direction = 'downup')%>%
  fill(Country,.direction = 'downup')%>%
  fill(DateTime,.direction = 'downup')

østersøen_whiting<-subset(østersøen_data2,Species==  "Merlangius merlangus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
østersøen_whiting1<-østersøen_whiting %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
østersøen_whiting2<- østersøen_whiting1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_whiting$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

østersøen_whiting_sum<-østersøen_whiting2 %>%
  group_by(haul) %>%
  fill(Gear, .direction = 'downup')%>%
  fill(Quarter,.direction = 'downup')%>%
  fill(SedimentDK,.direction = 'downup')%>%
  fill(Year,.direction = 'downup')%>%
  fill(Species,.direction = 'downup')%>%
  fill(Depth,.direction = 'downup')%>%
  fill(Survey,.direction = 'downup')%>%
  fill(Ship,.direction = 'downup')%>%
  fill(Country,.direction = 'downup')%>%
  fill(DateTime,.direction = 'downup')

size_function<-function(sizeclass,length_min,length_max){
  if (sizeclass==1){
    return(østersøen_whitingX<-subset(østersøen_whiting_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(østersøen_whitingX<-subset(østersøen_whiting_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(østersøen_whitingX<-subset(østersøen_whiting_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  østersøen_whitingX<-size_function(size_class,length_min,length_max)
  
  østersøen_whitingX1<-østersøen_whitingX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  østersøen_whitingX<-merge(østersøen_whitingX1,østersøen_whitingX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  østersøen_whitingX_occ<-østersøen_whitingX[order(østersøen_whitingX$haul,østersøen_whitingX$Country,decreasing = FALSE),] 
  østersøen_whitingX <-østersøen_whitingX_occ[!duplicated(østersøen_whitingX_occ$haul), ]
  
  return(østersøen_whitingX)
}

#------------------JUVENILES------------------#
østersøen_whitingJ<-Size_classes_function(1,200,300)
østersøen_whitingJ$source<-"østersøen_whitingJ" #add source so it can be subsetted in GAM
count(østersøen_whitingJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
østersøen_whitingM<-Size_classes_function(2,200,300)
østersøen_whitingM$source<-"østersøen_whitingM"
count(østersøen_whitingM) 

#------------------ADULTS------------------#
østersøen_whitingA<-Size_classes_function(3,200,300)
østersøen_whitingA$source<-"østersøen_whitingA" 
count(østersøen_whitingA)

###############################################################################
########################  SAVE EVERYTHING FOR GAM #############################
###############################################################################

save(øresund_whitingJ,øresund_whitingM,øresund_whitingA,kattegatnord_whitingA,
     kattegatnord_whitingM ,kattegatnord_whitingJ,kattegatmidt_whitingA,
     kattegatmidt_whitingM,kattegatmidt_whitingJ,kattegatsyd_whitingA,kattegatsyd_whitingM,
     kattegatsyd_whitingJ,femeren_whitingA,femeren_whitingM,femeren_whitingJ,storebælt_whitingA,storebælt_whitingM,storebælt_whitingJ
     ,østersøen_whitingA,østersøen_whitingJ,østersøen_whitingM,file = "Area_whitingAges.RData")


whiting_all_combines<-vctrs::vec_c(øresund_whitingJ,øresund_whitingM,øresund_whitingA,kattegatnord_whitingA,
                                   kattegatnord_whitingM ,kattegatnord_whitingJ,kattegatmidt_whitingA,
                                   kattegatmidt_whitingM,kattegatmidt_whitingJ,kattegatsyd_whitingA,kattegatsyd_whitingM,
                                   kattegatsyd_whitingJ,femeren_whitingA,femeren_whitingM,femeren_whitingJ,
                                   storebælt_whitingA,storebælt_whitingM,storebælt_whitingJ,
                                   østersøen_whitingA,østersøen_whitingJ,østersøen_whitingM,.name_spec = NULL)

whiting_all_combines$source<-as.factor(whiting_all_combines$source)
dat=whiting_all_combines

dat$POSIXyear<-as.POSIXct(dat$DateTime,format='%d/%m/%Y')
dat$numyear <- as.numeric(format(dat$POSIXyear, format = "%Y")) + as.numeric(format(dat$POSIXyear, format = "%m"))/12 + as.numeric(format(dat$POSIXyear, format = "%d"))/365

dat<-dat%>%
  rename(
    cpue=sum,
    time=numyear, 
    depth=Depth,
    loc=source
  )


write.csv(dat,"dat_whiting.csv")
