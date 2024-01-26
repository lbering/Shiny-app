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
  complete(Species = "Sprattus sprattus", fill = list(n = 0)) #FILL IN Sprattus sprattus IN ALL HAULS

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

øresund_sprat<-subset(øresund_data2,Species==  "Sprattus sprattus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
øresund_sprat1<-øresund_sprat %>% select('Year','Quarter','Species','LngtClass','CPUE_number_per_hour','haul','Ship','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
øresund_sprat2<- øresund_sprat1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_sprat$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

øresund_sprat_sum<-øresund_sprat2 %>%
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
    return(øresund_spratX<-subset(øresund_sprat_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(øresund_spratX<-subset(øresund_sprat_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(øresund_spratX<-subset(øresund_sprat_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  øresund_spratX<-size_function(size_class,length_min,length_max)
  
  øresund_spratX1<-øresund_spratX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  øresund_spratX<-merge(øresund_spratX1,øresund_spratX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so there's only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  øresund_spratX_occ<-øresund_spratX[order(øresund_spratX$haul,øresund_spratX$Country,decreasing = FALSE),] 
  øresund_spratX <-øresund_spratX_occ[!duplicated(øresund_spratX_occ$haul), ]
  
  return(øresund_spratX)
}

#------------------JUVENILES------------------#
øresund_spratJ<-Size_classes_function(1,100,150)
øresund_spratJ$source<-"øresund_spratJ" #add source so it can be subsetted in GAM
count(øresund_spratJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
øresund_spratM<-Size_classes_function(2,100,150)
øresund_spratM$source<-"øresund_spratM"
count(øresund_spratM) 

#------------------ADULTS------------------#
øresund_spratA<-Size_classes_function(3,100,150)
øresund_spratA$source<-"øresund_spratA" 
count(øresund_spratA)

###############################################################################
################################     kattegatnord.  ################################
###############################################################################

kattegatnord_data <- read_csv("DATRAS_data/kattegatnord_data.csv")#udtræk fra GIS
kattegatnord_data <-filter(kattegatnord_data ,Year>=2000)
kattegatnord_data<-subset(kattegatnord_data, Quarter=="1" |Quarter=="4")
kattegatnord_data$haul<-as.numeric(as.factor(with(kattegatnord_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatnord_data2<- kattegatnord_data%>% 
  group_by(haul) %>%
  complete(Species = "Sprattus sprattus", fill = list(n = 0)) #FILL IN Sprattus sprattus IN ALL HAULS

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

kattegatnord_sprat<-subset(kattegatnord_data2,Species==  "Sprattus sprattus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatnord_sprat1<-kattegatnord_sprat %>% select('Year','Quarter','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','Ship','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatnord_sprat2<- kattegatnord_sprat1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_sprat$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatnord_sprat_sum<-kattegatnord_sprat2 %>%
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
    return(kattegatnord_spratX<-subset(kattegatnord_sprat_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatnord_spratX<-subset(kattegatnord_sprat_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatnord_spratX<-subset(kattegatnord_sprat_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatnord_spratX<-size_function(size_class,length_min,length_max)
  
  kattegatnord_spratX1<-kattegatnord_spratX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatnord_spratX<-merge(kattegatnord_spratX1,kattegatnord_spratX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatnord_spratX_occ<-kattegatnord_spratX[order(kattegatnord_spratX$haul,kattegatnord_spratX$Country,decreasing = FALSE),] 
  kattegatnord_spratX <-kattegatnord_spratX_occ[!duplicated(kattegatnord_spratX_occ$haul), ]
  
  return(kattegatnord_spratX)
}

#------------------JUVENILES------------------#
kattegatnord_spratJ<-Size_classes_function(1,100,150)
kattegatnord_spratJ$source<-"kattegatnord_spratJ" #add source so it can be subsetted in GAM
count(kattegatnord_spratJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatnord_spratM<-Size_classes_function(2,100,150)
kattegatnord_spratM$source<-"kattegatnord_spratM"
count(kattegatnord_spratM) 

#------------------ADULTS------------------#
kattegatnord_spratA<-Size_classes_function(3,100,150)
kattegatnord_spratA$source<-"kattegatnord_spratA" 
count(kattegatnord_spratA)

###############################################################################
################################     kattegatmidt.  ################################
###############################################################################

kattegatmidt_data <- read_csv("DATRAS_data/kattegatmidt_data.csv")#udtræk fra GIS
kattegatmidt_data <-filter(kattegatmidt_data ,Year>=2000)
kattegatmidt_data<-subset(kattegatmidt_data, Quarter=="1" |Quarter=="4")
kattegatmidt_data$haul<-as.numeric(as.factor(with(kattegatmidt_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatmidt_data2<- kattegatmidt_data%>% 
  group_by(haul) %>%
  complete(Species = "Sprattus sprattus", fill = list(n = 0)) #FILL IN Sprattus sprattus IN ALL HAULS

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

kattegatmidt_sprat<-subset(kattegatmidt_data2,Species==  "Sprattus sprattus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatmidt_sprat1<-kattegatmidt_sprat %>% select('Year','Quarter','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Ship','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatmidt_sprat2<- kattegatmidt_sprat1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_sprat$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatmidt_sprat_sum<-kattegatmidt_sprat2 %>%
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
    return(kattegatmidt_spratX<-subset(kattegatmidt_sprat_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatmidt_spratX<-subset(kattegatmidt_sprat_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatmidt_spratX<-subset(kattegatmidt_sprat_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatmidt_spratX<-size_function(size_class,length_min,length_max)
  
  kattegatmidt_spratX1<-kattegatmidt_spratX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatmidt_spratX<-merge(kattegatmidt_spratX1,kattegatmidt_spratX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatmidt_spratX_occ<-kattegatmidt_spratX[order(kattegatmidt_spratX$haul,kattegatmidt_spratX$Country,decreasing = FALSE),] 
  kattegatmidt_spratX <-kattegatmidt_spratX_occ[!duplicated(kattegatmidt_spratX_occ$haul), ]
  
  return(kattegatmidt_spratX)
}

#------------------JUVENILES------------------#
kattegatmidt_spratJ<-Size_classes_function(1,100,150)
kattegatmidt_spratJ$source<-"kattegatmidt_spratJ" #add source so it can be subsetted in GAM
count(kattegatmidt_spratJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatmidt_spratM<-Size_classes_function(2,100,150)
kattegatmidt_spratM$source<-"kattegatmidt_spratM"
count(kattegatmidt_spratM) 

#------------------ADULTS------------------#
kattegatmidt_spratA<-Size_classes_function(3,100,150)
kattegatmidt_spratA$source<-"kattegatmidt_spratA" 
count(kattegatmidt_spratA)


###############################################################################
################################     kattegatsyd.  ################################
###############################################################################

kattegatsyd_data <- read_csv("DATRAS_data/kattegatsyd_data.csv")#udtræk fra GIS
kattegatsyd_data <-filter(kattegatsyd_data ,Year>=2000)
kattegatsyd_data<-subset(kattegatsyd_data, Quarter=="1" |Quarter=="4")
kattegatsyd_data$haul<-as.numeric(as.factor(with(kattegatsyd_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatsyd_data2<- kattegatsyd_data%>% 
  group_by(haul) %>%
  complete(Species = "Sprattus sprattus", fill = list(n = 0)) #FILL IN Sprattus sprattus IN ALL HAULS

kattegatsyd_data2<-kattegatsyd_data2 %>%
  group_by(haul) %>%
  fill(Gear, .direction = 'downup')%>%
  fill(Quarter,.direction = 'downup')%>%
  fill(SedimentDK,.direction = 'downup')%>%
  fill(Year,.direction = 'downup')%>%
  fill(Species,.direction = 'downup')%>%
  fill(Depth,.direction = 'downup')%>%
  fill(Survey,.direction = 'downup')%>%
  fill(DateTime,.direction = 'downup')

kattegatsyd_sprat<-subset(kattegatsyd_data2,Species==  "Sprattus sprattus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatsyd_sprat1<-kattegatsyd_sprat %>% select('Year','Quarter','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Ship','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatsyd_sprat2<- kattegatsyd_sprat1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_sprat$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatsyd_sprat_sum<-kattegatsyd_sprat2 %>%
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
    return(kattegatsyd_spratX<-subset(kattegatsyd_sprat_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatsyd_spratX<-subset(kattegatsyd_sprat_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatsyd_spratX<-subset(kattegatsyd_sprat_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatsyd_spratX<-size_function(size_class,length_min,length_max)
  
  kattegatsyd_spratX1<-kattegatsyd_spratX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatsyd_spratX<-merge(kattegatsyd_spratX1,kattegatsyd_spratX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatsyd_spratX_occ<-kattegatsyd_spratX[order(kattegatsyd_spratX$haul,kattegatsyd_spratX$Country,decreasing = FALSE),] 
  kattegatsyd_spratX <-kattegatsyd_spratX_occ[!duplicated(kattegatsyd_spratX_occ$haul), ]
  
  return(kattegatsyd_spratX)
}

#------------------JUVENILES------------------#
kattegatsyd_spratJ<-Size_classes_function(1,100,150)
kattegatsyd_spratJ$source<-"kattegatsyd_spratJ" #add source so it can be subsetted in GAM
count(kattegatsyd_spratJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatsyd_spratM<-Size_classes_function(2,100,150)
kattegatsyd_spratM$source<-"kattegatsyd_spratM"
count(kattegatsyd_spratM) 

#------------------ADULTS------------------#
kattegatsyd_spratA<-Size_classes_function(3,100,150)
kattegatsyd_spratA$source<-"kattegatsyd_spratA" 
count(kattegatsyd_spratA)

###############################################################################
################################     storebælt.  ################################
###############################################################################

storebælt_data <- read_csv("DATRAS_data/storebælt_data.csv")#udtræk fra GIS
storebælt_data <-filter(storebælt_data ,Year>=2000)
storebælt_data<-subset(storebælt_data, Quarter=="1" |Quarter=="4")
storebælt_data$haul<-as.numeric(as.factor(with(storebælt_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

storebælt_data2<- storebælt_data%>% 
  group_by(haul) %>%
  complete(Species = "Sprattus sprattus", fill = list(n = 0)) #FILL IN Sprattus sprattus IN ALL HAULS

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

storebælt_sprat<-subset(storebælt_data2,Species==  "Sprattus sprattus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
storebælt_sprat1<-storebælt_sprat %>% select('Year','Quarter','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Ship','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
storebælt_sprat2<- storebælt_sprat1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_sprat$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

storebælt_sprat_sum<-storebælt_sprat2 %>%
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
    return(storebælt_spratX<-subset(storebælt_sprat_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(storebælt_spratX<-subset(storebælt_sprat_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(storebælt_spratX<-subset(storebælt_sprat_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  storebælt_spratX<-size_function(size_class,length_min,length_max)
  
  storebælt_spratX1<-storebælt_spratX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  storebælt_spratX<-merge(storebælt_spratX1,storebælt_spratX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  storebælt_spratX_occ<-storebælt_spratX[order(storebælt_spratX$haul,storebælt_spratX$Country,decreasing = FALSE),] 
  storebælt_spratX <-storebælt_spratX_occ[!duplicated(storebælt_spratX_occ$haul), ]
  
  return(storebælt_spratX)
}

#------------------JUVENILES------------------#
storebælt_spratJ<-Size_classes_function(1,100,150)
storebælt_spratJ$source<-"storebælt_spratJ" #add source so it can be subsetted in GAM
count(storebælt_spratJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
storebælt_spratM<-Size_classes_function(2,100,150)
storebælt_spratM$source<-"storebælt_spratM"
count(storebælt_spratM) 

#------------------ADULTS------------------#
storebælt_spratA<-Size_classes_function(3,100,150)
storebælt_spratA$source<-"storebælt_spratA" 
count(storebælt_spratA)

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
  complete(Species = "Sprattus sprattus", fill = list(n = 0)) #FILL IN Sprattus sprattus IN ALL HAULS

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

femeren_sprat<-subset(femeren_data2,Species==  "Sprattus sprattus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
femeren_sprat1<-femeren_sprat %>% select('Year','Quarter','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Ship','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
femeren_sprat2<- femeren_sprat1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_sprat$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

femeren_sprat_sum<-femeren_sprat2 %>%
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
    return(femeren_spratX<-subset(femeren_sprat_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(femeren_spratX<-subset(femeren_sprat_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(femeren_spratX<-subset(femeren_sprat_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  femeren_spratX<-size_function(size_class,length_min,length_max)
  
  femeren_spratX1<-femeren_spratX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  femeren_spratX<-merge(femeren_spratX1,femeren_spratX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  femeren_spratX_occ<-femeren_spratX[order(femeren_spratX$haul,femeren_spratX$Country,decreasing = FALSE),] 
  femeren_spratX <-femeren_spratX_occ[!duplicated(femeren_spratX_occ$haul), ]
  
  return(femeren_spratX)
}

#------------------JUVENILES------------------#
femeren_spratJ<-Size_classes_function(1,100,150)
femeren_spratJ$source<-"femeren_spratJ" #add source so it can be subsetted in GAM
count(femeren_spratJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
femeren_spratM<-Size_classes_function(2,100,150)
femeren_spratM$source<-"femeren_spratM"
count(femeren_spratM) 

#------------------ADULTS------------------#
femeren_spratA<-Size_classes_function(3,100,150)
femeren_spratA$source<-"femeren_spratA" 
count(femeren_spratA)

###############################################################################
################################     østersøen.  ################################
###############################################################################

østersøen_data <- read_csv("DATRAS_data/østersøen_data.csv")#udtræk fra GIS
østersøen_data <-filter(østersøen_data ,Year>=2000)
østersøen_data<-subset(østersøen_data, Quarter=="1" |Quarter=="4")
østersøen_data$haul<-as.numeric(as.factor(with(østersøen_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

østersøen_data2<- østersøen_data%>% 
  group_by(haul) %>%
  complete(Species = "Sprattus sprattus", fill = list(n = 0)) #FILL IN Sprattus sprattus IN ALL HAULS

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

østersøen_sprat<-subset(østersøen_data2,Species==  "Sprattus sprattus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
østersøen_sprat1<-østersøen_sprat %>% select('Year','Quarter','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Ship','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
østersøen_sprat2<- østersøen_sprat1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_sprat$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

østersøen_sprat_sum<-østersøen_sprat2 %>%
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
    return(østersøen_spratX<-subset(østersøen_sprat_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(østersøen_spratX<-subset(østersøen_sprat_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(østersøen_spratX<-subset(østersøen_sprat_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  østersøen_spratX<-size_function(size_class,length_min,length_max)
  
  østersøen_spratX1<-østersøen_spratX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  østersøen_spratX<-merge(østersøen_spratX1,østersøen_spratX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  østersøen_spratX_occ<-østersøen_spratX[order(østersøen_spratX$haul,østersøen_spratX$Country,decreasing = FALSE),] 
  østersøen_spratX <-østersøen_spratX_occ[!duplicated(østersøen_spratX_occ$haul), ]
  
  return(østersøen_spratX)
}

#------------------JUVENILES------------------#
østersøen_spratJ<-Size_classes_function(1,100,150)
østersøen_spratJ$source<-"østersøen_spratJ" #add source so it can be subsetted in GAM
count(østersøen_spratJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
østersøen_spratM<-Size_classes_function(2,100,150)
østersøen_spratM$source<-"østersøen_spratM"
count(østersøen_spratM) 

#------------------ADULTS------------------#
østersøen_spratA<-Size_classes_function(3,100,150)
østersøen_spratA$source<-"østersøen_spratA" 
count(østersøen_spratA)

###############################################################################
########################  SAVE EVERYTHING FOR GAM #############################
###############################################################################

save(øresund_spratJ,øresund_spratM,øresund_spratA,kattegatnord_spratA,
     kattegatnord_spratM ,kattegatnord_spratJ,kattegatmidt_spratA,
     kattegatmidt_spratM,kattegatmidt_spratJ,kattegatsyd_spratA,kattegatsyd_spratM,
     kattegatsyd_spratJ,femeren_spratA,femeren_spratM,femeren_spratJ,storebælt_spratA,storebælt_spratM,storebælt_spratJ
     ,østersøen_spratA,østersøen_spratJ,østersøen_spratM,file = "Area_spratAges.RData")


sprat_all_combines<-vctrs::vec_c(øresund_spratJ,øresund_spratM,øresund_spratA,kattegatnord_spratA,
                                 kattegatnord_spratM ,kattegatnord_spratJ,kattegatmidt_spratA,
                                 kattegatmidt_spratM,kattegatmidt_spratJ,kattegatsyd_spratA,kattegatsyd_spratM,
                                 kattegatsyd_spratJ,femeren_spratA,femeren_spratM,femeren_spratJ,
                                 storebælt_spratA,storebælt_spratM,storebælt_spratJ,
                                 østersøen_spratA,østersøen_spratJ,østersøen_spratM,.name_spec = NULL)

sprat_all_combines$source<-as.factor(sprat_all_combines$source)
dat_sprat=sprat_all_combines

dat_sprat$POSIXyear<-as.POSIXct(dat_sprat$DateTime,format='%d/%m/%Y')
dat_sprat$numyear <- as.numeric(format(dat_sprat$POSIXyear, format = "%Y")) + as.numeric(format(dat_sprat$POSIXyear, format = "%m"))/12 + as.numeric(format(dat_sprat$POSIXyear, format = "%d"))/365

dat_sprat<-dat_sprat%>%
  rename(
    cpue=sum,
    time=numyear, 
    depth=Depth,
    loc=source
  )

write.csv(dat_sprat,"dat_sprat.csv")


