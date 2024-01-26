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
  complete(Species = "Clupea harengus", fill = list(n = 0)) #FILL IN Clupea harengus IN ALL HAULS

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

øresund_herring<-subset(øresund_data2,Species==  "Clupea harengus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
øresund_herring1<-øresund_herring %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
øresund_herring2<- øresund_herring1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_herring$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

øresund_herring_sum<-øresund_herring2 %>%
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
    return(øresund_herringX<-subset(øresund_herring_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(øresund_herringX<-subset(øresund_herring_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(øresund_herringX<-subset(øresund_herring_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  øresund_herringX<-size_function(size_class,length_min,length_max)
  
  øresund_herringX1<-øresund_herringX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  øresund_herringX<-merge(øresund_herringX1,øresund_herringX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  øresund_herringX_occ<-øresund_herringX[order(øresund_herringX$haul,øresund_herringX$Country,decreasing = FALSE),] 
  øresund_herringX <-øresund_herringX_occ[!duplicated(øresund_herringX_occ$haul), ]
  
  return(øresund_herringX)
}

#------------------JUVENILES------------------#
øresund_herringJ<-Size_classes_function(1,150,200)
øresund_herringJ$source<-"øresund_herringJ" #add source so it can be subsetted in GAM
count(øresund_herringJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
øresund_herringM<-Size_classes_function(2,150,200)
øresund_herringM$source<-"øresund_herringM"
count(øresund_herringM) 

#------------------ADULTS------------------#
øresund_herringA<-Size_classes_function(3,150,200)
øresund_herringA$source<-"øresund_herringA" 
count(øresund_herringA)

###############################################################################
################################     kattegatnord.  ################################
###############################################################################

kattegatnord_data <- read_csv("DATRAS_data/kattegatnord_data.csv")#udtræk fra GIS
kattegatnord_data <-filter(kattegatnord_data ,Year>=2000)
kattegatnord_data<-subset(kattegatnord_data, Quarter=="1" |Quarter=="4")
kattegatnord_data$haul<-as.numeric(as.factor(with(kattegatnord_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatnord_data2<- kattegatnord_data%>% 
  group_by(haul) %>%
  complete(Species = "Clupea harengus", fill = list(n = 0)) #FILL IN Clupea harengus IN ALL HAULS

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

kattegatnord_herring<-subset(kattegatnord_data2,Species==  "Clupea harengus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatnord_herring1<-kattegatnord_herring %>% select('Year','Quarter','Ship','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatnord_herring2<- kattegatnord_herring1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_herring$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatnord_herring_sum<-kattegatnord_herring2 %>%
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
    return(kattegatnord_herringX<-subset(kattegatnord_herring_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatnord_herringX<-subset(kattegatnord_herring_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatnord_herringX<-subset(kattegatnord_herring_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatnord_herringX<-size_function(size_class,length_min,length_max)
  
  kattegatnord_herringX1<-kattegatnord_herringX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatnord_herringX<-merge(kattegatnord_herringX1,kattegatnord_herringX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatnord_herringX_occ<-kattegatnord_herringX[order(kattegatnord_herringX$haul,kattegatnord_herringX$Country,decreasing = FALSE),] 
  kattegatnord_herringX <-kattegatnord_herringX_occ[!duplicated(kattegatnord_herringX_occ$haul), ]
  
  return(kattegatnord_herringX)
}

#------------------JUVENILES------------------#
kattegatnord_herringJ<-Size_classes_function(1,150,200)
kattegatnord_herringJ$source<-"kattegatnord_herringJ" #add source so it can be subsetted in GAM
count(kattegatnord_herringJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatnord_herringM<-Size_classes_function(2,150,200)
kattegatnord_herringM$source<-"kattegatnord_herringM"
count(kattegatnord_herringM) 

#------------------ADULTS------------------#
kattegatnord_herringA<-Size_classes_function(3,150,200)
kattegatnord_herringA$source<-"kattegatnord_herringA" 
count(kattegatnord_herringA)

###############################################################################
################################     kattegatmidt.  ################################
###############################################################################

kattegatmidt_data <- read_csv("DATRAS_data/kattegatmidt_data.csv")#udtræk fra GIS
kattegatmidt_data <-filter(kattegatmidt_data ,Year>=2000)
kattegatmidt_data<-subset(kattegatmidt_data, Quarter=="1" |Quarter=="4")
kattegatmidt_data$haul<-as.numeric(as.factor(with(kattegatmidt_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatmidt_data2<- kattegatmidt_data%>% 
  group_by(haul) %>%
  complete(Species = "Clupea harengus", fill = list(n = 0)) #FILL IN Clupea harengus IN ALL HAULS

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

kattegatmidt_herring<-subset(kattegatmidt_data2,Species==  "Clupea harengus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatmidt_herring1<-kattegatmidt_herring %>% select('Year','Quarter','Ship','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatmidt_herring2<- kattegatmidt_herring1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_herring$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatmidt_herring_sum<-kattegatmidt_herring2 %>%
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
    return(kattegatmidt_herringX<-subset(kattegatmidt_herring_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatmidt_herringX<-subset(kattegatmidt_herring_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatmidt_herringX<-subset(kattegatmidt_herring_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatmidt_herringX<-size_function(size_class,length_min,length_max)
  
  kattegatmidt_herringX1<-kattegatmidt_herringX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatmidt_herringX<-merge(kattegatmidt_herringX1,kattegatmidt_herringX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatmidt_herringX_occ<-kattegatmidt_herringX[order(kattegatmidt_herringX$haul,kattegatmidt_herringX$Country,decreasing = FALSE),] 
  kattegatmidt_herringX <-kattegatmidt_herringX_occ[!duplicated(kattegatmidt_herringX_occ$haul), ]
  
  return(kattegatmidt_herringX)
}

#------------------JUVENILES------------------#
kattegatmidt_herringJ<-Size_classes_function(1,150,200)
kattegatmidt_herringJ$source<-"kattegatmidt_herringJ" #add source so it can be subsetted in GAM
count(kattegatmidt_herringJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatmidt_herringM<-Size_classes_function(2,150,200)
kattegatmidt_herringM$source<-"kattegatmidt_herringM"
count(kattegatmidt_herringM) 

#------------------ADULTS------------------#
kattegatmidt_herringA<-Size_classes_function(3,150,200)
kattegatmidt_herringA$source<-"kattegatmidt_herringA" 
count(kattegatmidt_herringA)


###############################################################################
################################     kattegatsyd.  ################################
###############################################################################

kattegatsyd_data <- read_csv("DATRAS_data/kattegatsyd_data.csv")#udtræk fra GIS
kattegatsyd_data <-filter(kattegatsyd_data ,Year>=2000)
kattegatsyd_data<-subset(kattegatsyd_data, Quarter=="1" |Quarter=="4")
kattegatsyd_data$haul<-as.numeric(as.factor(with(kattegatsyd_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatsyd_data2<- kattegatsyd_data%>% 
  group_by(haul) %>%
  complete(Species = "Clupea harengus", fill = list(n = 0)) #FILL IN Clupea harengus IN ALL HAULS

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

kattegatsyd_herring<-subset(kattegatsyd_data2,Species==  "Clupea harengus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatsyd_herring1<-kattegatsyd_herring %>% select('Year','Quarter','Ship','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatsyd_herring2<- kattegatsyd_herring1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_herring$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatsyd_herring_sum<-kattegatsyd_herring2 %>%
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
    return(kattegatsyd_herringX<-subset(kattegatsyd_herring_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatsyd_herringX<-subset(kattegatsyd_herring_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatsyd_herringX<-subset(kattegatsyd_herring_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatsyd_herringX<-size_function(size_class,length_min,length_max)
  
  kattegatsyd_herringX1<-kattegatsyd_herringX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatsyd_herringX<-merge(kattegatsyd_herringX1,kattegatsyd_herringX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatsyd_herringX_occ<-kattegatsyd_herringX[order(kattegatsyd_herringX$haul,kattegatsyd_herringX$Country,decreasing = FALSE),] 
  kattegatsyd_herringX <-kattegatsyd_herringX_occ[!duplicated(kattegatsyd_herringX_occ$haul), ]
  
  return(kattegatsyd_herringX)
}

#------------------JUVENILES------------------#
kattegatsyd_herringJ<-Size_classes_function(1,150,200)
kattegatsyd_herringJ$source<-"kattegatsyd_herringJ" #add source so it can be subsetted in GAM
count(kattegatsyd_herringJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatsyd_herringM<-Size_classes_function(2,150,200)
kattegatsyd_herringM$source<-"kattegatsyd_herringM"
count(kattegatsyd_herringM) 

#------------------ADULTS------------------#
kattegatsyd_herringA<-Size_classes_function(3,150,200)
kattegatsyd_herringA$source<-"kattegatsyd_herringA" 
count(kattegatsyd_herringA)

###############################################################################
################################     storebælt.  ################################
###############################################################################

storebælt_data <- read_csv("DATRAS_data/storebælt_data.csv")#udtræk fra GIS
storebælt_data <-filter(storebælt_data ,Year>=2000)
storebælt_data<-subset(storebælt_data, Quarter=="1" |Quarter=="4")
storebælt_data$haul<-as.numeric(as.factor(with(storebælt_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

storebælt_data2<- storebælt_data%>% 
  group_by(haul) %>%
  complete(Species = "Clupea harengus", fill = list(n = 0)) #FILL IN Clupea harengus IN ALL HAULS

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

storebælt_herring<-subset(storebælt_data2,Species==  "Clupea harengus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
storebælt_herring1<-storebælt_herring %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
storebælt_herring2<- storebælt_herring1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_herring$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

storebælt_herring_sum<-storebælt_herring2 %>%
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
    return(storebælt_herringX<-subset(storebælt_herring_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(storebælt_herringX<-subset(storebælt_herring_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(storebælt_herringX<-subset(storebælt_herring_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  storebælt_herringX<-size_function(size_class,length_min,length_max)
  
  storebælt_herringX1<-storebælt_herringX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  storebælt_herringX<-merge(storebælt_herringX1,storebælt_herringX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  storebælt_herringX_occ<-storebælt_herringX[order(storebælt_herringX$haul,storebælt_herringX$Country,decreasing = FALSE),] 
  storebælt_herringX <-storebælt_herringX_occ[!duplicated(storebælt_herringX_occ$haul), ]
  
  return(storebælt_herringX)
}

#------------------JUVENILES------------------#
storebælt_herringJ<-Size_classes_function(1,150,200)
storebælt_herringJ$source<-"storebælt_herringJ" #add source so it can be subsetted in GAM
count(storebælt_herringJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
storebælt_herringM<-Size_classes_function(2,150,200)
storebælt_herringM$source<-"storebælt_herringM"
count(storebælt_herringM) 

#------------------ADULTS------------------#
storebælt_herringA<-Size_classes_function(3,150,200)
storebælt_herringA$source<-"storebælt_herringA" 
count(storebælt_herringA)

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
  complete(Species = "Clupea harengus", fill = list(n = 0)) #FILL IN Clupea harengus IN ALL HAULS

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

femeren_herring<-subset(femeren_data2,Species==  "Clupea harengus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
femeren_herring1<-femeren_herring %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
femeren_herring2<- femeren_herring1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_herring$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

femeren_herring_sum<-femeren_herring2 %>%
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
    return(femeren_herringX<-subset(femeren_herring_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(femeren_herringX<-subset(femeren_herring_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(femeren_herringX<-subset(femeren_herring_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  femeren_herringX<-size_function(size_class,length_min,length_max)
  
  femeren_herringX1<-femeren_herringX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  femeren_herringX<-merge(femeren_herringX1,femeren_herringX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  femeren_herringX_occ<-femeren_herringX[order(femeren_herringX$haul,femeren_herringX$Country,decreasing = FALSE),] 
  femeren_herringX <-femeren_herringX_occ[!duplicated(femeren_herringX_occ$haul), ]
  
  return(femeren_herringX)
}

#------------------JUVENILES------------------#
femeren_herringJ<-Size_classes_function(1,150,200)
femeren_herringJ$source<-"femeren_herringJ" #add source so it can be subsetted in GAM
count(femeren_herringJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
femeren_herringM<-Size_classes_function(2,150,200)
femeren_herringM$source<-"femeren_herringM"
count(femeren_herringM) 

#------------------ADULTS------------------#
femeren_herringA<-Size_classes_function(3,150,200)
femeren_herringA$source<-"femeren_herringA" 
count(femeren_herringA)

###############################################################################
################################     østersøen.  ################################
###############################################################################

østersøen_data <- read_csv("DATRAS_data/østersøen_data.csv")#udtræk fra GIS
østersøen_data <-filter(østersøen_data ,Year>=2000)
østersøen_data<-subset(østersøen_data, Quarter=="1" |Quarter=="4")
østersøen_data$haul<-as.numeric(as.factor(with(østersøen_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

østersøen_data2<- østersøen_data%>% 
  group_by(haul) %>%
  complete(Species = "Clupea harengus", fill = list(n = 0)) #FILL IN Clupea harengus IN ALL HAULS

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

østersøen_herring<-subset(østersøen_data2,Species==  "Clupea harengus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
østersøen_herring1<-østersøen_herring %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
østersøen_herring2<- østersøen_herring1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_herring$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

østersøen_herring_sum<-østersøen_herring2 %>%
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
    return(østersøen_herringX<-subset(østersøen_herring_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(østersøen_herringX<-subset(østersøen_herring_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(østersøen_herringX<-subset(østersøen_herring_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  østersøen_herringX<-size_function(size_class,length_min,length_max)
  
  østersøen_herringX1<-østersøen_herringX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  østersøen_herringX<-merge(østersøen_herringX1,østersøen_herringX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  østersøen_herringX_occ<-østersøen_herringX[order(østersøen_herringX$haul,østersøen_herringX$Country,decreasing = FALSE),] 
  østersøen_herringX <-østersøen_herringX_occ[!duplicated(østersøen_herringX_occ$haul), ]
  
  return(østersøen_herringX)
}

#------------------JUVENILES------------------#
østersøen_herringJ<-Size_classes_function(1,150,200)
østersøen_herringJ$source<-"østersøen_herringJ" #add source so it can be subsetted in GAM
count(østersøen_herringJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
østersøen_herringM<-Size_classes_function(2,150,200)
østersøen_herringM$source<-"østersøen_herringM"
count(østersøen_herringM) 

#------------------ADULTS------------------#
østersøen_herringA<-Size_classes_function(3,150,200)
østersøen_herringA$source<-"østersøen_herringA" 
count(østersøen_herringA)

###############################################################################
########################  SAVE EVERYTHING FOR GAM #############################
###############################################################################

save(øresund_herringJ,øresund_herringM,øresund_herringA,kattegatnord_herringA,
     kattegatnord_herringM ,kattegatnord_herringJ,kattegatmidt_herringA,
     kattegatmidt_herringM,kattegatmidt_herringJ,kattegatsyd_herringA,kattegatsyd_herringM,
     kattegatsyd_herringJ,femeren_herringA,femeren_herringM,femeren_herringJ,storebælt_herringA,storebælt_herringM,storebælt_herringJ
     ,østersøen_herringA,østersøen_herringJ,østersøen_herringM,file = "Area_herringAges.RData")


herring_all_combines<-vctrs::vec_c(øresund_herringJ,øresund_herringM,øresund_herringA,kattegatnord_herringA,
                                   kattegatnord_herringM ,kattegatnord_herringJ,kattegatmidt_herringA,
                                   kattegatmidt_herringM,kattegatmidt_herringJ,kattegatsyd_herringA,kattegatsyd_herringM,
                                   kattegatsyd_herringJ,femeren_herringA,femeren_herringM,femeren_herringJ,
                                   storebælt_herringA,storebælt_herringM,storebælt_herringJ,
                                   østersøen_herringA,østersøen_herringJ,østersøen_herringM,.name_spec = NULL)

herring_all_combines$source<-as.factor(herring_all_combines$source)
dat_herring=herring_all_combines
dat_herring$POSIXyear<-as.POSIXct(dat_herring$DateTime,format='%d/%m/%Y')
dat_herring$numyear <- as.numeric(format(dat_herring$POSIXyear, format = "%Y")) + as.numeric(format(dat_herring$POSIXyear, format = "%m"))/12 + as.numeric(format(dat_herring$POSIXyear, format = "%d"))/365
dat_herring<-dat_herring%>%
  rename(
    cpue=sum,
    time=numyear, 
    depth=Depth,
    loc=source
  )


write.csv(dat_herring,"dat_herring.csv")
