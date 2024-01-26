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
  complete(Species = "Platichthys flesus", fill = list(n = 0)) #FILL IN Platichthys flesus IN ALL HAULS

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

øresund_flounder<-subset(øresund_data2,Species==  "Platichthys flesus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
øresund_flounder1<-øresund_flounder %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
øresund_flounder2<- øresund_flounder1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_flounder$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

øresund_flounder_sum<-øresund_flounder2 %>%
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
    return(øresund_flounderX<-subset(øresund_flounder_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(øresund_flounderX<-subset(øresund_flounder_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(øresund_flounderX<-subset(øresund_flounder_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  øresund_flounderX<-size_function(size_class,length_min,length_max)
  
  øresund_flounderX1<-øresund_flounderX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  øresund_flounderX<-merge(øresund_flounderX1,øresund_flounderX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  øresund_flounderX_occ<-øresund_flounderX[order(øresund_flounderX$haul,øresund_flounderX$Country,decreasing = FALSE),] 
  øresund_flounderX <-øresund_flounderX_occ[!duplicated(øresund_flounderX_occ$haul), ]
  
  return(øresund_flounderX)
}

#------------------JUVENILES------------------#
øresund_flounderJ<-Size_classes_function(1,150,200)
øresund_flounderJ$source<-"øresund_flounderJ" #add source so it can be subsetted in GAM
count(øresund_flounderJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
øresund_flounderM<-Size_classes_function(2,150,200)
øresund_flounderM$source<-"øresund_flounderM"
count(øresund_flounderM) 

#------------------ADULTS------------------#
øresund_flounderA<-Size_classes_function(3,150,200)
øresund_flounderA$source<-"øresund_flounderA" 
count(øresund_flounderA)

###############################################################################
################################     kattegatnord.  ################################
###############################################################################

kattegatnord_data <- read_csv("DATRAS_data/kattegatnord_data.csv")#udtræk fra GIS
kattegatnord_data <-filter(kattegatnord_data ,Year>=2000)

kattegatnord_data<-subset(kattegatnord_data, Quarter=="1" |Quarter=="4")
kattegatnord_data$haul<-as.numeric(as.factor(with(kattegatnord_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatnord_data2<- kattegatnord_data%>% 
  group_by(haul) %>%
  complete(Species = "Platichthys flesus", fill = list(n = 0)) #FILL IN Platichthys flesus IN ALL HAULS

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

kattegatnord_flounder<-subset(kattegatnord_data2,Species==  "Platichthys flesus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatnord_flounder1<-kattegatnord_flounder %>% select('Year','Quarter','Ship','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatnord_flounder2<- kattegatnord_flounder1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_flounder$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatnord_flounder_sum<-kattegatnord_flounder2 %>%
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
    return(kattegatnord_flounderX<-subset(kattegatnord_flounder_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatnord_flounderX<-subset(kattegatnord_flounder_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatnord_flounderX<-subset(kattegatnord_flounder_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatnord_flounderX<-size_function(size_class,length_min,length_max)
  
  kattegatnord_flounderX1<-kattegatnord_flounderX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatnord_flounderX<-merge(kattegatnord_flounderX1,kattegatnord_flounderX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatnord_flounderX_occ<-kattegatnord_flounderX[order(kattegatnord_flounderX$haul,kattegatnord_flounderX$Country,decreasing = FALSE),] 
  kattegatnord_flounderX <-kattegatnord_flounderX_occ[!duplicated(kattegatnord_flounderX_occ$haul), ]
  
  return(kattegatnord_flounderX)
}

#------------------JUVENILES------------------#
kattegatnord_flounderJ<-Size_classes_function(1,150,200)
kattegatnord_flounderJ$source<-"kattegatnord_flounderJ" #add source so it can be subsetted in GAM
count(kattegatnord_flounderJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatnord_flounderM<-Size_classes_function(2,150,200)
kattegatnord_flounderM$source<-"kattegatnord_flounderM"
count(kattegatnord_flounderM) 

#------------------ADULTS------------------#
kattegatnord_flounderA<-Size_classes_function(3,150,200)
kattegatnord_flounderA$source<-"kattegatnord_flounderA" 
count(kattegatnord_flounderA)

###############################################################################
################################     kattegatmidt.  ################################
###############################################################################

kattegatmidt_data <- read_csv("DATRAS_data/kattegatmidt_data.csv")#udtræk fra GIS
kattegatmidt_data <-filter(kattegatmidt_data ,Year>=2000)
kattegatmidt_data<-subset(kattegatmidt_data, Quarter=="1" |Quarter=="4")
kattegatmidt_data$haul<-as.numeric(as.factor(with(kattegatmidt_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatmidt_data2<- kattegatmidt_data%>% 
  group_by(haul) %>%
  complete(Species = "Platichthys flesus", fill = list(n = 0)) #FILL IN Platichthys flesus IN ALL HAULS

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

kattegatmidt_flounder<-subset(kattegatmidt_data2,Species==  "Platichthys flesus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatmidt_flounder1<-kattegatmidt_flounder %>% select('Year','Quarter','Ship','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatmidt_flounder2<- kattegatmidt_flounder1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_flounder$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatmidt_flounder_sum<-kattegatmidt_flounder2 %>%
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
    return(kattegatmidt_flounderX<-subset(kattegatmidt_flounder_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatmidt_flounderX<-subset(kattegatmidt_flounder_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatmidt_flounderX<-subset(kattegatmidt_flounder_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatmidt_flounderX<-size_function(size_class,length_min,length_max)
  
  kattegatmidt_flounderX1<-kattegatmidt_flounderX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatmidt_flounderX<-merge(kattegatmidt_flounderX1,kattegatmidt_flounderX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatmidt_flounderX_occ<-kattegatmidt_flounderX[order(kattegatmidt_flounderX$haul,kattegatmidt_flounderX$Country,decreasing = FALSE),] 
  kattegatmidt_flounderX <-kattegatmidt_flounderX_occ[!duplicated(kattegatmidt_flounderX_occ$haul), ]
  
  return(kattegatmidt_flounderX)
}

#------------------JUVENILES------------------#
kattegatmidt_flounderJ<-Size_classes_function(1,150,200)
kattegatmidt_flounderJ$source<-"kattegatmidt_flounderJ" #add source so it can be subsetted in GAM
count(kattegatmidt_flounderJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatmidt_flounderM<-Size_classes_function(2,150,200)
kattegatmidt_flounderM$source<-"kattegatmidt_flounderM"
count(kattegatmidt_flounderM) 

#------------------ADULTS------------------#
kattegatmidt_flounderA<-Size_classes_function(3,150,200)
kattegatmidt_flounderA$source<-"kattegatmidt_flounderA" 
count(kattegatmidt_flounderA)


###############################################################################
################################     kattegatsyd.  ################################
###############################################################################

kattegatsyd_data <- read_csv("DATRAS_data/kattegatsyd_data.csv")#udtræk fra GIS
kattegatsyd_data <-filter(kattegatsyd_data ,Year>=2000)
kattegatsyd_data<-subset(kattegatsyd_data, Quarter=="1" |Quarter=="4")
kattegatsyd_data$haul<-as.numeric(as.factor(with(kattegatsyd_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatsyd_data2<- kattegatsyd_data%>% 
  group_by(haul) %>%
  complete(Species = "Platichthys flesus", fill = list(n = 0)) #FILL IN Platichthys flesus IN ALL HAULS

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

kattegatsyd_flounder<-subset(kattegatsyd_data2,Species==  "Platichthys flesus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatsyd_flounder1<-kattegatsyd_flounder %>% select('Year','Quarter','Ship','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatsyd_flounder2<- kattegatsyd_flounder1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_flounder$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatsyd_flounder_sum<-kattegatsyd_flounder2 %>%
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
    return(kattegatsyd_flounderX<-subset(kattegatsyd_flounder_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatsyd_flounderX<-subset(kattegatsyd_flounder_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatsyd_flounderX<-subset(kattegatsyd_flounder_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatsyd_flounderX<-size_function(size_class,length_min,length_max)
  
  kattegatsyd_flounderX1<-kattegatsyd_flounderX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatsyd_flounderX<-merge(kattegatsyd_flounderX1,kattegatsyd_flounderX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatsyd_flounderX_occ<-kattegatsyd_flounderX[order(kattegatsyd_flounderX$haul,kattegatsyd_flounderX$Country,decreasing = FALSE),] 
  kattegatsyd_flounderX <-kattegatsyd_flounderX_occ[!duplicated(kattegatsyd_flounderX_occ$haul), ]
  
  return(kattegatsyd_flounderX)
}

#------------------JUVENILES------------------#
kattegatsyd_flounderJ<-Size_classes_function(1,150,200)
kattegatsyd_flounderJ$source<-"kattegatsyd_flounderJ" #add source so it can be subsetted in GAM
count(kattegatsyd_flounderJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatsyd_flounderM<-Size_classes_function(2,150,200)
kattegatsyd_flounderM$source<-"kattegatsyd_flounderM"
count(kattegatsyd_flounderM) 

#------------------ADULTS------------------#
kattegatsyd_flounderA<-Size_classes_function(3,150,200)
kattegatsyd_flounderA$source<-"kattegatsyd_flounderA" 
count(kattegatsyd_flounderA)

###############################################################################
################################     storebælt.  ################################
###############################################################################

storebælt_data <- read_csv("DATRAS_data/storebælt_data.csv")#udtræk fra GIS
storebælt_data <-filter(storebælt_data ,Year>=2000)
storebælt_data<-subset(storebælt_data, Quarter=="1" |Quarter=="4")
storebælt_data$haul<-as.numeric(as.factor(with(storebælt_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

storebælt_data2<- storebælt_data%>% 
  group_by(haul) %>%
  complete(Species = "Platichthys flesus", fill = list(n = 0)) #FILL IN Platichthys flesus IN ALL HAULS

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

storebælt_flounder<-subset(storebælt_data2,Species==  "Platichthys flesus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
storebælt_flounder1<-storebælt_flounder %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
storebælt_flounder2<- storebælt_flounder1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_flounder$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

storebælt_flounder_sum<-storebælt_flounder2 %>%
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
    return(storebælt_flounderX<-subset(storebælt_flounder_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(storebælt_flounderX<-subset(storebælt_flounder_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(storebælt_flounderX<-subset(storebælt_flounder_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  storebælt_flounderX<-size_function(size_class,length_min,length_max)
  
  storebælt_flounderX1<-storebælt_flounderX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  storebælt_flounderX<-merge(storebælt_flounderX1,storebælt_flounderX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  storebælt_flounderX_occ<-storebælt_flounderX[order(storebælt_flounderX$haul,storebælt_flounderX$Country,decreasing = FALSE),] 
  storebælt_flounderX <-storebælt_flounderX_occ[!duplicated(storebælt_flounderX_occ$haul), ]
  
  return(storebælt_flounderX)
}

#------------------JUVENILES------------------#
storebælt_flounderJ<-Size_classes_function(1,150,200)
storebælt_flounderJ$source<-"storebælt_flounderJ" #add source so it can be subsetted in GAM
count(storebælt_flounderJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
storebælt_flounderM<-Size_classes_function(2,150,200)
storebælt_flounderM$source<-"storebælt_flounderM"
count(storebælt_flounderM) 

#------------------ADULTS------------------#
storebælt_flounderA<-Size_classes_function(3,150,200)
storebælt_flounderA$source<-"storebælt_flounderA" 
count(storebælt_flounderA)

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
  complete(Species = "Platichthys flesus", fill = list(n = 0)) #FILL IN Platichthys flesus IN ALL HAULS

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

femeren_flounder<-subset(femeren_data2,Species==  "Platichthys flesus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
femeren_flounder1<-femeren_flounder %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
femeren_flounder2<- femeren_flounder1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_flounder$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

femeren_flounder_sum<-femeren_flounder2 %>%
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
    return(femeren_flounderX<-subset(femeren_flounder_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(femeren_flounderX<-subset(femeren_flounder_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(femeren_flounderX<-subset(femeren_flounder_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  femeren_flounderX<-size_function(size_class,length_min,length_max)
  
  femeren_flounderX1<-femeren_flounderX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  femeren_flounderX<-merge(femeren_flounderX1,femeren_flounderX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  femeren_flounderX_occ<-femeren_flounderX[order(femeren_flounderX$haul,femeren_flounderX$Country,decreasing = FALSE),] 
  femeren_flounderX <-femeren_flounderX_occ[!duplicated(femeren_flounderX_occ$haul), ]
  
  return(femeren_flounderX)
}

#------------------JUVENILES------------------#
femeren_flounderJ<-Size_classes_function(1,150,200)
femeren_flounderJ$source<-"femeren_flounderJ" #add source so it can be subsetted in GAM
count(femeren_flounderJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
femeren_flounderM<-Size_classes_function(2,150,200)
femeren_flounderM$source<-"femeren_flounderM"
count(femeren_flounderM) 

#------------------ADULTS------------------#
femeren_flounderA<-Size_classes_function(3,150,200)
femeren_flounderA$source<-"femeren_flounderA" 
count(femeren_flounderA)

###############################################################################
################################     østersøen.  ################################
###############################################################################

østersøen_data <- read_csv("DATRAS_data/østersøen_data.csv")#udtræk fra GIS
østersøen_data <-filter(østersøen_data ,Year>=2000)
østersøen_data<-subset(østersøen_data, Quarter=="1" |Quarter=="4")
østersøen_data$haul<-as.numeric(as.factor(with(østersøen_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

østersøen_data2<- østersøen_data%>% 
  group_by(haul) %>%
  complete(Species = "Platichthys flesus", fill = list(n = 0)) #FILL IN Platichthys flesus IN ALL HAULS

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

østersøen_flounder<-subset(østersøen_data2,Species==  "Platichthys flesus") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
østersøen_flounder1<-østersøen_flounder %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
østersøen_flounder2<- østersøen_flounder1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_flounder$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

østersøen_flounder_sum<-østersøen_flounder2 %>%
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
    return(østersøen_flounderX<-subset(østersøen_flounder_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(østersøen_flounderX<-subset(østersøen_flounder_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(østersøen_flounderX<-subset(østersøen_flounder_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  østersøen_flounderX<-size_function(size_class,length_min,length_max)
  
  østersøen_flounderX1<-østersøen_flounderX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  østersøen_flounderX<-merge(østersøen_flounderX1,østersøen_flounderX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  østersøen_flounderX_occ<-østersøen_flounderX[order(østersøen_flounderX$haul,østersøen_flounderX$Country,decreasing = FALSE),] 
  østersøen_flounderX <-østersøen_flounderX_occ[!duplicated(østersøen_flounderX_occ$haul), ]
  
  return(østersøen_flounderX)
}

#------------------JUVENILES------------------#
østersøen_flounderJ<-Size_classes_function(1,150,200)
østersøen_flounderJ$source<-"østersøen_flounderJ" #add source so it can be subsetted in GAM
count(østersøen_flounderJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
østersøen_flounderM<-Size_classes_function(2,150,200)
østersøen_flounderM$source<-"østersøen_flounderM"
count(østersøen_flounderM) 

#------------------ADULTS------------------#
østersøen_flounderA<-Size_classes_function(3,150,200)
østersøen_flounderA$source<-"østersøen_flounderA" 
count(østersøen_flounderA)

###############################################################################
########################  SAVE EVERYTHING FOR GAM #############################
###############################################################################

save(øresund_flounderJ,øresund_flounderM,øresund_flounderA,kattegatnord_flounderA,
     kattegatnord_flounderM ,kattegatnord_flounderJ,kattegatmidt_flounderA,
     kattegatmidt_flounderM,kattegatmidt_flounderJ,kattegatsyd_flounderA,kattegatsyd_flounderM,
     kattegatsyd_flounderJ,femeren_flounderA,femeren_flounderM,femeren_flounderJ,storebælt_flounderA,storebælt_flounderM,storebælt_flounderJ
     ,østersøen_flounderA,østersøen_flounderJ,østersøen_flounderM,file = "Area_flounderAges.RData")


flounder_all_combines<-vctrs::vec_c(øresund_flounderJ,øresund_flounderM,øresund_flounderA,kattegatnord_flounderA,
                                    kattegatnord_flounderM ,kattegatnord_flounderJ,kattegatmidt_flounderA,
                                    kattegatmidt_flounderM,kattegatmidt_flounderJ,kattegatsyd_flounderA,kattegatsyd_flounderM,
                                    kattegatsyd_flounderJ,femeren_flounderA,femeren_flounderM,femeren_flounderJ,
                                    storebælt_flounderA,storebælt_flounderM,storebælt_flounderJ,
                                    østersøen_flounderA,østersøen_flounderJ,østersøen_flounderM,.name_spec = NULL)

flounder_all_combines$source<-as.factor(flounder_all_combines$source)
dat_flounder=flounder_all_combines
save(dat_flounder,file="Areas_flounder_combined.Rdata")

dat_flounder<-subset(dat_flounder, Quarter=="1" |Quarter=="4")
dat_flounder$POSIXyear<-as.POSIXct(dat_flounder$DateTime,format='%d/%m/%Y')
dat_flounder$numyear <- as.numeric(format(dat_flounder$POSIXyear, format = "%Y")) + as.numeric(format(dat_flounder$POSIXyear, format = "%m"))/12 + as.numeric(format(dat_flounder$POSIXyear, format = "%d"))/365

dat_flounder<-dat_flounder%>%
  rename(
    cpue=sum,
    time=numyear, 
    depth=Depth,
    loc=source
  )

write.csv(dat_flounder,"dat_flounder.csv")