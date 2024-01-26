#libraries
library(readr)
library(tidyverse)
library(dplyr)


load("species_weight.Rdata")

###############################################################################
################################     ØRESUND  ################################
###############################################################################

øresund_data <- read_csv("DATRAS_data/øresund_data.csv")#udtræk fra GIS
øresund_data <-filter(øresund_data ,Year>=2000)
øresund_data<-subset(øresund_data, Quarter=="1" |Quarter=="4")
øresund_data$haul<-as.numeric(as.factor(with(øresund_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

øresund_data2<- øresund_data%>% 
  group_by(haul) %>%
  complete(Species = "Pleuronectes platessa", fill = list(n = 0)) #FILL IN Pleuronectes platessa IN ALL HAULS

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

øresund_plaice<-subset(øresund_data2,Species==  "Pleuronectes platessa") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
øresund_plaice1<-øresund_plaice %>% select('Year','Quarter','Ship','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
øresund_plaice2<- øresund_plaice1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_plaice$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

øresund_plaice_sum<-øresund_plaice2 %>%
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
    return(øresund_plaiceX<-subset(øresund_plaice_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(øresund_plaiceX<-subset(øresund_plaice_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(øresund_plaiceX<-subset(øresund_plaice_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  øresund_plaiceX<-size_function(size_class,length_min,length_max)
  
  øresund_plaiceX1<-øresund_plaiceX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  øresund_plaiceX<-merge(øresund_plaiceX1,øresund_plaiceX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  øresund_plaiceX_occ<-øresund_plaiceX[order(øresund_plaiceX$haul,øresund_plaiceX$Country,decreasing = FALSE),] 
  øresund_plaiceX <-øresund_plaiceX_occ[!duplicated(øresund_plaiceX_occ$haul), ]
  
  return(øresund_plaiceX)
}

#------------------JUVENILES------------------#
øresund_plaiceJ<-Size_classes_function(1,150,200)
øresund_plaiceJ$source<-"øresund_plaiceJ" #add source so it can be subsetted in GAM
count(øresund_plaiceJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
øresund_plaiceM<-Size_classes_function(2,150,200)
øresund_plaiceM$source<-"øresund_plaiceM"
count(øresund_plaiceM) 

#------------------ADULTS------------------#
øresund_plaiceA<-Size_classes_function(3,150,200)
øresund_plaiceA$source<-"øresund_plaiceA" 
count(øresund_plaiceA)

###############################################################################
################################     kattegatnord.  ################################
###############################################################################

kattegatnord_data <- read_csv("DATRAS_data/kattegatnord_data.csv")#udtræk fra GIS
kattegatnord_data <-filter(kattegatnord_data ,Year>=2000)
kattegatnord_data<-subset(kattegatnord_data, Quarter=="1" |Quarter=="4")
kattegatnord_data$haul<-as.numeric(as.factor(with(kattegatnord_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatnord_data2<- kattegatnord_data%>% 
  group_by(haul) %>%
  complete(Species = "Pleuronectes platessa", fill = list(n = 0)) #FILL IN Pleuronectes platessa IN ALL HAULS

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

kattegatnord_plaice<-subset(kattegatnord_data2,Species==  "Pleuronectes platessa") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatnord_plaice1<-kattegatnord_plaice %>% select('Year','Quarter','Ship','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatnord_plaice2<- kattegatnord_plaice1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_plaice$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatnord_plaice_sum<-kattegatnord_plaice2 %>%
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
    return(kattegatnord_plaiceX<-subset(kattegatnord_plaice_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatnord_plaiceX<-subset(kattegatnord_plaice_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatnord_plaiceX<-subset(kattegatnord_plaice_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatnord_plaiceX<-size_function(size_class,length_min,length_max)
  
  kattegatnord_plaiceX1<-kattegatnord_plaiceX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatnord_plaiceX<-merge(kattegatnord_plaiceX1,kattegatnord_plaiceX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatnord_plaiceX_occ<-kattegatnord_plaiceX[order(kattegatnord_plaiceX$haul,kattegatnord_plaiceX$Country,decreasing = FALSE),] 
  kattegatnord_plaiceX <-kattegatnord_plaiceX_occ[!duplicated(kattegatnord_plaiceX_occ$haul), ]
  
  return(kattegatnord_plaiceX)
}

#------------------JUVENILES------------------#
kattegatnord_plaiceJ<-Size_classes_function(1,150,200)
kattegatnord_plaiceJ$source<-"kattegatnord_plaiceJ" #add source so it can be subsetted in GAM
count(kattegatnord_plaiceJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatnord_plaiceM<-Size_classes_function(2,150,200)
kattegatnord_plaiceM$source<-"kattegatnord_plaiceM"
count(kattegatnord_plaiceM) 

#------------------ADULTS------------------#
kattegatnord_plaiceA<-Size_classes_function(3,150,200)
kattegatnord_plaiceA$source<-"kattegatnord_plaiceA" 
count(kattegatnord_plaiceA)

###############################################################################
################################     kattegatmidt.  ################################
###############################################################################

kattegatmidt_data <- read_csv("DATRAS_data/kattegatmidt_data.csv")#udtræk fra GIS
kattegatmidt_data <-filter(kattegatmidt_data ,Year>=2000)
kattegatmidt_data<-subset(kattegatmidt_data, Quarter=="1" |Quarter=="4")
kattegatmidt_data$haul<-as.numeric(as.factor(with(kattegatmidt_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatmidt_data2<- kattegatmidt_data%>% 
  group_by(haul) %>%
  complete(Species = "Pleuronectes platessa", fill = list(n = 0)) #FILL IN Pleuronectes platessa IN ALL HAULS

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

kattegatmidt_plaice<-subset(kattegatmidt_data2,Species==  "Pleuronectes platessa") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatmidt_plaice1<-kattegatmidt_plaice %>% select('Year','Quarter','Ship','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatmidt_plaice2<- kattegatmidt_plaice1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_plaice$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatmidt_plaice_sum<-kattegatmidt_plaice2 %>%
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
    return(kattegatmidt_plaiceX<-subset(kattegatmidt_plaice_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatmidt_plaiceX<-subset(kattegatmidt_plaice_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatmidt_plaiceX<-subset(kattegatmidt_plaice_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatmidt_plaiceX<-size_function(size_class,length_min,length_max)
  
  kattegatmidt_plaiceX1<-kattegatmidt_plaiceX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatmidt_plaiceX<-merge(kattegatmidt_plaiceX1,kattegatmidt_plaiceX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatmidt_plaiceX_occ<-kattegatmidt_plaiceX[order(kattegatmidt_plaiceX$haul,kattegatmidt_plaiceX$Country,decreasing = FALSE),] 
  kattegatmidt_plaiceX <-kattegatmidt_plaiceX_occ[!duplicated(kattegatmidt_plaiceX_occ$haul), ]
  
  return(kattegatmidt_plaiceX)
}

#------------------JUVENILES------------------#
kattegatmidt_plaiceJ<-Size_classes_function(1,150,200)
kattegatmidt_plaiceJ$source<-"kattegatmidt_plaiceJ" #add source so it can be subsetted in GAM
count(kattegatmidt_plaiceJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatmidt_plaiceM<-Size_classes_function(2,150,200)
kattegatmidt_plaiceM$source<-"kattegatmidt_plaiceM"
count(kattegatmidt_plaiceM) 

#------------------ADULTS------------------#
kattegatmidt_plaiceA<-Size_classes_function(3,150,200)
kattegatmidt_plaiceA$source<-"kattegatmidt_plaiceA" 
count(kattegatmidt_plaiceA)


###############################################################################
################################     kattegatsyd.  ################################
###############################################################################

kattegatsyd_data <- read_csv("DATRAS_data/kattegatsyd_data.csv")#udtræk fra GIS
kattegatsyd_data <-filter(kattegatsyd_data ,Year>=2000)
kattegatsyd_data<-subset(kattegatsyd_data, Quarter=="1" |Quarter=="4")
kattegatsyd_data$haul<-as.numeric(as.factor(with(kattegatsyd_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatsyd_data2<- kattegatsyd_data%>% 
  group_by(haul) %>%
  complete(Species = "Pleuronectes platessa", fill = list(n = 0)) #FILL IN Pleuronectes platessa IN ALL HAULS

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

kattegatsyd_plaice<-subset(kattegatsyd_data2,Species==  "Pleuronectes platessa") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatsyd_plaice1<-kattegatsyd_plaice %>% select('Year','Quarter','Ship','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatsyd_plaice2<- kattegatsyd_plaice1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_plaice$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatsyd_plaice_sum<-kattegatsyd_plaice2 %>%
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
    return(kattegatsyd_plaiceX<-subset(kattegatsyd_plaice_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatsyd_plaiceX<-subset(kattegatsyd_plaice_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatsyd_plaiceX<-subset(kattegatsyd_plaice_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatsyd_plaiceX<-size_function(size_class,length_min,length_max)
  
  kattegatsyd_plaiceX1<-kattegatsyd_plaiceX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatsyd_plaiceX<-merge(kattegatsyd_plaiceX1,kattegatsyd_plaiceX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatsyd_plaiceX_occ<-kattegatsyd_plaiceX[order(kattegatsyd_plaiceX$haul,kattegatsyd_plaiceX$Country,decreasing = FALSE),] 
  kattegatsyd_plaiceX <-kattegatsyd_plaiceX_occ[!duplicated(kattegatsyd_plaiceX_occ$haul), ]
  
  return(kattegatsyd_plaiceX)
}

#------------------JUVENILES------------------#
kattegatsyd_plaiceJ<-Size_classes_function(1,150,200)
kattegatsyd_plaiceJ$source<-"kattegatsyd_plaiceJ" #add source so it can be subsetted in GAM
count(kattegatsyd_plaiceJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatsyd_plaiceM<-Size_classes_function(2,150,200)
kattegatsyd_plaiceM$source<-"kattegatsyd_plaiceM"
count(kattegatsyd_plaiceM) 

#------------------ADULTS------------------#
kattegatsyd_plaiceA<-Size_classes_function(3,150,200)
kattegatsyd_plaiceA$source<-"kattegatsyd_plaiceA" 
count(kattegatsyd_plaiceA)

###############################################################################
################################     storebælt.  ################################
###############################################################################

storebælt_data <- read_csv("DATRAS_data/storebælt_data.csv")#udtræk fra GIS
storebælt_data <-filter(storebælt_data ,Year>=2000)
storebælt_data<-subset(storebælt_data, Quarter=="1" |Quarter=="4")
storebælt_data$haul<-as.numeric(as.factor(with(storebælt_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

storebælt_data2<- storebælt_data%>% 
  group_by(haul) %>%
  complete(Species = "Pleuronectes platessa", fill = list(n = 0)) #FILL IN Pleuronectes platessa IN ALL HAULS

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

storebælt_plaice<-subset(storebælt_data2,Species==  "Pleuronectes platessa") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
storebælt_plaice1<-storebælt_plaice %>% select('Year','Quarter','Ship','Species','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
storebælt_plaice2<- storebælt_plaice1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_plaice$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

storebælt_plaice_sum<-storebælt_plaice2 %>%
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
    return(storebælt_plaiceX<-subset(storebælt_plaice_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(storebælt_plaiceX<-subset(storebælt_plaice_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(storebælt_plaiceX<-subset(storebælt_plaice_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  storebælt_plaiceX<-size_function(size_class,length_min,length_max)
  
  storebælt_plaiceX1<-storebælt_plaiceX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  storebælt_plaiceX<-merge(storebælt_plaiceX1,storebælt_plaiceX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  storebælt_plaiceX_occ<-storebælt_plaiceX[order(storebælt_plaiceX$haul,storebælt_plaiceX$Country,decreasing = FALSE),] 
  storebælt_plaiceX <-storebælt_plaiceX_occ[!duplicated(storebælt_plaiceX_occ$haul), ]
  
  return(storebælt_plaiceX)
}

#------------------JUVENILES------------------#
storebælt_plaiceJ<-Size_classes_function(1,150,200)
storebælt_plaiceJ$source<-"storebælt_plaiceJ" #add source so it can be subsetted in GAM
count(storebælt_plaiceJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
storebælt_plaiceM<-Size_classes_function(2,150,200)
storebælt_plaiceM$source<-"storebælt_plaiceM"
count(storebælt_plaiceM) 

#------------------ADULTS------------------#
storebælt_plaiceA<-Size_classes_function(3,150,200)
storebælt_plaiceA$source<-"storebælt_plaiceA" 
count(storebælt_plaiceA)

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
  complete(Species = "Pleuronectes platessa", fill = list(n = 0)) #FILL IN Pleuronectes platessa IN ALL HAULS

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

femeren_plaice<-subset(femeren_data2,Species==  "Pleuronectes platessa") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
femeren_plaice1<-femeren_plaice %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
femeren_plaice2<- femeren_plaice1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_plaice$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

femeren_plaice_sum<-femeren_plaice2 %>%
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
    return(femeren_plaiceX<-subset(femeren_plaice_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(femeren_plaiceX<-subset(femeren_plaice_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(femeren_plaiceX<-subset(femeren_plaice_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  femeren_plaiceX<-size_function(size_class,length_min,length_max)
  
  femeren_plaiceX1<-femeren_plaiceX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  femeren_plaiceX<-merge(femeren_plaiceX1,femeren_plaiceX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  femeren_plaiceX_occ<-femeren_plaiceX[order(femeren_plaiceX$haul,femeren_plaiceX$Country,decreasing = FALSE),] 
  femeren_plaiceX <-femeren_plaiceX_occ[!duplicated(femeren_plaiceX_occ$haul), ]
  
  return(femeren_plaiceX)
}

#------------------JUVENILES------------------#
femeren_plaiceJ<-Size_classes_function(1,150,200)
femeren_plaiceJ$source<-"femeren_plaiceJ" #add source so it can be subsetted in GAM
count(femeren_plaiceJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
femeren_plaiceM<-Size_classes_function(2,150,200)
femeren_plaiceM$source<-"femeren_plaiceM"
count(femeren_plaiceM) 

#------------------ADULTS------------------#
femeren_plaiceA<-Size_classes_function(3,150,200)
femeren_plaiceA$source<-"femeren_plaiceA" 
count(femeren_plaiceA)

###############################################################################
################################     østersøen.  ################################
###############################################################################

østersøen_data <- read_csv("DATRAS_data/østersøen_data.csv")#udtræk fra GIS
østersøen_data <-filter(østersøen_data ,Year>=2000)
østersøen_data<-subset(østersøen_data, Quarter=="1" |Quarter=="4")
østersøen_data$haul<-as.numeric(as.factor(with(østersøen_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

østersøen_data2<- østersøen_data%>% 
  group_by(haul) %>%
  complete(Species = "Pleuronectes platessa", fill = list(n = 0)) #FILL IN Pleuronectes platessa IN ALL HAULS

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

østersøen_plaice<-subset(østersøen_data2,Species==  "Pleuronectes platessa") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
østersøen_plaice1<-østersøen_plaice %>% select('Year','Quarter','Species','Ship','LngtClass','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
østersøen_plaice2<- østersøen_plaice1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_plaice$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

østersøen_plaice_sum<-østersøen_plaice2 %>%
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
    return(østersøen_plaiceX<-subset(østersøen_plaice_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(østersøen_plaiceX<-subset(østersøen_plaice_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(østersøen_plaiceX<-subset(østersøen_plaice_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  østersøen_plaiceX<-size_function(size_class,length_min,length_max)
  
  østersøen_plaiceX1<-østersøen_plaiceX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  østersøen_plaiceX<-merge(østersøen_plaiceX1,østersøen_plaiceX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  østersøen_plaiceX_occ<-østersøen_plaiceX[order(østersøen_plaiceX$haul,østersøen_plaiceX$Country,decreasing = FALSE),] 
  østersøen_plaiceX <-østersøen_plaiceX_occ[!duplicated(østersøen_plaiceX_occ$haul), ]
  
  return(østersøen_plaiceX)
}

#------------------JUVENILES------------------#
østersøen_plaiceJ<-Size_classes_function(1,150,200)
østersøen_plaiceJ$source<-"østersøen_plaiceJ" #add source so it can be subsetted in GAM
count(østersøen_plaiceJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
østersøen_plaiceM<-Size_classes_function(2,150,200)
østersøen_plaiceM$source<-"østersøen_plaiceM"
count(østersøen_plaiceM) 

#------------------ADULTS------------------#
østersøen_plaiceA<-Size_classes_function(3,150,200)
østersøen_plaiceA$source<-"østersøen_plaiceA" 
count(østersøen_plaiceA)

###############################################################################
########################  SAVE EVERYTHING FOR GAM #############################
###############################################################################

save(øresund_plaiceJ,øresund_plaiceM,øresund_plaiceA,kattegatnord_plaiceA,
     kattegatnord_plaiceM ,kattegatnord_plaiceJ,kattegatmidt_plaiceA,
     kattegatmidt_plaiceM,kattegatmidt_plaiceJ,kattegatsyd_plaiceA,kattegatsyd_plaiceM,
     kattegatsyd_plaiceJ,femeren_plaiceA,femeren_plaiceM,femeren_plaiceJ,storebælt_plaiceA,storebælt_plaiceM,storebælt_plaiceJ
     ,østersøen_plaiceA,østersøen_plaiceJ,østersøen_plaiceM,file = "Area_plaiceAges.RData")


plaice_all_combines<-vctrs::vec_c(øresund_plaiceJ,øresund_plaiceM,øresund_plaiceA,kattegatnord_plaiceA,
                                  kattegatnord_plaiceM ,kattegatnord_plaiceJ,kattegatmidt_plaiceA,
                                  kattegatmidt_plaiceM,kattegatmidt_plaiceJ,kattegatsyd_plaiceA,kattegatsyd_plaiceM,
                                  kattegatsyd_plaiceJ,femeren_plaiceA,femeren_plaiceM,femeren_plaiceJ,
                                  storebælt_plaiceA,storebælt_plaiceM,storebælt_plaiceJ,
                                  østersøen_plaiceA,østersøen_plaiceJ,østersøen_plaiceM,.name_spec = NULL)

plaice_all_combines$source<-as.factor(plaice_all_combines$source)
dat=plaice_all_combines

dat$POSIXyear<-as.POSIXct(dat$DateTime,format='%d/%m/%Y')
dat$numyear <- as.numeric(format(dat$POSIXyear, format = "%Y")) + as.numeric(format(dat$POSIXyear, format = "%m"))/12 + as.numeric(format(dat$POSIXyear, format = "%d"))/365

dat<-dat%>%
  rename(
    cpue=sum,
    time=numyear, 
    depth=Depth,
    loc=source
  )

write.csv(dat,"dat_plaice.csv")


