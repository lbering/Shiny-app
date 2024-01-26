#libraries
library(readr)
library(tidyverse)
library(dplyr)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
load("species_weight.Rdata")

###############################################################################
################################     ØRESUND.  ################################
###############################################################################

øresund_data <- read_csv("DATRAS_data/øresund_data.csv")#udtræk fra GIS
øresund_data <-filter(øresund_data ,Year>=2000)
øresund_data<-subset(øresund_data, Quarter=="1" |Quarter=="4")
øresund_data$haul<-as.numeric(as.factor(with(øresund_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

years<-c(2000:2022)
for (i in years){
  n<-length(unique(subset(øresund_data,Year==i)$haul))
  print(n)
}

øresund_data2<- øresund_data%>% 
  group_by(haul) %>%
  complete(Species = "Gadus morhua", fill = list(n = 0)) #FILL IN GADUS MORHUA IN ALL HAULS

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

øresund_cod<-subset(øresund_data2,Species==  "Gadus morhua") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
øresund_cod1<-øresund_cod %>% select('Year','Quarter','Species','LngtClass','Ship','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
øresund_cod2<- øresund_cod1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_cod$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

øresund_cod_sum<-øresund_cod2 %>%
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
       return(øresund_codX<-subset(øresund_cod_sum, LngtClass<length_min))
  } else if (sizeclass==2){
       return(øresund_codX<-subset(øresund_cod_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
       return(øresund_codX<-subset(øresund_cod_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  øresund_codX<-size_function(size_class,length_min,length_max)

  øresund_codX1<-øresund_codX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  øresund_codX<-merge(øresund_codX1,øresund_codX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  øresund_codX_occ<-øresund_codX[order(øresund_codX$haul,øresund_codX$Country,decreasing = FALSE),] 
  øresund_codX <-øresund_codX_occ[!duplicated(øresund_codX_occ$haul), ]
  
  return(øresund_codX)
}

#------------------JUVENILES------------------#
øresund_codJ<-Size_classes_function(1,350,650)
øresund_codJ$source<-"øresund_codJ" #add source so it can be subsetted in GAM
count(øresund_codJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
øresund_codM<-Size_classes_function(2,350,650)
øresund_codM$source<-"øresund_codM"
count(øresund_codM) 

#------------------ADULTS------------------#
øresund_codA<-Size_classes_function(3,350,650)
øresund_codA$source<-"øresund_codA" 
count(øresund_codA)



sum(øresund_codJ$sum)
sum(øresund_codM$sum)
sum(øresund_codA$sum)

###############################################################################
################################     kattegatnord.  ################################
###############################################################################

kattegatnord_data <- read_csv("DATRAS_data/kattegatnord_data.csv")#udtræk fra GIS
kattegatnord_data <-filter(kattegatnord_data ,Year>=2000)
kattegatnord_data<-subset(kattegatnord_data, Quarter=="1" |Quarter=="4")
kattegatnord_data$haul<-as.numeric(as.factor(with(kattegatnord_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatnord_data2<- kattegatnord_data%>% 
  group_by(haul) %>%
  complete(Species = "Gadus morhua", fill = list(n = 0)) #FILL IN GADUS MORHUA IN ALL HAULS

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

kattegatnord_cod<-subset(kattegatnord_data2,Species==  "Gadus morhua") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatnord_cod1<-kattegatnord_cod %>% select('Year','Quarter','Species','LngtClass','Ship','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatnord_cod2<- kattegatnord_cod1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_cod$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatnord_cod_sum<-kattegatnord_cod2 %>%
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
    return(kattegatnord_codX<-subset(kattegatnord_cod_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatnord_codX<-subset(kattegatnord_cod_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatnord_codX<-subset(kattegatnord_cod_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatnord_codX<-size_function(size_class,length_min,length_max)
  
  kattegatnord_codX1<-kattegatnord_codX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatnord_codX<-merge(kattegatnord_codX1,kattegatnord_codX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatnord_codX_occ<-kattegatnord_codX[order(kattegatnord_codX$haul,kattegatnord_codX$Country,decreasing = FALSE),] 
  kattegatnord_codX <-kattegatnord_codX_occ[!duplicated(kattegatnord_codX_occ$haul), ]
  
  return(kattegatnord_codX)
}

#------------------JUVENILES------------------#
kattegatnord_codJ<-Size_classes_function(1,350,650)
kattegatnord_codJ$source<-"kattegatnord_codJ" #add source so it can be subsetted in GAM
count(kattegatnord_codJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatnord_codM<-Size_classes_function(2,350,650)
kattegatnord_codM$source<-"kattegatnord_codM"
count(kattegatnord_codM) 

#------------------ADULTS------------------#
kattegatnord_codA<-Size_classes_function(3,350,650)
kattegatnord_codA$source<-"kattegatnord_codA" 
count(kattegatnord_codA)

###############################################################################
################################     kattegatmidt.  ################################
###############################################################################

kattegatmidt_data <- read_csv("DATRAS_data/kattegatmidt_data.csv")#udtræk fra GIS
kattegatmidt_data <-filter(kattegatmidt_data ,Year>=2000)
kattegatmidt_data<-subset(kattegatmidt_data, Quarter=="1" |Quarter=="4")
kattegatmidt_data$haul<-as.numeric(as.factor(with(kattegatmidt_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatmidt_data2<- kattegatmidt_data%>% 
  group_by(haul) %>%
  complete(Species = "Gadus morhua", fill = list(n = 0)) #FILL IN GADUS MORHUA IN ALL HAULS

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

kattegatmidt_cod<-subset(kattegatmidt_data2,Species==  "Gadus morhua") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatmidt_cod1<-kattegatmidt_cod %>% select('Year','Quarter','Species','LngtClass','Ship','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatmidt_cod2<- kattegatmidt_cod1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_cod$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatmidt_cod_sum<-kattegatmidt_cod2 %>%
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
    return(kattegatmidt_codX<-subset(kattegatmidt_cod_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatmidt_codX<-subset(kattegatmidt_cod_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatmidt_codX<-subset(kattegatmidt_cod_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatmidt_codX<-size_function(size_class,length_min,length_max)
  
  kattegatmidt_codX1<-kattegatmidt_codX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatmidt_codX<-merge(kattegatmidt_codX1,kattegatmidt_codX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatmidt_codX_occ<-kattegatmidt_codX[order(kattegatmidt_codX$haul,kattegatmidt_codX$Country,decreasing = FALSE),] 
  kattegatmidt_codX <-kattegatmidt_codX_occ[!duplicated(kattegatmidt_codX_occ$haul), ]
  
  return(kattegatmidt_codX)
}

#------------------JUVENILES------------------#
kattegatmidt_codJ<-Size_classes_function(1,350,650)
kattegatmidt_codJ$source<-"kattegatmidt_codJ" #add source so it can be subsetted in GAM
count(kattegatmidt_codJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatmidt_codM<-Size_classes_function(2,350,650)
kattegatmidt_codM$source<-"kattegatmidt_codM"
count(kattegatmidt_codM) 

#------------------ADULTS------------------#
kattegatmidt_codA<-Size_classes_function(3,350,650)
kattegatmidt_codA$source<-"kattegatmidt_codA" 
count(kattegatmidt_codA)


###############################################################################
################################     kattegatsyd.  ################################
###############################################################################

kattegatsyd_data <- read_csv("DATRAS_data/kattegatsyd_data.csv")#udtræk fra GIS
kattegatsyd_data <-filter(kattegatsyd_data ,Year>=2000)
kattegatsyd_data<-subset(kattegatsyd_data, Quarter=="1" |Quarter=="4")
kattegatsyd_data$haul<-as.numeric(as.factor(with(kattegatsyd_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

kattegatsyd_data2<- kattegatsyd_data%>% 
  group_by(haul) %>%
  complete(Species = "Gadus morhua", fill = list(n = 0)) #FILL IN GADUS MORHUA IN ALL HAULS

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

kattegatsyd_cod<-subset(kattegatsyd_data2,Species==  "Gadus morhua") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
kattegatsyd_cod1<-kattegatsyd_cod %>% select('Year','Quarter','Species','LngtClass','Ship','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
kattegatsyd_cod2<- kattegatsyd_cod1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_cod$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

kattegatsyd_cod_sum<-kattegatsyd_cod2 %>%
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
    return(kattegatsyd_codX<-subset(kattegatsyd_cod_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(kattegatsyd_codX<-subset(kattegatsyd_cod_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(kattegatsyd_codX<-subset(kattegatsyd_cod_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  kattegatsyd_codX<-size_function(size_class,length_min,length_max)
  
  kattegatsyd_codX1<-kattegatsyd_codX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  kattegatsyd_codX<-merge(kattegatsyd_codX1,kattegatsyd_codX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  kattegatsyd_codX_occ<-kattegatsyd_codX[order(kattegatsyd_codX$haul,kattegatsyd_codX$Country,decreasing = FALSE),] 
  kattegatsyd_codX <-kattegatsyd_codX_occ[!duplicated(kattegatsyd_codX_occ$haul), ]
  
  return(kattegatsyd_codX)
}

#------------------JUVENILES------------------#
kattegatsyd_codJ<-Size_classes_function(1,350,650)
kattegatsyd_codJ$source<-"kattegatsyd_codJ" #add source so it can be subsetted in GAM
count(kattegatsyd_codJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
kattegatsyd_codM<-Size_classes_function(2,350,650)
kattegatsyd_codM$source<-"kattegatsyd_codM"
count(kattegatsyd_codM) 

#------------------ADULTS------------------#
kattegatsyd_codA<-Size_classes_function(3,350,650)
kattegatsyd_codA$source<-"kattegatsyd_codA" 
count(kattegatsyd_codA)

###############################################################################
################################     storebælt.  ################################
###############################################################################

storebælt_data <- read_csv("DATRAS_data/storebælt_data.csv")#udtræk fra GIS
storebælt_data <-filter(storebælt_data ,Year>=2000)
storebælt_data<-subset(storebælt_data, Quarter=="1" |Quarter=="4")
storebælt_data$haul<-as.numeric(as.factor(with(storebælt_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

storebælt_data2<- storebælt_data%>% 
  group_by(haul) %>%
  complete(Species = "Gadus morhua", fill = list(n = 0)) #FILL IN GADUS MORHUA IN ALL HAULS

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

storebælt_cod<-subset(storebælt_data2,Species==  "Gadus morhua") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
storebælt_cod1<-storebælt_cod %>% select('Year','Quarter','Species','LngtClass','Ship','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
storebælt_cod2<- storebælt_cod1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_cod$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

storebælt_cod_sum<-storebælt_cod2 %>%
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
    return(storebælt_codX<-subset(storebælt_cod_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(storebælt_codX<-subset(storebælt_cod_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(storebælt_codX<-subset(storebælt_cod_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  storebælt_codX<-size_function(size_class,length_min,length_max)
  
  storebælt_codX1<-storebælt_codX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  storebælt_codX<-merge(storebælt_codX1,storebælt_codX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  storebælt_codX_occ<-storebælt_codX[order(storebælt_codX$haul,storebælt_codX$Country,decreasing = FALSE),] 
  storebælt_codX <-storebælt_codX_occ[!duplicated(storebælt_codX_occ$haul), ]
  
  return(storebælt_codX)
}

#------------------JUVENILES------------------#
storebælt_codJ<-Size_classes_function(1,350,650)
storebælt_codJ$source<-"storebælt_codJ" #add source so it can be subsetted in GAM
count(storebælt_codJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
storebælt_codM<-Size_classes_function(2,350,650)

storebælt_codM$source<-"storebælt_codM"
count(storebælt_codM) 

#------------------ADULTS------------------#
storebælt_codA<-Size_classes_function(3,350,650)
storebælt_codA$source<-"storebælt_codA" 
count(storebælt_codA)

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
  complete(Species = "Gadus morhua", fill = list(n = 0)) #FILL IN GADUS MORHUA IN ALL HAULS

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

femeren_cod<-subset(femeren_data2,Species==  "Gadus morhua") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
femeren_cod1<-femeren_cod %>% select('Year','Quarter','Species','LngtClass','Ship','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
femeren_cod2<- femeren_cod1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_cod$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

femeren_cod_sum<-femeren_cod2 %>%
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
    return(femeren_codX<-subset(femeren_cod_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(femeren_codX<-subset(femeren_cod_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(femeren_codX<-subset(femeren_cod_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  femeren_codX<-size_function(size_class,length_min,length_max)
  
  femeren_codX1<-femeren_codX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  femeren_codX<-merge(femeren_codX1,femeren_codX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  femeren_codX_occ<-femeren_codX[order(femeren_codX$haul,femeren_codX$Country,decreasing = FALSE),] 
  femeren_codX <-femeren_codX_occ[!duplicated(femeren_codX_occ$haul), ]
  
  return(femeren_codX)
}

#------------------JUVENILES------------------#
femeren_codJ<-Size_classes_function(1,350,650)
femeren_codJ$source<-"femeren_codJ" #add source so it can be subsetted in GAM
count(femeren_codJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
femeren_codM<-Size_classes_function(2,350,650)
femeren_codM$source<-"femeren_codM"
count(femeren_codM) 

#------------------ADULTS------------------#
femeren_codA<-Size_classes_function(3,350,650)
femeren_codA$source<-"femeren_codA" 
count(femeren_codA)

###############################################################################
################################     østersøen.  ################################
###############################################################################

østersøen_data <- read_csv("DATRAS_data/østersøen_data.csv")#udtræk fra GIS
østersøen_data <-filter(østersøen_data ,Year>=2000)
østersøen_data<-subset(østersøen_data, Quarter=="1" |Quarter=="4")
østersøen_data$haul<-as.numeric(as.factor(with(østersøen_data,paste(Year,HaulNo,Quarter,Survey,Country,Ship,Gear,sep="_")))) #ny kolonne med unikt haul ID

østersøen_data2<- østersøen_data%>% 
  group_by(haul) %>%
  complete(Species = "Gadus morhua", fill = list(n = 0)) #FILL IN GADUS MORHUA IN ALL HAULS

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

østersøen_cod<-subset(østersøen_data2,Species==  "Gadus morhua") 

#-----------------------------------------------------------------------------#
#------------------------------ PREPARE DATA ---------------------------------#
#-----------------------------------------------------------------------------#

#filter away some columns that are not being used
østersøen_cod1<-østersøen_cod %>% select('Year','Quarter','Species','LngtClass','Ship','CPUE_number_per_hour','haul','Gear','Depth','Survey','Country','DateTime','DayNight','SedimentDK')

# GIVE EVERY UNIQUE HAUL ALL LENGTH CLASSES AND FILL IN ALL OTHER COLUMNS
østersøen_cod2<- østersøen_cod1%>% 
  group_by(haul) %>%
  complete(LngtClass = weight_cod$LngtClass, fill = list(n = 0)) #add NA when nothing has been caught in specific weight class

østersøen_cod_sum<-østersøen_cod2 %>%
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
    return(østersøen_codX<-subset(østersøen_cod_sum, LngtClass<length_min))
  } else if (sizeclass==2){
    return(østersøen_codX<-subset(østersøen_cod_sum, LngtClass>length_min & LngtClass<length_max))
  } else {
    return(østersøen_codX<-subset(østersøen_cod_sum, LngtClass>length_max))
  }
}

Size_classes_function<-function(size_class,length_min,length_max){
  østersøen_codX<-size_function(size_class,length_min,length_max)
  
  østersøen_codX1<-østersøen_codX %>%         #summér CPUE'er efter unikt haul ID as lookuptable 
    group_by(haul) %>%
    summarise(sum = sum(CPUE_number_per_hour,na.rm=T))
  
  østersøen_codX<-merge(østersøen_codX1,østersøen_codX, by="haul") #and add to dataframe
  
  #order so that NA appear last sso that we can remove duplicates of hauls so theres only ONE haul number with a sum CPUE and it is NOT a column with NAs that will be passed on
  østersøen_codX_occ<-østersøen_codX[order(østersøen_codX$haul,østersøen_codX$Country,decreasing = FALSE),] 
  østersøen_codX <-østersøen_codX_occ[!duplicated(østersøen_codX_occ$haul), ]
  
  return(østersøen_codX)
}

#------------------JUVENILES------------------#
østersøen_codJ<-Size_classes_function(1,350,650)
østersøen_codJ$source<-"østersøen_codJ" #add source so it can be subsetted in GAM
count(østersøen_codJ) #count to be sure all hauls are there 

#------------------YOUNG ADULTS------------------#
østersøen_codM<-Size_classes_function(2,350,650)
østersøen_codM$source<-"østersøen_codM"
count(østersøen_codM) 

#------------------ADULTS------------------#
østersøen_codA<-Size_classes_function(3,350,650)
østersøen_codA$source<-"østersøen_codA" 
count(østersøen_codA)

###############################################################################
########################  SAVE EVERYTHING FOR GAM #############################
###############################################################################

save(øresund_codJ,øresund_codM,øresund_codA,kattegatnord_codA,
     kattegatnord_codM ,kattegatnord_codJ,kattegatmidt_codA,
     kattegatmidt_codM,kattegatmidt_codJ,kattegatsyd_codA,kattegatsyd_codM,
     kattegatsyd_codJ,femeren_codA,femeren_codM,femeren_codJ,storebælt_codA,storebælt_codM,storebælt_codJ
     ,østersøen_codA,østersøen_codJ,østersøen_codM,file = "Area_CODAges.RData")


Cod_all_combines<-vctrs::vec_c(øresund_codJ,øresund_codM,øresund_codA,kattegatnord_codA,
                               kattegatnord_codM ,kattegatnord_codJ,kattegatmidt_codA,
                               kattegatmidt_codM,kattegatmidt_codJ,kattegatsyd_codA,kattegatsyd_codM,
                               kattegatsyd_codJ,femeren_codA,femeren_codM,femeren_codJ,
                               storebælt_codA,storebælt_codM,storebælt_codJ,
                               østersøen_codA,østersøen_codJ,østersøen_codM,.name_spec = NULL)

Cod_all_combines$source<-as.factor(Cod_all_combines$source)
dat_cod=Cod_all_combines
save(dat_cod,file="Areas_cod_combined.Rdata")

dat_cod<-subset(dat_cod, Quarter=="1" |Quarter=="4")
dat_cod$POSIXyear<-as.POSIXct(dat_cod$DateTime,format='%d/%m/%Y')
dat_cod$numyear <- as.numeric(format(dat_cod$POSIXyear, format = "%Y")) + as.numeric(format(dat_cod$POSIXyear, format = "%m"))/12 + as.numeric(format(dat_cod$POSIXyear, format = "%d"))/365

dat_cod<-dat_cod%>%
  rename(
    cpue=sum,
    time=numyear, 
    depth=Depth,
    loc=source
  )

write.csv(dat_cod,"dat_cod.csv")
