setwd("~/Documents/DTU/Speciale/R")
library(readr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearth)
library(tidyverse)
library(dplyr)


#----------- TIL AT TJEKKE STØRRELSER ---------#
BITS <- read_csv("DATRAS/BITS_allspecies_CPUE per length per haul per hour_2023-03-23 11_19_30/CPUE per length per haul per hour_2023-03-23 11_19_30.csv")
NS_IBTS <- read_csv("DATRAS/NS_IBTS_CPUE per length per haul per hour_2023-03-07 11_23_47/CPUE per length per haul per hour_2023-03-07 11_23_47.csv")
BITS_IBTS<-rbind(BITS,NS_IBTS)
BITS_IBTS_sf<-st_as_sf(BITS_IBTS,
                       coords=c("ShootLong", "ShootLat"),
                       crs=4326)
st_crs(BITS_IBTS_sf) #crs EPSG 4326

unique(BITS_IBTS_sf$Species)

# [1] "Clupea harengus"          "Sprattus sprattus"        "Gadus morhua"             "Melanogrammus aeglefinus"
# [5] "Merlangius merlangus"     "Pollachius virens"        "Scomber scombrus"         "Platichthys flesus"      
# [9] "Pleuronectes platessa"    "Trisopterus esmarkii" 

#--------- WEIGHT COD --------------#
BITS_IBTS_sf_cod<-subset(BITS_IBTS_sf, Species=="Gadus morhua")
unique(BITS_IBTS_sf_cod$LngtClass)

LngtClass<-seq(from=0, to=1200, by=10)
cod_weight_g<-c(NA, NA,NA,NA, NA, NA, NA, NA, NA,NA, 8.3,
                11.3, 14.55, 18.61, 23.38, 28.92, 35.28, 42.52,50.71,59.9,70.5,
                81.52,94.08, 107.89, 123, 139.48, 157.39, 176.79, 197.74, 220.31, 244.56,
                270.55, 298.34, 328, 359.59, 393.17, 428.81, 466.56, 506.5, 548.69, 593.19,
                640.06, 689.38, 741.19, 795.58, 852.6, 912.31, 974.79, 1040.09, 1108.29, 1179.44,
                1253.62, 1330.88, 1411.3, 1494.93, 1581.85, 1672.12, 1765.81, 1862.98, 1963.69, 2068.02,
                2176.03, 2287.79, 2403.36, 2522.8, 2646.2, 2773.6, 2905.09, 3040.72, 3180.57, 3324.69,
                3473.16, 3626.04, 3783.41, 3945.33, 4111.85, 4283.07, 4459.03, 4639.81, 4825.48, 5016.1,
                5211.74, 5412.47, 5618.36, 5829.47, 6045.88, 6267.64, 6494.84, 6727.53, 6965.79, 7209.68, 
                7459.27, 7714.64, 7975.84, 8242.95, 8516.04, 8795.18, 9080.42, 9371.85, 9669.53, 9973.53,
                10283.93, 10600.78, 10924.15, 11254.13, 11590.77,  11934.14, 12284.32, 12641.37, 13005.37, 13376.38,
                13754.47,14139.71, 14532.18, 14931.93, 15339.05, 15753.6, 16175.64, 16605.26, 17042.52, 17487.48
)
weight_cod<-data.frame(LngtClass,cod_weight_g)
weight_cod$Species<-"Gadus morhua" 
weight_cod<-weight_cod[weight_cod$LngtClass != 0, ] #remove Length classes =0. CPUE er også 0 i disse tilfælde og må tælle som at intet er fanget 


#--------- WEIGHT PLAICE --------------#
BITS_IBTS_sf_plaice<-subset(BITS_IBTS_sf, Species=="Pleuronectes platessa")
unique(BITS_IBTS_sf_plaice$LngtClass)

LngtClass<-seq(from=0, to=650, by=10)
plaice_weight_g<-c(0,0.01, 0.07, 0.25, 0.6, 1.19, 2.07,  3.3, 4.95, 7.08, 9.76,
                   13.04, 16.99, 21.67,27.14, 33.47, 40.73, 48.97, 58.27, 68.68, 80.26,
                   93.1, 107.24, 122.76, 139.71, 158.17, 178.2, 199.86, 223.23, 248.36, 275.32,
                   304.18, 335, 367.85, 402.8, 439.9, 479.24, 520.86, 564.85, 611.26, 660.16,
                   711.63, 765.72, 822.5, 882.04, 944.4, 1009.66, 1077.87, 1149.11, 1223.45, 1300.94,
                   1381.67, 1465.68, 1553.06, 1643.87, 1738.17, 1836.04, 1937.54, 2042.73, 2151.69, 2264.49,
                   2381.18, 2501.85, 2626.55, 2755.35, 2888.33
)
weight_plaice<-data.frame(LngtClass,plaice_weight_g)
weight_plaice$Species<-"Pleuronectes platessa"
weight_plaice<-weight_plaice[weight_plaice$LngtClass != 0, ]

#--------- WEIGHT FLOUNDER --------------#
BITS_IBTS_sf_flounder<-subset(BITS_IBTS_sf, Species=="Platichthys flesus" )
unique(BITS_IBTS_sf_flounder$LngtClass)

LngtClass<-seq(from=0, to=500, by=10)
flounder_weight_g<-c(NA, 0.01, 0.08, 0.27, 0.64, 1.27, 2.2, 3.52, 5.29, 7.56,10.42, #10
                     13.92, 18.13, 23.13, 28.97, 35.73, 43.48, 52.27, 62.19,73.31,85.68, #20
                     99.37,114.47, 131.03, 149.13, 168.83, 190.21, 213.34, 238.28, 265.1, 293.88,#30
                     324.69, 357.59,392.65, 429.95, 469.56, 511.54, 555.98, 602.93, 652.47,704.67,#40
                     759.6, 817.34, 877.95, 941.5, 1008.07, 1077.72,1150.54,1226.58, 1305.93, 1388.65 #50
)
weight_flounder<-data.frame(LngtClass,flounder_weight_g)
weight_flounder$Species<-"Platichthys flesus" 
weight_flounder<-weight_flounder[weight_flounder$LngtClass != 0, ]

#--------- WEIGHT HERRING --------------#
BITS_IBTS_sf_herring<-subset(BITS_IBTS_sf, Species=="Clupea harengus")
unique(BITS_IBTS_sf_herring$LngtClass) #de fanger ikke nogen under 75 og over 310

LngtClass<-seq(from=0, to=350, by=5)
herring_weight_g<-c(NA,NA,NA,0.01, 0.02,0.05,0.1,0.18,0.28,0.43,0.85,1.14,1.5,1.92,2.41,2.98,3.64,4.39,5.24,6.19,7.26, #0-10cm
                    8.44,9.74,11.18,12.75,14.46,16.33,18.35,20.53,22.88,25.41, #10.5-15cm
                    28.12,31.02,34.11,37.41,40.91,44.63,48.57,52.75,57.16,61.81, #15.5-20cm
                    66.71,71.86, 77.28,82.97,88.94,95.19,101.73,108.57,115.71,123.16,#20.5-25
                    130.94,139.03,147.46,156.23,165.34,174.81,184.64,194.83,205.4,216.35,#25.5-30
                    227.69,251.55,251.55,264.1,277.06,290.44,304.26,318.51,333.21,348.36 #30.5-35
)
weight_herring<-data.frame(LngtClass,herring_weight_g)
weight_herring$Species<-"Clupea harengus" 
weight_herring<-weight_herring[weight_herring$LngtClass != 0, ]

#--------- WEIGHT SPRAT --------------#
BITS_IBTS_sf_sprat<-subset(BITS_IBTS_sf, Species=="Sprattus sprattus")
unique(BITS_IBTS_sf_sprat$LngtClass) 

LngtClass<-seq(from=0, to=200, by=5)
sprat_weight_g<-c(NA,NA,0.01,0.02,0.05,0.09,0.16,0.26,0.4,0.57,0.79, #0-5cm
                  1.07, 1.4, 1.79, 2.25,2.78,3.4,4.1,4.89,5.77,6.77,#5.5-10cm
                  7.87,9.08,10.42,11.89,13.48,15.22,17.1,19.14,21.33,23.69,#10.5-15cm
                  26.21, 28.91,31.8,34.87,38.14,41.61,45.28,49.17,53.28,57.62#15.5-20cm
)
weight_sprat<-data.frame(LngtClass,sprat_weight_g)
weight_sprat$Species<-"Sprattus sprattus"
weight_sprat<-weight_sprat[weight_sprat$LngtClass != 0, ]

#--------- WEIGHT HADDOCK --------------#
BITS_IBTS_sf_haddock<-subset(BITS_IBTS_sf, Species=="Melanogrammus aeglefinus")
unique(BITS_IBTS_sf_haddock$LngtClass) #0-630mm

LngtClass<-seq(from=0, to=630, by=10)
haddock_weight_g<-c(0,0.01,0.05,0.18,0.45,0.91,1.61,2.61,3.96,5.72,7.96, #0-10cm
                    10.73,14.08,18.09,22.82,28.32,34.65,41.89,50.1,59.34,69.67,#11-20cm
                    81.17,93.89,107.91,123.29,140.09,158.39,178.25,199.74,222.92,247.88,#21-30cm
                    274.67,303.37,334.04,366.76,401.59,438.61,477.89,519.49,563.49,609.96,#39-40cm
                    658.97,710.6,764.91,821.98,881.88,944.68,1010.46,1079.29,1151.24,1226.39,#41-50cm
                    1304.81,1386.57,1471.75,1560.43,1652.67,1748.56,1848.16,1951.56,2058.82,2170.03,#51-60cm
                    2285.25,2404.57,2528.06#61-63cm
)
weight_haddock<-data.frame(LngtClass,haddock_weight_g)
weight_haddock$Species<-"Melanogrammus aeglefinus" 

#--------- WEIGHT WHITING --------------#
BITS_IBTS_sf_whiting<-subset(BITS_IBTS_sf, Species=="Merlangius merlangus")
unique(BITS_IBTS_sf_whiting$LngtClass) #0-570

LngtClass<-seq(from=0, to=600, by=10)
whiting_weight_g<-c(0,0.01,0.05,0.19,0.45,0.88,1.54,2.46,3.69,5.29,7.29, #0-10cm
                    9.75, 12.72,16.23,20.35,25.12,30.58,36.79,43.8,51.65,60.4,#11-20
                    70.09,80.78,92.51,105.33,119.3,134.46,150.86,168.56,187.6,208.03,#21-30cm
                    229.91,253.29,278.22,304.74,332.91,362.77,394.39,427.81,463.08,500.26,#31-40
                    539.39,580.53,623.72,669.03,716.49,766.17,818.11,872.37,928.99,988.03,#41-50cm
                    1049.55,1113.59,1180.2,1249.44,1321.36,1396,1473.44,1553.71,1636.86,1722.96#51-60
                    )
weight_whiting<-data.frame(LngtClass,whiting_weight_g)
weight_whiting$Species<-"Merlangius merlangus" 
weight_whiting<-weight_whiting[weight_whiting$LngtClass != 0, ]

#--------- WEIGHT SAITHE --------------#
BITS_IBTS_sf_saithe<-subset(BITS_IBTS_sf, Species=="Pollachius virens" )
unique(BITS_IBTS_sf_saithe$LngtClass) #0-680

LngtClass<-seq(from=0, to=700, by=10)
saithe_weight_g<-c(0,0.01,0.06,0.21,0.51,1,1.75,2.8,4.2,6.02,8.3,#0-10cm
                   11.1,14.48,18.48,23.17,28.6,34.82,41.89,49.87,58.81,68.77,#11-20cm
                   79.8,91.96,105.32,119.92,135.82,153.07,171.75,191.9,213.57,236.84,#21-30cm
                   261.75,288.36,316.74,346.93,379,413,449,487.05,527.2,569.53,#31-40cm
                   614.08,660.91,710.09,761.66,815.7,872.25,931.39,993.16,1057.62,1124.84,#41-50cm
                   1194.87,1267.77,1343.61,1422.44,1504.31,1589.3,1677.45,1768.83,1863.5,1961.52,#51-60cm
                   2062.95,2167.84,2276.25,2388.25,2503.9,2623.26,2746.38,2873.32,3004.15,3138.92#61-70cm
)
weight_saithe<-data.frame(LngtClass,saithe_weight_g)
weight_saithe$Species<-"Pollachius virens"  
#--------- WEIGHT MACKEREL --------------#

BITS_IBTS_sf_mackerel<-subset(BITS_IBTS_sf, Species=="Scomber scombrus" )
unique(BITS_IBTS_sf_mackerel$LngtClass) #0-560

LngtClass<-seq(from=0, to=600, by=10)
mackerel_weight_g<-c(0,0,0.03,0.13,0.32,0.66,1.2,1.99,3.08,4.52,6.37,#0-10cm
                     8.69,11.54,14.98,19.07,23.89,29.48,35.92,43.28,51.62,61.01,#11-20cm
                     71.53,83.25,96.23,110.55,126.29,143.51,162.3,182.73,204.87,228.81,#21-30cm
                     254.63,282.39,312.19,344.1,378.21,414.59,453.32,494.5,538.19,584.5,#31-40cm
                     633.5,685.27,739.9,797.49,858.11,921.85,988.8,1059.05,1132.68,1209.79,#41-50cm
                     1290.47,1374.8,1462.88,1554.79,1650.63,1750.5,1854.47,1962.65,2075.13,2192#51-60cm
)
weight_mackerel<-data.frame(LngtClass,mackerel_weight_g)
weight_mackerel$Species<-"Scomber scombrus"
#--------- WEIGHT NORWAY POUT --------------#

BITS_IBTS_sf_norwaypout<-subset(BITS_IBTS_sf, Species=="Trisopterus esmarkii" )
unique(BITS_IBTS_sf_norwaypout$LngtClass) #0-420

LngtClass<-seq(from=0, to=450, by=10)
norwaypout_weight_g<-c(0,0.01,0.04,0.15,0.38,0.76,1.34,2.17,3.29,4.74,6.59,#0-10cm
                       8.87,11.64,14.94,18.83,23.35,28.56,34.51,41.25,48.83,57.3,#11-20cm
                       66.73,77.15,88.63,101.21,114.96,129.92,146.16,163.72,182.66,203.04,#21-30cm
                       224.92,248.34,273.36,300.04,328.44,358.62,390.62,424.52,460.35,498.19,#31-40cm
                       538.09,580.11,624.3,670.72,719.44#41-45cm
)
weight_norwaypout<-data.frame(LngtClass,norwaypout_weight_g)
weight_norwaypout$Species<-"Trisopterus esmarkii" 



#-----------------save----------------#
save(weight_cod, weight_flounder,weight_herring, weight_sprat, weight_saithe,weight_plaice,weight_norwaypout, weight_haddock,weight_mackerel,weight_whiting,file = "species_weight.RData")
# To load the data again
#load("species_weight.RData")
