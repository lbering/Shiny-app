###########################################
############------kattegatnord------############
###########################################
library(mgcv)
library(gratia)
library(readr)
library(tidyverse)
library(dplyr)
library(gdata)
library(lubridate)
library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#--------------------------------------------#
source("funcs.R")
periods <- seq(2000,2024,6)

#------------Make as factors----------------_#
dat_cod <- read_csv("../data_cleaning/dat_cod.csv")
dat_cod <- check.data(dat_cod, periods = periods)

dat_flounder <- read_csv("../data_cleaning/dat_flounder.csv")
dat_flounder <- check.data(dat_flounder, periods = periods)

dat_plaice <- read_csv("../data_cleaning/dat_plaice.csv")
dat_plaice <- check.data(dat_plaice, periods = periods)

dat_herring<- read_csv("../data_cleaning/dat_herring.csv")
dat_herring <- check.data(dat_herring, periods = periods)

dat_sprat<- read_csv("../data_cleaning/dat_sprat.csv")
dat_sprat <- check.data(dat_sprat, periods = periods)

dat_whiting<- read_csv("../data_cleaning/dat_whiting.csv")
dat_whiting <- check.data(dat_whiting, periods = periods)
##########################################
################ MODELS ##################
##########################################

#--------COD -------#
mod.codM <- gam(cpue ~ 
                  period+
                  s(depth,k=15) + 
                  s(Gear,bs="re")+ 
                  Quarter,
                #    s(SedimentDK,bs="re"), 
                data = subset(dat_cod, loc == "kattegatnord_codM"), 
                family=tw) 
mod.codM <- get.mini.model(mod.codM, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------FLOUNDER -------#
mod.flounderM <- gam(cpue ~ 
                  period+
                  s(depth,k=15) + 
                  s(Gear,bs="re")+
                  Quarter,
                #  s(SedimentDK,bs="re"),
                data = subset(dat_flounder, loc == "kattegatnord_flounderM"), 
                family=nb) 
mod.flounderM <- get.mini.model(mod.flounderM, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))
#--------PLAICE -------#
mod.plaiceM <- gam(cpue ~ 
                  period+
                  s(depth,k=15) + 
                  s(Gear,bs="re")+ 
                  Quarter, 
                #  s(SedimentDK,bs="re"), 
                data = subset(dat_plaice, loc == "kattegatnord_plaiceM"), 
                family=tw) 
mod.plaiceM <- get.mini.model(mod.plaiceM, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------HERRING -------#
mod.herringM <- gam(cpue ~ 
                  period+
                  s(depth,k=5) + 
                  s(Gear,bs="re")+ 
                  Quarter, 
                # s(SedimentDK,bs="re"), 
                data = subset(dat_herring, loc == "kattegatnord_herringM"), 
                family=tw) 
mod.herringM <- get.mini.model(mod.herringM, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------SPRAT -------#
mod.spratM <- gam(cpue ~ 
                  period+
                  s(depth,k=15) + 
                  s(Gear,bs="re")+ 
                  Quarter,
                #  s(SedimentDK,bs="re"), 
                data = subset(dat_sprat, loc == "kattegatnord_spratM"), 
                family=tw) 
mod.spratM <- get.mini.model(mod.spratM, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))
#--------WHITING -------#
mod.whitingM <- gam(cpue ~ 
                  period+
                  s(depth,k=5) + 
                  s(Gear,bs="re")+ 
                  Quarter,#
                #   s(SedimentDK,bs="re"), 
                data = subset(dat_whiting, loc == "kattegatnord_whitingM"), 
                family=tw) 
mod.whitingM <- get.mini.model(mod.whitingM, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

############################### Predict CPUE ############################### 
#depth.by.loc <- by(dat_cod$depth, dat_cod$loc, median)
## Assuming one overall depth for all locations is one approach, alternatively, one could use the median depth for each location

newdat <- expand.grid(period = levels(dat_cod$period),
                      loc = c("kattegatnord_codM","kattegatnord_flounderM",
                              "kattegatnord_plaiceM","kattegatnord_herringM",
                              "kattegatnord_spratM","kattegatnord_whitingM"),
                      depth = 19, #kattegatnord is 19
                      Gear=c("TVS"),
                      Quarter=c("4"),
                      SedimentDK=c("5")
)
#to check the depth of each location: 
#newdat$depth <- depth.by.loc[match(newdat$loc,names(depth.by.loc))] #overwrite for each location
## individual models
tmp1 <- predict(mod.codM, newdata = newdat, type = "response", se.fit = TRUE)
tmp2 <- predict(mod.flounderM, newdata = newdat, type = "response", se.fit = TRUE)
tmp3 <- predict(mod.plaiceM, newdata = newdat, type = "response", se.fit = TRUE)
tmp4 <- predict(mod.herringM, newdata = newdat, type = "response", se.fit = TRUE)
tmp5 <- predict(mod.spratM, newdata = newdat, type = "response", se.fit = TRUE)
tmp6 <- predict(mod.whitingM, newdata = newdat, type = "response", se.fit = TRUE)

#make new data predictions
newdat$cpue_pred <- NA
newdat$cpue_pred[newdat$loc == "kattegatnord_codM"] <- tmp1$fit[newdat$loc == "kattegatnord_codM"]
newdat$cpue_pred[newdat$loc == "kattegatnord_flounderM"] <- tmp2$fit[newdat$loc == "kattegatnord_flounderM"]
newdat$cpue_pred[newdat$loc == "kattegatnord_plaiceM"] <- tmp3$fit[newdat$loc == "kattegatnord_plaiceM"]
newdat$cpue_pred[newdat$loc == "kattegatnord_herringM"] <- tmp4$fit[newdat$loc == "kattegatnord_herringM"]
newdat$cpue_pred[newdat$loc == "kattegatnord_spratM"] <- tmp5$fit[newdat$loc == "kattegatnord_spratM"]
newdat$cpue_pred[newdat$loc == "kattegatnord_whitingM"] <- tmp6$fit[newdat$loc == "kattegatnord_whitingM"]

newdat$cpue_sd <- NA
newdat$cpue_sd[newdat$loc == "kattegatnord_codM"] <- tmp1$se.fit[newdat$loc == "kattegatnord_codM"]
newdat$cpue_sd[newdat$loc == "kattegatnord_flounderM"] <- tmp2$se.fit[newdat$loc == "kattegatnord_flounderM"]
newdat$cpue_sd[newdat$loc == "kattegatnord_plaiceM"] <- tmp3$se.fit[newdat$loc == "kattegatnord_plaiceM"]
newdat$cpue_sd[newdat$loc == "kattegatnord_herringM"] <- tmp4$se.fit[newdat$loc == "kattegatnord_herringM"]
newdat$cpue_sd[newdat$loc == "kattegatnord_spratM"] <- tmp5$se.fit[newdat$loc == "kattegatnord_spratM"]
newdat$cpue_sd[newdat$loc == "kattegatnord_whitingM"] <- tmp6$se.fit[newdat$loc == "kattegatnord_whitingM"]


newdat$cpue_lo <- newdat$cpue_pred - 1.96 * newdat$cpue_sd
newdat$cpue_up <- newdat$cpue_pred + 1.96 * newdat$cpue_sd

## individual models
tmp1.log <- predict(mod.codM, newdata = newdat, type = "link", se.fit = TRUE)
tmp2.log <- predict(mod.flounderM, newdata = newdat, type = "link", se.fit = TRUE)
tmp3.log <- predict(mod.plaiceM, newdata = newdat, type = "link", se.fit = TRUE)
tmp4.log <- predict(mod.herringM, newdata = newdat, type = "link", se.fit = TRUE)
tmp5.log <- predict(mod.spratM, newdata = newdat, type = "link", se.fit = TRUE)
tmp6.log <- predict(mod.whitingM, newdata = newdat, type = "link", se.fit = TRUE)

## for error propagations
newdat$cpue_pred.log <- NA
newdat$cpue_pred.log[newdat$loc == "kattegatnord_codM"] <- tmp1.log$fit[newdat$loc == "kattegatnord_codM"]
newdat$cpue_pred.log[newdat$loc == "kattegatnord_flounderM"] <- tmp2.log$fit[newdat$loc == "kattegatnord_flounderM"]
newdat$cpue_pred.log[newdat$loc == "kattegatnord_plaiceM"] <- tmp3.log$fit[newdat$loc == "kattegatnord_plaiceM"]
newdat$cpue_pred.log[newdat$loc == "kattegatnord_herringM"] <- tmp4.log$fit[newdat$loc == "kattegatnord_herringM"]
newdat$cpue_pred.log[newdat$loc == "kattegatnord_spratM"] <- tmp5.log$fit[newdat$loc == "kattegatnord_spratM"]
newdat$cpue_pred.log[newdat$loc == "kattegatnord_whitingM"] <- tmp6.log$fit[newdat$loc == "kattegatnord_whitingM"]
newdat$cpue_sd.log <- NA
newdat$cpue_sd.log[newdat$loc == "kattegatnord_codM"] <- tmp1.log$se.fit[newdat$loc == "kattegatnord_codM"]
newdat$cpue_sd.log[newdat$loc == "kattegatnord_flounderM"] <- tmp2.log$se.fit[newdat$loc == "kattegatnord_flounderM"]
newdat$cpue_sd.log[newdat$loc == "kattegatnord_plaiceM"] <- tmp3.log$se.fit[newdat$loc == "kattegatnord_plaiceM"]
newdat$cpue_sd.log[newdat$loc == "kattegatnord_herringM"] <- tmp4.log$se.fit[newdat$loc == "kattegatnord_herringM"]
newdat$cpue_sd.log[newdat$loc == "kattegatnord_spratM"] <- tmp5.log$se.fit[newdat$loc == "kattegatnord_spratM"]
newdat$cpue_sd.log[newdat$loc == "kattegatnord_whitingM"] <- tmp6.log$se.fit[newdat$loc == "kattegatnord_whitingM"]



  write_rds (newdat, "newdat/newdat_kattegatnordM.rds")

