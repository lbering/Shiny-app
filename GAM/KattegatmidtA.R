###########################################
############------kattegatmidt------############
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
mod.codA <- gam(cpue ~ #response
                  period+
                  s(depth,k=15) + 
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                data = subset(dat_cod, loc == "kattegatmidt_codA"), 
                family=nb) 
mod.codA <- get.mini.model(mod.codA, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------FLOUNDER -------#
mod.flounderA <- gam(cpue ~ #response
                       period+
                  s(depth,k=15) + 
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                data = subset(dat_flounder, loc == "kattegatmidt_flounderA"), 
                family=nb) 
mod.flounderA <- get.mini.model(mod.flounderA, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))
#--------PLAICE -------#
mod.plaiceA <- gam(cpue ~ #response
                     period+
                  s(depth,k=5) +
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                data = subset(dat_plaice, loc == "kattegatmidt_plaiceA"),
                family=tw) 
mod.plaiceA <- get.mini.model(mod.plaiceA, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------HERRING -------#
mod.herringA <- gam(cpue ~ #response
                      period+
                  s(depth,k=15) +
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                data = subset(dat_herring, loc == "kattegatmidt_herringA"), 
                family=tw) 
mod.herringA <- get.mini.model(mod.herringA, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------SPRAT -------#
mod.spratA <- gam(cpue ~ #response
                    period+
                  s(depth,k=5) + 
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                data = subset(dat_sprat, loc == "kattegatmidt_spratA"), 
                family=tw) 
mod.spratA <- get.mini.model(mod.spratA, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))

#--------WHITING -------#
mod.whitingA<- gam(cpue ~ #response
                     period+
                  s(depth,k=15) + 
                  s(Gear,bs="re")+ 
                  Quarter+ 
                  SedimentDK, 
                data = subset(dat_whiting, loc == "kattegatmidt_whitingA"), 
                family=tw) 
mod.whitingA <- get.mini.model(mod.whitingA, check.terms = c("period","depth",
                                                     "Quarter","SedimentDK"))
############################### Predict CPUE ############################### 
#depth.by.loc <- by(dat_cod$depth, dat_cod$loc, median)

newdat <- expand.grid(period = levels(dat_cod$period),
                      loc = c("kattegatmidt_codA","kattegatmidt_flounderA",
                              "kattegatmidt_plaiceA","kattegatmidt_herringA",
                              "kattegatmidt_spratA","kattegatmidt_whitingA"),
                      depth = 19, 
                      Gear=c("TVS"),
                      Quarter=c("4"),
                      SedimentDK=c("5")
)
#to check the depth of each location: 
#newdat$depth <- depth.by.loc[match(newdat$loc,names(depth.by.loc))] #overwrite for each location
## individual models
tmp1 <- predict(mod.codA, newdata = newdat, type = "response", se.fit = TRUE)
tmp2 <- predict(mod.flounderA, newdata = newdat, type = "response", se.fit = TRUE)
tmp3 <- predict(mod.plaiceA, newdata = newdat, type = "response", se.fit = TRUE)
tmp4 <- predict(mod.herringA, newdata = newdat, type = "response", se.fit = TRUE)
tmp5 <- predict(mod.spratA, newdata = newdat, type = "response", se.fit = TRUE)
tmp6 <- predict(mod.whitingA, newdata = newdat, type = "response", se.fit = TRUE)

#make new data predictions
newdat$cpue_pred <- NA
newdat$cpue_pred[newdat$loc == "kattegatmidt_codA"] <- tmp1$fit[newdat$loc == "kattegatmidt_codA"]
newdat$cpue_pred[newdat$loc == "kattegatmidt_flounderA"] <- tmp2$fit[newdat$loc == "kattegatmidt_flounderA"]
newdat$cpue_pred[newdat$loc == "kattegatmidt_plaiceA"] <- tmp3$fit[newdat$loc == "kattegatmidt_plaiceA"]
newdat$cpue_pred[newdat$loc == "kattegatmidt_herringA"] <- tmp4$fit[newdat$loc == "kattegatmidt_herringA"]
newdat$cpue_pred[newdat$loc == "kattegatmidt_spratA"] <- tmp5$fit[newdat$loc == "kattegatmidt_spratA"]
newdat$cpue_pred[newdat$loc == "kattegatmidt_whitingA"] <- tmp6$fit[newdat$loc == "kattegatmidt_whitingA"]

newdat$cpue_sd <- NA
newdat$cpue_sd[newdat$loc == "kattegatmidt_codA"] <- tmp1$se.fit[newdat$loc == "kattegatmidt_codA"]
newdat$cpue_sd[newdat$loc == "kattegatmidt_flounderA"] <- tmp2$se.fit[newdat$loc == "kattegatmidt_flounderA"]
newdat$cpue_sd[newdat$loc == "kattegatmidt_plaiceA"] <- tmp3$se.fit[newdat$loc == "kattegatmidt_plaiceA"]
newdat$cpue_sd[newdat$loc == "kattegatmidt_herringA"] <- tmp4$se.fit[newdat$loc == "kattegatmidt_herringA"]
newdat$cpue_sd[newdat$loc == "kattegatmidt_spratA"] <- tmp5$se.fit[newdat$loc == "kattegatmidt_spratA"]
newdat$cpue_sd[newdat$loc == "kattegatmidt_whitingA"] <- tmp6$se.fit[newdat$loc == "kattegatmidt_whitingA"]


newdat$cpue_lo <- newdat$cpue_pred - 1.96 * newdat$cpue_sd
newdat$cpue_up <- newdat$cpue_pred + 1.96 * newdat$cpue_sd

## individual models
tmp1.log <- predict(mod.codA, newdata = newdat, type = "link", se.fit = TRUE)
tmp2.log <- predict(mod.flounderA, newdata = newdat, type = "link", se.fit = TRUE)
tmp3.log <- predict(mod.plaiceA, newdata = newdat, type = "link", se.fit = TRUE)
tmp4.log <- predict(mod.herringA, newdata = newdat, type = "link", se.fit = TRUE)
tmp5.log <- predict(mod.spratA, newdata = newdat, type = "link", se.fit = TRUE)
tmp6.log <- predict(mod.whitingA, newdata = newdat, type = "link", se.fit = TRUE)

## for error propagations
newdat$cpue_pred.log <- NA
newdat$cpue_pred.log[newdat$loc == "kattegatmidt_codA"] <- tmp1.log$fit[newdat$loc == "kattegatmidt_codA"]
newdat$cpue_pred.log[newdat$loc == "kattegatmidt_flounderA"] <- tmp2.log$fit[newdat$loc == "kattegatmidt_flounderA"]
newdat$cpue_pred.log[newdat$loc == "kattegatmidt_plaiceA"] <- tmp3.log$fit[newdat$loc == "kattegatmidt_plaiceA"]
newdat$cpue_pred.log[newdat$loc == "kattegatmidt_herringA"] <- tmp4.log$fit[newdat$loc == "kattegatmidt_herringA"]
newdat$cpue_pred.log[newdat$loc == "kattegatmidt_spratA"] <- tmp5.log$fit[newdat$loc == "kattegatmidt_spratA"]
newdat$cpue_pred.log[newdat$loc == "kattegatmidt_whitingA"] <- tmp6.log$fit[newdat$loc == "kattegatmidt_whitingA"]
newdat$cpue_sd.log <- NA
newdat$cpue_sd.log[newdat$loc == "kattegatmidt_codA"] <- tmp1.log$se.fit[newdat$loc == "kattegatmidt_codA"]
newdat$cpue_sd.log[newdat$loc == "kattegatmidt_flounderA"] <- tmp2.log$se.fit[newdat$loc == "kattegatmidt_flounderA"]
newdat$cpue_sd.log[newdat$loc == "kattegatmidt_plaiceA"] <- tmp3.log$se.fit[newdat$loc == "kattegatmidt_plaiceA"]
newdat$cpue_sd.log[newdat$loc == "kattegatmidt_herringA"] <- tmp4.log$se.fit[newdat$loc == "kattegatmidt_herringA"]
newdat$cpue_sd.log[newdat$loc == "kattegatmidt_spratA"] <- tmp5.log$se.fit[newdat$loc == "kattegatmidt_spratA"]
newdat$cpue_sd.log[newdat$loc == "kattegatmidt_whitingA"] <- tmp6.log$se.fit[newdat$loc == "kattegatmidt_whitingA"]


  write_rds (newdat, "newdat/newdat_kattegatmidtA.rds")
