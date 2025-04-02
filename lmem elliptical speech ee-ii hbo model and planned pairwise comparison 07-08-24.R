    #LMEM analysis script for fNIRS Beta Values 
    #elliptical speech ee-ii hbo model
    #dummy coding and planned pairwise comparison
    #CDT & MJS
rm(list = ls())

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(pbkrtest)
library(robustlmm)
library(dplyr)
library(ggExtra)
library(gridExtra)

#load dataframe
df_5sec <- read.csv("/Users/coletrent/Library/CloudStorage/Box-Box/BRAiN Lab/current projects/elliptical-speech-project/code/LMEM/csv/elliptical_speech_GLM_output_5-sec-boxcar_ee-ii_LR-auditory.csv")

    #increase model iterations
control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2000000))

    #factor categorical variables
df_5sec$Vocoder <- as.factor(df_5sec$Vocoder)
df_5sec$ID <- as.factor(df_5sec$ID)
df_5sec$Condition <- as.factor(df_5sec$Condition)
df_5sec$Chroma <- as.factor(df_5sec$Chroma)
df_5sec$ROI <- as.factor(df_5sec$ROI)

df_5sec <- filter(df_5sec, Condition == "ee"|
                    Condition == "ii")


df_5sec_hbo <- filter(df_5sec, Chroma == "hbo")

df_5sec_hbo$Vocoder = relevel(df_5sec_hbo$Vocoder, ref = "16")
df_5sec_hbo$Condition = relevel(df_5sec_hbo$Condition, ref = "ii")
df_5sec_hbo$ROI = relevel(df_5sec_hbo$ROI, ref = "DLPFC_roi")


    #interaction only model - no intercept
mod_full_5sec_hbo <- lmer(theta ~ -1 + ROI:Condition:Vocoder + (1|ID), 
                        data = df_5sec_hbo, 
                        control = control)

summary(mod_full_5sec_hbo)


  #creating HBR model for supplamental material
df_5sec_hbr <- filter(df_5sec, Chroma == "hbr")

df_5sec_hbr$Vocoder = relevel(df_5sec_hbr$Vocoder, ref = "16")
df_5sec_hbr$Condition = relevel(df_5sec_hbr$Condition, ref = "ii")
df_5sec_hbr$ROI = relevel(df_5sec_hbr$ROI, ref = "DLPFC_roi")


#interaction only model - no intercept
mod_full_5sec_hbr <- lmer(theta ~ -1 + ROI:Condition:Vocoder + (1|ID), 
                          data = df_5sec_hbr, 
                          control = control)

summary(mod_full_5sec_hbr)















emm <- emmeans(mod_full_5sec_hbo, list(~Condition + ROI + Vocoder))
emm

con_stim_con <- list(
  ii_ee_DLPFC_16 = c(1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  ii_ee_LA_16 = c(0,0,1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  ii_ee_MFG_16 = c(0,0,0,0,1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  ii_ee_PM_16 = c(0,0,0,0,0,0,1,-1,0,0,0,0,0,0,0,0,0,0,0,0),
  ii_ee_RA_16 = c(0,0,0,0,0,0,0,0,1,-1,0,0,0,0,0,0,0,0,0,0),
  ii_ee_DLPFC_4 = c(0,0,0,0,0,0,0,0,0,0,1,-1,0,0,0,0,0,0,0,0),
  ii_ee_LA_4 = c(0,0,0,0,0,0,0,0,0,0,0,0,1,-1,0,0,0,0,0,0),
  ii_ee_MFG_4 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,-1,0,0,0,0),
  ii_ee_PM_4 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,-1,0,0),
  ii_ee_RA_4 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,-1),
  ii_DLPFC_16_4 = c(1,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0),
  ii_LA_16_4 = c(0,0,1,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0),
  ii_MFG_16_4 = c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0),
  ii_PM_16_4 = c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,-1,0,0,0),
  ii_RA_16_4 = c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,-1,0),
  ee_DLPFC_16_4 = c(0,1,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0),
  ee_LA_16_4 = c(0,0,0,1,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0),
  ee_MFG_16_4 = c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,-1,0,0,0,0),
  ee_PM_16_4 = c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,-1,0,0),
  ee_RA_16_4 = c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,-1)
)

posthoc <- emmeans(mod_full_5sec_hbo, ~ Condition + ROI + Vocoder, contr = con_stim_con, adjust = "BH")
posthoc


