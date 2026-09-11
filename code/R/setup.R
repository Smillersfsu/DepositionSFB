##load packages 

# data import and database management 
# reading files, reshaping data, and efficient data handling 


if (!requireNamespace("fitdistrplus", quietly = TRUE)) {
  install.packages("fitdistrplus")
}
#######################################################

library(readxl) # import excel spreadsheets
library(stringr) # text and string processing 
library(lubridate) # data and time handling 
library(writexl) # exports editted databases into excel files
library(moments) # skewness in the data

#######################################################

# General data science workflow 
# Collection of packages for data manipulation and plotting

#######################################################

library(tidyverse)

#######################################################

#Statistical analyses and mixed models 
# Hypothesis testing, summaries, and model fitting 

#######################################################

library(gt) # displaying formatted tables for statistics 
library(rstatix) # statistical tests and assumption checking
library(lmerTest) # p-values and tests for lme4 models 
library(glmmTMB) # for generalized linear mixed effects models specifically 
library(lme4) # linear and generalized mixed-effects models
library(ggeffects) # predicted values and marginal effects 
library(DHARMa) # for measuring residuals on the GLMM
library(fitdistrplus) # descdist()/fitdist()/gofstat() test which 
# distribution best fits raw Flux 
library(gamlss) # another version of fitdistrplus 
library(brunnermunzel) # robust Mann-Whitney alternative for when spread 
# differs between groups 



#######################################################

# Data visualization 
# Plot creation, figure assembly, and publication formatting 

#######################################################

library(patchwork) #comvine ggplots into multi-panel figures
library(cowplot) # figure arrangement and plot annotations
library(rcartocolor) # color palettes
library(ggh4x) # advanced ggplot extensions
library(scales) # axis labels, formatting, and scaling 
library(webshot2) # for uploading to PNG
library(ggdist) # for half-violin density layers for raincloud plot


#######################################################

#Tables and report outputs 
# Interactive tables and report presentation 

#######################################################

library(reactable) #interactive tables for reports
library(gt)
library(flextable)

#######################################################

# FIX: fitdistrplus Depends on MASS (not just Imports), so loading it
# silently attaches MASS to the search path as a side effect -- no
# library(MASS) anywhere in this project, but MASS::select()/filter()
# end up masking dplyr's versions anyway. Force dplyr's versions to win,
# placed here so it applies no matter what else above this point loads
# MASS as a hidden dependency.
select <- dplyr::select
filter <- dplyr::filter

