## Picarro Processing Script

## This script cleans the raw Picarro data and extracts CO2/CH4 concentrations (ppm)
## and computes gas fluxes
## This script load the {picarro.data} package and `source` additional functions from the picarro_data.R script.

## Code created by BBL
## edits by KFP, May 2022


####################### #
####################### #

# 1. setup ----
## load packages
library(tidyverse) # to clean/tidy data, and plot data
library(lubridate) # to work with dates

# My 'picarro.data' package isn't on CRAN (yet) so need to install it via:
# devtools::install_github("PNNL-TES/picarro.data")
library(picarro.data)

## load the picarro processing functions
source("code/1a-picarro_data.R")

## set Picarro Path
# this is the directory where the Picarro data files are being stored
PICARROPATH = "1-data/picarro_data"


#
# 2. process the Picarro data ----
core_key = googlesheets4::read_sheet("1wLWthk7H5SQjXyM6tjlxBnPgJUJgKeYPOMupw9Tx65k") %>% mutate(Core = as.character(Core))

valve_key_df = googlesheets4::read_sheet("1PIAhiXA64Dk6BHql1hwvmxptAYcvl4-1ywCc5bOZSjU")
valve_key = 
  valve_key_df %>% 
  dplyr::select(-c(Start_Date, Start_Time, Stop_Date, Stop_Time)) %>% 
  filter(!is.na(Start_datetime)) %>% 
  mutate(Core = as.character(Core),
         Start_datetime = ymd_hms(Start_datetime),
         Stop_datetime = ymd_hms(Stop_datetime)) %>%
  left_join(core_key %>% mutate(Core = as.character(Core)))


# import and combine all the Picarro (raw) data files  
picarro_raw = sapply(list.files(path = PICARROPATH, pattern = "dat$", recursive = TRUE,full.names = TRUE),
                     read.table, header=TRUE, simplify = FALSE) %>% bind_rows()  

# clean the Picarro data
picarro_clean = clean_picarro_data(picarro_raw)

# Match Picarro data with the valve key data
pcm = match_picarro_data(pd = picarro_clean, valve_key)
picarro_clean_matched = pcm$pd
picarro_match_count = pcm$pmc
valve_key_match_count = pcm$vkmc


# get CO2 concentrations (ppm)
ghg_ppm =
  subset(merge(picarro_clean_matched, 
               valve_key %>% dplyr::select(Core, Start_datetime, Stop_datetime, Treatment)),
         DATETIME <= Stop_datetime & DATETIME >= Start_datetime & Core == Core) %>% 
  dplyr::select(-Start_datetime, -Stop_datetime) %>% 
  left_join(core_key) %>% 
  dplyr::select(Core, DATETIME, MPVPosition, CH4_dry, CO2_dry, Elapsed_seconds, 
                Core_assignment)


# compute fluxes
ghg_fluxes = compute_ghg_fluxes(picarro_clean_matched, valve_key)


#
# 3. make graphs ----

ghg_ppm %>% 
  ggplot(aes(x = DATETIME, y = CO2_dry))+
  geom_point()+
  facet_wrap(~Core_assignment)  






# ----


qc3 = qc_fluxes(ghg_fluxes, valve_key)

gf = 
  ghg_fluxes %>% 
  left_join(core_key) %>% 
  filter(flux_co2_umol_s >= 0) %>% 
  # remove outliers
  group_by(Core_assignment) %>% 
  dplyr::mutate(mean = mean(flux_co2_umol_s),
                median = median(flux_co2_umol_s),
                sd = sd(flux_co2_umol_s)) 


gf %>% 
  ggplot(aes(x = DATETIME, y = flux_co2_umol_s))+
  geom_point()+
  facet_wrap(~Core_assignment)



ungroup %>% 
  dplyr::mutate(outlier = flux_co2_umol_g_s - mean > 4 * sd)

gf_no_outliers = dplyr::filter(gf, !outlier)

gf_output =
  subset(merge(gf, valve_key %>% dplyr::select(Core, Start_datetime, Stop_datetime, Treatment)), 
         DATETIME <= Stop_datetime & DATETIME >= Start_datetime & Core == Core) %>% 
  dplyr::select(-mean,-median, -sd, -Start_datetime, -Stop_datetime, -outlier)


#summarizing  
cum_flux = 
  gf_no_outliers %>%
  group_by(Core) %>% 
  dplyr::summarise(cum = sum(flux_co2_umol_g_s),
                   max = max(flux_co2_umol_g_s),
                   #cumC = sum(flux_co2_umol_gC_s),
                   #maxC = max(flux_co2_umol_gC_s),
                   mean = mean(flux_co2_umol_g_s),
                   #meanC = mean(flux_co2_umol_gC_s),
                   median = median(flux_co2_umol_g_s),
                   #medianC = median(flux_co2_umol_gC_s),
                   sd = sd(flux_co2_umol_g_s),
                   #sdC = sd(flux_co2_umol_gC_s),
                   cv = sd/mean,
                   #cvC = sdC/meanC,
                   se = sd/sqrt(n()),
                   n = n()) %>% 
  left_join(core_key, by = "Core"
  )

meanflux = 
  cum_flux %>% 
  group_by(Site, drying, length) %>% 
  dplyr::summarize(cum = mean(cum),
                   max = mean(max),
                   #cumC = mean(cumC),
                   #maxC = mean(maxC),
                   mean = mean(mean),
                   #meanC = mean(meanC),
                   median = mean(median),
                   #medianC = mean(medianC)
  )
