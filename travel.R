# Transportation Visualizations
library(tidyverse)

travel <- read_csv("data/faps_household_puf.csv")
travel <- travel %>%
  select(rural, targetgroup, pctpovguidehh_r, anyvehicle, vehiclenum, caraccess, snapnowhh, adltfscat, primstoretravelmode)
