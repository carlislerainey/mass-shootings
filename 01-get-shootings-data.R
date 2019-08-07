
# load packages
library(tidyverse)

# download shootings data from mother jones 
# https://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/
df <- rio::import("https://docs.google.com/spreadsheets/d/1b9o6uDO18sLxBqPwl_Gh9bnhW-ev_dABH83M5Vb5L8o/htmlview?sle=true#gid=0")

# write data to file
write_csv(df, "shootings-raw.csv")
