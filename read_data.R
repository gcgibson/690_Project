# Project
source("helpers.R")

url <- "https://wwwn.cdc.gov/Nchs/Nhanes/"

surveys <- c("2007-2008","2009-2010","2011-2012","2013-2014","2015-2016")
letters <- paste("_",LETTERS[5:9], sep = "")

# stack the surveys together
nhanes0 <- map2_df(surveys, letters, get_data)

# write raw data csv for 690 project
write.csv(nhanes0, "nhanes0.csv", row.names = FALSE)
