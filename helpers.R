# packages used
library(MASS)
library(Hmisc)
library(tidyverse)
library(purrr)
library(broom)
library(haven)
library(stringr)
library(caret)

# helper functions

# convert NAs coded with one of 2 int values
convert_to_na <- function(x, nas = c(7,9)){
  x <- na_if(x, nas[1])
  na_if(x, nas[2])
}

# join two data frames on id
join_by_id <- function(df1, df2, id= "seqn"){
  inner_join(df1, df2, by = id)
}

# read data from NHANES website
get_data <- function(survey, letter){
  demo <- sasxport.get(paste(url,survey,"/DEMO",letter,".XPT",sep=""))
  bmx <- sasxport.get(paste(url,survey,"/BMX",letter,".XPT",sep=""))
  bpx <- sasxport.get(paste(url,survey,"/BPX",letter,".XPT",sep=""))
  ghb <- sasxport.get(paste(url,survey,"/GHB",letter,".XPT",sep=""))
  hdl <- sasxport.get(paste(url,survey,"/HDL",letter,".XPT",sep=""))
  tchol <- sasxport.get(paste(url,survey,"/TCHOL",letter,".XPT",sep=""))
  bpq <- sasxport.get(paste(url,survey,"/BPQ",letter,".XPT",sep=""))
  diq <- sasxport.get(paste(url,survey,"/DIQ",letter,".XPT",sep=""))
  alq <- sasxport.get(paste(url,survey,"/ALQ",letter,".XPT",sep=""))
  mcq <- sasxport.get(paste(url,survey,"/MCQ",letter,".XPT",sep=""))
  # rxq_rx <- sasxport.get(paste(url,survey,"/RXQ_RX",letter,".XPT",sep=""))
  # we may not need dpq? it's for depression
  dpq <- sasxport.get(paste(url,survey,"/DPQ",letter,".XPT",sep=""))
  inq <- sasxport.get(paste(url,survey,"/INQ",letter,".XPT",sep=""))
  paq <- sasxport.get(paste(url,survey,"/PAQ",letter,".XPT",sep=""))
  pfq <- sasxport.get(paste(url,survey,"/PFQ",letter,".XPT",sep=""))
  slq <- sasxport.get(paste(url,survey,"/SLQ",letter,".XPT",sep=""))
  smq <- sasxport.get(paste(url,survey,"/SMQ",letter,".XPT",sep=""))
  trigly <-  sasxport.get(paste(url,survey,"/TRIGLY",letter,".XPT",sep=""))
  datalist <- list(demo, bmx, bpx, ghb, hdl, tchol, bpq, diq, alq,
                   mcq, dpq, inq, paq, pfq, slq, smq, trigly) # rxq_rx, 
  Reduce(join_by_id, datalist) %>% mutate(survey = survey)
}
