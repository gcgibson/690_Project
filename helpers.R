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
  demo <- sasxport.get(paste(url,survey,"/DEMO",letter,".XPT",sep="")) %>%
    select(seqn, riagendr, ridageyr, ridreth1, dmdeduc2, dmdmartl, indfmpir)
  bmx <- sasxport.get(paste(url,survey,"/BMX",letter,".XPT",sep="")) %>%
    select(seqn, bmxbmi, bmxht, bmxwt, bmxwaist)
  bpx <- sasxport.get(paste(url,survey,"/BPX",letter,".XPT",sep="")) %>%
    select(seqn, starts_with("bpxsy"), starts_with("bpxdi"), bpxpuls)
  ghb <- sasxport.get(paste(url,survey,"/GHB",letter,".XPT",sep="")) %>%
    select(seqn, lbxgh)
  hdl <- sasxport.get(paste(url,survey,"/HDL",letter,".XPT",sep="")) %>%
    select(seqn, lbdhdd)
  tchol <- sasxport.get(paste(url,survey,"/TCHOL",letter,".XPT",sep="")) %>%
    select(seqn, lbxtc)
  bpq <- sasxport.get(paste(url,survey,"/BPQ",letter,".XPT",sep=""))
  alq <- sasxport.get(paste(url,survey,"/ALQ",letter,".XPT",sep="")) %>%
    select(seqn, alq130)
  mcq <- sasxport.get(paste(url,survey,"/MCQ",letter,".XPT",sep="")) %>%
    select(seqn, mcq160e, mcq160f, mcq220)
  # rxq_rx <- sasxport.get(paste(url,survey,"/RXQ_RX",letter,".XPT",sep=""))
  # we may not need dpq? it's for depression
  dpq <- sasxport.get(paste(url,survey,"/DPQ",letter,".XPT",sep="")) %>%
    select(seqn, dpq010:dpq090)
  inq <- sasxport.get(paste(url,survey,"/INQ",letter,".XPT",sep=""))
  paq <- sasxport.get(paste(url,survey,"/PAQ",letter,".XPT",sep="")) %>%
    select(seqn, paq620, paq665)
  pfq <- sasxport.get(paste(url,survey,"/PFQ",letter,".XPT",sep="")) %>%
    select(seqn, pfq059)
  slq <- sasxport.get(paste(url,survey,"/SLQ",letter,".XPT",sep="")) %>%
    select(seqn, slq060)
  smq <- sasxport.get(paste(url,survey,"/SMQ",letter,".XPT",sep="")) %>%
    select(seqn, smq020)
  trigly <-  sasxport.get(paste(url,survey,"/TRIGLY",letter,".XPT",sep="")) %>%
    select(seqn, lbxtr, lbdldl)
  datalist <- list(demo, bmx, bpx, ghb, tchol, alq, bpq, mcq, dpq, 
                   inq, paq, pfq, slq, smq, trigly) # rxq_rx, 
  Reduce(join_by_id, datalist) %>% mutate(survey = survey)
}
