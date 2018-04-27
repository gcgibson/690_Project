# Project
source("helpers.R")

DOWNLOADED <- FALSE
if (!DOWNLOADED){
  url <- "https://wwwn.cdc.gov/Nchs/Nhanes/"
  
  surveys <- c("2007-2008","2009-2010","2011-2012","2013-2014")
  letters <- paste0("_",LETTERS[5:8])
  
  # stack the surveys together
  nhanes0 <- map2_df(surveys, letters, get_data)
  
  # write raw data csv for 690 project
  write.csv(nhanes0, "nhanes0.csv", row.names = FALSE)
} else{
  nhanes0 <- read.csv("nhanes0.csv")
}

nhanes1 <- nhanes0 %>%
  select(seqn, gender = riagendr, age = ridageyr, race = ridreth1, 
         education = dmdeduc2, dmdmartl, pir = indfmpir,
         bmi = bmxbmi, height = bmxht, weight = bmxwt, waistcirc = bmxwaist,
         starts_with("bpxsy"), starts_with("bpxdi"), pulsetype = bpxpuls,
         gh = lbxgh, hdl = lbdhdd, totchol = lbxtc, diabetes = diq010,
         drinks = alq130, chd = mcq160c, heartattack = mcq160e, stroke = mcq160f,
         cancer = mcq220, dpq010:dpq090, modworkact = paq620, modrecract = paq665,
         limited = pfq059, sleephrs = sld010h,
         sleepdisorder = slq060, smoke100 = smq020, smoker = smq040,
         triglyceride = lbxtr, ldl = lbdldl)

# convert dpq series to NAs, combine to form phq
# also calculate mean of 4 spb measureements
nhanes2 <- nhanes1 %>% 
  # repeated seqn, so discard repeats
  distinct(seqn, .keep_all = TRUE) %>%
  mutate(dpq010 = convert_to_na(dpq010),
         dpq020 = convert_to_na(dpq020),
         dpq030 = convert_to_na(dpq030),
         dpq040 = convert_to_na(dpq040),
         dpq050 = convert_to_na(dpq050),
         dpq060 = convert_to_na(dpq060),
         dpq070 = convert_to_na(dpq070),
         dpq080 = convert_to_na(dpq080),
         dpq090 = convert_to_na(dpq090)) %>%
  rowwise() %>%
  mutate(dpqna = sum(is.na(c(dpq010,dpq020,dpq030,dpq040,dpq050,
                             dpq060,dpq070,dpq080,dpq090))),
         phq = sum(c(dpq010,dpq020,dpq030,dpq040,dpq050,
                     dpq060,dpq070,dpq080,dpq090)),
         sbp = mean(c(bpxsy1,bpxsy2,bpxsy3,bpxsy4), na.rm = TRUE),
         dbp = mean(c(bpxdi1,bpxdi2,bpxdi3,bpxdi4), na.rm = TRUE))
  
  
