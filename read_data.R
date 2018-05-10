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

dim(nhanes0)

nhanes1 <- nhanes0 %>%
  select(seqn, gender = riagendr, age = ridageyr, race = ridreth1, 
         education = dmdeduc2, marital = dmdmartl, pir = indfmpir,
         bmi = bmxbmi, height = bmxht, weight = bmxwt, waistcirc = bmxwaist,
         starts_with("bpxsy"), starts_with("bpxdi"), pulsetype = bpxpuls,
         gh = lbxgh, hdl = lbdhdd, totchol = lbxtc, diabetes = diq010,
         drinks = alq130, chd = mcq160c, heartattack = mcq160e, stroke = mcq160f,
         cancer = mcq220, dpq010:dpq090, modworkact = paq620, modrecract = paq665,
         limited = pfq059, sleephrs = sld010h,
         sleepdisorder = slq060, smoke100 = smq020, smoker = smq040,
         triglyceride = lbxtr, ldl = lbdldl)

dim(nhanes1)

write.csv(nhanes1, "nhanes1.csv", row.names = FALSE)

# convert dpq series to NAs, combine to form phq
# also calculate mean of 4 spb, dbp measureements
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
         dpq090 = convert_to_na(dpq090),
         education = convert_to_na(education),
         diabetes = convert_to_na(diabetes),
         marital = convert_to_na(marital, c(77,99)),
         drinks = convert_to_na(drinks,c(777,999)),
         chd = convert_to_na(chd),
         heartattack = convert_to_na(heartattack),
         stroke = convert_to_na(stroke),
         cancer = convert_to_na(cancer),
         modworkact = convert_to_na(modworkact),
         modrecract = convert_to_na(modrecract),
         limited = convert_to_na(limited),
         sleephrs = convert_to_na(sleephrs,c(77,99)),
         sleepdisorder = convert_to_na(sleepdisorder),
         smoke100 = convert_to_na(smoke100)) %>%
  rowwise() %>%
  mutate(dpqna = sum(is.na(c(dpq010,dpq020,dpq030,dpq040,dpq050,
                             dpq060,dpq070,dpq080,dpq090))),
         phq = sum(c(dpq010,dpq020,dpq030,dpq040,dpq050,
                     dpq060,dpq070,dpq080,dpq090)),
         sbp = mean(c(bpxsy1,bpxsy2,bpxsy3,bpxsy4), na.rm = TRUE),
         dbp = mean(c(bpxdi1,bpxdi2,bpxdi3,bpxdi4), na.rm = TRUE)) %>%
  select(-starts_with("dpq0"),-starts_with("bpx"))

# create bmi cuts
nhanes2$bmicat <- cut(nhanes2$bmi, c(0, 18.5, 25, 30, 70),
                      labels = 1:4, right = FALSE)

# create age cutsr
nhanes2$agecat <- cut(nhanes2$age, c(20, 45, 65, 90),
                      labels = 1:3, right = FALSE)

# create pir cuts
# http://jamanetwork.com/journals/jamapediatrics/fullarticle/191114
nhanes2$pircat <- cut(nhanes2$pir, c(0, 1.3, 3, 6),
                      labels = 1:3, right = FALSE)  

write.csv(nhanes2, "nhanes2.csv", row.names = FALSE)
