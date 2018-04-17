# Project
source("helpers.R")

url <- "https://wwwn.cdc.gov/Nchs/Nhanes/"

surveys <- c("2007-2008","2009-2010","2011-2012","2013-2014")
letters <- paste("_",LETTERS[5:8], sep = "")


# stack the surveys together
nhanes0 <- map2_df(surveys, letters, get_data)

# convert dpq series to NAs, combine to form phq
# also calculate mean of 4 spb measureements
nhanes1 <- nhanes0 %>% 
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
         sbp = mean(c(bpxsy1,bpxsy2,bpxsy3,bpxsy4), na.rm = TRUE))

# remove bpxsy* and dpq0* vars as they might have NAs 
nhanes2 <- nhanes1 %>%
  select(survey, seqn, everything(), -contains("bpxsy"), -contains("dpq0")) 

# convert NAs
nhanes3 <- nhanes2 %>% 
  mutate(female = gender - 1, # create female variable
         education = convert_to_na(education),
         college = if_else(education == 5, 1, 0),
         marital = convert_to_na(marital, c(77, 99)),
         married = if_else(marital == 1, 1, 0), # recode so 1 is married, 0 is not
         poverty = if_else(pir <= 1, 1, 0), # create poverty variable
         drinks = convert_to_na(drinks, c(777, 999)),
         heartattack = convert_to_na(heartattack),
         heartattack = 2 - heartattack, # recode so 1 is yes, 0 is no
         stroke = convert_to_na(stroke),
         stroke = 2 - stroke, # recode so 1 is yes, 0 is no
         cancer = convert_to_na(cancer),
         cancer = 2 - cancer, # recode so 1 is yes, 0 is no
         rxduse = convert_to_na(rxduse),
         rxduse = 2 - rxduse, # recode so 1 is yes, 0 is no
         modwork = convert_to_na(modwork),
         modwork = 2 - modwork, # recode so 1 is yes, 0 is no
         modrec = convert_to_na(modrec),
         modrec = 2 - modrec, # recode so 1 is yes, 0 is no
         limited = convert_to_na(limited),
         limited = 2 - limited, # recode so 1 is yes, 0 is no
         sleep = convert_to_na(sleep),
         sleep = 2 - sleep, # recode so 1 is yes, 0 is no
         smoke100 = convert_to_na(smoke100),
         smoke100 = 2 - smoke100, # recode so 1 is yes, 0 is no
         depressed = if_else(phq >= 5, 1, 0),
         highbp = if_else(sbp > 140, 1, 0),
         white = if_else(race == 3, 1, 0),
         college = if_else(education == 5, 1, 0),
         elderly = if_else(age >= 65, 1, 0),
         diabetic = if_else(glucose >= 100, 1, 0),
         overwt = if_else(bmi >= 25, 1, 0))

# create bmi cuts
nhanes3$bmicat <- cut(nhanes3$bmi, c(0, 18.5, 25, 30, 70),
                     labels = 1:4, right = FALSE)

# create age cuts
nhanes3$agecat <- cut(nhanes3$age, c(20, 45, 65, 90),
                     labels = 1:3, right = FALSE)

# relevel race factor so white is reference
nhanes3$race <- factor(nhanes3$race)
nhanes3$race <- relevel(nhanes3$race, ref = 3)

# set up alcohol use risk
# https://www.niaaa.nih.gov/alcohol-health/overview-alcohol-consumption/moderate-binge-drinking
nhanes3$highdrink <- if_else(
  nhanes3$gender == 0,
  if_else(nhanes3$drinks > 1, 1, 0),
  if_else(nhanes3$drinks >= 2, 1, 0)
)

# combine diseases into one variable
nhanes3$disease <- rowSums(nhanes3[c("heartattack","stroke","cancer")])
nhanes3$disease <- if_else(nhanes3$disease > 0, 1, 0)

# keep complete cases
nhanes4 <- nhanes3[complete.cases(nhanes3),]

# for UNE GPH 716 Summer 2017
set.seed(10)
rows <- sample(1:nrow(nhanes4),1200)
nhanes5 <- nhanes4[rows,] %>% arrange(survey,seqn) %>%
  select(-female, -dpqna) %>%
  mutate(sbp = round(sbp))
write.csv(nhanes5, "nhanes716.csv", row.names = FALSE)


write.csv(nhanes4, "nhanes.csv", row.names = FALSE)
