library(dplyr)

eb_pums <- eb_pums %>% select(year, metaread, puma, 
                              ownershp, builtyr, hhincome, 
                              perwt, age, sex, race, hispan, 
                              tranwork, carpool, trantime)



#Note: People of any race can also be Hispanic (Hispanic is not a race category), 
#so when you recode Hispanics as ???4???, they will no longer be coded in the ???White,??? ???Black,??? or ???Asian??? categories. 
#Rename a variable: metaread to detailed.meta.area
names(eb_pums)[2] <- 'detailed.meta.area'

eb_pums <- eb_pums %>% mutate(
  race=as.character(race), 
  hispan=as.character(hispan),
  racehisp=case_when(
    race=="Chinese" ~ "Asian", 
    race=="Japanese" ~ "Asian", 
    race=="Other Asian or Pacific" ~ "Asian",
    race=="White" ~ "White", 
    race=="Black" ~ "Black", 
    race=="American Indian or Alaskan" ~ "American Indian or Alaskan", 
    hispan != "Not Hispanic" ~ "Hispanic",
    TRUE~"Other"))

table(eb_pums$racehisp)

# recode modes (https://usa.ipums.org/usa-action/variables/TRANWORK#codes_section)
eb_pums <- eb_pums %>%
  mutate(
    tranwork = as.integer(tranwork),
    mode = case_when(
      tranwork %in% c(10, 11, 12, 13, 14, 15, 20) ~ "driving",
      tranwork %in% c(30, 31, 32, 33, 34, 35, 36) ~ "transit",
      tranwork %in% c(40, 50) ~ "bike/walk",
      tranwork %in% c(60, 70) ~ "other",
      TRUE ~ as.character(NA)
    )
  )

table(eb_pums$mode)

# recode modes (https://usa.ipums.org/usa-action/variables/TRANWORK#codes_section)
eb_pums <- eb_pums %>%
  mutate(
    builtyr2 = case_when(
      as.integer(builtyr) %in% c(2, 3) ~ "0-10",
      as.integer(builtyr) == 4 ~ "11-20",
      as.integer(builtyr) == 5 ~ "21-30",
      as.integer(builtyr) == 6 ~ "31-40",
      as.integer(builtyr) == 7 ~ "41-50",
      as.integer(builtyr) == 8 ~ "51-60",
      as.integer(builtyr) == 9 ~ "61+"
    )
  )

table(eb_pums$builtyr2)

## Recode income and tenure

eb_pums <- eb_pums %>%
  mutate(
    increc = ifelse(eb_pums$hhincome <=0 | eb_pums$hhincome >= 999999, NA, hhincome),
    tenure = recode(ownershp, `1`="Owned", `2`="Rented", .default=as.character(NA))
  )


table(eb_pums$tenure)