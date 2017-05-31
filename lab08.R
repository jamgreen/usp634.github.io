## ---- message=FALSE, warning=FALSE, include=FALSE------------------------

pums <- readRDS("eb_pums_2000.rds")

# create a subset copy of the original data to work on 
pums = subset(pums, select=c(year, metaread, puma, ownershp, builtyr, hhincome, perwt, age, sex, race, hispan, tranwork, carpool, trantime))

#Rename a variable: metaread to detailed.meta.area
names(pums)[2] = 'detailed.meta.area'

# the code book for pums data can be found at https://usa.ipums.org/usa-action/variables/group?id=race_eth
pums$racehisp = NA  
pums$racehisp[pums$race =='White']=1 



#Check your work ???? first 30 rows of dataset. A faster way to check your data
pums[1:30,c('race','hispan', 'racehisp')]

#Recode African-American. Enter a value of ???2??? for those cases where race is African-American: 
pums$racehisp[as.integer(pums$race) ==2]=2  

#Recode Asian.  To recode the 3 Asian categories to ???3???, enter (the ???|??? sign indicates ???or???): 
pums$racehisp[as.integer(pums$race) ==4 | as.integer(pums$race) ==5 | as.integer(pums$race) ==6]=3 
#
#OR, use the following command:
#pums$racehisp[as.integer(pums$race) %in% c(3,4,5)] = 3

#Recode Hispanic.  Code all Hispanic subcategories as ???4??? (???!=??? means ???does not equal???): 
pums$racehisp[as.integer(pums$hispan)!=1] = 4 

#Note: People of any race can also be Hispanic (Hispanic is not a race category), 
#so when you recode Hispanics as ???4???, they will no longer be coded in the ???White,??? ???Black,??? or ???Asian??? categories. 

#Recode Other Race.  Create the residual ???Other??? category: 
pums$racehisp[is.na(pums$racehisp)] = 5


#Label your new race categories with text labels.
#Use the following command to apply text labels to your new race categories: 
pums$racehisp = factor(pums$racehisp, levels=1:5, 
                       labels=c('White non-Hisp', 'Black non-Hisp', 'Asian non-Hisp', 'Hispanic', 'Other'))


# recode modes
pums$mode = as.integer(pums$carpool)  # this creates a copy of "carpool" with drive-alone & carpool commuters
pums$mode[as.integer(pums$tranwork) %in% c(10, 11, 12, 13, 14, 15)] = 3  # this creates a "transit" category
pums$mode[as.integer(pums$tranwork) %in% c(16, 17)] = 4  # this creates "bike/walk"
pums$mode[as.integer(pums$tranwork) %in% c(8, 18, 19)] = 5 # this creates "other"
pums$mode[pums$mode==0] = NA  #this sets NA where the original "carpool" variable to missing
pums$mode = factor(pums$mode, levels=1:5, labels=c('Drive alone', 'Carpool', 'Transit', 'Bike/walk', 'Other')) 
comment(pums$mode) = "Mode of transportation to work (recoded from carpool and tranwork)"

# recode builtyr variable
pums$builtyr2 = as.integer(pums$builtyr)  # copy of builtyr
pums$builtyr2[as.integer(pums$builtyr) %in% c(2, 3)]=1 #recodes first two categories to create 0-10 years
pums$builtyr2[as.integer(pums$builtyr)==4]=2 #set numeric codes so they are equivalent to # decades
pums$builtyr2[as.integer(pums$builtyr)==5]=3
pums$builtyr2[as.integer(pums$builtyr)==6]=4
pums$builtyr2[as.integer(pums$builtyr)==7]=5
pums$builtyr2[as.integer(pums$builtyr)==8]=6
pums$builtyr2[as.integer(pums$builtyr)==9]=7
pums$builtyr2 = factor(pums$builtyr2, levels=1:7, labels=c("0-10 yrs", "11-20 yrs", "21-30 yrs", "31-40 yrs", "41-50 yrs","51-60 yrs", "61+"))
comment(pums$builtyr2) = "recode of builtyr, categories equal one decade"

pums$increc = pums$hhincome
pums$increc[pums$hhincome <=0 | pums$hhincome >= 999999] = NA

pums$tenure = as.integer(pums$ownershp)
pums$tenure[pums$tenure==1] = NA
pums$tenure[pums$tenure==2] = "Owned"
pums$tenure[pums$tenure==3] = "Rented"
table(pums$tenure)

# keep 2 decimal places in output
options(digits=2)

