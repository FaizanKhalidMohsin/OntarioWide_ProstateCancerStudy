require(survival)
require(tableone)
data()
data(cancer)
str(cancer)
dput(names(cancer))
# setwd("")

varlist = c("inst", "time", "status", "age", "sex", "ph.ecog", "ph.karno", 
"pat.karno", "meal.cal", "wt.loss")
varlist2 =  c("inst", "time", "age", "sex", "ph.ecog", "ph.karno", 
"pat.karno", "meal.cal", "wt.loss")
catvar1 = c(  "status", "age", "sex")
catvar2 = c(  "status", "sex")
print(CreateTableOne(data = cancer) , showAllLevels = TRUE)

table1 = CreateTableOne( vars = varlist, data = cancer, factorVars = catvar1)
table1
table1.1 = CreateTableOne( vars = varlist, data = cancer, factorVars = catvar2)

print(table1.1, showAllLevels = T)

table2 = CreateTableOne( vars = varlist2, data = cancer, factorVars = catvar2, strata = c("status"))
table2.1 = CreateTableOne( vars = varlist2, data = cancer, factorVars = c("sex"), strata = c("status"))
table2
dim(table2.1)


################ CODE FOR HANAN DATASET TABLE 1

require(tableone)
require(dplyr)
require(ggplot2)

data1 = read.csv("Outcomes_cohort_cleaned_up.csv") 

varlist1 = c("adg_score", "rurality_score", "rurality_cat", "income_quint", 
		 "mortality_risk", "age_cat")

catvars = c("rurality_cat", "income_quint", "age_cat")

#CreateTableOne(data1)

############ Table 1 with no strata. Overall Cohort means and percentages. ######

table1 = CreateTableOne( vars = varlist1, data = data1, factorVars = catvars )
print(table1, showAllLevels = T)
summary(table1)

############ Table 1 with age catagory as strata                           ######

varlist2 = c("adg_score", "rurality_score", "rurality_cat", "income_quint", 
		 "mortality_risk")

catvars2 = c("rurality_cat", "income_quint")

table1.1 = CreateTableOne(vars = varlist, data1, factorVars = catvars  , strata = c("age_cat"))
print(table1.1, showAllLevels = T)
summary(table1.1)

############ Table 2 Biopsy data with time interval info

data2 = read.csv("cohort_time_dependent_variables_cleaned_up.csv")

############ Use dplyr for counting cumulative biospies over all the time intervals for each patient. 

##  Creating dataset with total number of biopsies per patient. ##

#group by patient id so can call summarise function.  
patients = group_by(data2, patient_id) 

biop_count = summarise(patients, biopsies = sum(Num_biopsies), n = n()) 
# Here we have created the dataset with total number of biopsies per patient. 
# Can now use this to find the number of average biopsies for the entire cohort. 

mean(biop_count$biopsies) # OR
mean(select(biop_count, biopsies))

############ Find out number of patients per number of biopsies.

table(bio_count$biopsies)

#   Test CODE on CANCER DATASET in survival  #
#str(cancer)
#head(cancer) 
#require(dplyr)
#instant_group = group_by(cancer, inst)
#time_count = summarise(instant_group, Total_time = sum(time))
#time_count
#mean(time_count$Total_time)accordingly. 
# Group by Age_Catagory to then find average number of biopsies per age group. # Could have also used tableone. 
NumBiopsiesPerAgeCat = group_by(patient_biop_age, Age_Catagory)
num_biopsy_per_age = summarise(NumBiopsiesPerAgeCat, Average_Biopsies = mean(Num_Biopsies), n = n()) #perhaps do not need n())


############## Table 3 drug usage throughout study period. 

# use data1 for this table. 
# Check how the data looks for the different medications


medications = c("AGI_pret0", "DP4I_pret0", "Insulin_pret0", 
#table(time_count$Total_time)

############ For each Age catagory find out number of patients per number of biopsies.

# Can simply group by patient again. Do not need to group by patient & age_cat
# Make sure age_cat is a numerical integer 1,2,3,4,5. Check how this looks like in the dataset. It should be the same number (age cat) for all rows of the same person. 

# patient_age_cat = group_by(data2, patient_id, age_cat) # note that patient_id variable name is dependant on the dataset used. 

# Create the dataset with Age_catagory and total number of biopsies per patient. 
patient_biop_age = summarise(patients, Age_Catagory = mean(age_cat), Number_Biopsies = sum(Num_biopsies))
print(patient_biop_age)

# Check for missing values. If they are present, modify code 
					"meglitinides_pret0",
					"metformin_pret0",
					"SGL2_pret0",
					"sulfo_pret0",
					"TZDs_pret0",

					"hydrophilic_statin_pret0",
					"hydrophobic_statin_pret0",
					"alphablocker_pret0",
					"X5ari_pret0", # R automatically puts an X in front of var names starting w/ numbers
					"chloroquine_pret0",
					"dipyridamole_pret0",

					"PPI_all_pret0",
					"PPI_all_but_panto_pret0",
					"PPI_pantoprazole_pret0",
					"glaucoma_drops_pret0" ) 

medications

table_3.0 = CreateTableOne(vars = medications, data = data1, factorVar = medications) #No stratification so far. 
print(table_3.0, showAllLevels = T)
write.csv(table_3.0, file = "Table_3.0_.csv")

# Count the number of people not taking any medication. That should be total people in cohort minus the total number of people in table3.1. 
# Use subset() putting all the medication names equal to zero and using &. 


############### Number of patients and % of patients taking each drug (ever) during study period
# Again first look at the data. Using different data set "data2"

medications_during = c("AGI_used_in_interval", 
                 "DP4I_used_in_interval",
                 "Insulin_used_in_interval",
                 "meglitinides_used_in_interval",
                 "metformin_used_in_interval",
                 "SGL2_used_in_interval",
                 "sulfo_used_in_interval",
                 "TZDs_used_in_interval",
                 
                 "hydrophilic_statin_used_in_interval",
                 "hydrophobic_statin_used_in_interval",
                 "alphablocker_used_in_interval",
                 "X5ari_used_in_interval",
                 "chloroquine_used_in_interval",
                 "dipyridamole_ used_in_interval",
                 
                 "PPI_other_used_in_interval",
                 "PPI_panto_used_in_interval",
                 "glaucomadrop_used_in_interval"  )

table_3.1 = CreateTableOne(vars = medications_during, data = data2, factorVar = medications_during) #No stratification so far. 
print(table_3.1, showAllLevels = T)
write.csv(table_3.1, file = "Table_3.1_.csv")

# Be Carefull here, because the during medication could be in the    
# interval information, not as a: 0, 1 Yes, No.
# If it is like this then need to count using summarise() function in dplyr
# where we will be counting the cumulative dose which if at the end is zero
# would mean the drug was not used. 

# Assuming the structure of the data is that under the variable name interval
# we have 1, 2, 3, etc, the interval number. And the variable with the name of 
# medication whose row entry corresponds to the amount of dose given in that 
# interval. Hence, for each individual we will count the total dose he got. 
# Hence, he will either be censored at the end or died due to prostate, or 
# due to a competing risk factor. 


#require(dplyr)
# Perhaps will need to change empty cells to zero's, b/c R will read them as NA's. 

# Create dataframe as test
x1 = sample(c(NA, 1, 2), 10, replace = T)
x2 = sample(c(NA, 2, 1, 4), 10, replace = T)
x3 = sample(c(NA, 2, 1, 4), 10, replace = T)
x4 = sample(c(NA, 2, 1, 4), 10, replace = T)
x5 = sample(c(NA, "Him", "em", "Name Me"), 10, replace = T)
test_data = data.frame(x1, x2, x3, x4, x5) 

#aa = which(test_data == NA, arr.ind=TRUE)

# # before we do that create new dataset that replaces NA's with 0's. Call it data2.1 
# 
# replace_NA_with_0 = function(data, start, end){
# 
#           # data: enter a dataframe or a matrix
#           # start: enter the col number where to begin
#           # end: enter the col number where to end 
# 
#       for (i in start:end){
#             data[, i][is.na(data[,i])] <- 0
#       }
# }
# 
# test_data[, 4][is.na(test_data[,4])] <- 0
# 
# replace_NA_with_0(test_data, 2, 4)

# Better helper function: 

change_missing_to_0 <- function(x) {
  x[x == NA] <- 0
  x
}
test_data1 = test_data
test_data1[2:4] <- lapply(test_data1[2:4], change_missing_to_0)
test_data1

#Final Answer: Careful that does not work for col with characters.
###############################################
test_data1[1:4][is.na(test_data1[,1:4])] <- 0##
test_data1                                   ##
###############################################

# again group by patients: id. 
grouped_by_patients = group_by(data2.1, id)

# Now we need to begin the counting process. 

summarise(data2.1, cum_dose_AGI = sum(AGI_used_in_interval) )



########################### Table 4 ##########################

#Number and Percent of patients diagnosed with cancer In general in study period
# data1 = Outcomes_cohort_cleaned_up, var =	PCcancer_dx
Can put this in the very first table:  Table_1.0

#Number and percentage of patients diagnosed with prostate cancer in each age catergory	
#Cohort_time_dependent_variables_cleaned_up	age_category, PC_diagnosed_in_interval
# could 
?Surv

?survSplit
library(XLConnect)
dat <- data.frame(rsp = rnorm(100, 0, 1), 
pred1 = rnorm(100, 0, 1), 
pred2 = rnorm(100, 0, 1))
model <- lm(rsp ~ pred1 + pred2, data = dat)
writeWorksheetToFile("model1.xlsx", 
data = summary(dat), 
sheet = "summary", 
header = TRUE,
clearSheets = TRUE)


?rep

id = NULL
for (i in 1:10){
  v = c(3, 5, 4, 8, 7, 12,  8, 18, 7, 3)
  id = append(id, rep(i, v[i]))
  
}
id 

head(jasa)

dim(jasa)

dim(jasa[-1,])
jasa[-1,]

jasa
dim(jasa)


vector = NULL
append(NULL, 1)
for (i in 1:nrow(jasa)){
  if (jasa$timetotransplant[i] == Inf) {
    vector = append(vector,i)
  }
}
vector

