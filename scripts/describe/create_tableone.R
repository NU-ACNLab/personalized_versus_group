### This script creates Table 1
###
### Ellyn Butler
### November 2, 2025

# Load libraries
library(table1) # v 1.4.3
library(dplyr) # v 1.1.4

# Load data
d1 <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/demographic/ses_2025-11-02.csv')
d2 <- read.csv('~/Documents/Northwestern/projects/personalized_versus_group/data/processed/combined/combined_2025-09-29.csv')

d2 <- merge(d1, d2)

# Clean up
d2$Age <- d2$age

d2$Sex <- recode(d2$sex, `1`='Female', `0`='Male') #checked
d2$Sex <- factor(d2$Sex)

d2$Race <- recode(d2$race, `0`='Caucasian/White', `1`='African American or Black',
                  `2`='Asian-American or Asian', `3`='Native American / Alaska Native', 
                  `4`='Native Hawaiian or Other Pacific Islander', 
                  `5`='Biracial/Multiracial', `6`='Other', `-999`='Other')
d2$Race <- factor(d2$Race)

d2$Ethnicity <- recode(d2$latino, `0`='Not Hispanic or Latino/a', 
                       `1`='Hispanic or Latino/a')
d2$Ethnicity <- factor(d2$Ethnicity)
d2$Average_Caregiver_Years_of_Education <- d2$average_edu_years
d2$income_category <- ordered(d2$income_category, c('$0 - $4,999', '$5,000 - $19,999', 
                        '$20,000 - $34,999', '$35,000 - $49,999', '$50,000 - $74,999',
                        '$75,000 - $99,999', '$100,000 - $149,999', '$150,000 - $199,999',
                        '$200,000 - $249,999', '$250,000 - $299,999', '$300,000 and higher',
                        'Not known'))
d2$Familys_Gross_Total_Income <- d2$income_category



table1(~ Age + Race + Ethnicity + Average_Caregiver_Years_of_Education + Familys_Gross_Total_Income | Sex, data = d2)