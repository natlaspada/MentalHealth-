#Final project draft 1
#Download some packages for importing data
install.packages("survey")
install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)
library(foreign)
library(survey)
options(survey.lonely.psu='adjust')
options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)

#First merge in the full 2015 Population Characteristics dataset from MEPS (HC-181)
#Load MEPS data from internet 
#Code adapted from Week 3 R activity "Importing Data Sets" 
download.file("https://meps.ahrq.gov/mepsweb/data_files/pufs/h181ssp.zip", temp <- tempfile())
unzipped_file = unzip(temp)
h181full = read.xport(unzipped_file)
unlink(temp)  
#Unlink to delete temporary file

#After this, keep relevant variables
#For self-reported mental health status, the vars we want to keep are: MNHLTH31, MNHLTH42, and MNHLTH53
#For demographics, the vars we want to keep are: EDRECODE, TTLP15X, FAMINC15, POVCAT15
#We will also keep the variable "PERWT15F" to tell us about the survey weights of the dataset and "DUPERSID" for ID
#Keep only variables you need
h181<-h181full[c("DUPERSID","MNHLTH31","MNHLTH42","MNHLTH53","EDRECODE","TTLP15X","FAMINC15","POVCAT15","PERWT15F","EMPST31","EMPST42","EMPST53","EMPST31H","EMPST42H","EMPST53H")]
View(h181)

#Next, do an analysis of missing data as part of short assignment 3 
#We will calculate the % of missing data for each variable 
#Use the MEPS codebook at https://meps.ahrq.gov/data_stats/download_data/pufs/h181/h181cb.pdf
#Create a table for each variable and then run proportion for each table
#This will give the percent of responses for each values for each variable

tableMNHLTH31<-table(h181$MNHLTH31)
prop.table(tableMNHLTH31)
tableMNHLTH42<-table(h181$MNHLTH42)
prop.table(tableMNHLTH42)
tableMNHLTH53<-table(h181$MNHLTH53)
prop.table(tableMNHLTH53)
tableEDRECODE<-table(h181$EDRECODE)
prop.table(tableEDRECODE)
tablePOVCAT15<-table(h181$POVCAT15)
prop.table(tablePOVCAT15)
tableEMPST31<-table(h181$EMPST31)
prop.table(tableEMPST31)
tableEMPST42<-table(h181$EMPST42)
prop.table(tableEMPST42)
tableEMPST53<-table(h181$EMPST53)
prop.table(tableEMPST53)
tableEMPST31H<-table(h181$EMPST31H)
prop.table(tableEMPST31H)
tableEMPST42H<-table(h181$EMPST42H)
prop.table(tableEMPST42H)
tableEMPST53H<-table(h181$EMPST53H)
prop.table(tableEMPST53H)

#To combine missing data categories (-9, -8, and -7), recode each relevant variable: 
#Then calculate the new % missing, now listed as the response "-6" after the recoding 
h181$MNHLTH31_recode <- ifelse(h181$MNHLTH31==-9|h181$MNHLTH31==-8|h181$MNHLTH31==-7,-6,h181$MNHLTH31)
tableMNHLTH31_recode<-table(h181$MNHLTH31_recode)
prop.table(tableMNHLTH31_recode)
h181$MNHLTH42_recode <- ifelse(h181$MNHLTH42==-9|h181$MNHLTH42==-8|h181$MNHLTH42==-7,-6,h181$MNHLTH42)
tableMNHLTH42_recode<-table(h181$MNHLTH42_recode)
prop.table(tableMNHLTH42_recode)
h181$MNHLTH53_recode <- ifelse(h181$MNHLTH53==-9|h181$MNHLTH53==-8|h181$MNHLTH53==-7,-6,h181$MNHLTH53)
tableMNHLTH53_recode<-table(h181$MNHLTH53_recode)
prop.table(tableMNHLTH53_recode)
h181$EDRECODE_recode <- ifelse(h181$EDRECODE==-9|h181$EDRECODE==-8|h181$EDRECODE==-7,-6,h181$EDRECODE)
tableEDRECODE_recode<-table(h181$EDRECODE_recode)
prop.table(tableEDRECODE_recode)
h181$EMPST31_recode <- ifelse(h181$EMPST31==-9|h181$EMPST31==-8|h181$EMPST31==-7,-6,h181$EMPST31)
tableEMPST31_recode<-table(h181$EMPST31_recode)
prop.table(tableEMPST31_recode)
h181$EMPST42_recode <- ifelse(h181$EMPST42==-9|h181$EMPST42==-8|h181$EMPST42==-7,-6,h181$EMPST42)
tableEMPST42_recode<-table(h181$EMPST42_recode)
prop.table(tableEMPST42_recode)
h181$EMPST53_recode <- ifelse(h181$EMPST53==-9|h181$EMPST53==-8|h181$EMPST53==-7,-6,h181$EMPST53)
tableEMPST53_recode<-table(h181$EMPST53_recode)
prop.table(tableEMPST53_recode)

#POVCAT15 has no missing data so therefore not recoded 

#Two additional variables (TTLP15X, FAMINC15) are continuous. To cut them to categorical, use the following code:
#Examine the range of individual person incomes
range(h181$TTLP15X)
#Examine the range of family incomes
range(h181$FAMINC15)
#Adapted code from Week 8 Lecture R Activity "Frequencies and Correlations" to create new variables 
#Based on MEPS Codebook, create 6 categories of person income: -149,923 - -58; 0; 2-12000; 12001-24082; 24083-45000; 45001-409118
labels_TTLP15X<-c("Negative","None","Very Low","Low","Medium","High")
breaks1<-c(-149923,0,2,12001,24083,45001,409119)
h181$TTLP15X_cat1<-cut(h181$TTLP15X,breaks1,right=FALSE,include.highest=TRUE,labels=labels_TTLP15X)

#Check to ensure categories worked. The values correspond to MEPS categories listed in Codebook.
tableTTLP15X_cat1<-table(h181$TTLP15X_cat1)
prop.table(tableTTLP15X_cat1)

#Based on MEPS Codebook, create 6 categories of family income: -134723 - -523; 0; 9-23000; 230001-47000; 47001-85514; 85515-521685
labels_FAMINC<-c("Negative","None","Very Low","Low","Medium","High")
breaks2<-c(-134723,0,9,23001,47001,85515,521686)
h181$FAMINC15_cat1<-cut(h181$FAMINC15,breaks2,right=FALSE,include.highest=TRUE,labels=labels_FAMINC)
#Check to ensure categories worked. The values correspond to MEPS categories listed in Codebook. 
tableFAMINC15_cat1<-table(h181$FAMINC15_cat1)
prop.table(tableFAMINC15_cat1)
#Randomly check range of ID var to put into data dictionary
tableID<-data.frame(table(h181$DUPERSID))
summary(tableID)

#Based on Sara's rec, I recoded the vars as missing vs not missing
#Then created 2x2 frequency tables, same as the 27 tables above but with dichotomous missing vs not
h181$MNHLTH31_recode2 <- ifelse(h181$MNHLTH31_recode==-1|h181$MNHLTH31_recode==1|h181$MNHLTH31_recode==2|h181$MNHLTH31_recode==3|h181$MNHLTH31_recode==4|h181$MNHLTH31_recode==5,0,h181$MNHLTH31_recode)
h181$MNHLTH31_recode3 <- ifelse(h181$MNHLTH31_recode==-6,1,h181$MNHLTH31_recode2)
tableMNHLTH31_recode3<-table(h181$MNHLTH31_recode3)
prop.table(tableMNHLTH31_recode3)
h181$MNHLTH42_recode2 <- ifelse(h181$MNHLTH42_recode==-1|h181$MNHLTH42_recode==1|h181$MNHLTH42_recode==2|h181$MNHLTH42_recode==3|h181$MNHLTH42_recode==4|h181$MNHLTH42_recode==5,0,h181$MNHLTH42_recode)
h181$MNHLTH42_recode3 <- ifelse(h181$MNHLTH42_recode==-6,1,h181$MNHLTH42_recode2)
tableMNHLTH42_recode3<-table(h181$MNHLTH42_recode3)
prop.table(tableMNHLTH42_recode3)
h181$MNHLTH53_recode2 <- ifelse(h181$MNHLTH53_recode==-1|h181$MNHLTH53_recode==1|h181$MNHLTH53_recode==2|h181$MNHLTH53_recode==3|h181$MNHLTH53_recode==4|h181$MNHLTH53_recode==5,0,h181$MNHLTH53_recode)
h181$MNHLTH53_recode3 <- ifelse(h181$MNHLTH53_recode==-6,1,h181$MNHLTH53_recode2)
tableMNHLTH53_recode3<-table(h181$MNHLTH53_recode3)
prop.table(tableMNHLTH53_recode3)
h181$EDRECODE_recode2 <- ifelse(h181$EDRECODE_recode==-1|h181$EDRECODE_recode==1|h181$EDRECODE_recode==2|h181$EDRECODE_recode==13|h181$EDRECODE_recode==14|h181$EDRECODE_recode==15|h181$EDRECODE_recode==16,0,h181$EDRECODE_recode)
h181$EDRECODE_recode3 <- ifelse(h181$EDRECODE_recode==-6,1,h181$EDRECODE_recode2)
tableEDRECODE_recode3<-table(h181$EDRECODE_recode3)
prop.table(tableEDRECODE_recode3)
h181$EMPST31_recode2 <- ifelse(h181$EMPST31_recode==-1|h181$EMPST31_recode==1|h181$EMPST31_recode==2|h181$EMPST31_recode==3|h181$EMPST31_recode==4,0,h181$EMPST31_recode)
h181$EMPST31_recode3 <- ifelse(h181$EMPST31_recode==-6,1,h181$EMPST31_recode2)
tableEMPST31_recode3<-table(h181$EMPST31_recode3)
prop.table(tableEMPST31_recode3)
h181$EMPST42_recode2 <- ifelse(h181$EMPST42_recode==-1|h181$EMPST42_recode==1|h181$EMPST42_recode==2|h181$EMPST42_recode==3|h181$EMPST42_recode==4,0,h181$EMPST42_recode)
h181$EMPST42_recode3 <- ifelse(h181$EMPST42_recode==-6,1,h181$EMPST42_recode2)
tableEMPST42_recode3<-table(h181$EMPST42_recode3)
prop.table(tableEMPST42_recode3)
h181$EMPST53_recode2 <- ifelse(h181$EMPST53_recode==-1|h181$EMPST53_recode==1|h181$EMPST53_recode==2|h181$EMPST53_recode==3|h181$EMPST53_recode==4,0,h181$EMPST53_recode)
h181$EMPST53_recode3 <- ifelse(h181$EMPST53_recode==-6,1,h181$EMPST53_recode2)
tableEMPST53_recode3<-table(h181$EMPST53_recode3)
prop.table(tableEMPST53_recode3)

#Create 2x2 tables 
table1<-table(h181$MNHLTH31_recode3,h181$EDRECODE_recode3)
summary(table1)
View(table1)
table2<-table(h181$MNHLTH42_recode3,h181$EDRECODE_recode3)
summary(table2)
View(table2)
table3<-table(h181$MNHLTH53_recode3,h181$EDRECODE_recode3)
summary(table3)
View(table3)
#Skip povcat, ttlp15x, and faminc bc all data present so just create tables based on that
table4<-table(h181$MNHLTH31_recode3,h181$EMPST31_recode3)
summary(table4)
View(table4)
table5<-table(h181$MNHLTH42_recode3,h181$EMPST31_recode3)
summary(table5)
View(table5)
table6<-table(h181$MNHLTH53_recode3,h181$EMPST31_recode3)
summary(table6)
View(table6)
table7<-table(h181$MNHLTH31_recode3,h181$EMPST42_recode3)
summary(table7)
View(table7)
table8<-table(h181$MNHLTH42_recode3,h181$EMPST42_recode3)
summary(table8)
View(table8)
table9<-table(h181$MNHLTH53_recode3,h181$EMPST42_recode3)
summary(table9)
View(table9)
table10<-table(h181$MNHLTH31_recode3,h181$EMPST53_recode3)
summary(table10)
View(table10)
table11<-table(h181$MNHLTH42_recode3,h181$EMPST53_recode3)
summary(table11)
View(table11)
table12<-table(h181$MNHLTH53_recode3,h181$EMPST53_recode3)
summary(table12)
View(table12)
table13<-table(h181$MNHLTH31_recode3,h181$MNHLTH42_recode3)
summary(table13)
View(table13)
table14<-table(h181$MNHLTH31_recode3,h181$MNHLTH53_recode3)
summary(table14)
View(table14)
table15<-table(h181$MNHLTH42_recode3,h181$MNHLTH53_recode3)
summary(table15)
View(table15)
table16<-table(h181$EMPST31_recode3,h181$EMPST42_recode3)
summary(table16)
View(table16)
table17<-table(h181$EMPST31_recode3,h181$EMPST53_recode3)
summary(table17)
View(table17)
table18<-table(h181$EMPST42_recode3,h181$EMPST53_recode3)
summary(table18)
View(table18)

#Compare % missing of var 1 in each table w/ % missing of var 1, given var 2 is missing 
#For table 1, % var 1 (mnhlth31) missing: 
(52+48)/35427
#For table 1, % var 1 (mnhlth31) missing, GIVEN var 2 (edrecode) is missing: 
48/35427/((158+48)/35427)
#For table 1, % var 2 (edrecode) missing: 
(158+48)/35427
#For table 1, % var 2 (edrecode) missing, given var 2 (mnhlth) is missing:
48/35427/((52+48)/35427)
#if the % of var 1 missing DIFFERS from the % of var 1 missing, given that var 2 is missing, 
#we know that the missing data of these 2 vars IS related
#So in the above, table 31, vars 1 and 2 are related
#Bc having var 1 missing increases the likelihood of having var 2 missing 

#We can repeat conditional probabilities for the other tables, revealing associations
#Between most missing variables:
#For table 1, % MNHLTH31 missing = 
(57+43)/35427
#% MNHLTH 31 missing GIVEN that EDRECODE is missing = 
(43/35427)/((315+43)/35427)
#% EDRECODE missing = 
(315+43)/35427
#% EDRECODE missing GIVEN that MNHLTH31 is missing = 
(43/35427)/((57+43)/35427)

#For table 2, % MNHLTH42 missing = 
(50+33)/35427
#% MNHLTH 42 missing GIVEN that EDRECODE is missing = 
(33/35427)/((325+33)/35427)
#% EDRECODE missing = 
(325+33)/35427
#% EDRECODE missing GIVEN that MNHLTH42 missing = 
(33/35427)/((50+33)/35427)

#For table 3, % MNHLTH53 missing = 
(74+38)/35427
#% MNHLTH 53 missing GIVEN that EDRECODE is missing = 
(38/35427)/((320+38)/35427)
#% EDRECODE missing = 
(320+38)/35427
#% EDRECODE missing GIVEN that MNHLTH 53 is missing = 
(38/35427)/((38+74)/35427)

#For table 4, % MNHLTH31 missing = 
(52+48)/35427
#% MNHLTH 31 missing GIVEN that EMPST31 is missing = 
(48/35427)/((48+158)/35427)
#% EMPST31 missing = 
(158+48)/35427
#% EMPST31 missing GIVEN that MNHLTH 31 is missing = 
(48/35427)/((48+52)/35427)

#For table 5, % MNHLTH42 missing = 
(66+17)/35427
#% MNHLTH 42 missing GIVEN that EMPST31 is missing = 
(17/35427)/((17+189)/35427)
#% EMPST31 missing = 
(189+17)/35427
#% EMPST31 missing GIVEN that MNHLTH 42 is missing = 
(17/35427)/((17+66)/35427)

#For table 6, % MNHLTH53 missing = 
(89+23)/35427
#% MNHLTH 53 missing GIVEN that EMPST31 is missing = 
(23/35427)/((23+183)/35427)
#% EMPST31 missing = 
(23+183)/35427
#% EMPST31 missing GIVEN that MNHLTH 53 is missing = 
(23/35427)/((23+89)/35427)

#For table 7, % MNHLTH 31 missing = 
(78+22)/35427
#% MNHLTH 31 missing GIVEN that EMPST42 is missing =
(22/35427)/((22+109)/35427)
#% EMPST42 missing = 
(22+109)/35427
#% EMPST42 missing GIVEN that MNHLTH 31 is missing = 
(22/35427)/((22+78)/35427)

#For table 8, % MNHLTH 42 missing = 
#% MNHLTH 42 missing GIVEN that EMPST42 is missing = 
#% EMPST42 missing = 
#% EMPST42 missing GIVEN that MNHLTH 42 is missing = 

#For table 9, % MNHLTH 53 missing = 
#% MNHLTH 53 missing GIVEN that EMPST42 is missing =
#% EMPST42 missing = 
#% EMPST42 missing GIVEN that MNHLTH 53 is missing = 

#For table 10, % MNHLTH 31 missing = 
(81+19)/35427
#% MNHLTH 31 missing GIVEN that EMPST53 is missing =
(19/35427)/((19+102)/35427)
#% EMPST53 missing = 
(19+102)/35427
#% EMPST53 missing GIVEN that MNHLTH 31 is missing = 
(19/35427)/((19+81)/35427)

#To perform final data analysis, perform chi-squared test on the relevant variables
#Chi-squared test between the Mental Health variables (ignoring all missing values,per complete case analysis)
#With the imputed employment status variables
#And with the education variable (ignoring missing values, per complete case analysis)

#First recode Mental Health variables and Education variables, so that missing is NA.
#Also, lable all variables 
#Citation for labeling: https://www.statmethods.net/input/valuelabels.html
#Purpose of labeling is for use in charts etc, so both versions of the variables can be utilized
h181$MNHLTH31_analysis<-ifelse(h181$MNHLTH31_recode==-6,NA,h181$MNHLTH31_recode)
tableMNHLTH31_analysis<-table(h181$MNHLTH31_analysis)
prop.table(tableMNHLTH31_analysis)
h181$MNHLTH31_label<-factor(h181$MNHLTH31_analysis,
                               levels=c(-1,1,2,3,4,5),
                               labels=c("Inapplicable","Excellent","Very Good","Good","Fair","Poor"))
tableMNHLTH31_label<-table(h181$MNHLTH31_label)
prop.table(tableMNHLTH31_label)

h181$MNHLTH42_analysis<-ifelse(h181$MNHLTH42_recode==-6,NA,h181$MNHLTH42_recode)
tableMNHLTH42_analysis<-table(h181$MNHLTH42_analysis)
prop.table(tableMNHLTH42_analysis)
h181$MNHLTH42_label<-factor(h181$MNHLTH42_analysis,
                               levels=c(-1,1,2,3,4,5),
                               labels=c("Inapplicable","Excellent","Very Good","Good","Fair","Poor"))
tableMNHLTH42_label<-table(h181$MNHLTH42_label)
prop.table(tableMNHLTH42_label)

h181$MNHLTH53_analysis<-ifelse(h181$MNHLTH53_recode==-6,NA,h181$MNHLTH53_recode)
tableMNHLTH53_analysis<-table(h181$MNHLTH53_analysis)
prop.table(tableMNHLTH53_analysis)
h181$MNHLTH53_label<-factor(h181$MNHLTH53_analysis,
                               levels=c(-1,1,2,3,4,5),
                               labels=c("Inapplicable","Excellent","Very Good","Good","Fair","Poor"))
tableMNHLTH53_label<-table(h181$MNHLTH53_label)
prop.table(tableMNHLTH53_label)

h181$EDRECODE_analysis<-ifelse(h181$EDRECODE_recode==-6,NA,h181$EDRECODE_recode)
tableEDRECODE_analysis<-table(h181$EDRECODE_analysis)
prop.table(tableEDRECODE_analysis)
h181$EDRECODE_label<-factor(h181$EDRECODE_analysis,
                               levels=c(-1,1,2,13,14,15,16),
                               labels=c("Inapplicable","1","2",
                                        "3","4",
                                        "5","6"))
tableEDRECODE_label<-table(h181$EDRECODE_label)
prop.table(tableEDRECODE_label)

h181$EMPST31H_label<-factor(h181$EMPST31H,
                               levels=c(-1,1,2,34),
                               labels=c("Inapplicable","Employed","Job to Return to","Unemployed"))
tableEMPST31H_label<-table(h181$EMPST31H_label)
prop.table(tableEMPST31H_label)

h181$EMPST42H_label<-factor(h181$EMPST42H,
                               levels=c(-1,1,2,34),
                               labels=c("Inapplicable","Employed","Job to Return to","Unemployed"))
tableEMPST42H_label<-table(h181$EMPST42H_label)
prop.table(tableEMPST42H_label)

h181$EMPST53H_label<-factor(h181$EMPST53H,
                               levels=c(-1,1,2,34),
                               labels=c("Inapplicable","Employed","Job to Return to","Not Employed"))
tableEMPST53H_label<-table(h181$EMPST53H_label)
prop.table(tableEMPST53H_label)

h181$POVCAT15_label<-factor(h181$POVCAT15,
                            levels=c(1,2,3,4,5),
                            labels=c("Poor/Negative","Near Poor","Low Income","Middle Income","High Income"))
tablePOVCAT15_label<-table(h181$POVCAT15_label)
prop.table(tablePOVCAT15_label)

#Ideal plot would show MEAN mental health compared to each category of education or employment status
#Run chi-sq, na.rm=TRUE. 
#Chi-sq tests between all variables, including the inapplicables, and thus can use the labeled version or the analysis version
#Show that labelled and none labelled 
chisq.test(h181$MNHLTH31_label,h181$EMPST31H_label)
chisq.test(h181$MNHLTH31_analysis,h181$EMPST31H)
#Repeat for all variable combinations, noting p-values along the way 
chisq.test(h181$MNHLTH31_analysis,h181$EMPST31H)
chisq.test(h181$MNHLTH31_analysis,h181$EDRECODE_analysis)
chisq.test(h181$MNHLTH31_analysis,h181$POVCAT15)
chisq.test(h181$MNHLTH31_analysis,h181$TTLP15X_cat1)
chisq.test(h181$MNHLTH31_analysis,h181$FAMINC15_cat1)
chisq.test(h181$MNHLTH42_analysis,h181$EMPST42H)
chisq.test(h181$MNHLTH42_analysis,h181$EDRECODE_analysis)
chisq.test(h181$MNHLTH42_analysis,h181$POVCAT15)
chisq.test(h181$MNHLTH42_analysis,h181$TTLP15X_cat1)
chisq.test(h181$MNHLTH42_analysis,h181$FAMINC15_cat1)
chisq.test(h181$MNHLTH53_analysis,h181$EMPST53H)
chisq.test(h181$MNHLTH53_analysis,h181$EDRECODE_analysis)
chisq.test(h181$MNHLTH53_analysis,h181$POVCAT15)
chisq.test(h181$MNHLTH53_analysis,h181$TTLP15X_cat1)
chisq.test(h181$MNHLTH53_analysis,h181$FAMINC15_cat1)

#To show table 3, an example of the exact values in a chi-sq test, use the following code: 
table<-table(h181$MNHLTH31_analysis,h181$EMPST31H)
View(table)

#Creating plots to view data
#Helps us to understand exactly what the significant p-values mean
#And how one's category of mental health is associated w/ employment status and education
#Again, retaining all inapplicables 
#Bc the assumption is that all inapplicables could have something in common 
#And tell us more about who is inapplicable vs who is not 
#Inapplicability is also based on age 
#Cannot do complete case analysis to eliminate inapplicables bc they make up more than 5%
#Decided to retain inapplicables in ALL variables, even if they COULD be eliminated due to making up less than 5%

#Charts w/ bars for each category w/in each mental health category 
#Adapted from:https://www.statmethods.net/graphs/bar.html
#And adapted from: https://stackoverflow.com/questions/18688847/position-legend-of-a-stacked-bar-plot
#(to adjust the location of the legend and move it over a bit)
counts<-table(h181$EMPST31H_label,h181$MNHLTH31_label)
barplot(counts,main="Distribution of Mental Health by Employment Status RD 3/1",
        xlab="Mental Health RD 3/1",ylab="Individuals",col=c("white","green","orange","red"),
        legend=rownames(counts),beside=TRUE,
        args.legend = list(x="topright",bty="n",inset=c(-0.1,0)))
counts2<-table(h181$EMPST42H_label,h181$MNHLTH42_label)
barplot(counts2,main="Distribution of Mental Health by Employment Status RD 4/2",
        xlab="Mental Health RD 4/2",col=c("white","green","orange","red"),
        legend=rownames(counts2),beside=TRUE,
        args.legend=list(x="topright",bty="n",inset=c(-0.1,-0)))
counts3<-table(h181$EMPST53H_label,h181$MNHLTH53_label)
barplot(counts3,main="Distribution of Mental Health by Employment Status RD 5/3",
        xlab="Mental Health Round 3",col=c("white","green","orange","red"),
        legend=rownames(counts3),beside=TRUE,
        args.legend=list(x="topright",bty="n",inset=c(-0.1,0)))
counts4<-table(h181$EDRECODE_analysis,h181$MNHLTH31_label)
barplot(counts4,main="Distribution by Mental Health and Education Level",
        xlab="Mental Health Round 1",col=c("white","red","yellow","green","lightblue","darkblue","purple"),
        legend=rownames(counts4),beside=TRUE,
        args.legend=list(x="topright",bty="n",inset=c(-0.1,0)))
counts14<-table(h181$EDRECODE_analysis,h181$MNHLTH42_label)
barplot(counts14,main="Distribution by Mental Health and Education Level",
        xlab="Mental Health Round 2",col=c("white","red","yellow","green","lightblue","darkblue","purple"),
        legend=rownames(counts14),beside=TRUE,
        args.legend=list(x="topright",bty="n",inset=c(-0.1,0)))
counts15<-table(h181$EDRECODE_analysis,h181$MNHLTH53_label)
barplot(counts15,main="Distribution by Mental Health and Education Level",
        xlab="Mental Health Round 3",col=c("white","red","yellow","green","lightblue","darkblue","purple"),
        legend=rownames(counts15),beside=TRUE,
        args.legend=list(x="topright",bty="n",inset=c(-0.1,0)))
counts5<-table(h181$TTLP15X_cat1,h181$MNHLTH31_label)
barplot(counts5,main="Distribution by Mental Health and Person-Level Income",
        xlab="Mental Health Round 1",col=c("red","yellow","green","lightblue","darkblue","purple"),
        legend=rownames(counts5),beside=TRUE,
        args.legend=list(x="topright",bty="n",inset=c(-0.1,0)))
counts6<-table(h181$TTLP15X_cat1,h181$MNHLTH42_label)
barplot(counts6,main="Distribution by Mental Health and Person-Level Income",
        xlab="Mental Health Round 2",col=c("red","yellow","green","lightblue","darkblue","purple"),
        legend=rownames(counts6),beside=TRUE,
        args.legend=list(x="topright",bty="n",inset=c(-0.1,0)))
counts7<-table(h181$TTLP15X_cat1,h181$MNHLTH53_label)
barplot(counts7,main="Distribution by Mental Health and Person-Level Income",
        xlab="Mental Health Round 3",col=c("red","yellow","green","lightblue","darkblue","purple"),
        legend=rownames(counts7),beside=TRUE,
        args.legend=list(x="topright",bty="n",inset=c(-0.1,0)))
counts8<-table(h181$FAMINC15_cat1,h181$MNHLTH31_label)
barplot(counts8,main="Distribution by Mental Health and Family-Level Income",
        xlab="Mental Health Round 1",col=c("red","yellow","green","lightblue","darkblue","purple"),
        legend=rownames(counts8),beside=TRUE,
        args.legend=list(x="topright",bty="n",inset=c(-0.1,0)))
counts9<-table(h181$FAMINC15_cat1,h181$MNHLTH42_label)
barplot(counts9,main="Distribution by Mental Health and Family-Level Income",
        xlab="Mental Health Round 2",col=c("red","yellow","green","lightblue","darkblue","purple"),
        legend=rownames(counts9),beside=TRUE,
        args.legend=list(x="topright",bty="n",inset=c(-0.1,0)))
counts10<-table(h181$FAMINC15_cat1,h181$MNHLTH53_label)
barplot(counts10,main="Distribution by Mental Health and Family-Level Income",
        xlab="Mental Health Round 3",col=c("red","yellow","green","lightblue","darkblue","purple"),
        legend=rownames(counts10),beside=TRUE,
        args.legend=list(x="topright",bty="n",inset=c(-0.1,0)))
counts11<-table(h181$POVCAT15_label,h181$MNHLTH31_label)
barplot(counts11,main="Distribution by Mental Health and Poverty Category",
        xlab="Mental Health Round 1",col=c("white","red","yellow","blue","green"),
        legend=rownames(counts11),beside=TRUE,
        args.legend=list(x="topright",bty="n",inset=c(-0.1,0)))
counts12<-table(h181$POVCAT15_label,h181$MNHLTH42_label)
barplot(counts12,main="Distribution by Mental Health and Poverty Category",
        xlab="Mental Health Round 2",col=c("white","red","yellow","blue","green"),
        legend=rownames(counts12),beside=TRUE,
        args.legend=list(x="topright",bty="n",inset=c(-0.1,0)))
counts13<-table(h181$POVCAT15_label,h181$MNHLTH53_label)
barplot(counts13,main="Distribution by Mental Health and Poverty Category",
        xlab="Mental Health Round 3",col=c("white","red","yellow","blue","green"),
        legend=rownames(counts13),beside=TRUE,
        args.legend=list(x="topright",bty="n",inset=c(-0.1,0)))







