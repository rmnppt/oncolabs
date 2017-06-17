library(tidyverse)
library(lubridate)
library(tidyr)

#Co-morbidity data
smr01 = read.csv('synthetic_data_smr01_small.csv')
smr06 = read.csv('synthetic_data_smr06_small.csv')

#Cancer incidence

smr06$main_condition_2 = substr(smr06$site_icd10, 1, 3)

smr06_ca = subset(smr06, main_condition_2 == 'C18' | main_condition_2 == 'C19' | main_condition_2 == 'C21' | main_condition_2 == 'C15' |
                    main_condition_2 == 'C34' | main_condition_2 == 'C25' | main_condition_2 == 'C61' | main_condition_2 == 'C50' |
                    main_condition_2 == 'C16' | main_condition_2 == 'C38' | main_condition_2 == 'D00' | main_condition_2 == 'D01' |
                    main_condition_2 == 'D05')

smr06_ca <- smr06_ca[order(smr06_ca$unique_record_identifier, smr06_ca$date_of_incidence),]

smr06_ca <- smr06_ca[!duplicated(smr06_ca$unique_record_identifier), ]

smr06_ca$age_diagnosis = year(as.Date(smr06_ca$date_of_incidence, format = '%d/%m/%Y')) - year(as.Date(smr06_ca$date_of_birth, format = '%d/%m/%Y'))

smr06_ca$date_of_diagnosis = smr06_ca$date_of_incidence

smr06_ca$diagnosis = smr06_ca$main_condition_2


#remove other cancers
smr06_ca <- smr06_ca[!duplicated(smr06_ca$unique_record_identifier), ]

smr06_ca$age_diagnosis = year(as.Date(smr06_ca$date_of_incidence, format = '%d/%m/%Y')) - year(as.Date(smr06_ca$date_of_birth, format = '%d/%m/%Y'))

smr06_ca$date_of_diagnosis = smr06_ca$date_of_incidence

smr06_ca$diagnosis = smr06_ca$main_condition_2

names_keep = names(smr06_ca) %in% c('unique_record_identifier', 'age_diagnosis', 'sex', 'date_of_diagnosis', 'date_of_birth', 'diagnosis')

#
smr06_ca_lite = smr06_ca[,names_keep]

#Remove diseases after cancer diagnosis 

names_keep = names(smr06_ca) %in% c('unique_record_identifier', 'date_of_diagnosis')

smr06_ca_diag_date = smr06_ca[,names_keep]

colnames(smr06_ca_diag_date) = c('unique_record_identifier', 'date_of_cancer_diagnosis')

#Main comorbidities for cancer
smr01$main_condition_0 = substr(smr01$main_condition, 1, 3)

smr01$main_condition_0 = as.character(smr01$main_condition_0)

#pop date of cancer diagnosis on to merge...

smr01 = merge(smr01, smr06_ca_diag_date, by.x = 'unique_record_identifier', by.y = 'unique_record_identifier',all.x = T)

smr01$date_of_admission = as.Date(smr01$date_of_admission, format = '%d/%m/%Y')

smr01$date_of_cancer_diagnosis = as.Date(smr01$date_of_cancer_diagnosis, format = '%d/%m/%Y')

smr01_nc = subset(smr01, is.na(date_of_cancer_diagnosis))
smr01_c = subset(smr01, !is.na(date_of_cancer_diagnosis))

smr01_c = smr01_c[(smr01_c$date_of_admission > smr01_c$date_of_cancer_diagnosis),]

smr01 = data.frame(rbind(smr01_c, smr01_nc))

#Now merge in cancer diags

patient_data = smr01[!duplicated(smr01$unique_record_identifier),]

names_keep_smr01 = names(patient_data) %in% c('unique_record_identifier', 'sex', 'date_of_birth', 'age_in_years', 'post_code')

patient_data_lite = patient_data[,names_keep_smr01]

patient_data_lite = merge(patient_data_lite, smr06_ca_lite, by.x = 'unique_record_identifier', by.y = 'unique_record_identifier', all.x = T)

patient_data_lite$date_of_birth = ifelse(!is.na(patient_data_lite$date_of_birth.y), as.character(patient_data_lite$date_of_birth.y), as.character(patient_data_lite$date_of_birth.x))

patient_data_lite$cancer = 0

patient_data_lite$sex = patient_data_lite$sex.x

patient_data_lite$sex.x = NULL

patient_data_lite$sex.y = NULL

patient_data_lite$date_of_birth.x = NULL

patient_data_lite$date_of_birth.y = NULL

patient_data_lite$cancer = as.character(patient_data_lite$cancer)

patient_data_lite$cancer[!is.na(patient_data_lite$diagnosis)] = '1'

patient_data_lite$cancer = as.factor(patient_data_lite$cancer)


#Get DOBs ages etc. calculate age, gender, get simd, add in simulated for smoking etc.
#today.date = Sys.Date()
#today.date = format(today.date, "%d/%m/%Y")

#SIMD
imd_lookup = read.csv('Postcode lookup (revised 100113)_vTD.csv')

patient_data_lite$post_code_r = gsub(' ', '', patient_data_lite$post_code)

imd_lookup$Postcode = gsub(' ', '', imd_lookup$Postcode)

patient_data_overall = merge(patient_data_lite, imd_lookup, by.x = 'post_code_r', by.y = 'Postcode', all.x = T)

#Simulate Smoking

#no cancer
patient_data_overall_nc_y = subset(patient_data_overall, age_in_years <= 16 & cancer == 0)#young
patient_data_overall_nc_m = subset(patient_data_overall, age_in_years >16 & age_in_years <=59 & cancer == 0)#middle aged
patient_data_overall_nc_o = subset(patient_data_overall, age_in_years > 59 & cancer == 0)#old

#with cancer
patient_data_overall_c_y = subset(patient_data_overall, age_in_years <= 16 & cancer == 1)#young
patient_data_overall_c_m = subset(patient_data_overall, age_in_years >16 & age_in_years <=59 & cancer == 1)#middle aged
patient_data_overall_c_o = subset(patient_data_overall, age_in_years > 59 & cancer == 1)#old

noyes = c('No', 'Yes')

#Smoking
patient_data_overall_nc_y$smoking <- sample(noyes[1:2], nrow(patient_data_overall_nc_y), replace=TRUE, prob=c(0.99, 0.01))
patient_data_overall_nc_m$smoking <- sample(noyes[1:2], nrow(patient_data_overall_nc_m), replace=TRUE, prob=c(0.80, 0.20))
patient_data_overall_nc_o$smoking <- sample(noyes[1:2], nrow(patient_data_overall_nc_o), replace=TRUE, prob=c(0.55, 0.45))

patient_data_overall_c_y$smoking <- sample(noyes[1:2], nrow(patient_data_overall_c_y), replace=TRUE, prob=c(0.95, 0.05))
patient_data_overall_c_m$smoking <- sample(noyes[1:2], nrow(patient_data_overall_c_m), replace=TRUE, prob=c(0.55, 0.45))
patient_data_overall_c_o$smoking <- sample(noyes[1:2], nrow(patient_data_overall_c_o), replace=TRUE, prob=c(0.45, 0.55))

#Simulate BMI
library(truncnorm)
patient_data_overall_nc_y$bmi <- rtruncnorm(nrow(patient_data_overall_nc_y), a=15, b=35, mean=24, sd=4)
patient_data_overall_nc_m$bmi <- rtruncnorm(nrow(patient_data_overall_nc_m), a=15, b=60, mean=26, sd=4)
patient_data_overall_nc_o$bmi <- rtruncnorm(nrow(patient_data_overall_nc_o), a=15, b=60, mean=29, sd=6)

patient_data_overall_c_y$bmi <- rtruncnorm(nrow(patient_data_overall_c_y), a=15, b=35, mean=24, sd=4)
patient_data_overall_c_m$bmi <- rtruncnorm(nrow(patient_data_overall_c_m), a=15, b=60, mean=28, sd=4)
patient_data_overall_c_o$bmi <- rtruncnorm(nrow(patient_data_overall_c_o), a=15, b=60, mean=30, sd=6)


#Simulate Alcohol
#units per week
patient_data_overall_nc_y$alcohol <- rtruncnorm(nrow(patient_data_overall_nc_y), a=0, b=2, mean=0.1, sd=0.2)
patient_data_overall_nc_m$alcohol <- rtruncnorm(nrow(patient_data_overall_nc_m), a=0, b=60, mean=8, sd=6)
patient_data_overall_nc_o$alcohol <- rtruncnorm(nrow(patient_data_overall_nc_o), a=0, b=60, mean=8, sd=8)

patient_data_overall_c_y$alcohol <- rtruncnorm(nrow(patient_data_overall_c_y), a=0, b=2, mean=0.12, sd=0.25)
patient_data_overall_c_m$alcohol <- rtruncnorm(nrow(patient_data_overall_c_m), a=0, b=80, mean=10, sd=8)
patient_data_overall_c_o$alcohol <- rtruncnorm(nrow(patient_data_overall_c_o), a=0, b=60, mean=10, sd=10)

patient_data_overall = data.frame(rbind(patient_data_overall_nc_y, patient_data_overall_nc_m, patient_data_overall_nc_o,
                                        patient_data_overall_c_y,patient_data_overall_c_m, patient_data_overall_c_o))


#
#names_keep = names(smr01) %in% c('unique_record_identifier', 'date_of_diagnosis')

#smr06_lite = smr01[,names_keep]



#continue
smr01_condition = data.frame(cbind(smr01$unique_record_identifier, smr01$main_condition_0))

colnames(smr01_condition) = c('unique_record_identifier', 'main_condition_0')

smr01_condition$main_condition_0 = as.character(smr01_condition$main_condition_0)

## Get a contingency table of counts
X <- with(smr01_condition, table(smr01_condition$unique_record_identifier,smr01_condition$main_condition_0))

## Massage it into the format you're wanting 
smr01_collapsed = data.frame(cbind(name = rownames(X), apply(X, 2, as.character)))

smr01_collapsed[,2:782] = lapply(smr01_collapsed[,2:782], function(x) as.numeric(as.character(x)))

smr01_collapsed = smr01_collapsed[, -grep("C", colnames(smr01_collapsed))]

smr01_collapsed[,2:693] %>% summarise_each(funs(sum)) -> common_diags

cancer_yes_no = data.frame(patient_data_overall$unique_record_identifier, patient_data_overall$cancer)

colnames(cancer_yes_no) = c('unique_record_identifier', 'cancer')

common_diags = data.frame(t(common_diags))

smr01_collapsed = merge(smr01_collapsed,cancer_yes_no, by.x = 'name', by.y = 'unique_record_identifier', all.x = T)

smr01_collapsed[,2:694] %>% 
  summarise_each(funs(chisq.test(., 
                                 smr01_collapsed$cancer)$p.value), -one_of("Cancer")) -> chi.tests

chi.tests = data.frame(t(chi.tests))

colnames(chi.tests) = 'val'

chi.tests$val = chi.tests$val[order(chi.tests$val)] 

names_keep = names(smr01_collapsed) %in% c('name', rownames(chi.tests)[1:25])

#
comorbidities = smr01_collapsed[,names_keep]

patient_data_overall = merge(patient_data_overall, comorbidities, by.x ='unique_record_identifier', by.y = 'name', all.x = T )

prescr.freq = read.csv('fil2.csv')

prescr.freq$X.1 = NULL

prescr.freq$drug_freq = as.numeric(as.character(prescr.freq$x))

prescr.freq$drug_freq

#prescr.freq$x = NULL

patient_data_overall = merge(patient_data_overall, prescr.freq, by.x = 'unique_record_identifier', by.y = 'unique_record_identifier', all.x = T)

patient_data_overall$drug_freq[is.na(patient_data_overall$drug_freq)] = 0

rm(list=setdiff(ls(), c('patient_data_overall')))
 
