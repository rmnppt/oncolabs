library(lubridate)
library(tidyverse)

prescribing = read.csv('synthetic_data_prescribing_small.csv')
smr01 = read.csv('synthetic_data_smr01_small.csv')
smr06 = read.csv('synthetic_data_smr06_small.csv')
death = read.csv('synthetic_data_deaths_small.csv')

head(prescribing)
head(smr01)
head(smr06)
head(death)

#PRECANCER

#

smr01$main_condition_2 = substr(smr01$main_condition, 1, 3)

smr01_ca = subset(smr01, main_condition_2 == 'C18' | main_condition_2 == 'C19' | main_condition_2 == 'C21' | main_condition_2 == 'C15' |
                    main_condition_2 == 'C34' | main_condition_2 == 'C25' | main_condition_2 == 'C61' | main_condition_2 == 'C50' |
                    main_condition_2 == 'C16' | main_condition_2 == 'C38' | main_condition_2 == 'D00' | main_condition_2 == 'D01' |
                    main_condition_2 == 'D05')

smr01_ca <- smr01_ca[order(smr01_ca$unique_record_identifier, smr01_ca$date_of_admission),]

#remove other cancers
smr01_ca <- smr01_ca[!duplicated(smr01_ca$unique_record_identifier), ]

smr01_ca$age_diagnosis = year(as.Date(smr01_ca$date_of_admission, format = '%d/%m/%Y')) - year(as.Date(smr01_ca$date_of_birth, format = '%d/%m/%Y'))

smr01_ca$date_of_diagnosis = smr01_ca$date_of_admission

smr01_ca$diagnosis = smr01_ca$main_condition_2

names_keep = names(smr01_ca) %in% c('unique_record_identifier', 'age_diagnosis', 'sex', 'date_of_diagnosis', 'date_of_birth', 'diagnosis')

smr01_ca_lite = smr01_ca[,names_keep]

#Get individual patients
patient_data = smr01[!duplicated(smr01$unique_record_identifier),]

names_keep_smr01 = names(patient_data) %in% c('unique_record_identifier', 'sex', 'date_of_birth')

patient_data_lite = patient_data[,names_keep_smr01]

patient_data_lite = merge(patient_data_lite, smr01_ca_lite, by.x = 'unique_record_identifier', by.y = 'unique_record_identifier', all.x = T)

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

#prescription data


#names_keep_prescribing = names(prescribing) %in% c('unique_record_identifier', 'prescription_barcode', 'prescriber_location_code', 
#                                                  'prescriber_type', 'prescriber_year_of_birth', 'prescriber_gender', 'date_prescribed',
#                                                 'date_dispensed', 'postcode', 'approved_name', 'bnf_item_code', 'prescribed_quantity', 
#                                                'ePrescribed_native_dose_instructions')

rm(list=setdiff(ls(), c('patient_data_lite')))

save.image('cancer_innovation.RData')

prescribing = read.csv('synthetic_data_prescribing_small.csv')

names_keep_prescribing = names(prescribing) %in% c('unique_record_identifier', 'prescriber_type', 'date_prescribed', 'date_dispensed', 
                                                   'postcode', 'approved_name', 'bnf_item_code', 'prescribed_quantity')

prescr_lite = prescribing[,names_keep_prescribing]

#prescr_lite %>% 
 #group_by(unique_record_identifier) %>% 
  #nest() -> prescribing_nested

#alternative


prescr_lite$pasted = paste(prescr_lite$unique_record_identifier, prescr_lite$prescriber_type, prescr_lite$date_prescribed, prescr_lite$date_dispensed,
                           prescr_lite$postcode, prescr_lite$approved_name, prescr_lite$bnf_item_code, prescr_lite$prescribed_quantity, sep = ",")

names_keep_prescribing_2 = names(prescr_lite) %in% c('unique_record_identifier', 'pasted')

prescr_merge = prescr_lite[,names_keep_prescribing_2]

colnames(prescr_merge) = c("unique_record_identifier", "unique_record_identifier, prescriber_type, date_prescribed, date_dispensed, postcode, approved_name, bnf_item_code, prescribed_quantity")

prescr_pasted = aggregate( .~ unique_record_identifier, prescr_merge, function(x) paste(x, sep = ';'))

patient_data_lite_prescr = merge(patient_data_lite, prescr_pasted, by.x = 'unique_record_identifier', by.y = 'unique_record_identifier', all.x = T)

rm(list=setdiff(ls(), c('patient_data_lite_prescr')))

write.csv(patient_data_lite_prescr, 'patient_data_lite_prescr.csv')

save.image('cancer_innovation.RData')

#all drugs

#pre-cancer drugs
