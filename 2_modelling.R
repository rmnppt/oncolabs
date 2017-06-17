#2 Modelling
#library(glmnet)

#colnames(patient_data_overall)

#patient_data_overall = subset(patient_data_overall, !is.na(SIMD.2012.vigintile))


#xfactors <- model.matrix(patient_data_overall$cancer ~ patient_data_overall$sex + patient_data_overall$smoking + )

#x        <- as.matrix(data.frame(patient_data_overall$bmi, xfactors))#, bmi, alcohol, SIMD.2012.vigintile, xfactors))

#detach(patient_data_overall)


#attach(patient_data_overall)
#xfactors <- model.matrix(cancer ~ sex + UR6_Desc + smoking + 
#                           A03 + A15 + A18 + A20 + A24 + A27 + A35 + A39 + A49 + A54 + A66 + 
#                           A67 + A69 + A74 + A83 + A87 + A93 + A95 + A97 + B01 + B05 + B09 + 
#                           B21 + B34 + B38)[, -1]
#x        <- as.matrix(data.frame(bmi, xfactors))#, bmi, alcohol, SIMD.2012.vigintile, xfactors))
#detach(patient_data_overall)

# Note alpha=1 for lasso only and can blend with ridge penalty down to
# alpha=0 ridge only.
#glmmod <- glmnet(x, y=as.factor(patient_data_overall$cancer), alpha=1, family="binomial")

#summary(glmmod)
#coef(glmmod)
# Plot variable coefficients vs. shrinkage parameter lambda.

library(summarizer)
#the glm approach
source('/home/common/summary_tables.R')

explanatory = c('sex','UR6_Desc','smoking','A03','A15','A18','A20','A24','A27','A35',
                'A39','A49','A54','A66','A67','A69','A74','A83','A87','A93','A95',
                'A97','B01','B05','B09','B21','B34','B38', 'bmi', 'alcohol', 'SIMD.2012.vigintile', 'age_in_years')

#uni.glm = fit2df(glmuni(patient_data_overall, 'cancer', explanatory))

names_lite_all = names(patient_data_overall) %in% c('cancer', 'age_in_years', 'smoking', 'alcohol', 'bmi', 'sex', 'A15',
                                                    'A54', 'A97', 'A24', 'SIMD.2012.decile', 'drug_freq')


#
patient_data_overall_lite = patient_data_overall[,names_lite_all]

#glm
library(Hmisc)
fit.cancer  = glm(cancer ~ age_in_years*smoking*alcohol*bmi + sex + A15 + A54 + A97 + A24 + drug_freq + SIMD.2012.decile, 
                  data = patient_data_overall_lite, family = 'binomial')

exp(coef(fit.cancer))

patient_data_overall$sex
#PREDICT
sarah.data = data.frame(age_in_years= 42, cancer = NA, sex = 2, SIMD.2012.decile = 2, smoking = 'No', bmi = 30, alcohol = 16,        
                      A15 = 0, A24 = 0, A54 = 0, A97 = 1, drug_freq = 1)

sarah.n.data = data.frame(age_in_years= 42, cancer = NA, sex = 2, SIMD.2012.decile = 5, smoking = 'No', bmi = 25, alcohol = 10,        
                        A15 = 0, A24 = 0, A54 = 0, A97 = 0, drug_freq = 0)

alan.data = data.frame(age_in_years= 79, cancer = NA, sex = 1, SIMD.2012.decile = 3, smoking = 'Yes', bmi = 30, alcohol = 20,        
                        A15 = 0, A24 = 0, A54 = 0, A97 = 1, drug_freq = 1)

alan.n.data = data.frame(age_in_years= 79, cancer = NA, sex = 1, SIMD.2012.decile = 5, smoking = 'No', bmi = 25, alcohol = 10,        
                       A15 = 0, A24 = 0, A54 = 0, A97 = 0, drug_freq = 1)

#Strip attr
#for (i in 1:dim(new.data)[2]){
#  attr(new.data[,i], "label") = NULL
#}
#str(new.data)

#Now simulate

sarah.risk = predict(fit.cancer, type='response', newdata=sarah.data, re.form=NULL, allow.new.levels=T)#Only appears to take fixed effects into consideration

alan.risk = predict(fit.cancer, type='response', newdata=alan.data, re.form=NULL, allow.new.levels=T)#Only appears to take fixed effects into consideration

sarah.n.risk = predict(fit.cancer, type='response', newdata=sarah.n.data, re.form=NULL, allow.new.levels=T)#Only appears to take fixed effects into consideration

alan.n.risk = predict(fit.cancer, type='response', newdata=alan.n.data, re.form=NULL, allow.new.levels=T)#Only appears to take fixed effects into consideration

alan.risk = alan.risk*100

sarah.risk = sarah.risk*100

sarah.n.risk = sarah.n.risk*100

alan.n.risk = alan.n.risk*100




