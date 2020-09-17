# library loading ####
library(data.table)
library(caret)

# dataset import and backup ####
df <- fread(file.choose())
df_bkup <- df

# dataset study ####
names <- data.frame(names(df))

# data cleaning & dimensionality reduction ####
# 1,2,7,8,9,11-19, 23-25, 35-37, 40-42, 76-77,178,179,180
df1 <- df[ , c(1,2,7,8,9,11:19, 23:25, 35:37, 40:42, 76, 77,178,179,180)]
df1 <- df1[ !is.na(df1$dmgy_region_1_2_3_4_5), ]
df1 <- df1[ , -c('mdl_prv_ug_grad_Univ_1_2_3_4_5', 'mdl_prv_ug_gpa_bins_1_2_3_4_5',
                 'mdl_student_famcontribution_1_2_3_4_5')]

df1$asn_prct_late_count_term_seq_4 <- ifelse(is.na(df1$asn_prct_late_count_term_seq_4), 0 , 
                                             df1$asn_prct_late_count_term_seq_4)

df1$asn_prct_late_count_term_seq_5 <- ifelse(is.na(df1$asn_prct_late_count_term_seq_5), 0 , 
                                             df1$asn_prct_late_count_term_seq_5)

df1$ods_last_program_gpa <- ifelse(is.na(df1$ods_last_program_gpa), 0 , 
                                             df1$ods_last_program_gpa)

df1$ods_cnt_programs_completed <- ifelse(is.na(df1$ods_cnt_programs_completed), 0 , 
                                             df1$ods_cnt_programs_completed)

df1$ods_programs_avg_gpa <- ifelse(is.na(df1$ods_programs_avg_gpa), 0 , 
                                             df1$ods_programs_avg_gpa)

df1 <- df1[ , -c('enrl_course_success_count_term_seq_3', 'enrl_course_success_count_term_seq_4', 
                 'enrl_course_success_count_term_seq_5', 'enrl_course_retake_count_term_seq_3',
                 'enrl_course_retake_count_term_seq_4', 'enrl_course_retake_count_term_seq_5')]

df1$ods_last_program_gpa <- ifelse(df1$ods_last_program_gpa == 0, 0,
                                   ifelse(df1$ods_last_program_gpa>0 & df1$ods_last_program_gpa<2, 1,
                                          ifelse(df1$ods_last_program_gpa>=2 & df1$ods_last_program_gpa <2.5, 2.5,
                                                 ifelse(df1$ods_last_program_gpa>2.5 & df1$ods_last_program_gpa< 3, 3,
                                                        ifelse(df1$ods_last_program_gpa>=3 & df1$ods_last_program_gpa<4 , 3.5, 4)))))

df1$ods_programs_avg_gpa <- ifelse(df1$ods_programs_avg_gpa == 0, 0,
                                   ifelse(df1$ods_programs_avg_gpa>0 & df1$ods_programs_avg_gpa<2, 1,
                                          ifelse(df1$ods_programs_avg_gpa>=2 & df1$ods_programs_avg_gpa <2.5, 2.5,
                                                 ifelse(df1$ods_programs_avg_gpa>2.5 & df1$ods_programs_avg_gpa< 3, 3,
                                                        ifelse(df1$ods_programs_avg_gpa>=3 & df1$ods_programs_avg_gpa<4 , 3.5, 4)))))

df1$fye_debt_prior_Univ <- ifelse(df1$fye_debt_prior_Univ > 0 , 1, 0)
df1$fye_Univ_debt <- ifelse(df1$fye_Univ_debt > 0 , 1, 0)

df1$dmgy_age_1_2_3_4_5 <- ifelse(df1$dmgy_age_1_2_3_4_5>=15 & df1$dmgy_age_1_2_3_4_5<25, '15-24',
                                 ifelse(df1$dmgy_age_1_2_3_4_5>=25 & df1$dmgy_age_1_2_3_4_5 < 35, '25-34',
                                        ifelse(df1$dmgy_age_1_2_3_4_5>=35 & df1$dmgy_age_1_2_3_4_5 < 45, '35-45',
                                               ifelse(df1$dmgy_age_1_2_3_4_5>=45 & df1$dmgy_age_1_2_3_4_5<55, '45-55',
                                                      ifelse(df1$dmgy_age_1_2_3_4_5>=55 & df1$dmgy_age_1_2_3_4_5<65, '55-65', '66+')))))

df1 <- df1[ , -c('asn_prct_late_count_term_seq_4', 'asn_prct_late_count_term_seq_5')]

names(df1)

# primary model building####
form <- as.formula( as.factor(fye_retention_term_seq_6) ~
                       as.factor(EnrollmentTerm)+as.factor(fye_retention_term_seq_4)
                      +as.factor(fye_retention_term_seq_5)+as.factor(fye_debt_prior_Univ)
                      +as.factor(fye_Univ_debt) +as.factor(fye_success_term_seq_1)
                      +as.factor(dmgy_military_flag_1_2_3_4_5) +as.factor(dmgy_ethnicity_1_2_3_4_5)
                      +as.factor(dmgy_gender_1_2_3_4_5) +as.factor(dmgy_region_1_2_3_4_5)
                      +as.factor(dmgy_state_1_2_3_4_5)+as.factor(dmgy_age_1_2_3_4_5)
                      +as.factor(ods_last_program_gpa) +as.factor(ods_cnt_programs_completed)
                      +as.factor(ods_programs_avg_gpa))

any(is.na(df1))
all(complete.cases(df1))

FullModel <- glm(form,family=binomial(link="logit"),data=df1)
mod <- step(FullModel,direction="backward",trace=FALSE)

summary(FullModel)
summary(mod)

# primary model #2 ####
form2 <- as.formula( as.factor(fye_retention_term_seq_6) ~
                        as.factor(fye_retention_term_seq_4)
                      +as.factor(fye_retention_term_seq_5)+as.factor(fye_debt_prior_Univ)
                      +as.factor(fye_Univ_debt) +as.factor(fye_success_term_seq_1)
                      +as.factor(dmgy_gender_1_2_3_4_5)+as.factor(dmgy_age_1_2_3_4_5)
                      +as.factor(ods_last_program_gpa) +as.factor(ods_cnt_programs_completed)
                      +as.factor(ods_programs_avg_gpa))

FullModel2 <- glm(form2,family=binomial(link="logit"),data=df1)
mod2 <- step(FullModel2,direction="backward",trace=FALSE)

summary(FullModel2)
summary(mod2)

# modified model #3 ####
form3 <- as.formula( as.factor(fye_retention_term_seq_6) ~
                         as.factor(fye_retention_term_seq_4)
                       +as.factor(fye_retention_term_seq_5)+as.factor(fye_debt_prior_Univ)
                       +as.factor(fye_Univ_debt) +as.factor(fye_success_term_seq_1)
                       +as.factor(ods_programs_avg_gpa))

FullModel3 <- glm(form3,family=binomial(link="logit"),data=df1)
mod3 <- step(FullModel3,direction="backward",trace=FALSE)

summary(FullModel3)
summary(mod3)


# final model building #4 ####
library(caTools)
set.seed(seed = 123)
split <- sample.split(df1$fye_retention_term_seq_6, SplitRatio = 0.8)

training_set <- subset(df1, split == T)
test_set <- subset(df1, split == F)

finalForm <- as.formula( as.factor(fye_retention_term_seq_6) ~
                         as.factor(fye_retention_term_seq_5)+as.factor(fye_debt_prior_Univ)
                       +as.factor(fye_Univ_debt) +as.factor(fye_success_term_seq_1))

final_mod_full <- glm(finalForm,family=binomial(link="logit"),data=training_set)
final_new <- step(final_mod_full,direction="backward",trace=FALSE)
step(final_mod_full, test="LRT") # checking likelihood ratio test in addition to backward elimination

summary(final_mod_full)
summary(final_new)

# + as.factor(fye_debt_prior_Univ)+as.factor(fye_Univ_debt) 

# removing more features and changing to Polynomial reg #5 ####
poly_reg <- lm(fye_retention_term_seq_6 ~ 
                 as.factor(fye_retention_term_seq_5)+ as.factor(fye_success_term_seq_1)
               + poly(fye_debt_prior_Univ, degree = 1, raw = T)
               + poly(fye_Univ_debt, degree = 1, raw = T)
               + poly(ods_programs_avg_gpa, degree = 3, raw = T), data = training_set)

summary(poly_reg)

# prediction 
y_pred_poly <- predict(poly_reg, test_set)

# factorising preditions to 0/1 
x <- y_pred_poly
x <- ifelse(x<0.1, 0, 1)
table(x)
# confusion matrix on test set
table(x, test_set$fye_retention_term_seq_6)

# model evaluation on training set
y_pred_train <- predict(poly_reg, training_set)
x1 <- y_pred_train
x1 <- ifelse(x1<0.1, 0, 1)
table(x1)
table(training_set$fye_retention_term_seq_6)
# confusion matrix on training set
table(x1, training_set$fye_retention_term_seq_6)




#### FINAL MODEL USED and MODEL EVALUATIONS ####
summary(poly_reg)
# Training set accuracy & confusion matrix
confusionMatrix(table(x1, training_set$fye_retention_term_seq_6))
# Test set accuracy & confusion matrix
confusionMatrix(table(x, test_set$fye_retention_term_seq_6))
