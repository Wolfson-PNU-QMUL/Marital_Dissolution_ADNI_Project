#begin with data preparation steps (outlier detection and transformations)
#start with outlier detection

#use a QQ plot to determine whether there are any outliers for the amyloid data
#Based on the graph, there is one very clear outlier but further investigation needs to be done

qqnorm(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO$CENTILOIDS, main = "Normal Q-Q plot")

#determine the value of this outlier;value = 198

table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO$CENTILOIDS)

#transform the amyloid centiloids data into a scaled (z) score to determine how many SDs above the mean this outlier is.
#use the table function to determine the precise value and a histogram to visualise this
#this outlier was further classified as extreme with a z-score of 5.26937035079519. The next highest value is 3.65674877585306. 

WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO$CENTILOIDS_zscore <- scale(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO$CENTILOIDS)
table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO$CENTILOIDS_zscore)
hist(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO$CENTILOIDS_zscore,
     main = "", 
     xlab = "Aβ Centiloid value (z score)")

#1 extreme outlier detected with a z-score of 5.26937035079519 
#outlier detected - remove in next step; value = 198

#use a QQ plot to determine whether there are any outliers for the Executive Functioning data
#no clear/obvious outlier

qqnorm(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO$ADNI_EF2, main = "Normal Q-Q plot")

#use a QQ plot to determine whether there are any outliers for the Episodic Memory data
#no clear/obvious outlier

qqnorm(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO$ADNI_MEM, main = "Normal Q-Q plot")

#remove the amyloid-specific outlier by only including data less than 150 Centiloids. This will filter out the extreme case of 198 (next highest value is 143)
#cases (n) in database reduced from 544 to 543
WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER <- subset(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO, CENTILOIDS<150)

#doublecheck the variable names to ensure you have the right file
colnames(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)


#after dropping this outlier, continue with tests of normality

#test for normality in amyloid data using the Shapiro-Wilk test
#data is not normally distributed (W = 0.82403, p-value < 2.2e-16)

shapiro.test(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$CENTILOIDS)

#confirm non-normal distribution for amyloid data using histogram

hist(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$CENTILOIDS, 
     main = "", 
     xlab = "Aβ Centiloid value")

#test for normality in EF data using the Shapiro-Wilk test
#data is not normally distributed (W = 0.98981, p-value = 0.0008109)

shapiro.test(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$ADNI_EF2)

#confirm non-normal distribution for EF data using histogram

hist(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$ADNI_EF2, 
     main = "", 
     xlab = "Executive Functioning score")


#test for normality in EM data using the Shapiro-Wilk test
#data is not normally distributed (W = 0.99199, p-value = 0.005054)

shapiro.test(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$ADNI_MEM)

#confirm non-normal distribution for EM data using histogram
hist(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$ADNI_MEM, 
     main = "", 
     xlab = "Episodic Memory score")
table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$ADNI_MEM)


#due to the non-normal distribution transform the variables using cube root
#advice for calculating real cube root in R: sign(x) * abs(x)^(1 / 3)

WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$CUBEROOT_CENTILOIDS <- sign(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$CENTILOIDS) * (abs(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$CENTILOIDS)^(1/3))
WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$CUBEROOT_ADNI_EF2 <- sign(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$ADNI_EF2) * (abs(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$ADNI_EF2)^(1/3))
WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$CUBEROOT_ADNI_MEM <- sign(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$ADNI_MEM) * (abs(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$ADNI_MEM)^(1/3))

#check cube rooted variables to ensure that the numbers make sense (sanity check)

table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$CUBEROOT_CENTILOIDS)
table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$CUBEROOT_ADNI_EF2)
table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$CUBEROOT_ADNI_MEM)

##create a csv version of this file

write.csv(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER, "~/ADNI/WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER.csv", row.names=TRUE)

#########################################################################################################################################################################
#data is ready for inferential statistical analysis - use the outlier dropped database (n=543) and the cube rooted outcome variables
#this is the main analysis

#run regression between amyloid and marriage dissolution
#association was significant with increased amyloid associated with marital dissolution

Dissolution_Amyloid_1 <- lm(CUBEROOT_CENTILOIDS ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + TRACER_FACTOR + APOE4_Additive_Factor + Marriage_Dissolution_Factor, 
                            data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(Dissolution_Amyloid_1)
confint(Dissolution_Amyloid_1)

##install and call all necessary packages needed for forest plot creation

install.packages("jtools")
library(jtools)
install.packages("broom.mixed")
library(broom.mixed)
install.packages("ggplot2")
library(ggplot2) 
install.packages("sjPlot")
library(sjPlot)
install.packages("sjmisc")
library(sjmisc)

#create forest plot of regression equation examining the relationship between amyloid and marital dissolution

plot1_amyloid <- plot_summs(Dissolution_Amyloid_1, coefs = c("Age" = "AGE_AB_PETSCAN_NUMERIC", "Sex" = "PTGENDER_FactorFemale", "Education" = "PTEDUCAT", 
                                                             "Tracer" = "TRACER_FACTORFBP","APOE e4 heterozygous" = "APOE4_Additive_FactorHetCarrier", 
                                                             "APOE e4 homozygous" = "APOE4_Additive_FactorHomoCarrier", 
                                                             "Marriage Dissolution" = "Marriage_Dissolution_FactorMarriage Dissolved", inner_ci_level = .95))

#add a label for beta estimate

plot1_amyloid + labs(x = "\n Beta Estimate \n ", y = NULL) 


#repeat the process with different formatting options and saving in high res


plot3_amyloid <- plot_summs(Dissolution_Amyloid_1, coefs = c("Age" = "AGE_AB_PETSCAN_NUMERIC", "Sex" = "PTGENDER_FactorFemale", "Education" = "PTEDUCAT", 
                                                             "Tracer" = "TRACER_FACTORFBP","APOE e4 heterozygous" = "APOE4_Additive_FactorHetCarrier", 
                                                             "APOE e4 homozygous" = "APOE4_Additive_FactorHomoCarrier", 
                                                             "Marriage Dissolution" = "Marriage_Dissolution_FactorMarriage Dissolved", inner_ci_level = .95))

#add a label for beta estimate and change the theme to provide gridlines, and finally save as PDF

figure1_plot = plot3_amyloid + labs(x = "\n Beta Estimate \n ", y = NULL) + theme_bw()

pdf(file = "./ADNI/figure_1_revised.pdf")
figure1_plot 
dev.off()


#now conduct interaction testing between marital status and the covariates for amyloid

#start with age*dissolution
#interaction not significant

Dissolution_Amyloid_Age <- lm(CUBEROOT_CENTILOIDS ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + TRACER_FACTOR + APOE4_Additive_Factor + Marriage_Dissolution_Factor
                              + Marriage_Dissolution_Factor*AGE_AB_PETSCAN_NUMERIC, 
                              data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(Dissolution_Amyloid_Age)
confint(Dissolution_Amyloid_Age)

#next sex*dissolution
#interaction not significant

Dissolution_Amyloid_Sex <- lm(CUBEROOT_CENTILOIDS ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + TRACER_FACTOR + APOE4_Additive_Factor + Marriage_Dissolution_Factor
                              + Marriage_Dissolution_Factor*PTGENDER_Factor, 
                              data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(Dissolution_Amyloid_Sex)
confint(Dissolution_Amyloid_Sex)

#next education*dissolution
#interaction not significant

Dissolution_Amyloid_Educ <- lm(CUBEROOT_CENTILOIDS ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + TRACER_FACTOR + APOE4_Additive_Factor + Marriage_Dissolution_Factor
                               + Marriage_Dissolution_Factor*PTEDUCAT, 
                               data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(Dissolution_Amyloid_Educ)
confint(Dissolution_Amyloid_Educ)

#next APOE*dissolution
#interactions not significant

Dissolution_Amyloid_APOE <- lm(CUBEROOT_CENTILOIDS ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + TRACER_FACTOR + APOE4_Additive_Factor + Marriage_Dissolution_Factor
                               + Marriage_Dissolution_Factor*APOE4_Additive_Factor, 
                               data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(Dissolution_Amyloid_APOE)
confint(Dissolution_Amyloid_APOE)

#run regression between EF and marriage dissolution
#association was not significant

Dissolution_EF_1 <- lm(CUBEROOT_ADNI_EF2 ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor, 
                       data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(Dissolution_EF_1)
confint(Dissolution_EF_1)

#run regression between EM and marriage dissolution
#association was significant with worse EM performance associated with marital dissolution

Dissolution_EM_1 <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor, 
                       data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(Dissolution_EM_1)
confint(Dissolution_EM_1)

#create forest plot of regression equation examining the relationship between EF/EM and marital dissolution

cogplot <- plot_summs(Dissolution_EF_1, Dissolution_EM_1, coefs = c("Age" = "AGE_AB_PETSCAN_NUMERIC", "Sex" = "PTGENDER_FactorFemale", "Education" = "PTEDUCAT", 
                                                                    "Marriage Dissolution" = "Marriage_Dissolution_FactorMarriage Dissolved"), 
                      inner_ci_level = .95, model.names = c("Executive Functioning Performance", "Episodic Memory Performance"))


#add a label for beta estimate

cogplot + labs(x = "\n Beta Estimate \n ", y = NULL) 




###try colors - EF is purple, EM is red now

cogplot2 <- plot_summs(Dissolution_EF_1, Dissolution_EM_1, coefs = c("Age" = "AGE_AB_PETSCAN_NUMERIC", "Sex" = "PTGENDER_FactorFemale", "Education" = "PTEDUCAT", 
                                                                     "Marriage Dissolution" = "Marriage_Dissolution_FactorMarriage Dissolved"), 
                       inner_ci_level = .95, colors = "Rainbow", model.names = c("Executive Functioning Performance", "Episodic Memory Performance"))



#add a label for beta estimate for new colored plot and change the theme to provide gridlines, and finally save as PDF

figure2_plot = cogplot2 + labs(x = "\n Beta Estimate \n ", y = NULL) + theme_bw()

pdf(file = "./ADNI/figure_2_revised.pdf")
figure2_plot 
dev.off()


#now conduct interaction testing between marital status and the covariates for EM

#start with age*dissolution
#interaction not significant

Dissolution_EM_Age <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor
                         + Marriage_Dissolution_Factor*AGE_AB_PETSCAN_NUMERIC, 
                         data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(Dissolution_EM_Age)
confint(Dissolution_EM_Age)

#next sex*dissolution
#interaction is significant

Dissolution_EM_Sex <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor
                         + Marriage_Dissolution_Factor*PTGENDER_Factor, 
                         data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(Dissolution_EM_Sex)
confint(Dissolution_EM_Sex)


#graph significant interaction effect
Interaction_Plot <- plot_model(Dissolution_EM_Sex, type = "int", title = "", legend.title = "Marital Status") + geom_line() 

p = Interaction_Plot + labs(y = "Episodic Memory Score (Cube Root)", x= "Sex") 

#change the theme to provide gridlines, and finally save as PDF

figure3_plot = Interaction_Plot + theme_bw()

pdf(file = "./ADNI/figure_3_revised.pdf")
figure3_plot 
dev.off()


#next education*dissolution
#interaction not significant

Dissolution_EM_Educ <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor
                          + Marriage_Dissolution_Factor*PTEDUCAT, 
                          data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(Dissolution_EM_Educ)
confint(Dissolution_EM_Educ)


#next moderation analysis between amyloid and dissolution on memory scores
#interaction not significant

Dissolution_EM_MODERATION <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor + 
                                  CUBEROOT_CENTILOIDS + Marriage_Dissolution_Factor*CUBEROOT_CENTILOIDS, 
                                data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(Dissolution_EM_MODERATION)
confint(Dissolution_EM_MODERATION)



#now we continue with mediation analysis
#first we use the Baron and Kenney criteria for partial mediation otherwise known as the simple regression method

#start with criteria 1 associations between marriage dissolution and episodic memory performance
#as shown previously this is significant

model.0 <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor, 
              data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(model.0)
confint(model.0)

#continue with criteria 2 association between marriage dissolution and amyloid levels
#as shown previously this is significant

model.m <- lm(CUBEROOT_CENTILOIDS ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + TRACER_FACTOR + APOE4_Additive_Factor + Marriage_Dissolution_Factor, 
              data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(model.m)
confint(model.m)

#continue with criteria 3 association between amyloid levels and episodic memory performance - while controlling for marriage dissolution
#this is significant

model.y <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor + CUBEROOT_CENTILOIDS, 
              data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(model.y)
confint(model.y)


#evaluate criteria 4 - Whether correlation between X (marriage dissolution) and Y (cognition) 
#is reduced when including M (Aβ levels) in the model (partial mediation) or disappears (total mediation).

#model.0 results: β: –0.094; 95% CI: –0.155 to –0.033; P= 0.003 <- without amyloid
#model.y results: β: –0.088; 95% CI: –0.149 to –0.027; P= 0.005 <- with amyloid

#when including Aβ pathology in the regression equation, the relationship between marriage dissolution and 
#EM scores becomes weaker in terms of effect size and significance value 
#there is evidence for partial mediation - effect size and p value of dissolution decrease in magnitude

#proceed with causual mediation analysis to confirm results
#further information on this: https://cran.r-project.org/web/packages/mediation/vignettes/mediation.pdf

#install and call the mediation package for this
install.packages("mediation")
library(mediation)

#change the number of digits to 6 using the follow code manually to get the full values for the confidence interval
print.summary.mediate <- mediation:::print.summary.mediate
fix(print.summary.mediate)

#set your seed to maintain a consistent result
set.seed(2023)

#run boostrapping and obtain results 
#model.m is the  med.fit model or the mediator model where the mediator (amyloid) is modeled as a function of the treatment (dissolution)
#in addition to covariates
#model.y is the out.fit model or the outcome model where the outcome (EM scores) is modeled as function of the mediator (amyloid) 
#and treatment variable (dissolution) in addition to covariates

#evidence of a mediation effect
results <- mediate(model.m, model.y, treat='Marriage_Dissolution_Factor', mediator='CUBEROOT_CENTILOIDS',
                   boot=TRUE, sims=5000)
summary(results)

#########################################################################################################################################################################
#main analysis complete
#now ready for sensitivity analysis

#given significant interaction for sex and EM - repeat moderation and mediation analyses in stratified gender groups

#first create a male only database (n=232)

WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_MALE_ONLY <- subset(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER, PTGENDER_Factor=="Male")

#now create a female only database (n=311)

WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_FEMALE_ONLY <- subset(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER, PTGENDER_Factor=="Female")

#conduct moderation analysis between amyloid and dissolution on memory scores specifically for males
#interaction not significant

Dissolution_EM_Male_MODERATION <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTEDUCAT + Marriage_Dissolution_Factor + 
                                       CUBEROOT_CENTILOIDS + Marriage_Dissolution_Factor*CUBEROOT_CENTILOIDS, 
                                     data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_MALE_ONLY)
summary(Dissolution_EM_Male_MODERATION)
confint(Dissolution_EM_Male_MODERATION)

#conduct moderation analysis between amyloid and dissolution on memory scores specifically for females
#interaction not significant

Dissolution_EM_Female_MODERATION <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTEDUCAT + Marriage_Dissolution_Factor + 
                                         CUBEROOT_CENTILOIDS + Marriage_Dissolution_Factor*CUBEROOT_CENTILOIDS, 
                                       data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_FEMALE_ONLY)
summary(Dissolution_EM_Female_MODERATION)
confint(Dissolution_EM_Female_MODERATION)

#mediation analysis regression method for males using the Baron and Kenney criteria 
#overall no evidence of mediation

#start with criteria 1 associations between marriage dissolution and episodic memory performance for males
#this was significant

model.0_male <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTEDUCAT + Marriage_Dissolution_Factor, 
                   data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_MALE_ONLY)
summary(model.0_male)
confint(model.0_male)

#continue with criteria 2 association between marriage dissolution and amyloid levels for males
#this was not significant

model.m_male <- lm(CUBEROOT_CENTILOIDS ~ AGE_AB_PETSCAN_NUMERIC + PTEDUCAT + TRACER_FACTOR + APOE4_Additive_Factor + Marriage_Dissolution_Factor, 
                   data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_MALE_ONLY)
summary(model.m_male)
confint(model.m_male)

#continue with criteria 3 association between amyloid levels and episodic memory performance - while controlling for marriage dissolution for males
#this is significant
model.y_male <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTEDUCAT + Marriage_Dissolution_Factor + CUBEROOT_CENTILOIDS, 
                   data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_MALE_ONLY)
summary(model.y_male)
confint(model.y_male)

#mediation analysis regression method for males using the Baron and Kenney criteria 
#overall no evidence of mediation

#start with criteria 1 associations between marriage dissolution and episodic memory performance for females
#this was not significant

model.0_female <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTEDUCAT + Marriage_Dissolution_Factor, 
                     data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_FEMALE_ONLY)
summary(model.0_female)
confint(model.0_female)

#continue with criteria 2 association between marriage dissolution and amyloid levels for females
#this was significant

model.m_female <- lm(CUBEROOT_CENTILOIDS ~ AGE_AB_PETSCAN_NUMERIC + PTEDUCAT + TRACER_FACTOR + APOE4_Additive_Factor + Marriage_Dissolution_Factor, 
                     data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_FEMALE_ONLY)
summary(model.m_female)
confint(model.m_female)

#continue with criteria 3 association between amyloid levels and episodic memory performance - while controlling for marriage dissolution for females
#this is not significant

model.y_female <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTEDUCAT + Marriage_Dissolution_Factor + CUBEROOT_CENTILOIDS, 
                     data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_FEMALE_ONLY)
summary(model.y_female)
confint(model.y_female)

# to test non-linear effects of age - repeat moderation and mediation analyses using age squared
##A significant age squared implies that the increase in incidence rates varies with age. 
#It would also indicate the age of a rate decline, if such decline exists.
#A meaningful level-off effect, we believe, should occur at an age where there 
#is a considerable proportion of the population still at risk.

##calculate age squared variable 

WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$AGE_AB_PETSCAN_NUMERIC_SQUARED <- WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$AGE_AB_PETSCAN_NUMERIC ^ 2

##ensure it is in the dataset

colnames(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)

#conduct moderation analysis between amyloid and dissolution on memory scores including age squared
#interaction not significant

Dissolution_EM_MODERATION_AgeSquared <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + AGE_AB_PETSCAN_NUMERIC_SQUARED + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor + 
                                             CUBEROOT_CENTILOIDS + Marriage_Dissolution_Factor*CUBEROOT_CENTILOIDS, 
                                           data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(Dissolution_EM_MODERATION_AgeSquared)
confint(Dissolution_EM_MODERATION_AgeSquared)

#mediation analysis regression method using the Baron and Kenney criteria
#as before all criteria met

#start with criteria 1 associations between marriage dissolution and episodic memory performance
#this is significant

model.0_agesquared <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + AGE_AB_PETSCAN_NUMERIC_SQUARED + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor, 
                         data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(model.0_agesquared)
confint(model.0_agesquared)

#continue with criteria 2 association between marriage dissolution and amyloid levels
#this is significant

model.m_agesquared <- lm(CUBEROOT_CENTILOIDS ~ AGE_AB_PETSCAN_NUMERIC + AGE_AB_PETSCAN_NUMERIC_SQUARED + PTGENDER_Factor + PTEDUCAT + TRACER_FACTOR + APOE4_Additive_Factor + Marriage_Dissolution_Factor, 
                         data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(model.m_agesquared)
confint(model.m_agesquared)

#continue with criteria 3 association between amyloid levels and episodic memory performance - while controlling for marriage dissolution for females
#this is significant
model.y_agedsquared <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + AGE_AB_PETSCAN_NUMERIC_SQUARED + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor + CUBEROOT_CENTILOIDS, 
                          data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER)
summary(model.y_agedsquared)
confint(model.y_agedsquared)


#evaluate criteria 4 - Whether correlation between X (marriage dissolution) and Y (cognition) 
#is reduced when including M (Aβ levels) in the model (partial mediation) or disappears (total mediation).

#model.0 results: β: –0.093; 95% CI: –0.154 to –0.032; P= 0.003 <- without amyloid
#model.y results: β: –0.087; 95% CI: –0.148 to –0.026; P= 0.006 <- with amyloid


#set your seed to maintain a consistent result

set.seed(2024)

#run boostrapping and obtain results 
#further evidence of a mediation effect
results_agesquared_seed <- mediate(model.m_agesquared, model.y_agedsquared, treat='Marriage_Dissolution_Factor', mediator='CUBEROOT_CENTILOIDS',
                                   boot=TRUE, sims=5000)

summary(results_agesquared_seed)



#########################################################################################################################################################################
#finally generate descriptive statistics

#start with age at PET scan
#calculate mean and sds and split by marital status group

tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$AGE_AB_PETSCAN_NUMERIC, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, mean)
tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$AGE_AB_PETSCAN_NUMERIC, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, sd)

#conduct a t-test to examine difference in group means 
t.test(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$AGE_AB_PETSCAN_NUMERIC~
         WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor)

#continue with years of education
#calculate mean and sds and split by marital status group

tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$PTEDUCAT, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, mean)
tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$PTEDUCAT, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, sd)

#conduct a t-test to examine difference in group means 
t.test(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$PTEDUCAT~
         WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor)

#continue with sex
#calculate frequency split by marital status group

tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$PTGENDER_Factor, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, table)

#conduct a Chi Square-test to examine difference in frequency 

chisq.test(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$PTGENDER_Factor, 
           WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor)

#next is race
#first create a binary variable for race 

WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$RACE_BINARY [WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$RACE_Factor == "Am Indian/Alaskan" | WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$RACE_Factor == "Asian"| WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$RACE_Factor == "Black"| WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$RACE_Factor == "Hawaiian/Other PI"| WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$RACE_Factor == "More than one"| WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$RACE_Factor == "Unknown"]<- "Other"
WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$RACE_BINARY [WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$RACE_Factor == "White"]<- "White"

#ensure variable creation successful

table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$RACE_BINARY)

#calculate frequency split by marital status group

tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$RACE_BINARY, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, table)

#conduct a Chi Square-test to examine difference in frequency 
chisq.test(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$RACE_BINARY, 
           WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor)

#next is months between baseline and PET scan
#compute a new variable

WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL [WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$VISCODE2 == "bl"] <- 0
WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL [WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$VISCODE2 == "m120"] <- 120
WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL [WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$VISCODE2 == "m132"] <- 132
WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL [WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$VISCODE2 == "m174"] <- 174
WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL [WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$VISCODE2 == "m42"] <- 42
WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL [WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$VISCODE2 == "m48"] <- 48
WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL [WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$VISCODE2 == "m60"] <- 60
WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL [WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$VISCODE2 == "m72"] <- 72
WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL [WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$VISCODE2 == "m84"] <- 84
table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL)

#convert this new variable to numeric

WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL_NUMERIC <- as.numeric(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL)
table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL_NUMERIC)
summary(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL_NUMERIC)

#calculate mean and sds and split by marital status group

tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL_NUMERIC, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, mean)
tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL_NUMERIC, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, sd)

#conduct a t-test to examine difference in group means 

t.test(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$MONTHS_FROM_BL_NUMERIC~
         WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor)

#next is APOE e4 carriers
#calculate frequency split by marital status group

tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$APOE4_Additive_Factor, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, table)

#conduct a Chi Square-test to examine difference in frequency 
chisq.test(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$APOE4_Dominant_Factor, 
           WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor)

#next is amyloid Centiloids score
#calculate mean and sds and split by marital status group

tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$CENTILOIDS, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, mean)
tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$CENTILOIDS, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, sd)

#conduct a t-test to examine difference in group means 

t.test(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$CENTILOIDS~
         WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor)

#next is amyloid tracer
#calculate frequency split by marital status group

tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$TRACER_FACTOR, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, table)

#conduct a Chi Square-test to examine difference in frequency 

chisq.test(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$TRACER_FACTOR, 
           WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor)

#next is EF score
#calculate mean and sds and split by marital status group

tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$ADNI_EF2, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, mean)
tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$ADNI_EF2, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, sd)

#conduct a t-test to examine difference in group means 

t.test(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$ADNI_EF2~
         WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor)

#next is EM score
#calculate mean and sds and split by marital status group

tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$ADNI_MEM, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, mean)
tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$ADNI_MEM, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, sd)

#conduct a t-test to examine difference in group means 

t.test(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$ADNI_MEM~
         WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor)

#finally examine the frequencies of the dissolution groups themselves (widowed v divorced)

tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, table)

tapply(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$PTMARRY_Factor_DROP, 
       WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$Marriage_Dissolution_Factor, table)


#run one additional sensitivity analysis  - split by widowed and divorced groups

#start with the widowhood vs. married analysis

#prepare the data for analysis

table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$PTMARRY_Factor_DROP)

WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_WIDOWSMARRIED <- subset(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER, PTMARRY_Factor_DROP !="Divorced")

table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_WIDOWSMARRIED$PTMARRY_Factor_DROP)

WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_WIDOWSMARRIED$PTMARRY_Factor_DROP_v2 <- droplevels(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_WIDOWSMARRIED$PTMARRY_Factor_DROP)

table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_WIDOWSMARRIED$Marriage_Dissolution_Factor)

#Mediation analysis using Baron-Kenney criteria
#start with criteria 1 associations between marriage dissolution and episodic memory performance for widows vs. married
#this was significant

model.0_widows <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor, 
                     data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_WIDOWSMARRIED)
summary(model.0_widows)
confint(model.0_widows)


#continue with criteria 2 association between marriage dissolution and amyloid levels for widows vs. married
#this was not significant


model.1_widows <- lm(CUBEROOT_CENTILOIDS ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + TRACER_FACTOR + APOE4_Additive_Factor + Marriage_Dissolution_Factor, 
                     data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_WIDOWSMARRIED)
summary(model.1_widows)
confint(model.1_widows)

#continue with criteria 3 association between amyloid levels and episodic memory performance - while controlling for marriage dissolution for widows vs. married
#this is significant

model.y_widows <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor + CUBEROOT_CENTILOIDS, 
                     data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_WIDOWSMARRIED)
summary(model.y_widows)
confint(model.y_widows)


#evaluate criteria 4 - Whether correlation between X (marriage dissolution) and Y (cognition) 
#is reduced when including M (Aβ levels) in the model (partial mediation) or disappears (total mediation).

#model.0 results: β: -0.124961; 95% CI: -0.20907838 to -0.040842663; P= 0.003679 <- without amyloid
#model.y results: β: -0.011485; 95% CI: -0.02271514 to -0.000255029; P= 0.005584 <- with amyloid


#sanity check: associations between marriage dissolution and executive functioning performance for widows vs. married
#this was not significant

model.z_widows <- lm(CUBEROOT_ADNI_EF2 ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor, 
                     data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_WIDOWSMARRIED)
summary(model.z_widows)
confint(model.z_widows)


#conduct moderation analysis between amyloid and dissolution on memory scores for widows vs. married
#interaction not significant

Dissolution_EM_MODERATION_widows <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor + 
                                         CUBEROOT_CENTILOIDS + Marriage_Dissolution_Factor*CUBEROOT_CENTILOIDS, 
                                       data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_WIDOWSMARRIED)
summary(Dissolution_EM_MODERATION_widows)
confint(Dissolution_EM_MODERATION_widows)



#continue with the divorced vs. married analysis

#prepare the data for analysis

table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER$PTMARRY_Factor_DROP)

WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_DIVORCEDSMARRIED <- subset(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER, PTMARRY_Factor_DROP !="Widowed")

table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_DIVORCEDSMARRIED$PTMARRY_Factor_DROP)

WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_DIVORCEDSMARRIED$PTMARRY_Factor_DROP_v2 <- droplevels(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_DIVORCEDSMARRIED$PTMARRY_Factor_DROP)

table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_DIVORCEDSMARRIED$PTMARRY_Factor_DROP_v2)

table(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_DIVORCEDSMARRIED$Marriage_Dissolution_Factor)

#Mediation analysis using Baron-Kenney criteria
#start with criteria 1 associations between marriage dissolution and episodic memory performance for divorced vs. married
#this was not significant

model.0_divorced <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor, 
                       data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_DIVORCEDSMARRIED)
summary(model.0_divorced)
confint(model.0_divorced)


#continue with criteria 2 association between marriage dissolution and amyloid levels for divorced vs. married
#this was significant


model.1_divorced <- lm(CUBEROOT_CENTILOIDS ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + TRACER_FACTOR + APOE4_Additive_Factor + Marriage_Dissolution_Factor, 
                       data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_DIVORCEDSMARRIED)
summary(model.1_divorced)
confint(model.1_divorced)

#continue with criteria 3 association between amyloid levels and episodic memory performance - while controlling for marriage dissolution for divorced vs. married
#this was significant

model.y_divorced <- lm(CUBEROOT_ADNI_MEM ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor + CUBEROOT_CENTILOIDS, 
                       data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_DIVORCEDSMARRIED)
summary(model.y_divorced)
confint(model.y_divorced)


#evaluate criteria 4 - cannot be evaluated since there is no correlation between X (marriage dissolution) and Y (cognition) 


#sanity check: associations between marriage dissolution and executive functioning performance for divorced vs. married
#this was not significant

model.z_divorced <- lm(CUBEROOT_ADNI_EF2 ~ AGE_AB_PETSCAN_NUMERIC + PTGENDER_Factor + PTEDUCAT + Marriage_Dissolution_Factor, 
                       data = WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO_NOOUTLIER_DIVORCEDSMARRIED)
summary(model.z_divorced)
confint(model.z_divorced)


#cannot conduct moderation analysis between amyloid and dissolution on memory scores for widows vs. married - association with memory NOT significant
