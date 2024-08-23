#install and call dplyr which will be essential for all database construction
install.packages("dplyr")
library(dplyr)

#########################################################################################################################################################################
#ADNI Amyloid PET Database Construction

#create your amyloid PET database for which you will need 3 files from ADNI: Florbetapir PET (AV45PET_DATA_UCBERKELEYAV45_04_26_22.csv)
#Florbetaben PET(UCBERKELEYFBB_8mm_02_17_23_12Jul2023.csv) and finally the Centiloids PET (UCBERKELEY_AMY_6MM_07Jul2023_RAW.csv)

#start by importing the your latest version of the Centiloids PET data (for this study we are using the version dated 07/07/2023) 
#annoyingly this version is missing all data on VISCODE and VISCODE2 making it not possible to merge with demographic data directly

AMYLOID_PET_CENTELOIDS <- read.csv ("~/ADNI/UCBERKELEY_AMY_6MM_07Jul2023_RAW.csv", header=TRUE)

#doublecheck the variable names to ensure you have the right file

colnames(AMYLOID_PET_CENTELOIDS)

#import the Florbetapir PET data (for this study we are using the version dated 26/04/2022)

AMYLOID_PET_FLORBETAPIR_SUVR <- read.csv ("~/ADNI/AV45PET_DATA_UCBERKELEYAV45_04_26_22.csv", header=TRUE)

#extract the variables that only provide information on the amyloid scans from the (old) Florbetapir data
#these include study ID (RID), VISCODE, VISCODE2, exam date, and scan number (TP)

AMYLOID_PET_FLORBETAPIR_SUVR_INFO <- AMYLOID_PET_FLORBETAPIR_SUVR %>% 
  select(RID, VISCODE, VISCODE2, EXAMDATE, TP)

#merge the extracted (informational) variables from the (old) Florbetapir data with the (new) Centiloids data

AMYLOID_PET_CENTELOIDS_v2 <- left_join(AMYLOID_PET_CENTELOIDS,AMYLOID_PET_FLORBETAPIR_SUVR_INFO)

#import the Florbetaben PET data (for this study we are using the version dated 17/02/2022)

AMYLOID_PET_FBB_SUVR <- read.csv ("~/ADNI/UCBERKELEYFBB_8mm_02_17_23_12Jul2023.csv", header=TRUE)

#rename the VISCODE and VISCODE2 variable from your Florbetaben data - this will be important later
#rename them to VISCODE_v2 and VISCODE2_v2 respectively 

AMYLOID_PET_FBB_SUVR$VISCODE_v2 <-AMYLOID_PET_FBB_SUVR$VISCODE
AMYLOID_PET_FBB_SUVR$VISCODE2_v2 <-AMYLOID_PET_FBB_SUVR$VISCODE2

#extract the variables that only provide information on the amyloid scans from the (old) Florbetaben data
#these include study ID (RID), VISCODE, VISCODE2, and exam date

AMYLOID_PET_FBB_SUVR_INFO <- AMYLOID_PET_FBB_SUVR %>% 
  select(RID, VISCODE_v2, VISCODE2_v2, EXAMDATE)

#merge the  extracted (informational) variables from the (old) Florbetaben data with the most recent version of the previously merged Centiloids data (v2)
#you will be using this merged data for the reminder of the PET database construction

AMYLOID_PET_CENTELOIDS_v3 <- left_join(AMYLOID_PET_CENTELOIDS_v2,AMYLOID_PET_FBB_SUVR_INFO)

#use an if else statement to create VISCODE values that include data on both FBP and FBB

AMYLOID_PET_CENTELOIDS_v3$VISCODE_FINAL <- ifelse(AMYLOID_PET_CENTELOIDS_v3$TRACER == "FBB", AMYLOID_PET_CENTELOIDS_v3$VISCODE_v2, AMYLOID_PET_CENTELOIDS_v3$VISCODE)
AMYLOID_PET_CENTELOIDS_v3$VISCODE2_FINAL <- ifelse(AMYLOID_PET_CENTELOIDS_v3$TRACER == "FBB", AMYLOID_PET_CENTELOIDS_v3$VISCODE2_v2, AMYLOID_PET_CENTELOIDS_v3$VISCODE2)

#extract the variables that we plan on using for our PET analysis which are imaging ID, study ID (RID), site ID, patient ID (not too improtant)
#viscode 1 and 2 final variables, scan number (TP), date of processing, image resolution, flagged in QC, tracer name, centiloids value, amyloid positivity, 
#and global SUVR

AMYLOID_PET_CENTELOIDS_v4 <- AMYLOID_PET_CENTELOIDS_v3 %>% 
  select(LONIUID, RID, SITEID, PTID, EXAMDATE, VISCODE_FINAL, VISCODE2_FINAL, TP, PROCESSDATE, IMAGE_RESOLUTION, qc_flag, TRACER, CENTILOIDS,AMYLOID_STATUS, 
         AMYLOID_STATUS_COMPOSITE_REF, SUMMARY_SUVR)

##rename VISCODE values to match their original format

AMYLOID_PET_CENTELOIDS_v4$VISCODE <-AMYLOID_PET_CENTELOIDS_v4$VISCODE_FINAL
AMYLOID_PET_CENTELOIDS_v4$VISCODE2 <-AMYLOID_PET_CENTELOIDS_v4$VISCODE2_FINAL

##extract the variables again including the new VISCODE values

AMYLOID_PET_CENTELOIDS_v5 <- AMYLOID_PET_CENTELOIDS_v4 %>% 
  select(LONIUID, RID, SITEID, PTID, EXAMDATE, VISCODE, VISCODE2, TP, PROCESSDATE, IMAGE_RESOLUTION, qc_flag, TRACER, CENTILOIDS,AMYLOID_STATUS, 
         AMYLOID_STATUS_COMPOSITE_REF, SUMMARY_SUVR)

##create a scan number variable

AMYLOID_PET_CENTELOIDS_v5$SCAN_NUMBER <- ave(AMYLOID_PET_CENTELOIDS_v5$RID, AMYLOID_PET_CENTELOIDS_v5$RID, FUN=seq_along)

##extract the variables again including the scan number

AMYLOID_PET_CENTELOIDS_v6 <- AMYLOID_PET_CENTELOIDS_v5 %>% 
  select(LONIUID, RID, SITEID, PTID, EXAMDATE, VISCODE, VISCODE2, SCAN_NUMBER, PROCESSDATE, IMAGE_RESOLUTION, qc_flag, TRACER, CENTILOIDS,AMYLOID_STATUS, 
         AMYLOID_STATUS_COMPOSITE_REF, SUMMARY_SUVR)

#doublecheck that you have extracted and created all the right variables that you need for the amyloid PET analysis

colnames(AMYLOID_PET_CENTELOIDS_v6)

#export your most recently merged amyloid PET data so that you can manually download it for the next steps (be sure it is downloaded to a secure file location)

write.csv(AMYLOID_PET_CENTELOIDS_v6, "~/ADNI/AMYLOID_PET_CENTELOIDS_v6.csv", row.names=TRUE)

#there are 83 cases that have missing data on VISCODE and/or VISCODE2
#manually enter in this data by using the RID, patient ID, and scan number in combination with the advanced image search on the ADNI server
#you will also need to use the master diagnosis file (DXSUM_PDXCONV_ADNIALL_04Aug2023.csv) to find the VISCODE2 values that 
#correspond with VISCODE2 visit for the respective VISCODE visit (that is noted in the image file finder)

#NOTE 10/04/2023 - THIS PROCESS WAS COMPLETED TWICE BY AC AS A QUALITY CONTROL MEASURE); AC DOUBLECHECKED EACH MISSING VISCODE AND
#VISCODE2 VALUE WITH THE FILE THAT WAS ULTIMATELY IMPORTED BACK INTO R (AFTER MANUALLY FILLING IN VISIT DATA) AND RE-INPUTTING THE 
#MATCHED VALUES INTO A COPY OF THE AMYLOID PET DATA THAT WAS MISSING VISCODE AND VISCODE2 DATA. HE USED THE ADVANCED IMAGE SEARCH IN 
#COMBINATION WITH THE MASTER DIAGNOSIS FILE. ALL OF THE CHANGES MADE ON THE ORIGINAL PET DATA FILE ARE NOTED BELOW IN THIS SCRIPT
#PLEASE CONTACT AC AT A.CHANDRA@QMUL.AC.UK IF THERE ARE ANY ISSUE ABOUT THIS OR HAVE ANY FURTHER QUESTIONS ON THIS PROCESS
#IF YOU HAVE ADNI ACCESS, PLEASE CONTACT AC TO INQUIRE ON THE DETAILS OF THE SPECIFIC SCANS MISSING


#import the Centiloids data with the completed visit code information

AMYLOID_PET_CENTELOIDS_v7 <- read.csv ("~/ADNI/AMYLOID_PET_CENTELOIDS_TO_IMPORT.csv", header=TRUE)

#doublecheck the variable names to ensure you have the right file

colnames(AMYLOID_PET_CENTELOIDS_v7)

#rename the exam date and site ID variables to indicate that these correspond to the amyloid visit 

AMYLOID_PET_CENTELOIDS_v7$EXAMDATE_AMYLOID <- AMYLOID_PET_CENTELOIDS_v7$EXAMDATE
AMYLOID_PET_CENTELOIDS_v7$SITEID_AMYLOID <- AMYLOID_PET_CENTELOIDS_v7$SITEID

##extract the variables again including the newly renamed exam date and site ID variables

AMYLOID_PET_CENTELOIDS_v8 <- AMYLOID_PET_CENTELOIDS_v7 %>% 
  select(RID, SITEID_AMYLOID, VISCODE2, SCAN_NUMBER, PROCESSDATE, IMAGE_RESOLUTION, qc_flag, TRACER, CENTILOIDS,AMYLOID_STATUS, 
         AMYLOID_STATUS_COMPOSITE_REF, SUMMARY_SUVR, EXAMDATE_AMYLOID)

#doublecheck the variable names to ensure you have the right file

colnames(AMYLOID_PET_CENTELOIDS_v8)

##remove any duplicates in your final Centiloid PET data; however there should be none
AMYLOID_PET_CENTELOIDS_v8_NODUP <- AMYLOID_PET_CENTELOIDS_v8 %>% distinct(RID, VISCODE2, .keep_all = TRUE)

#congratulations! your longitudinal ADNI Centiloid PET data is now final!


#########################################################################################################################################################################
#ADNI Diagnosis  Database Construction

#import the master diagnosis file (for this study we are using the version downloaded 04/08/2023)

DIAGNOSIS_FILE_ADNI <- read.csv ("~/ADNI/DXSUM_PDXCONV_ADNIALL_04Aug2023.csv", header=TRUE)

#doublecheck the variable names to ensure you have the right file

colnames (DIAGNOSIS_FILE_ADNI)

#start to recode diagnostic variables from numeric form to catgorical

#this is the advice on determining diagnosis from ADNI: You can extract the baseline diagnosis from LONI Download Study Data 
#>> Diagnosis >> Diagnosis Summary [ADNI1, GO, 2] (DXSUM_PDXCONV_ADNIALL.csv). ADNI1 baseline diagnosis can be 
#>obtained using the “DXCURREN” variable at VISCODE==”bl,” ADNIGO/2 baseline diagnoses can be obtained using the 
#>“DXCHANGE” variable, and ADNI3 baseline diagnosis can be obtained using the “DIAGNOSIS” variable.

#recode DXCURREN variable into diagnostic categories

#KEY: DXCURREN: 1=NL;2=MCI;3=AD

table(DIAGNOSIS_FILE_ADNI$DXCURREN)
DIAGNOSIS_FILE_ADNI$ADNI1DX [DIAGNOSIS_FILE_ADNI$DXCURREN == 1] <- "CN"
DIAGNOSIS_FILE_ADNI$ADNI1DX [DIAGNOSIS_FILE_ADNI$DXCURREN == 2]<- "MCI"
DIAGNOSIS_FILE_ADNI$ADNI1DX [DIAGNOSIS_FILE_ADNI$DXCURREN == 3]<- "AD"
table(DIAGNOSIS_FILE_ADNI$ADNI1DX)

#recode DXCHANGE variable into CNs, MCIs, and Dementia. 

#KEY: DXCHANGE: 1=Stable: NL to NL; 2=Stable: MCI to MCI; 3=Stable: Dementia to Dementia; 4=Conversion: NL to MCI; 
#5=Conversion: MCI to Dementia; 6=Conversion: NL to Dementia; 7=Reversion: MCI to NL; 8=Reversion: Dementia to MCI;
#9=Reversion: Dementia to NL

table(DIAGNOSIS_FILE_ADNI$DXCHANGE)
DIAGNOSIS_FILE_ADNI$ADNI2GODX [DIAGNOSIS_FILE_ADNI$DXCHANGE == 1] <- "CN"
DIAGNOSIS_FILE_ADNI$ADNI2GODX [DIAGNOSIS_FILE_ADNI$DXCHANGE == 2]<- "MCI"
DIAGNOSIS_FILE_ADNI$ADNI2GODX [DIAGNOSIS_FILE_ADNI$DXCHANGE == 3]<- "Dementia"
DIAGNOSIS_FILE_ADNI$ADNI2GODX [DIAGNOSIS_FILE_ADNI$DXCHANGE == 4] <- "MCI"
DIAGNOSIS_FILE_ADNI$ADNI2GODX [DIAGNOSIS_FILE_ADNI$DXCHANGE == 5]<- "Dementia"
DIAGNOSIS_FILE_ADNI$ADNI2GODX [DIAGNOSIS_FILE_ADNI$DXCHANGE == 6]<- "Dementia"
DIAGNOSIS_FILE_ADNI$ADNI2GODX [DIAGNOSIS_FILE_ADNI$DXCHANGE == 7] <- "CN"
DIAGNOSIS_FILE_ADNI$ADNI2GODX [DIAGNOSIS_FILE_ADNI$DXCHANGE == 8]<- "MCI"
DIAGNOSIS_FILE_ADNI$ADNI2GODX [DIAGNOSIS_FILE_ADNI$DXCHANGE == 9]<- "CN"
table(DIAGNOSIS_FILE_ADNI$ADNI2GODX)

#recode DIAGNOSIS variable into CNs, MCIs, and Dementia. 

#KEY: DIAGNOSIS:1=CN;2=MCI;3=Dementia

table(DIAGNOSIS_FILE_ADNI$DIAGNOSIS)
DIAGNOSIS_FILE_ADNI$ADNI3DX [DIAGNOSIS_FILE_ADNI$DIAGNOSIS == 1] <- "CN"
DIAGNOSIS_FILE_ADNI$ADNI3DX [DIAGNOSIS_FILE_ADNI$DIAGNOSIS == 2]<- "MCI"
DIAGNOSIS_FILE_ADNI$ADNI3DX [DIAGNOSIS_FILE_ADNI$DIAGNOSIS == 3]<- "Dementia"
table(DIAGNOSIS_FILE_ADNI$ADNI3DX)

#recode DXCONV into DiseaseStageConverterADNI1. This variable only applies for ADNI1 subjects

#KEY: 1=Yes - Conversion;2=Yes - Reversion; 0=No

table(DIAGNOSIS_FILE_ADNI$DXCONV)
DIAGNOSIS_FILE_ADNI$DiseaseStageConverterADNI1 <- DIAGNOSIS_FILE_ADNI$DXCONV
DIAGNOSIS_FILE_ADNI$DiseaseStageConverterADNI1 [DIAGNOSIS_FILE_ADNI$DXCONV == 0] <- "No change"
DIAGNOSIS_FILE_ADNI$DiseaseStageConverterADNI1 [DIAGNOSIS_FILE_ADNI$DXCONV == 1]<- "Converter"
DIAGNOSIS_FILE_ADNI$DiseaseStageConverterADNI1 [DIAGNOSIS_FILE_ADNI$DXCONV == 2]<- "Reverter"
table(DIAGNOSIS_FILE_ADNI$DiseaseStageConverterADNI1)

#create new variable called DiseaseStageConverter_Visit_ADNI1 to capture subjects that changed diagnosis compared to remaining stable

DIAGNOSIS_FILE_ADNI$DiseaseStageConverter_Visit_ADNI1 <- ifelse(DIAGNOSIS_FILE_ADNI$DiseaseStageConverterADNI1 == "Converter" 
                                                                | DIAGNOSIS_FILE_ADNI$DiseaseStageConverterADNI1 == "Reverter", 
                                                                DIAGNOSIS_FILE_ADNI$VISCODE2, ifelse( DIAGNOSIS_FILE_ADNI$DiseaseStageConverterADNI1 == 
                                                                                                        "No change", "None", "NA"))
table(DIAGNOSIS_FILE_ADNI$DiseaseStageConverter_Visit_ADNI1)

#recode DXCONTYP into ConversionTypeADNI1. This variable only applies for ADNI1 subjects

#KEY: 1=Normal Control to MCI; 2=Normal Control to AD; 3=MCI to AD

table(DIAGNOSIS_FILE_ADNI$DXCONTYP)
DIAGNOSIS_FILE_ADNI$ConversionTypeADNI1 <- DIAGNOSIS_FILE_ADNI$DXCONTYP
DIAGNOSIS_FILE_ADNI$ConversionTypeADNI1 [DIAGNOSIS_FILE_ADNI$DXCONTYP == 1] <- "CN to MCI"
DIAGNOSIS_FILE_ADNI$ConversionTypeADNI1 [DIAGNOSIS_FILE_ADNI$DXCONTYP == 2]<- "CN to AD"
DIAGNOSIS_FILE_ADNI$ConversionTypeADNI1 [DIAGNOSIS_FILE_ADNI$DXCONTYP == 3]<- "MCI to AD"
DIAGNOSIS_FILE_ADNI$ConversionTypeADNI1 [DIAGNOSIS_FILE_ADNI$DXCONTYP == -4]<- "NA"
table(DIAGNOSIS_FILE_ADNI$ConversionTypeADNI1)

#replace NA values as missing values for the ConversionTypeADNI1 variable

DIAGNOSIS_FILE_ADNI <-DIAGNOSIS_FILE_ADNI %>% mutate (ConversionTypeADNI1 = na_if(ConversionTypeADNI1, "NA"))
table(DIAGNOSIS_FILE_ADNI$ConversionTypeADNI1)

#doublecheck missing values excluded for the ConversionTypeADNI1 variable

sum(is.na(DIAGNOSIS_FILE_ADNI$ConversionTypeADNI1))

#recode DXREV into ReversionTypeADNI1. This variable only applies for ADNI1 subjects

#KEY: 1=MCI to Normal Control; 2=AD to MCI; 3=AD to Normal Control

table(DIAGNOSIS_FILE_ADNI$DXREV)
DIAGNOSIS_FILE_ADNI$ReversionTypeADNI1 <- DIAGNOSIS_FILE_ADNI$DXREV
DIAGNOSIS_FILE_ADNI$ReversionTypeADNI1 [DIAGNOSIS_FILE_ADNI$DXREV == 1] <- "MCI to CN"
DIAGNOSIS_FILE_ADNI$ReversionTypeADNI1 [DIAGNOSIS_FILE_ADNI$DXREV == 2]<- "AD to MCI"
DIAGNOSIS_FILE_ADNI$ReversionTypeADNI1 [DIAGNOSIS_FILE_ADNI$DXREV == -4]<- "NA"
table(DIAGNOSIS_FILE_ADNI$ReversionTypeADNI1)

#replace NA values as missing values for the ReversionTypeADNI1 variable

DIAGNOSIS_FILE_ADNI <-DIAGNOSIS_FILE_ADNI %>% mutate (ReversionTypeADNI1 = na_if(ReversionTypeADNI1, "NA"))
table(DIAGNOSIS_FILE_ADNI$ReversionTypeADNI1)

#doublecheck missing values excludedfor the ReversionTypeADNI1 variable

sum(is.na(DIAGNOSIS_FILE_ADNI$ReversionTypeADNI1))

#recode DXNODEP into MildDepressionADNI1. This variable only applies for ADNI1 subjects

#KEY: 1=Yes, Mild depression

table(DIAGNOSIS_FILE_ADNI$DXNODEP)
DIAGNOSIS_FILE_ADNI$MildDepressionADNI1 <- DIAGNOSIS_FILE_ADNI$DXNODEP
DIAGNOSIS_FILE_ADNI$MildDepressionADNI1 [DIAGNOSIS_FILE_ADNI$DXNODEP == 1] <- "Mild Depression"
DIAGNOSIS_FILE_ADNI$MildDepressionADNI1 [DIAGNOSIS_FILE_ADNI$DXNODEP == -4]<- "No Depression"
table(DIAGNOSIS_FILE_ADNI$MildDepressionADNI1)

#recode DXMDES into MCITypeADNI

#KEY: 1=MCI (Memory features); 2=MCI (Non-memory features). Values also include 1:02, 1:2, 1|2, (assuming these are both)

table(DIAGNOSIS_FILE_ADNI$DXMDES)
DIAGNOSIS_FILE_ADNI$MCITypeADNI <- DIAGNOSIS_FILE_ADNI$DXMDES
DIAGNOSIS_FILE_ADNI$MCITypeADNI [DIAGNOSIS_FILE_ADNI$DXMDES == 1] <- "Amnestic MCI"
DIAGNOSIS_FILE_ADNI$MCITypeADNI [DIAGNOSIS_FILE_ADNI$DXMDES == 2]<- "Non-amnestic MCI"
DIAGNOSIS_FILE_ADNI$MCITypeADNI [DIAGNOSIS_FILE_ADNI$DXMDES == -4]<- "NA"
DIAGNOSIS_FILE_ADNI$MCITypeADNI [DIAGNOSIS_FILE_ADNI$DXMDES == "01:02" | DIAGNOSIS_FILE_ADNI$DXMDES == "1:2" | DIAGNOSIS_FILE_ADNI$DXMDES == "1|2"] <- "Both amnestic and non-amnestic MCI"
DIAGNOSIS_FILE_ADNI$MCITypeADNI [DIAGNOSIS_FILE_ADNI$DXMDES == ""]<- "NA"
table(DIAGNOSIS_FILE_ADNI$MCITypeADNI)

#replace NA values as missing values for MCITypeADNI variable
DIAGNOSIS_FILE_ADNI <-DIAGNOSIS_FILE_ADNI %>% mutate (MCITypeADNI = na_if(MCITypeADNI, "NA"))
table(DIAGNOSIS_FILE_ADNI$MCITypeADNI)

#doublecheck missing values excluded for MCITypeADNI variable
sum(is.na(DIAGNOSIS_FILE_ADNI$MCITypeADNI))

#recode DXMDUE into MCIEtiologyADNI

#KEY: 1=MCI due to Alzheimer's Disease; 2=MCI due to other etiology

table(DIAGNOSIS_FILE_ADNI$DXMDUE)
DIAGNOSIS_FILE_ADNI$MCIEtiologyADNI <- DIAGNOSIS_FILE_ADNI$DXMDUE
DIAGNOSIS_FILE_ADNI$MCIEtiologyADNI [DIAGNOSIS_FILE_ADNI$DXMDUE == 1] <- "MCI due to AD"
DIAGNOSIS_FILE_ADNI$MCIEtiologyADNI [DIAGNOSIS_FILE_ADNI$DXMDUE == 2]<- "MCI to to other etiology"
DIAGNOSIS_FILE_ADNI$MCIEtiologyADNI [DIAGNOSIS_FILE_ADNI$DXMDUE == -4]<- "NA"
table(DIAGNOSIS_FILE_ADNI$MCIEtiologyADNI)

#replace NA values as missing values for MCIEtiologyADNI variable

DIAGNOSIS_FILE_ADNI <-DIAGNOSIS_FILE_ADNI %>% mutate (MCIEtiologyADNI = na_if(MCIEtiologyADNI, "NA"))
table(DIAGNOSIS_FILE_ADNI$MCIEtiologyADNI)

#doublecheck missing values excluded for MCIEtiologyADNI variable

sum(is.na(DIAGNOSIS_FILE_ADNI$MCIEtiologyADNI))

#recode DXDSEV into DementiaSeverityADNI23GO This is Dementia Severity - Clinician's Impression. 
#this variable only applies for ADNI2, ADNI3, and ADNIGO subjects

#KEY:1=Mild; 2=Moderate; 3=Severe

table(DIAGNOSIS_FILE_ADNI$DXDSEV)
DIAGNOSIS_FILE_ADNI$DementiaSeverityADNI23GO <- DIAGNOSIS_FILE_ADNI$DXDSEV
DIAGNOSIS_FILE_ADNI$DementiaSeverityADNI23GO [DIAGNOSIS_FILE_ADNI$DXDSEV == 1] <- "Mild Dementia"
DIAGNOSIS_FILE_ADNI$DementiaSeverityADNI23GO [DIAGNOSIS_FILE_ADNI$DXDSEV == 2]<- "Moderate Dementia"
DIAGNOSIS_FILE_ADNI$DementiaSeverityADNI23GO [DIAGNOSIS_FILE_ADNI$DXDSEV == 3]<- "Severe Dementia"
table(DIAGNOSIS_FILE_ADNI$DementiaSeverityADNI23GO)

#recode DXDDUE into DementiaDueToADADNI23GO This is whether the Dementia is due to AD or not
#this variable only applies for ADNI2, ADNI3, and ADNIGO subjects

#KEY:1=Dementia due to Alzheimer's Disease; 2=Dementia due to other etiology

table(DIAGNOSIS_FILE_ADNI$DXDDUE)
DIAGNOSIS_FILE_ADNI$DementiaDueToADADNI23GO <- DIAGNOSIS_FILE_ADNI$DXDDUE
DIAGNOSIS_FILE_ADNI$DementiaDueToADADNI23GO [DIAGNOSIS_FILE_ADNI$DXDDUE == 1] <- "Dementia due to AD"
DIAGNOSIS_FILE_ADNI$DementiaDueToADADNI23GO [DIAGNOSIS_FILE_ADNI$DXDDUE == 2]<- "Dementia due to other etiology"
table(DIAGNOSIS_FILE_ADNI$DementiaDueToADADNI23GO)

#recode DXADES into ADSeverityADNI1 This is AD severity. This variable only applies for ADNI1 subjects

#KEY:1=Mild; 2=Moderate; 3=Severe

table(DIAGNOSIS_FILE_ADNI$DXADES)
DIAGNOSIS_FILE_ADNI$ADSeverityADNI1 <- DIAGNOSIS_FILE_ADNI$DXADES
DIAGNOSIS_FILE_ADNI$ADSeverityADNI1 [DIAGNOSIS_FILE_ADNI$DXADES == 1] <- "Mild AD"
DIAGNOSIS_FILE_ADNI$ADSeverityADNI1 [DIAGNOSIS_FILE_ADNI$DXADES == 2]<- "Moderate AD"
DIAGNOSIS_FILE_ADNI$ADSeverityADNI1 [DIAGNOSIS_FILE_ADNI$DXADES == 3]<- "Severe AD"
DIAGNOSIS_FILE_ADNI$ADSeverityADNI1 [DIAGNOSIS_FILE_ADNI$DXADES == -4]<- "NA"
table(DIAGNOSIS_FILE_ADNI$ADSeverityADNI1)

#replace NA values as missing values for ADSeverityADNI1 variable

DIAGNOSIS_FILE_ADNI <-DIAGNOSIS_FILE_ADNI %>% mutate (ADSeverityADNI1 = na_if(ADSeverityADNI1, "NA"))
table(DIAGNOSIS_FILE_ADNI$ADSeverityADNI1)

#doublecheck missing values excluded for ADSeverityADNI1 variable

sum(is.na(DIAGNOSIS_FILE_ADNI$ADSeverityADNI1))

#recode DXAPP into AD_Status_ADNI. This is whether AD is probable or possible. This variable  applies for all ADNI subjects

#KEY: 1=Probable; 2=Possible

table(DIAGNOSIS_FILE_ADNI$DXAPP)
DIAGNOSIS_FILE_ADNI$AD_Status_ADNI <- DIAGNOSIS_FILE_ADNI$DXAPP
DIAGNOSIS_FILE_ADNI$AD_Status_ADNI [DIAGNOSIS_FILE_ADNI$DXAPP == 1] <- "Probable AD"
DIAGNOSIS_FILE_ADNI$AD_Status_ADNI [DIAGNOSIS_FILE_ADNI$DXAPP == 2]<- "Possible AD"
DIAGNOSIS_FILE_ADNI$AD_Status_ADNI [DIAGNOSIS_FILE_ADNI$DXAPP == -4]<- "NA"
table(DIAGNOSIS_FILE_ADNI$AD_Status_ADNI)

#replace NA values as missing values for AD_Status_ADNI variable

DIAGNOSIS_FILE_ADNI <-DIAGNOSIS_FILE_ADNI %>% mutate (AD_Status_ADNI = na_if(AD_Status_ADNI, "NA"))
table(DIAGNOSIS_FILE_ADNI$AD_Status_ADNI)

#doublecheck missing values excluded for AD_Status_ADNI variable

sum(is.na(DIAGNOSIS_FILE_ADNI$AD_Status_ADNI))

#recode DXPARK into Parkinsonism_Status_ADNI. This is whether subject has Parkinsonism
#This variable  applies for all ADNI subjects

#KEY:1=Yes parkinsonism, 0=No parkinsonism

table(DIAGNOSIS_FILE_ADNI$DXPARK)
DIAGNOSIS_FILE_ADNI$Parkinsonism_Status_ADNI <- DIAGNOSIS_FILE_ADNI$DXPARK
DIAGNOSIS_FILE_ADNI$Parkinsonism_Status_ADNI [DIAGNOSIS_FILE_ADNI$DXPARK == 0] <- "No parkinsonism"
DIAGNOSIS_FILE_ADNI$Parkinsonism_Status_ADNI [DIAGNOSIS_FILE_ADNI$DXPARK == 1]<- "Parkinsonism"
DIAGNOSIS_FILE_ADNI$Parkinsonism_Status_ADNI [DIAGNOSIS_FILE_ADNI$DXPARK == -4]<- "NA"
table(DIAGNOSIS_FILE_ADNI$Parkinsonism_Status_ADNI)

#replace NA values as missing values for Parkinsonism_Status_ADNI variable

DIAGNOSIS_FILE_ADNI <-DIAGNOSIS_FILE_ADNI %>% mutate (Parkinsonism_Status_ADNI = na_if(Parkinsonism_Status_ADNI, "NA"))
table(DIAGNOSIS_FILE_ADNI$Parkinsonism_Status_ADNI)

#doublecheck missing values excluded for Parkinsonism_Status_ADNI variable

sum(is.na(DIAGNOSIS_FILE_ADNI$Parkinsonism_Status_ADNI))

#recode DXDEP into DepressiveSymptomsADNI23. This is whether depressive symptoms present.
#this variable  applies only for ADNI2 and ADNI3 subjects

#KEY:1=Yes; 0=No

table(DIAGNOSIS_FILE_ADNI$DXDEP)
DIAGNOSIS_FILE_ADNI$DepressiveSymptomsADNI23 <- DIAGNOSIS_FILE_ADNI$DXDEP
DIAGNOSIS_FILE_ADNI$DepressiveSymptomsADNI23 [DIAGNOSIS_FILE_ADNI$DXDEP == 0] <- "No depressive symptoms"
DIAGNOSIS_FILE_ADNI$DepressiveSymptomsADNI23 [DIAGNOSIS_FILE_ADNI$DXDEP == 1]<- "Depressive symptoms"
table(DIAGNOSIS_FILE_ADNI$DepressiveSymptomsADNI23)

#recode DXOTHDEM into OtherDementiaADNI1. This is Other Dementia (not Alzheimer's Disease).
#this variable only applies for ADNI1 subjects

#KEY:1=Yes

table(DIAGNOSIS_FILE_ADNI$DXOTHDEM)
DIAGNOSIS_FILE_ADNI$OtherDementiaADNI1 <- DIAGNOSIS_FILE_ADNI$DXOTHDEM
DIAGNOSIS_FILE_ADNI$OtherDementiaADNI1 [DIAGNOSIS_FILE_ADNI$DXOTHDEM == 1]<- "Other Dementia"
DIAGNOSIS_FILE_ADNI$OtherDementiaADNI1 [DIAGNOSIS_FILE_ADNI$DXOTHDEM == -4]<- "NA"
table(DIAGNOSIS_FILE_ADNI$OtherDementiaADNI1)

#replace NA values as missing values for OtherDementiaADNI1 variable

DIAGNOSIS_FILE_ADNI <-DIAGNOSIS_FILE_ADNI %>% mutate (OtherDementiaADNI1 = na_if(OtherDementiaADNI1, "NA"))
table(DIAGNOSIS_FILE_ADNI$OtherDementiaADNI1)

#doublecheck missing values excluded for OtherDementiaADNI1 variable

sum(is.na(DIAGNOSIS_FILE_ADNI$OtherDementiaADNI1))

#drop unneeded or redundent variables from the diagnosis file. These include:
#DXNORM (Normal controls), DXMCI (MCI subjects), DXMPTR1 DXMPTR2 DXMPTR3 DXMPTR4
#DXMPTR5 DXMPTR6 (Specific Pedersen criteria), DXMOTHET (other MCI etiology - codes confusing and differ across ADNI stages)
#DXMOTHSP (specify other etiology - too complicated and redundant). DXAD (ADNI1 AD subjects)
#DXAPROB (If Probable AD, select box(es) for other symptoms present- codes confusing and differ across ADNI stages)
#DXAMETASP (Metabolic/Toxic Disorder (specify) - only 1 person with B12 deficiency), DXAOTHRSP (Other (specify) - only 13 subjects with data) )
#DXAPOSS (If Possible AD, select box(es) to indicate reason: - overlapping categories, differing across ADNI stages, small sample size)
#DXAATYSP (Atypical clinical course or features (specify) - all string and low sample size)
#DXAMETSP (Metabolic / Toxic Disorder (specify) only 5 subjects with data)
#DXAOTHSP (Other (specify) - only 25 subjects and string); DXPARKSP (Describe parkinsonism sypmtoms - string)
#DXPDES (If Parkinsonism, select box which indicates best diagnosis: - only 6 cases)
#DXPCOG (If Parkinsonism with cognitive impairment, demented - only 4 cases)
#DXPATYP (If Atypical Parkinsonism - only 2 cases); DXPOTHSP (Other (specify) - only 1 case)
#DXDEPSP (If yes, please describe - string and difficult to code)
#DXOOTHSP (Other (specify) - only 8 cases)
#DXODES (Other Dementia diagnosis - inconsistent coding across ADNI phases)
#DXCONFID (Physician Confidence in Diagnosis - not relevant)

#extract the included and transformed diagnostic variables

DIAGNOSIS_FILE_ADNI_Filtered <- DIAGNOSIS_FILE_ADNI %>% 
  dplyr::select(Phase, RID, SITEID, VISCODE, VISCODE2, EXAMDATE, ADNI2GODX, ADNI1DX, ADNI3DX, 
                DiseaseStageConverterADNI1, ConversionTypeADNI1, ReversionTypeADNI1, MildDepressionADNI1, 
                DiseaseStageConverter_Visit_ADNI1, MCITypeADNI, MCIEtiologyADNI, DementiaSeverityADNI23GO, 
                DementiaDueToADADNI23GO, ADSeverityADNI1, AD_Status_ADNI, Parkinsonism_Status_ADNI, 
                DepressiveSymptomsADNI23, OtherDementiaADNI1)

#doublecheck the variable names to ensure you have the right file

colnames(DIAGNOSIS_FILE_ADNI_Filtered)  

#Included variable list:
#ADNI1 Variables:ADNI1DX, DiseaseStageConverterADNI1, ConversionTypeADNI1, ReversionTypeADNI1, MildDepressionADNI1,
#DiseaseStageConverter_Visit_ADNI1, ADSeverityADNI1, OtherDementiaADNI1
#ADNI2, ADNIGO, ADNI3 Variables: ADNI2GODX, ADNI3DX, DementiaSeverityADNI23GO, DementiaDueToADADNI23GO, DepressiveSymptomsADNI23
#All ADNI Variables: MCITypeADNI,MCIEtiologyADNI, AD_Status_ADNI, Parkinsonism_Status_ADNI

#the following steps will illustrate how to discuss the FINAL diagnoses variables (across all ADNI stages)


#recode applicable dementia cases into AD cases in ADNI2GODX variable. In essence, you are excluding the non AD dementia cases (n=22) for ADNI2 and GO subjects

DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX_v2 <- ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX == "Dementia" & 
                                                      DIAGNOSIS_FILE_ADNI_Filtered$DementiaDueToADADNI23GO == 
                                                      "Dementia due to AD", "AD", 
                                                    ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX == "MCI", 
                                                           "MCI", ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX == "CN", 
                                                                         "CN", "NA")))
table(DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX)
table(DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX_v2)

#replace NA values as missing values for ADNI2GODX_v2 variable

DIAGNOSIS_FILE_ADNI_Filtered <-DIAGNOSIS_FILE_ADNI_Filtered %>% mutate (ADNI2GODX_v2 = na_if(ADNI2GODX_v2, "NA"))
table(DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX_v2)

#doublecheck missing values excluded for ADNI2GODX_v2 variable

sum(is.na(DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX_v2))

#recode applicable dementia cases into AD cases in ADNI3DX variable. In essence, you are excluding the non AD dementia cases (n=20) for ADNI3 subjects

DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX_v2 <- ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX == "Dementia" & 
                                                    DIAGNOSIS_FILE_ADNI_Filtered$DementiaDueToADADNI23GO == 
                                                    "Dementia due to AD", "AD", ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX == "MCI", 
                                                                                       "MCI", ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX == "CN", 
                                                                                                     "CN", "NA")))
table(DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX)
table(DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX_v2)

#replace NA values as missing values for ADNI3DX_v2 variable

DIAGNOSIS_FILE_ADNI_Filtered <-DIAGNOSIS_FILE_ADNI_Filtered %>% mutate (ADNI3DX_v2 = na_if(ADNI3DX_v2, "NA"))
table(DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX_v2)

#doublecheck missing values excluded for ADNI3DX_v2 variable

sum(is.na(DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX_v2))

#recode MCI cases into MCI due to AD and other etiology in ADNIDX variable for ADNI1 subjects

DIAGNOSIS_FILE_ADNI_Filtered$ADNI1DX_v2 <- ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI1DX == "MCI" & 
                                                    DIAGNOSIS_FILE_ADNI_Filtered$MCIEtiologyADNI == "MCI due to AD", 
                                                  "MCI due to AD", ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI1DX == "MCI" 
                                                                          & DIAGNOSIS_FILE_ADNI_Filtered$MCIEtiologyADNI == "MCI to to other etiology", 
                                                                          "MCI to to other etiology", ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI1DX 
                                                                                                             == "CN", "CN", 
                                                                                                             ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI1DX 
                                                                                                                    == "AD", "AD", "NA"))))
table(DIAGNOSIS_FILE_ADNI_Filtered$ADNI1DX)
table(DIAGNOSIS_FILE_ADNI_Filtered$ADNI1DX_v2)

#recode MCI cases into MCI due to AD and other etiology in ADNI3DX_v2 variable for ADNI3 subjects

DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX_v3 <- ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX_v2 == "MCI" & 
                                                    DIAGNOSIS_FILE_ADNI_Filtered$MCIEtiologyADNI == "MCI due to AD", 
                                                  "MCI due to AD", ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX_v2 == "MCI" 
                                                                          & DIAGNOSIS_FILE_ADNI_Filtered$MCIEtiologyADNI == "MCI to to other etiology", 
                                                                          "MCI to to other etiology", ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX_v2 
                                                                                                             == "CN", "CN", 
                                                                                                             ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX_v2 
                                                                                                                    == "AD", "AD", "NA"))))
table(DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX_v2)
table(DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX_v3)

#recode MCI cases into MCI due to AD and other etiology in ADNI2GODX_v2 variable for ADNI2 and ADNIGO subjects

DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX_v3 <- ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX_v2 == "MCI" & 
                                                      DIAGNOSIS_FILE_ADNI_Filtered$MCIEtiologyADNI == "MCI due to AD", 
                                                    "MCI due to AD", ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX_v2 == 
                                                                              "MCI" & DIAGNOSIS_FILE_ADNI_Filtered$MCIEtiologyADNI == 
                                                                              "MCI to to other etiology", "MCI to to other etiology", 
                                                                            ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX_v2 == "CN", 
                                                                                   "CN", ifelse(DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX_v2 == 
                                                                                                  "AD", "AD", "NA"))))
table(DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX_v2)
table(DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX_v3)

#using the latest ADNI1, ADNI3, and ADNI2GO diagnosis variables, create a final diagnosis variable across all ADNI subjects called DIAGNOSIS_FINAL_ADNI

DIAGNOSIS_FILE_ADNI_Filtered$DIAGNOSIS_FINAL_ADNI [DIAGNOSIS_FILE_ADNI_Filtered$ADNI1DX_v2 == "CN" | DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX_v3== "CN" | DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX_v3== "CN"]   <- "CN"
DIAGNOSIS_FILE_ADNI_Filtered$DIAGNOSIS_FINAL_ADNI [DIAGNOSIS_FILE_ADNI_Filtered$ADNI1DX_v2 == "MCI due to AD" | DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX_v3== "MCI due to AD" | DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX_v3== "MCI due to AD"]   <- "MCI due to AD"
DIAGNOSIS_FILE_ADNI_Filtered$DIAGNOSIS_FINAL_ADNI [DIAGNOSIS_FILE_ADNI_Filtered$ADNI1DX_v2 == "MCI to to other etiology" | DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX_v3== "MCI to to other etiology" | DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX_v3== "MCI to to other etiology"]   <- "MCI to to other etiology"
DIAGNOSIS_FILE_ADNI_Filtered$DIAGNOSIS_FINAL_ADNI [DIAGNOSIS_FILE_ADNI_Filtered$ADNI1DX_v2 == "AD" | DIAGNOSIS_FILE_ADNI_Filtered$ADNI3DX_v3== "AD" | DIAGNOSIS_FILE_ADNI_Filtered$ADNI2GODX_v3== "AD"]   <- "AD"
table(DIAGNOSIS_FILE_ADNI_Filtered$DIAGNOSIS_FINAL_ADNI)

#using latest ADNI1, ADNI3,and ADNI depression variables, create a final depression variable called DEPRESSION_FINAL_ADNI across ADNI123 (no data from ADNIGO for this)

DIAGNOSIS_FILE_ADNI_Filtered$DEPRESSION_FINAL_ADNI [DIAGNOSIS_FILE_ADNI_Filtered$MildDepressionADNI1 == "No Depression" | DIAGNOSIS_FILE_ADNI_Filtered$DepressiveSymptomsADNI23== "No depressive symptoms"]  <- "No depression"
DIAGNOSIS_FILE_ADNI_Filtered$DEPRESSION_FINAL_ADNI [DIAGNOSIS_FILE_ADNI_Filtered$MildDepressionADNI1 == "Mild Depression" | DIAGNOSIS_FILE_ADNI_Filtered$DepressiveSymptomsADNI23== "Depressive symptoms"]  <- "Depressive symptoms"
table(DIAGNOSIS_FILE_ADNI_Filtered$DEPRESSION_FINAL_ADNI)

#using the latest ADNI1, ADNI3, and ADNI2GO AD severity variables (ADSeverityADNI1, DementiaSeverityADNI23GO), 
#create a final AD severity variable across all ADNI subjects called AD_Severity_ADNI_FINAL
#this requires creation of new dataset 

#create a new dataset (DIAGNOSIS_FILE_ADNI_Filtered_v2) with the severity, diagnosis and merging variables filtered by AD cases

DIAGNOSIS_FILE_ADNI_Filtered_v2 <- DIAGNOSIS_FILE_ADNI_Filtered %>% 
  dplyr::select(RID, VISCODE2, VISCODE, DIAGNOSIS_FINAL_ADNI, ADSeverityADNI1,DementiaSeverityADNI23GO) %>% 
  filter(DIAGNOSIS_FINAL_ADNI == "AD" ) 

#create the AD_Severity_ADNI_FINAL variable in this new dataset

DIAGNOSIS_FILE_ADNI_Filtered_v2$AD_Severity_ADNI_FINAL [DIAGNOSIS_FILE_ADNI_Filtered_v2$ADSeverityADNI1 == "Mild AD" | DIAGNOSIS_FILE_ADNI_Filtered_v2$DementiaSeverityADNI23GO == "Mild Dementia"] <- "Mild AD"
DIAGNOSIS_FILE_ADNI_Filtered_v2$AD_Severity_ADNI_FINAL [DIAGNOSIS_FILE_ADNI_Filtered_v2$ADSeverityADNI1 == "Moderate AD" | DIAGNOSIS_FILE_ADNI_Filtered_v2$DementiaSeverityADNI23GO == "Moderate Dementia"] <- "Moderate AD"
DIAGNOSIS_FILE_ADNI_Filtered_v2$AD_Severity_ADNI_FINAL [DIAGNOSIS_FILE_ADNI_Filtered_v2$ADSeverityADNI1 == "Severe AD" | DIAGNOSIS_FILE_ADNI_Filtered_v2$DementiaSeverityADNI23GO == "Severe Dementia"] <- "Severe AD"

#merge this new data set with the original one (add the additional data on the AD cases)

DIAGNOSIS_FILE_ADNI_Filtered_v3 <- left_join(DIAGNOSIS_FILE_ADNI_Filtered,DIAGNOSIS_FILE_ADNI_Filtered_v2)

#doublecheck the variable names to ensure you have the right file

colnames(DIAGNOSIS_FILE_ADNI_Filtered_v3)

#extract the final diagnosis variables

DIAGNOSIS_FILE_ADNI_Filtered_v4 <- DIAGNOSIS_FILE_ADNI_Filtered_v3 %>% 
  dplyr::select(Phase, RID, SITEID,VISCODE, VISCODE2, EXAMDATE, DiseaseStageConverterADNI1, ConversionTypeADNI1, ReversionTypeADNI1, 
                DiseaseStageConverter_Visit_ADNI1,MCITypeADNI, AD_Status_ADNI, Parkinsonism_Status_ADNI,DEPRESSION_FINAL_ADNI, 
                DIAGNOSIS_FINAL_ADNI, AD_Severity_ADNI_FINAL)

#doublecheck the variable names to ensure you have the right file

colnames(DIAGNOSIS_FILE_ADNI_Filtered_v4)          

#rename final diagnosis file (DIAGNOSIS_FILE_ADNI_Filtered_FINAL)

DIAGNOSIS_FILE_ADNI_Filtered_FINAL <- DIAGNOSIS_FILE_ADNI_Filtered_v4

#doublecheck the variable names to ensure you have the right file

colnames(DIAGNOSIS_FILE_ADNI_Filtered_FINAL)          

#export your final diagnosis file to determine reasons for missing diagnosis (be sure it is downloaded to a secure file location)

write.csv(DIAGNOSIS_FILE_ADNI_Filtered_FINAL, "~/ADNI/DIAGNOSIS_FILE_ADNI_Filtered_FINAL.csv", row.names=TRUE)

#reasons include dementia due to other etiology, MCI not classified as due to AD or other
#no classification of dementia as due to AD or other, or no diagnosis data for the observation

#remove any duplicates in your final diagnosis data

DIAGNOSIS_FILE_ADNI_Filtered_FINAL_NODUP <- DIAGNOSIS_FILE_ADNI_Filtered_FINAL %>% distinct(RID, VISCODE2, .keep_all = TRUE)

#congratulations! your longitudinal ADNI diagnostic data is now final!

#########################################################################################################################################################################
#ADNI Demographic Database Construction (ADNIMERGE)

#import the master demographics file, within ADNI, this is the ADNIMERGE file (for this study we are using the version downloaded 04/08/2023)

ADNI_MERGE <- read.csv ("~/ADNI/ADNIMERGE_04Aug2023.csv", header=TRUE)

#doublecheck the variable names to ensure you have the right file

colnames(ADNI_MERGE)

#two issues with the ADNIMERGE data is that the data in VISCODE aligns with the VISCODE2 data but there is no VISCODE 2 variable
#also instead of bl the value is listed as m0 for 1 case

#replace the one "m0" value for the VISCODE variable and rename VISCODE as VISCODE2 to align with the other ADNI databases
table(ADNI_MERGE$VISCODE)
ADNI_MERGE$VISCODE2 <- ifelse(ADNI_MERGE$VISCODE == "m0", "bl", ADNI_MERGE$VISCODE)
table(ADNI_MERGE$VISCODE2)

#rename site, exam date, and age (which is just baseline age for all cases) variables in anticipation of a later data merge

ADNI_MERGE$SITE_MERGE <- ADNI_MERGE$SITE 
ADNI_MERGE$EXAMDATE_MERGE <- ADNI_MERGE$EXAMDATE 
ADNI_MERGE$AGE_BASELINE_MERGE <- ADNI_MERGE$AGE

#extract the variables that we plan on using for future analyses which are  study ID (RID), site ID, VISCODE2, exam date, baseline age
#sex/gender education, ethnicity, race, marital status, and APOE e4 carrier status info

ADNI_MERGE_FILTERED <- ADNI_MERGE %>% 
  dplyr::select(RID, SITE_MERGE, VISCODE2, EXAMDATE_MERGE, AGE_BASELINE_MERGE, PTGENDER, PTEDUCAT, PTETHCAT, PTRACCAT, PTMARRY, APOE4)

#doublecheck the variable names to ensure you have the right file
colnames(ADNI_MERGE_FILTERED)

#replace any missing values for the marital status variable with NA and ensure that R recognises this cases as NA
ADNI_MERGE_FILTERED$PTMARRY[ADNI_MERGE_FILTERED$PTMARRY == ""] <- NA
ADNI_MERGE_FILTERED <-ADNI_MERGE_FILTERED %>% mutate (PTMARRY = na_if(PTMARRY, NA))
table(ADNI_MERGE_FILTERED$PTMARRY)

#remove any duplicates in your final demographics (ADNIMERGE) data
ADNI_MERGE_FILTERED_NODUP <- ADNI_MERGE_FILTERED %>% distinct(RID, VISCODE2, .keep_all = TRUE)
colnames(ADNI_MERGE_FILTERED_NODUP)

#congratulations! your longitudinal ADNI demographics data is now final!

#########################################################################################################################################################################
#ADNI demographics + diagnosis database merging and transforming your variables in this merged data (future analyses: cognition merge)

##merge the final diagnosis and ADNIMERGE files (the ones with duplicates dropped)

DX_ADNIMERGE_NODUP <- left_join(DIAGNOSIS_FILE_ADNI_Filtered_FINAL_NODUP,ADNI_MERGE_FILTERED_NODUP)

#doublecheck the variable names to ensure you have the right file

colnames(DX_ADNIMERGE_NODUP)

#proceed with final variable transformations for the demographic and diagnostic variables in your merged database

#convert the gender variable into a factor using male as the reference group

table(DX_ADNIMERGE_NODUP$PTGENDER)
DX_ADNIMERGE_NODUP$PTGENDER_Factor <- as.factor(DX_ADNIMERGE_NODUP$PTGENDER)
is.factor(DX_ADNIMERGE_NODUP$PTGENDER_Factor)
table(DX_ADNIMERGE_NODUP$PTGENDER_Factor)
DX_ADNIMERGE_NODUP$PTGENDER_Factor<-relevel(DX_ADNIMERGE_NODUP$PTGENDER_Factor, ref ="Male")
table(DX_ADNIMERGE_NODUP$PTGENDER_Factor)

#convert APOE status variable into dominant model variable (NonCarriers=1; Carriers=2) and into a factor using non-carrier as the reference group

DX_ADNIMERGE_NODUP$APOE4_Dominant [DX_ADNIMERGE_NODUP$APOE4 == 0] <- "NonCarrier"
DX_ADNIMERGE_NODUP$APOE4_Dominant [DX_ADNIMERGE_NODUP$APOE4 == 1| DX_ADNIMERGE_NODUP$APOE4== 2] <- "Carrier"
table(DX_ADNIMERGE_NODUP$APOE4)
table(DX_ADNIMERGE_NODUP$APOE4_Dominant)
DX_ADNIMERGE_NODUP$APOE4_Dominant_Factor <- as.factor(DX_ADNIMERGE_NODUP$APOE4_Dominant)
table(DX_ADNIMERGE_NODUP$APOE4_Dominant_Factor)
DX_ADNIMERGE_NODUP$APOE4_Dominant_Factor<-relevel(DX_ADNIMERGE_NODUP$APOE4_Dominant_Factor, ref ="NonCarrier")
table(DX_ADNIMERGE_NODUP$APOE4_Dominant_Factor)

#convert APOE status variable into additive model (NonCarriers=1; HetCarriers=1; HomoCarriers=2) and into a factor using non-carrier as the reference group

DX_ADNIMERGE_NODUP$APOE4_Additive [DX_ADNIMERGE_NODUP$APOE4 == 0] <- "NonCarrier"
DX_ADNIMERGE_NODUP$APOE4_Additive [DX_ADNIMERGE_NODUP$APOE4 == 1] <- "HetCarrier"
DX_ADNIMERGE_NODUP$APOE4_Additive [DX_ADNIMERGE_NODUP$APOE4 == 2] <- "HomoCarrier"
table(DX_ADNIMERGE_NODUP$APOE4)
table(DX_ADNIMERGE_NODUP$APOE4_Additive)
DX_ADNIMERGE_NODUP$APOE4_Additive_Factor <- as.factor(DX_ADNIMERGE_NODUP$APOE4_Additive)
table(DX_ADNIMERGE_NODUP$APOE4_Additive_Factor)
DX_ADNIMERGE_NODUP$APOE4_Additive_Factor<-relevel(DX_ADNIMERGE_NODUP$APOE4_Additive_Factor, ref ="NonCarrier")
table(DX_ADNIMERGE_NODUP$APOE4_Additive_Factor)

#convert marital status variable into a factor using married as the reference group

table(DX_ADNIMERGE_NODUP$PTMARRY)
DX_ADNIMERGE_NODUP$PTMARRY_Factor <- as.factor(DX_ADNIMERGE_NODUP$PTMARRY)
is.factor(DX_ADNIMERGE_NODUP$PTMARRY_Factor)
table(DX_ADNIMERGE_NODUP$PTMARRY_Factor)
DX_ADNIMERGE_NODUP$PTMARRY_Factor<- relevel(DX_ADNIMERGE_NODUP$PTMARRY_Factor, ref ="Married")
table(DX_ADNIMERGE_NODUP$PTMARRY_Factor)

#convert disease stage converter (ADNI1) variable into a factor using no change as the reference group

table(DX_ADNIMERGE_NODUP$DiseaseStageConverterADNI1)
DX_ADNIMERGE_NODUP$DiseaseStageConverterADNI1_Factor <- as.factor(DX_ADNIMERGE_NODUP$DiseaseStageConverterADNI1)
is.factor(DX_ADNIMERGE_NODUP$DiseaseStageConverterADNI1_Factor)
table(DX_ADNIMERGE_NODUP$DiseaseStageConverterADNI1_Factor)
DX_ADNIMERGE_NODUP$DiseaseStageConverterADNI1_Factor<-relevel(DX_ADNIMERGE_NODUP$DiseaseStageConverterADNI1_Factor, ref ="No change")
table(DX_ADNIMERGE_NODUP$DiseaseStageConverterADNI1_Factor)

#convert conversion type (ADNI1) variable into a factor using CN to AD as the reference group

table(DX_ADNIMERGE_NODUP$ConversionTypeADNI1)
DX_ADNIMERGE_NODUP$ConversionTypeADNI1_Factor <- as.factor(DX_ADNIMERGE_NODUP$ConversionTypeADNI1)
is.factor(DX_ADNIMERGE_NODUP$ConversionTypeADNI1_Factor)
table(DX_ADNIMERGE_NODUP$ConversionTypeADNI1_Factor)
DX_ADNIMERGE_NODUP$ConversionTypeADNI1_Factor<-relevel(DX_ADNIMERGE_NODUP$ConversionTypeADNI1_Factor, ref ="CN to AD")
table(DX_ADNIMERGE_NODUP$ConversionTypeADNI1_Factor)

#convert reversion type (ADNI1) variable into factor using MCI to CN as the reference group

table(DX_ADNIMERGE_NODUP$ReversionTypeADNI1)
DX_ADNIMERGE_NODUP$ReversionTypeADNI1_Factor <- as.factor(DX_ADNIMERGE_NODUP$ReversionTypeADNI1)
is.factor(DX_ADNIMERGE_NODUP$ReversionTypeADNI1_Factor)
table(DX_ADNIMERGE_NODUP$ReversionTypeADNI1_Factor)
DX_ADNIMERGE_NODUP$ReversionTypeADNI1_Factor<-relevel(DX_ADNIMERGE_NODUP$ReversionTypeADNI1_Factor, ref ="MCI to CN")
table(DX_ADNIMERGE_NODUP$ReversionTypeADNI1_Factor)

#convert MCI type variable into factor using Amnestic MCI as the reference group

table(DX_ADNIMERGE_NODUP$MCITypeADNI)
DX_ADNIMERGE_NODUP$MCITypeADNI_Factor <- as.factor(DX_ADNIMERGE_NODUP$MCITypeADNI)
is.factor(DX_ADNIMERGE_NODUP$MCITypeADNI_Factor)
table(DX_ADNIMERGE_NODUP$MCITypeADNI_Factor)
DX_ADNIMERGE_NODUP$MCITypeADNI_Factor<-relevel(DX_ADNIMERGE_NODUP$MCITypeADNI_Factor, ref ="Amnestic MCI")
table(DX_ADNIMERGE_NODUP$MCITypeADNI_Factor)

#convert AD status variable into factor using probable AD as the reference group

table(DX_ADNIMERGE_NODUP$AD_Status_ADNI)
DX_ADNIMERGE_NODUP$AD_Status_ADNI_Factor <- as.factor(DX_ADNIMERGE_NODUP$AD_Status_ADNI)
is.factor(DX_ADNIMERGE_NODUP$AD_Status_ADNI_Factor)
table(DX_ADNIMERGE_NODUP$AD_Status_ADNI_Factor)
DX_ADNIMERGE_NODUP$AD_Status_ADNI_Factor<-relevel(DX_ADNIMERGE_NODUP$AD_Status_ADNI_Factor, ref ="Probable AD")
table(DX_ADNIMERGE_NODUP$AD_Status_ADNI_Factor)

#convert Parkinsonism status variable into factor using no parkinsonism as the reference group

table(DX_ADNIMERGE_NODUP$Parkinsonism_Status_ADNI)
DX_ADNIMERGE_NODUP$Parkinsonism_Status_ADNI_Factor <- as.factor(DX_ADNIMERGE_NODUP$Parkinsonism_Status_ADNI)
is.factor(DX_ADNIMERGE_NODUP$Parkinsonism_Status_ADNI_Factor)
table(DX_ADNIMERGE_NODUP$Parkinsonism_Status_ADNI_Factor)
DX_ADNIMERGE_NODUP$Parkinsonism_Status_ADNI_Factor<-relevel(DX_ADNIMERGE_NODUP$Parkinsonism_Status_ADNI_Factor, ref ="No parkinsonism")
table(DX_ADNIMERGE_NODUP$Parkinsonism_Status_ADNI_Factor)

##convert final diagnosis variable into factor using CN as the reference group

table(DX_ADNIMERGE_NODUP$DIAGNOSIS_FINAL_ADNI)
DX_ADNIMERGE_NODUP$DIAGNOSIS_FINAL_ADNI_Factor <- as.factor(DX_ADNIMERGE_NODUP$DIAGNOSIS_FINAL_ADNI)
is.factor(DX_ADNIMERGE_NODUP$DIAGNOSIS_FINAL_ADNI_Factor)
table(DX_ADNIMERGE_NODUP$DIAGNOSIS_FINAL_ADNI_Factor)
DX_ADNIMERGE_NODUP$DIAGNOSIS_FINAL_ADNI_Factor<-relevel(DX_ADNIMERGE_NODUP$DIAGNOSIS_FINAL_ADNI_Factor, ref ="CN")
table(DX_ADNIMERGE_NODUP$DIAGNOSIS_FINAL_ADNI_Factor)

#convert AD severity variable into factor using mild AD as the reference group

table(DX_ADNIMERGE_NODUP$AD_Severity_ADNI_FINAL)
DX_ADNIMERGE_NODUP$AD_Severity_ADNI_FINAL_Factor <- as.factor(DX_ADNIMERGE_NODUP$AD_Severity_ADNI_FINAL)
is.factor(DX_ADNIMERGE_NODUP$AD_Severity_ADNI_FINAL_Factor)
table(DX_ADNIMERGE_NODUP$AD_Severity_ADNI_FINAL_Factor)
DX_ADNIMERGE_NODUP$AD_Severity_ADNI_FINAL_Factor<-relevel(DX_ADNIMERGE_NODUP$AD_Severity_ADNI_FINAL_Factor, ref ="Mild AD")
table(DX_ADNIMERGE_NODUP$AD_Severity_ADNI_FINAL_Factor)

#convert Depression variable into factor using no depression as the reference group

table(DX_ADNIMERGE_NODUP$DEPRESSION_FINAL_ADNI)
DX_ADNIMERGE_NODUP$DEPRESSION_FINAL_ADNI_Factor <- as.factor(DX_ADNIMERGE_NODUP$DEPRESSION_FINAL_ADNI)
is.factor(DX_ADNIMERGE_NODUP$DEPRESSION_FINAL_ADNI_Factor)
table(DX_ADNIMERGE_NODUP$DEPRESSION_FINAL_ADNI_Factor)
DX_ADNIMERGE_NODUP$DEPRESSION_FINAL_ADNI_Factor<-relevel(DX_ADNIMERGE_NODUP$DEPRESSION_FINAL_ADNI_Factor, ref ="No depression")
table(DX_ADNIMERGE_NODUP$DEPRESSION_FINAL_ADNI_Factor)

#convert race variable into factor using white as the reference group

table(DX_ADNIMERGE_NODUP$PTRACCAT)
DX_ADNIMERGE_NODUP$RACE_Factor <- as.factor(DX_ADNIMERGE_NODUP$PTRACCAT)
is.factor(DX_ADNIMERGE_NODUP$RACE_Factor)
table(DX_ADNIMERGE_NODUP$RACE_Factor)
DX_ADNIMERGE_NODUP$RACE_Factor<-relevel(DX_ADNIMERGE_NODUP$RACE_Factor, ref ="White")
table(DX_ADNIMERGE_NODUP$RACE_Factor)

#create an educational degree variable based on the years of education variable
#convert educational degree variable into factor using high school degree as the reference group

table(DX_ADNIMERGE_NODUP$PTEDUCAT)
DX_ADNIMERGE_NODUP$EDUCATIONAL_DEGREE_STATUS_v4 [DX_ADNIMERGE_NODUP$PTEDUCAT <= 12] <- "High School Degree"
DX_ADNIMERGE_NODUP$EDUCATIONAL_DEGREE_STATUS_v4 [DX_ADNIMERGE_NODUP$PTEDUCAT >12 & DX_ADNIMERGE_NODUP$PTEDUCAT <=16  ] <- "Undergraduate Degree"
DX_ADNIMERGE_NODUP$EDUCATIONAL_DEGREE_STATUS_v4 [DX_ADNIMERGE_NODUP$PTEDUCAT >16 & DX_ADNIMERGE_NODUP$PTEDUCAT <=20  ] <- "Postgraduate Degree"
table(DX_ADNIMERGE_NODUP$EDUCATIONAL_DEGREE_STATUS_v4)
DX_ADNIMERGE_NODUP$EDUCATIONAL_DEGREE_STATUS_Factor <- as.factor(DX_ADNIMERGE_NODUP$EDUCATIONAL_DEGREE_STATUS_v4)
is.factor(DX_ADNIMERGE_NODUP$EDUCATIONAL_DEGREE_STATUS_Factor)
table(DX_ADNIMERGE_NODUP$EDUCATIONAL_DEGREE_STATUS_Factor)
DX_ADNIMERGE_NODUP$EDUCATIONAL_DEGREE_STATUS_Factor<-relevel(DX_ADNIMERGE_NODUP$EDUCATIONAL_DEGREE_STATUS_Factor, ref ="High School Degree")
table(DX_ADNIMERGE_NODUP$EDUCATIONAL_DEGREE_STATUS_Factor)

#congratulations! your longitudinal merged ADNI diagnosis/demographics data is now final!

#########################################################################################################################################################################
#ADNI dx/demo and amyloid database merging and subsequent filetering (initial scan/qc) needed for this analysis and merging cognitive scores 

#merge final Centiloid amyloid data with your merged dx and demographic database. 

DX_ADNIMERGE_AMYPET <- left_join(DX_ADNIMERGE_NODUP, AMYLOID_PET_CENTELOIDS_v8_NODUP)

#doublecheck the variable names to ensure you have the right file

colnames(DX_ADNIMERGE_AMYPET)

#the next several steps pertain to creating an age at PET scan variable. This is an involved process that is a bit complicated

#start by creating a new ADNI merge file to work off of

ADNI_MERGE_v2 <- DX_ADNIMERGE_NODUP

#next create a new dataset with just baseline cases

ADNI_MERGE_v2_bl <- subset(ADNI_MERGE_v2, VISCODE2 =="bl")

#rename exam date and baseline age variables in anticipation of a later data merge

ADNI_MERGE_v2_bl$EXAMDATE_MERGE_2 <- ADNI_MERGE_v2_bl$EXAMDATE_MERGE 
ADNI_MERGE_v2_bl$AGE_BASELINE_MERGE_2 <- ADNI_MERGE_v2_bl$AGE_BASELINE_MERGE

#extract  the RID variable and the newly transformed exam date and baseline age variables
#this is a barebones dataset with only data on age and exam date 

ADNI_MERGE_FILTERED_BLAGEEXAMDATE <- ADNI_MERGE_v2_bl %>% 
  dplyr::select(RID, EXAMDATE_MERGE_2, AGE_BASELINE_MERGE_2)

#doublecheck the variable names to ensure you have the right file

colnames(ADNI_MERGE_FILTERED_BLAGEEXAMDATE)

#merge this barebones bl age/exam date dataset with your merged dx, demographic, and centiloid database. 

DX_ADNIMERGE_AMYPET_v2 <- left_join(DX_ADNIMERGE_AMYPET, ADNI_MERGE_FILTERED_BLAGEEXAMDATE)

#whilst the age baseline merge variable is clear, rename exam date merge baseline variable (EXAMDATE_MERGE_2) 
#to more accurately reflect baseline status (EXAMDATE_MERGE_BASELINE)

DX_ADNIMERGE_AMYPET_v2$EXAMDATE_MERGE_BASELINE <- DX_ADNIMERGE_AMYPET_v2$EXAMDATE_MERGE_2 
colnames(DX_ADNIMERGE_AMYPET_v2)

#########################################################################################################################################################################

#next create a new dataset with just initial amyloid PET scans (in addition to dx/demographics)

DX_ADNIMERGE_AMYPET_v2_InitialScan <- subset(DX_ADNIMERGE_AMYPET_v2, SCAN_NUMBER=="1")

##create a new dataset excluding subjects where the baseline age is -1 (assuming this means it is missing)

DX_ADNIMERGE_AMYPET_v2_InitialScan_v2 <- subset(DX_ADNIMERGE_AMYPET_v2_InitialScan, AGE_BASELINE_MERGE>0)

#doublecheck the variable names to ensure you have the right file

colnames(DX_ADNIMERGE_AMYPET_v2_InitialScan_v2)

#install and call lubridate which will be essential for date transformations and calculation of age at PET scan variable

install.packages("lubridate")
library(lubridate)

#convert the 2 exam dates (amyloid scan date and baseline visit exam date) to dmy format

DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$EXAMDATE_AMYLOID_dmy <- dmy(DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$EXAMDATE_AMYLOID)
DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$EXAMDATE_MERGE_BASELINE_dmy <- dmy(DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$EXAMDATE_MERGE_BASELINE)

#calculate the difference between the amyloid PET exam date and baseline ADNI merge exam date (in days) using the date difference function

DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$DATEDIFFERENCE <- DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$EXAMDATE_AMYLOID_dmy  - DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$EXAMDATE_MERGE_BASELINE_dmy

#convert the difference in amyloid PET exam and baseline ADNI merge exam dates to years by dividing the previous value by 365.25 (accounting for leap years)

DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$DATEDIFFERENCE_YEARS <-(DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$DATEDIFFERENCE/365.25)

#add the difference in years between the two exam dates variable to the baseline ADNI merge age variable to generate an age at amyloid PET scan variable

DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$AGE_AB_PETSCAN <- DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$DATEDIFFERENCE_YEARS + DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$AGE_BASELINE_MERGE

#convert age at AB PET scan variable to numeric form

DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$AGE_AB_PETSCAN_NUMERIC <- as.numeric(DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$AGE_AB_PETSCAN)

#doublecheck the variable names to ensure you have the right file

colnames(DX_ADNIMERGE_AMYPET_v2_InitialScan_v2)

#compare the summary stats and values between the 3 age variables to ensure that this makes sense

summary(DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$AGE_AB_PETSCAN_NUMERIC)
summary(DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$AGE_BASELINE_MERGE_2)
summary(DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$AGE_BASELINE_MERGE)
table(DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$AGE_AB_PETSCAN_NUMERIC)
table(DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$AGE_BASELINE_MERGE_2)
table(DX_ADNIMERGE_AMYPET_v2_InitialScan_v2$AGE_BASELINE_MERGE)

#extract the final variables that you will use in your final merged dx/demo/amyloid  data (drop all ADNI1 dx vars as there is no data)

DX_ADNIMERGE_AMYPET_v2_InitialScan_v3 <- DX_ADNIMERGE_AMYPET_v2_InitialScan_v2 %>% 
  dplyr::select(RID, Phase, VISCODE, VISCODE2, DIAGNOSIS_FINAL_ADNI_Factor, MCITypeADNI_Factor, AD_Status_ADNI_Factor, Parkinsonism_Status_ADNI_Factor, AD_Severity_ADNI_FINAL_Factor,DEPRESSION_FINAL_ADNI_Factor,
                AGE_AB_PETSCAN_NUMERIC, AGE_BASELINE_MERGE, PTGENDER_Factor, PTEDUCAT, EDUCATIONAL_DEGREE_STATUS_Factor, RACE_Factor, PTMARRY_Factor, APOE4_Additive_Factor, APOE4_Dominant_Factor, 
                SCAN_NUMBER, PROCESSDATE, IMAGE_RESOLUTION, qc_flag, TRACER, CENTILOIDS,AMYLOID_STATUS, 
                AMYLOID_STATUS_COMPOSITE_REF, SUMMARY_SUVR)

#doublecheck the variable names to ensure you have the right file

colnames(DX_ADNIMERGE_AMYPET_v2_InitialScan_v3)

#extract data on only initial amyloid scans that passed QC (qc value=2). We lose 227 scans through this method and end up with 1478.

table(DX_ADNIMERGE_AMYPET_v2_InitialScan_v3$qc_flag)
DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc <- subset(DX_ADNIMERGE_AMYPET_v2_InitialScan_v3, qc_flag==2)
table(DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc$qc_flag)

#convert tracer variable into factor using florbetaben as the reference group

table(DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc$TRACER)
DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc$TRACER_FACTOR <- as.factor(DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc$TRACER)
is.factor(DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc$TRACER_FACTOR)
table(DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc$TRACER_FACTOR)
DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc$TRACER_FACTOR<-relevel(DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc$TRACER_FACTOR, ref ="FBB")
table(DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc$TRACER_FACTOR)

#import the master cognitive summary score data (for this study we are using the version 23/01/2023)

COGNITIVE_SUMMARYSCORE <- read.csv ("~/ADNI/UWNPSYCHSUM_01_23_23_04Aug2023.csv", header=TRUE)

#doublecheck the variable names to ensure you have the right file

colnames(COGNITIVE_SUMMARYSCORE)

#extract the variables that we plan on using for future analyses which are study ID (RID), VISCODE2, and cognitive summary scores (1 and 2 - USE 2 IN ANALYSIS)

COGNITIVE_SUMMARYSCORE_v2 <- COGNITIVE_SUMMARYSCORE %>% 
  dplyr::select(RID, VISCODE2, ADNI_MEM, ADNI_EF, ADNI_LAN, ADNI_VS, ADNI_EF2)

#merge cognitive data with your merged dx/demo/amyloid_intial_qced_scan database. Given that the dx/demo/amyloid files had duplicates dropped, 
#there should be no need for this step again

DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc_COG <- left_join(DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc, COGNITIVE_SUMMARYSCORE_v2)

#congratulations! your cross-sectional merged ADNI diagnosis/demographics/cognition/amyloid qced data is now final!

#########################################################################################################################################################################
#FINAL DATABASE FILTERING STEPS FOR CURRENT ANALYSIS

#create a new dataset with just CN subjects (this is based on your final merged cross-sectional database)
DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc_COG_CN <- subset(DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc_COG, DIAGNOSIS_FINAL_ADNI_Factor=="CN")
table(DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc_COG_CN$PTMARRY_Factor)

#########################################################################################################################################################################

#create a new dataset with just CN subjects (this is based on your final merged cross-sectional database) who are either married, widowed, or divorced

WIDOWEDDIVORCED_COMBINED <-  subset(DX_ADNIMERGE_AMYPET_v2_InitialScan_v4qc_COG_CN, PTMARRY_Factor=="Married" | PTMARRY_Factor=="Divorced"| PTMARRY_Factor=="Widowed")

#for the marital status variable drop variable levels that are empty (never married, and unknown) to make a new marital status variable
table(WIDOWEDDIVORCED_COMBINED$PTMARRY_Factor)
WIDOWEDDIVORCED_COMBINED$PTMARRY_Factor_DROP <- droplevels(WIDOWEDDIVORCED_COMBINED$PTMARRY_Factor)
table(WIDOWEDDIVORCED_COMBINED$PTMARRY_Factor_DROP)

#doublecheck the variable names to ensure you have the right file

colnames(WIDOWEDDIVORCED_COMBINED) 

#create a marraige dissolved variable that groups widows and divorcees into one category and married people into another

WIDOWEDDIVORCED_COMBINED$Marriage_Disolved [WIDOWEDDIVORCED_COMBINED$PTMARRY_Factor_DROP == "Widowed"| WIDOWEDDIVORCED_COMBINED$PTMARRY_Factor_DROP == "Divorced"] <- "Marriage Dissolved"
WIDOWEDDIVORCED_COMBINED$Marriage_Disolved [WIDOWEDDIVORCED_COMBINED$PTMARRY_Factor_DROP == "Married"] <- "Married"
table(WIDOWEDDIVORCED_COMBINED$Marriage_Disolved)

#convert the marraige dissolved variable into factor using married as the reference group

WIDOWEDDIVORCED_COMBINED$Marriage_Dissolution_Factor <- as.factor(WIDOWEDDIVORCED_COMBINED$Marriage_Disolved)
table(WIDOWEDDIVORCED_COMBINED$Marriage_Dissolution_Factor)
WIDOWEDDIVORCED_COMBINED$Marriage_Dissolution_Factor<-relevel(WIDOWEDDIVORCED_COMBINED$Marriage_Dissolution_Factor, ref ="Married")
table(WIDOWEDDIVORCED_COMBINED$Marriage_Dissolution_Factor)

#extract the final variables that you will be using for this analysis, including RID, VISCODE, demographics, and cognitive data (all CN)

WIDOWEDDIVORCED_COMBINED_v2 <- WIDOWEDDIVORCED_COMBINED %>% 
  dplyr::select(RID, VISCODE2, AGE_AB_PETSCAN_NUMERIC, PTGENDER_Factor, PTEDUCAT, CENTILOIDS, APOE4_Additive_Factor, APOE4_Dominant_Factor,
                TRACER_FACTOR, PTMARRY_Factor_DROP, Marriage_Dissolution_Factor, ADNI_MEM, ADNI_LAN, 
                ADNI_VS, ADNI_EF2, RACE_Factor)

#drop all cases with missing values

WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO <- na.omit(WIDOWEDDIVORCED_COMBINED_v2)

#congratulations! your data is now ready for the current analysis

##create a csv version of this file

write.csv(WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO, "~/ADNI/WIDOWEDDIVORCED_COMBINED_v2_MISSINGNO.csv", row.names=TRUE)
