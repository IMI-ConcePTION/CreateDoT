#-------------------------------
# example 2:

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dirinput <- paste0(thisdir,"/input/")

#load function
source(paste0(thisdir,"/../../R/MergeFilterAndCollapse_v5.R"))

# load data.table
if (!require("data.table")) install.packages("data.table")
library(data.table)


#load input
load(paste0(dirinput,"/D4_study_population.RData"))
load(paste0(dirinput,"/DIABETES.RData"))
load(paste0(dirinput,"/INSULIN.RData"))
load(paste0(dirinput,"/OTHER_AED.RData"))

#USE THE FUNCTION MergeFilterAndCollapse TO CHECK FOR THE PRESENCE OF DIABETES:	Diagnostic code recorded in 5 years of lookback, stratified per meaning of the diagnosis

diabetes_exist = MergeFilterAndCollapse(list(DIABETES),
                                        D4_study_population[,.(person_id)],
                                        key=c("person_id"),
                                        condition ="date>=index_date-365*5 & date<=index_date",
                                        strata=c("meaning_of_event","person_id"),
                                        summarystat = list(list(c("exist"),"event_code")))



#USE THE FUNCTION MergeFilterAndCollapse TO CHECK FOR THE PRESENCE OF DIABETES:	At least 2 recordings of dispensations and/or prescriptions of antidiabetes medications in 1 year of lookback
diabetes_count = MergeFilterAndCollapse(list(OTHER_AED,INSULIN),
                                        selected_population[,.(person_id)],
                                        key=c("person_id"),
                                        condition ="date>=index_date-365 & date<=index_date",
                                        strata=c("meaning_of_drug_record","person_id"),
                                        summarystat = list(list(c("count"),"date")))


#USE THE FUNCTION MergeFilterAndCollapse TO CHECK FOR THE USE OF INSULINE DURING 2018: at least one recording of dispensations and/or prescriptions during 2018
INSULIN_exist = MergeFilterAndCollapse(list(INSULIN),
                                       selected_population[,.(person_id)],
                                       key=c("person_id"),
                                       condition ="date>=index_date-365 & date<index_date+365",
                                       strata=c("person_id"),
                                       summarystat = list(list(c("exist"),"date")))


#MODIFY NAME OF VARIABLE exist_event_code
suppressMessages(diabetes_exist<-dcast(diabetes_exist, person_id + exist_event_code~ paste0("DIAB_",meaning_of_event) ))
setnames(diabetes_exist,old = c("exist_event_code"),new = c("DIAB_diag"))

save(diabetes_exist,file=paste0(dirtemp,"D3_DIAB_dia.RData"))


#MODIFY NAME OF VARIABLE exist_date
setnames(INSULIN_exist,old = c("exist_date"),new = c("use_insulin"))

save(diabetes_exist,file=paste0(dirtemp,"D3_use_insulin.RData"))
