-------------------------------
# example 1: recipe 1

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

setwd("..")
setwd("..")
dirbase<-getwd()
#load function
source(paste0(dirbase,"/CreateDOT.R"))

# load data.table
if (!require("data.table")) install.packages("data.table")
library(data.table)
library(readxl)


#load input
input <-as.data.table(read_excel(paste0(thisdir,"/data/input.xlsx")))

#USE 

output <- CreateDOT(dataframe = input, 
                    recipe = "Units of presentations per day", 
                    output_var = "DOT_recipe_1", #default days_of_treatment
                    disp_num_medicinal_product = "disp_num_medicinal_product",  
                    unit_of_presentation_num = "unit_of_presentation_num", 
                    presc_quantity_per_day = "dd", 
                    subst_amount_per_form_subst1 = "subst1_amount_per_form",
                    subst_amount_per_form_subst1_unit = "subst1_amount_unit",
                    concentration_subst1 = "subst1_concentration",
                    concentration_subst1_unit = "subst1_concentration_unit",
                    concentration_total_content= "concentration_total_content",
                    dd="dd",
                    dd_unit = "unit_dd"
                    )  

View(output)
fwrite(output,file=paste0(thisdir,"/data/output.csv"))
