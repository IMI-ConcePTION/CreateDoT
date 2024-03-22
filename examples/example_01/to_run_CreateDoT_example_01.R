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
                    output_var = "CALCULATED_DoT",
                    disp_num_medicinal_product = "disp_num_medicinal_product",  
                    unit_of_presentation_num = "unit_of_presentation_num", 
                    subst1_amount_per_form = "subst1_amount_per_form",
                    subst1_amount_per_form_unit = "subst1_amount_unit",
                    subst1_concentration = "subst1_concentration",
                    subst1_concentration_unit = "subst1_concentration_unit",
                    concentration_total_content= "concentration_total_content",
                    dd="dd",
                    dd_unit = "unit_dd", #name of the column containing the chosen dd
                    unit_of_presentation="unit_of_presentation_type",
                    output_dd1="CALCULATE_DD_subst1",
                    output_dd1_unit="CALCULATE_DD_subst1_unit"
                    )  

View(output)
DOT_recipe_1
fwrite(output,file=paste0(thisdir,"/data/output.csv"))
