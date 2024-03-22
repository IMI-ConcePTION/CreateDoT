-------------------------------
# example 3: recipe 3

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
                    recipe = "Amount of pharmaceutical dose form per day", 
                    output_var = "DOT_recipe_3", 
                    output_dd1="CALCULATE_DD_subst1",
                    output_dd1_unit="CALCULATE_DD_subst1_unit",
                    disp_num_medicinal_product = "disp_number_medicinal_product",  
                    concentration_total_content= "concentration_total_content",
                    concentration_total_content_unit= "concentration_total_content_unit",
                    subst1_concentration= "subst1_concentration",
                    subst1_concentration_unit= "subst1_concentration_unit",
                    dd = "dd",
                    dd_unit = "unit_dd"
                    )  

View(output)
fwrite(output,file=paste0(thisdir,"/input/output.csv"))
