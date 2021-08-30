-------------------------------
# example 4: recipe 4, with one single substance in the medicinal product

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


#load input
input <-fread(paste0(thisdir,"/input/input.csv"), sep = ",")


#USE 

output <- CreateDOT(dataframe = input, 
                    recipe = "Prescribed quantity-DD calculation", 
                    output_var = "DOT_recipe_4", 
                    output_dd1 = "output_dd1", 
                    disp_num_medicinal_product = "disp_num_medicinal_product",
                    unit_of_presentation_num = "unit_of_presentation_num", 
                    presc_quantity_per_day = "presc_quantity_per_day", 
                    subst_amount_per_form_subst1 = "subst_amount_per_form_subst1",
                    subst_amount_per_form_subst1_unit = "subst1_amount_unit"
                    )  

View(output)
fwrite(output,file=paste0(thisdir,"/input/output.csv"))
