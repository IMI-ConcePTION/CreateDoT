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


#load input
input <-fread(paste0(thisdir,"/input/input.csv"), sep = ",")



#USE 

output <- CreateDOT(dataframe = input, 
                    recipe = "Total substance amount", 
                    output_var = "DOT_recipe_3", 
                    disp_num_medicinal_product = "disp_num_medicinal_product",  
                    total_amount_per_medicinal_product = "total_amount_per_medicinal_product",
                    subst_amount_per_form_unit = "unit_subst_amount_per_form",
                    dd = "dd",
                    dd_unit = "unit_dd"
                    )  

View(output)
fwrite(output,file=paste0(thisdir,"/input/output.csv"))
