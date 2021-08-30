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


#load input
input <-fread(paste0(thisdir,"/input/input.csv"), sep = ",")

#USE 

output <- CreateDOT(dataframe = input, 
                    recipe = "Prescribed quantity", 
                    output_var = "DOT_recipe_1", 
                    disp_num_medicinal_product = "disp_num_medicinal_product",  
                    unit_of_presentation_num = "unit_of_presentation_num", 
                    presc_quantity_per_day = "presc_quantity_per_day", 
                    )  

View(output)
fwrite(output,file=paste0(thisdir,"/input/output.csv"))
