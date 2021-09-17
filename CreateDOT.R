#aggiungi equivalenza da g a mg

               CreateDOT <- function(dataframe, 
                                     recipe , 
                                     filter_var=NULL ,
                                     list_values=NULL ,
                                     output_var="days_of_treatment" , 
                                     disp_num_medicinal_product=NULL, 
                                     total_amount_per_medicinal_product=NULL,
                                     total_amount_per_medicinal_product_unit=NULL,
                                     unit_of_presentation_num=NULL, 
                                     subst_amount_per_form=NULL,
                                     subst_amount_per_form_subst1=NULL,
                                     subst_amount_per_form_subst2=NULL,
                                     subst_amount_per_form_subst3=NULL,
                                     subst_amount_per_form_subst1_unit=NULL,
                                     subst_amount_per_form_subst2_unit=NULL,
                                     subst_amount_per_form_subst3_unit=NULL,
                                     subst_amount_per_form_unit=NULL, 
                                     dd=NULL,
                                     dd_unit=NULL,
                                     presc_quantity_per_day=NULL,
                                     output_dd1="output_dd1",
                                     output_dd2="output_dd2",
                                     output_dd3="output_dd3"
                      ) {
  
  library(stringr)
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  units_included<-c("g","mg","ml","l","mol","mmol")
  rescale_table<-data.table(first_unit=c("g","mg","l","ml","mol","mmol"),second_unit=c("mg","g","ml","l","mmol","mol"),rescale_factor=c(rep(c(1000,0.001),3)))
  col_order<-colnames(dataframe)
  
  standard_unit<-c("mg","ml")
  
  list_parameters_names<-c("disp_num_medicinal_product","total_amount_per_medicinal_product","unit_of_presentation_num", "subst_amount_per_form", "subst_amount_per_form_unit","subst_amount_per_form_subst1", "subst_amount_per_form_subst1_unit","subst_amount_per_form_subst2", "subst_amount_per_form_subst2_unit","subst_amount_per_form_subst3", "subst_amount_per_form_subst3_unit", "dd","dd_unit", "presc_quantity_per_day")
  for (col in colnames(dataframe)){
    if (col %in% list_parameters_names) {
      setnames(dataframe,col,paste0(col,999))
      assign(col,paste0(col,999))
    }
  }

  
  if (recipe == "Prescribed quantity") {
    #check that all mandatory variables are present 
    if (is.null(disp_num_medicinal_product) | is.null(unit_of_presentation_num) | is.null(presc_quantity_per_day)) {
      stop("For Recipe 'Prescribed quantity', arguments disp_num_medicinal_product, unit_of_presentation_num, presc_quantity_per_day must be specified")
   }else{
     #compute the specific recipe formula
     if (disp_num_medicinal_product==1) {
       warning("The parameter disp_num_medicinal_product is set to 1")
     dataframe[,(output_var):=1 * get(unit_of_presentation_num) / get(presc_quantity_per_day)]
     }else{
       dataframe[,(output_var):=get(disp_num_medicinal_product) * get(unit_of_presentation_num) / get(presc_quantity_per_day)] 
     }
  }
}
 if (recipe == "Substance amount") {
   # dataframe<-tolower(dataframe[,..subst_amount_per_form_unit])
   #check that all mandatory variables are present and that all the units of measurement are supported
  if (is.null(disp_num_medicinal_product) | is.null(subst_amount_per_form) | is.null(unit_of_presentation_num) | is.null(dd) | is.null(subst_amount_per_form_unit) | is.null(dd_unit) ) {
    stop("For Recipe 'Substance amount' all the arguments disp_num_medicinal_product, subst_amount_per_form, unit_of_presentation_num,subst_amount_per_form_unit dd and dd_unit must be specified") #are unit of measurement also mandatory right?
   } else if (nrow(dataframe[get(subst_amount_per_form_unit)  %!in% units_included,])>0 | nrow(dataframe[get(dd_unit) %!in% units_included,])>0 ) {
     stop(paste0("The units of measurement inputted are not supported by the function (see the documentation for the complete list).Please check the input data"))
   } 
   #compute the specific recipe formula
   dataframe<-merge(dataframe,rescale_table,all.x=T,by.x=c(subst_amount_per_form_unit,dd_unit),by.y=c("first_unit","second_unit"))
   if (sum(!is.na(dataframe[,rescale_factor]))>0) message("The units of measurement has been rescaled to compute the correct number of days of treatment")
   dataframe[is.na(rescale_factor),rescale_factor:=1]
   dataframe[,(output_var):=get(disp_num_medicinal_product) * get(subst_amount_per_form) * get(unit_of_presentation_num) / get(dd)*rescale_factor][,rescale_factor:=NULL]
   setcolorder(dataframe,col_order)
 }

 if (recipe == "Total substance amount") {
  if (is.null(disp_num_medicinal_product) | is.null(total_amount_per_medicinal_product)  | is.null(dd) | is.null(subst_amount_per_form_unit) | is.null(dd_unit)) {
  stop("For Recipe 'Total substance amount', arguments disp_num_medicinal_product, total_amount_per_medicinal_product, unit_of_presentation_num, dd, subst_amount_per_form_unit and dd_unit must be specified")
  } else if (nrow(dataframe[get(subst_amount_per_form_unit)  %!in% units_included,])>0 | nrow(dataframe[get(dd_unit) %!in% units_included,])>0 ) {
    stop("The units of measurement inputted are not supported by the function (see the documentation for the complete list).Please check the input data") 
  } else if (nrow(dataframe[get(subst_amount_per_form_subst1_unit)  %!in% standard_unit,])>0 ) {
    warning("The units of measurement has been rescaled to compute the correct number of days of treatment") 
    dataframe[str_detect(get(subst_amount_per_form_subst1_unit), "^g") ,(output_dd1):=get(subst_amount_per_form_subst1) * get(presc_quantity_per_day)*1000]
  }
   #compute the specific recipe formula
   dataframe<-merge(dataframe,rescale_table,all.x=T,by.x=c(subst_amount_per_form_unit,dd_unit),by.y=c("first_unit","second_unit"))
   if (sum(!is.na(dataframe[,rescale_factor]))>0) message("The units of measurement has been rescaled to compute the correct number of days of treatment")
   dataframe[is.na(rescale_factor),rescale_factor:=1]
   dataframe[,(output_var):=get(disp_num_medicinal_product) * get(total_amount_per_medicinal_product) / get(dd)*rescale_factor][,rescale_factor:=NULL]
   setcolorder(dataframe,col_order)
  }
  
if (recipe == "Prescribed quantity-DD calculation") {
  if (is.null(disp_num_medicinal_product) | is.null(unit_of_presentation_num)  | is.null(presc_quantity_per_day) | is.null(subst_amount_per_form_subst1) |is.null(subst_amount_per_form_subst1_unit)) {
    stop("For Recipe 'Prescribed quantity-DD calculation', arguments disp_num_medicinal_product, unit_of_presentation_num, presc_quantity_per_day, subst_amount_per_form_subst1 and subst_amount_per_form_subst1_unit must be specified")
  } else if (nrow(dataframe[get(subst_amount_per_form_subst1_unit)  %!in% units_included,])>0 ) {
    stop(paste0("The units of measurement inputted are not supported by the function (see the documentation for the complete list).Please check the input data"))
  } else if (nrow(dataframe[get(subst_amount_per_form_subst1_unit)  %!in% standard_unit,])>0 ) {
    message("The units of measurement has been rescaled to compute the correct number of days of treatment") 
    #compute the specific recipe formula
    dataframe[get(subst_amount_per_form_subst1_unit)=="g" ,(output_dd1):=get(subst_amount_per_form_subst1) * get(presc_quantity_per_day)*1000]
    dataframe[!str_detect(get(subst_amount_per_form_subst1_unit), "^g") ,(output_dd1):=get(subst_amount_per_form_subst1) * get(presc_quantity_per_day)]
  }
 #if the second active principle is present
    if (!is.null(subst_amount_per_form_subst2)){
      if (is.null(subst_amount_per_form_subst2_unit)) {
        stop("For Recipe 'Prescribed quantity-DD calculation', argument subst_amount_per_form_subst2_unit must be specified")
      } else if (nrow(dataframe[get(subst_amount_per_form_subst2_unit)  %!in% units_included,])>0 ) {
        stop("The units of measurement inputted for the second active principle are supported by the function.Please check the input data") 
      }
      dataframe<-merge(dataframe,rescale_table,all.x=T,by.x=c(subst_amount_per_form_subst2_unit),by.y=c("first_unit"))
      if (sum(!is.na(dataframe[,rescale_factor]))>0) message("The units of measurement has been rescaled to compute the correct number of days of treatment")
      dataframe[is.na(rescale_factor),rescale_factor:=1]
      dataframe[,(output_dd2):=get(subst_amount_per_form_subst2) * get(presc_quantity_per_day) *rescale_factor]
    }
#if the third active principle is present
  if (!is.null(subst_amount_per_form_subst3)){
    if (is.null(subst_amount_per_form_subst3_unit)) {
      stop("For Recipe 'Prescribed quantity-DD calculation', argument subst_amount_per_form_subst3_unit must be specified")
    } else if (nrow(dataframe[get(subst_amount_per_form_subst3_unit)  %!in% units_included,])>0 ) {
      stop("The units of measurement inputted for the third active principle are not supported by the function. Check the input data") 
     }
    dataframe<-merge(dataframe,rescale_table,all.x=T,by.x=c(subst_amount_per_form_subst3_unit),by.y=c("first_unit"))
    if (sum(!is.na(dataframe[,rescale_factor]))>0) message("The units of measurement has been rescaled to compute the correct number of days of treatment")
    dataframe[is.na(rescale_factor),rescale_factor:=1]
    dataframe[,(output_dd3):=get(subst_amount_per_form_subst3) * get(presc_quantity_per_day) *rescale_factor]
  } 
  dataframe[,(output_var):=get(disp_num_medicinal_product) * get(unit_of_presentation_num) / get(presc_quantity_per_day)]
}

for (col in colnames(dataframe)){
  if (str_detect(col,"999") ) {
    setnames(dataframe,col,sub("999", "", col) )
  }
}
  return(dataframe) 
}
  

