#aggiungi equivalenza da g a mg

               CreateDOT <- function(dataframe, 
                                     recipe , 
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
  standard_unit<-c("mg")
  list_parameters_names<-c("disp_num_medicinal_product","total_amount_per_medicinal_product","unit_of_presentation_num", "subst_amount_per_form", "subst_amount_per_form_unit","subst_amount_per_form_subst1", "subst_amount_per_form_subst1_unit","subst_amount_per_form_subst2", "subst_amount_per_form_subst2_unit","subst_amount_per_form_subst3", "subst_amount_per_form_subst3_unit", "dd","dd_unit", "presc_quantity_per_day")
  for (col in colnames(dataframe)){
    if (col %in% list_parameters_names) {
      setnames(dataframe,col,paste0(col,999))
      assign(col,paste0(col,999))
    }
  }

  
  if (recipe == "Prescribed quantity") {
    if (is.null(disp_num_medicinal_product) | is.null(unit_of_presentation_num) | is.null(presc_quantity_per_day)) {
      stop("For Recipe 'Prescribed quantity', arguments disp_num_medicinal_product, unit_of_presentation_num, presc_quantity_per_day must be specified")
   }else{
     if (disp_num_medicinal_product==1) {
       warning("The parameter disp_num_medicinal_product is set to 1")
     dataframe[,(output_var):=1 * get(unit_of_presentation_num) / get(presc_quantity_per_day)]
     }else{
       dataframe[,(output_var):=get(disp_num_medicinal_product) * get(unit_of_presentation_num) / get(presc_quantity_per_day)] 
     }
  }
}
 if (recipe == "Substance amount") {
  if (is.null(disp_num_medicinal_product) | is.null(subst_amount_per_form) | is.null(unit_of_presentation_num) | is.null(dd) | is.null(subst_amount_per_form_unit) | is.null(dd_unit) ) {
    stop("For Recipe 'Substance amount' all the arguments disp_num_medicinal_product, subst_amount_per_form, unit_of_presentation_num,subst_amount_per_form_unit dd and dd_unit must be specified") #are unit of measurement also mandatory right?
   } else if (nrow(dataframe[get(subst_amount_per_form_unit)  %!in% units_included,])>0 | nrow(dataframe[get(dd_unit) %!in% units_included,])>0 ) {
     stop("The units of measurement inputted are inconsistent. Input data needs to be pre-processed so the units are consistent") 
   } else if (nrow(dataframe[get(subst_amount_per_form_unit)  %!in% units_included,])==0 & nrow(dataframe[get(dd_unit) %!in% units_included,])==0 & nrow(dataframe[get(subst_amount_per_form_unit)!=get(dd_unit),])>0) {
     warning("The units of measurement has been rescaled to compute the correct number of days of treatment") 
     dataframe[str_detect(get(subst_amount_per_form_unit), "^g") ,(output_var):=(get(disp_num_medicinal_product) * get(subst_amount_per_form) * get(unit_of_presentation_num)*1000) / get(dd)]
     dataframe[ str_detect(get(dd_unit), "^g"),(output_var):=get(disp_num_medicinal_product) * get(subst_amount_per_form) * get(unit_of_presentation_num) / (get(dd)*1000)]
  }

  dataframe[!str_detect(get(subst_amount_per_form_unit), "^g") & !str_detect(get(dd_unit), "^g"),(output_var):=get(disp_num_medicinal_product) * get(subst_amount_per_form) * get(unit_of_presentation_num) / get(dd)]
 }

 if (recipe == "Total substance amount") {
  if (is.null(disp_num_medicinal_product) | is.null(total_amount_per_medicinal_product)  | is.null(dd) | is.null(subst_amount_per_form_unit) | is.null(dd_unit)) {
  stop("For Recipe 'Total substance amount', arguments disp_num_medicinal_product, total_amount_per_medicinal_product, unit_of_presentation_num, dd, subst_amount_per_form_unit and dd_unit must be specified")
  } else if (nrow(dataframe[get(subst_amount_per_form_unit)  %!in% units_included,])>0 | nrow(dataframe[get(dd_unit) %!in% units_included,])>0 ) {
    stop("The units of measurement inputted are inconsistent. Input data needs to be pre-processed so the units are consistent") 
  } else if (nrow(dataframe[get(subst_amount_per_form_unit)  %!in% units_included,])==0 & nrow(dataframe[get(dd_unit) %!in% units_included,])==0 & nrow(dataframe[get(subst_amount_per_form_unit)!=get(dd_unit),])>0) {
    warning("The units of measurement has been rescaled to compute the correct number of days of treatment") 
    dataframe[str_detect(get(subst_amount_per_form_unit), "^g") ,(output_var):=(get(disp_num_medicinal_product) * get(total_amount_per_medicinal_product)*1000) / get(dd)]
    dataframe[ str_detect(get(dd_unit), "^g"),(output_var):=get(disp_num_medicinal_product) * get(total_amount_per_medicinal_product) / (get(dd)*1000)]
  }

  dataframe[!str_detect(get(subst_amount_per_form_unit), "^g") & !str_detect(get(dd_unit), "^g") ,(output_var):=get(disp_num_medicinal_product) * get(total_amount_per_medicinal_product) / get(dd)] 
    
  }
  
if (recipe == "Prescribed quantity-DD calculation") {
  if (is.null(disp_num_medicinal_product) | is.null(unit_of_presentation_num)  | is.null(presc_quantity_per_day) | is.null(subst_amount_per_form_subst1) |is.null(subst_amount_per_form_subst1_unit)) {
    stop("For Recipe 'Prescribed quantity-DD calculation', arguments disp_num_medicinal_product, unit_of_presentation_num, presc_quantity_per_day, subst_amount_per_form_subst1 and subst_amount_per_form_subst1_unit must be specified")
  } else if (nrow(dataframe[get(subst_amount_per_form_subst1_unit)  %!in% units_included,])>0 ) {
    stop("The units of measurement inputted for the first active principle are inconsistent. Input data needs to be pre-processed so the units are consistent") 
  } else if (nrow(dataframe[get(subst_amount_per_form_subst1_unit)  %!in% standard_unit,])>0 ) {
    warning("The units of measurement has been rescaled to compute the correct number of days of treatment") 
    dataframe[str_detect(get(subst_amount_per_form_subst1_unit), "^g") ,(output_dd1):=get(subst_amount_per_form_subst1) * get(presc_quantity_per_day)*1000]
    dataframe[!str_detect(get(subst_amount_per_form_subst1_unit), "^g") ,(output_dd1):=get(subst_amount_per_form_subst1) * get(presc_quantity_per_day)]
  }

    if (!is.null(subst_amount_per_form_subst2)){
      if (is.null(subst_amount_per_form_subst2_unit)) {
        stop("For Recipe 'Prescribed quantity-DD calculation', argument subst_amount_per_form_subst2_unit must be specified")
      } else if (nrow(dataframe[get(subst_amount_per_form_subst2_unit)  %!in% units_included,])>0 ) {
        stop("The units of measurement inputted for the second active principle are inconsistent. Input data needs to be pre-processed so the units are consistent") 
      }
      if (nrow(dataframe[get(subst_amount_per_form_subst2_unit)  %!in% standard_unit,])>0 ) {
        warning("The units of measurement has been rescaled to compute the correct number of days of treatment")
        dataframe[str_detect(get(subst_amount_per_form_subst2_unit), "^g") ,(output_dd2):=get(subst_amount_per_form_subst2) * get(presc_quantity_per_day)*1000]
        dataframe[!str_detect(get(subst_amount_per_form_subst2_unit), "^g") ,(output_dd2):=get(subst_amount_per_form_subst2) * get(presc_quantity_per_day)]
      }
  }
  if (!is.null(subst_amount_per_form_subst3)){
    if (is.null(subst_amount_per_form_subst3_unit)) {
      stop("For Recipe 'Prescribed quantity-DD calculation', argument subst_amount_per_form_subst3_unit must be specified")
    } else if (nrow(dataframe[get(subst_amount_per_form_subst3_unit)  %!in% units_included,])>0 ) {
      stop("The units of measurement inputted for the second active principle are inconsistent. Input data needs to be pre-processed so the units are consistent") 
    }
    if (nrow(dataframe[get(subst_amount_per_form_subst3_unit)  %!in% standard_unit,])>0 ) {
      warning("The units of measurement has been rescaled to compute the correct number of days of treatment")
      dataframe[str_detect(get(subst_amount_per_form_subst3_unit), "^g") ,(output_dd3):=get(subst_amount_per_form_subst3) * get(presc_quantity_per_day)*1000]
      dataframe[!str_detect(get(subst_amount_per_form_subst3_unit), "^g") ,(output_dd3):=get(subst_amount_per_form_subst3) * get(presc_quantity_per_day)]
    }
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
  

