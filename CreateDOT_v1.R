#aggiungi equivalenza da g a mg

CreateDOT <- function(dataframe, 
                                     recipe , 
                                     output_var="days_of_treatment" , 
                                     disp_num_medicinal_product= 1 , 
                                     total_amount_per_medicinal_product=NULL,
                                     unit_of_presentation_num=NULL, 
                                     subst_amount_per_form=NULL,
                                     unit_subst_amount_per_form=NULL,
                                     dd=NULL,
                                     unit_dd=NULL,
                                     presc_quantity_per_day=NULL) {
  
  library(stringr)
  list_parameters_names<-c("disp_num_medicinal_product","total_amount_per_medicinal_product","unit_of_presentation_num", "subst_amount_per_form", "unit_subst_amount_per_form", "dd","unit_dd", "presc_quantity_per_day")
  for (col in colnames(dataframe)){
    if (col %in% list_parameters_names) {
      setnames(dataframe,col,paste0(col,999))
      assign(col,paste0(col,999))
    }
  }

  if (recipe == 1) {
    if (is.null(disp_num_medicinal_product) | is.null(unit_of_presentation_num) | is.null(presc_quantity_per_day)) {
      stop("For Recipe 1, arguments disp_num_medicinal_product, unit_of_presentation_num, presc_quantity_per_day must be specified")
    # } else if ((!is.null(unit_of_presentation_num)) & (!is.null(presc_quantity_per_day)) & unit_of_presentation_num != presc_quantity_per_day) {
    #  stop("The units of measurement inputted are inconsistent. Input data needs to be pre-processed so the units are consistent")
   }else{
     dataframe[,(output_var):=get(disp_num_medicinal_product) * get(unit_of_presentation_num) / get(presc_quantity_per_day)] 
  }
}
if (recipe == 2) {
  if (is.null(disp_num_medicinal_product) | is.null(subst_amount_per_form) | is.null(unit_of_presentation_num) | is.null(dd)) {
    stop("For Recipe 2, arguments disp_num_medicinal_product, subst_amount_per_form, unit_of_presentation_num, dd must be specified")
  # } else if (!is.null(subst¬_amount_per_form_unit) & !is.null(dd_unit) & substr_amount_per_form_unit != dd_unit) {
  #   stop("The units of measurement inputted are inconsistent. Input data needs to be pre-processed so the units are consistent") 
  }else{
    dataframe[,(output_var):=get(disp_num_medicinal_product) * get(subst_amount_per_form) * get(unit_of_presentation_num) / get(dd)] 
  }
}
  if (recipe == 3) {
    if (is.null(disp_num_medicinal_product) | is.null(total_amount_per_medicinal_product)  | is.null(dd)) {
      stop("For Recipe 3, arguments disp_num_medicinal_product, total_amount_per_medicinal_product, unit_of_presentation_num, dd must be specified")
    # } else if (!is.null(total_amount_per_medicinal_product_unit) & !is.null(dd_unit) & total_amount_per_medicinal_product_unit != dd_unit) {
    #   stop("The units of measurement inputted are inconsistent. Input data needs to be pre-processed so the units are consistent")
    }else{
      dataframe[,(output_var):=get(disp_num_medicinal_product) * get(total_amount_per_medicinal_product) / get(dd)] 
    }
  }
for (col in colnames(dataframe)){
  if (str_detect(col,"999") ) {
    setnames(dataframe,col,sub("999", "", col) )
  }
}
  return(dataframe) 
}
  

