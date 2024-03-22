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
                      subst1_amount_per_form=NULL,
                      subst2_amount_per_form=NULL,
                      subst3_amount_per_form=NULL,
                      subst1_amount_per_form_unit=NULL,
                      subst2_amount_per_form_unit=NULL,
                      subst3_amount_per_form_unit=NULL,
                      subst1_concentration=NULL,
                      subst2_concentration=NULL,
                      subst3_concentration=NULL,
                      subst1_concentration_unit=NULL,
                      subst2_concentration_unit=NULL,
                      subst3_concentration_unit=NULL,
                      concentration_total_content=NULL,
                      concentration_total_content_unit=NULL,
                      dd=NULL,
                      dd_unit=NULL,
                      unit_of_presentation=NULL,
                      presc_quantity_per_day=NULL,
                      output_dd1="CALCULATE_DD_subst1",
                      output_dd2="CALCULATE_DD_subst2",
                      output_dd3="CALCULATE_DD_subst3",
                      output_dd1_unit="CALCULATE_DD_subst1_unit",
                      output_dd2_unit="CALCULATE_DD_subst2_unit",
                      output_dd3_unit="CALCULATE_DD_subst3_unit"
) {

  # list_parameters_names<-c("disp_num_medicinal_product","total_amount_per_medicinal_product","unit_of_presentation_num", "total_amount_per_medicinal_product_unit","subst1_amount_per_form", "subst1_amount_per_form_unit","subst2_amount_per_form", "subst2_amount_per_form_unit","subst3_amount_per_form", "subst3_amount_per_form_unit", "dd","dd_unit", "presc_quantity_per_day", "subst1_concentration","subst2_concentration", "subst3_concentration", "subst1_concentration_unit", "subst2_concentration_unit","subst3_concentration_unit", "concentration_total_content", "concentration_total_content_unit")
  
  library(stringr)
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  units_included<-c("g","mg","ml","l","mol","mmol","mg/ml")
  rescale_table<-data.table(first_unit=c("g","mg","l","ml","mol","mmol"),second_unit=c("mg","g","ml","l","mmol","mol"),rescale_factor=c(rep(c(1000,0.001),3)))
  col_order<-colnames(dataframe)
  
  standard_unit<-c("mg","ml")
  # 
  # list_parameters_names<-c("disp_num_medicinal_product","total_amount_per_medicinal_product","unit_of_presentation_num", "subst_amount_per_form", "subst_amount_per_form_unit","subst1_amount_per_form", "subst1_amount_per_form_unit","subst2_amount_per_form", "subst2_amount_per_form_unit","subst3_amount_per_form", "subst3_amount_per_form_unit", "dd","dd_unit", "presc_quantity_per_day", "subst1_concentration","subst2_concentration",                  "subst3_concentration", "subst1_concentration_unit", "subst2_concentration_unit",                            "subst3_concentration_unit", "concentration_total_content",                        "concentration_total_content_unit")
  
  for (col in colnames(dataframe)){
    if (col %in% names(as.list(environment(), all=TRUE))) {
      setnames(dataframe,col,paste0(col,999))
      assign(col,paste0(col,999))
    }
  }
  
  

  if (recipe == "Units of presentations per day") {
    #check that all mandatory variables are present 
    if (is.null(disp_num_medicinal_product) | is.null(unit_of_presentation_num) | is.null(dd)) {
      stop("For Recipe 'Prescribed quantity', arguments disp_num_medicinal_product, unit_of_presentation_num, dd must be specified")
    }else{
      #compute the specific recipe formula
      dataframe[,(output_var):=get(disp_num_medicinal_product) * get(unit_of_presentation_num) / get(dd)] 

      #Check if the DD chosen is homogeneous with respect to the unit of presentation that describes the medicinal product of interest (e.g., both are described in terms of tablets). 
      dataframe[,check:=ifelse(get(dd_unit)!=get(unit_of_presentation),1,0)]
      if (sum(dataframe[,check])>0) stop("The chosen DD is NOT homogeneous with respect to the unit of presentation that describes the medicinal product of interest. Please check")
      dataframe[,check:=NULL]
      
      #rescale the units of measurement, if necessary
      dataframe<-merge(dataframe,rescale_table,all.x=T,by.x=c(subst1_amount_per_form_unit,dd_unit),by.y=c("first_unit","second_unit"))
      if (sum(!is.na(dataframe[,rescale_factor]))>0) message("The units of measurement has been rescaled to compute the correct number of days of treatment")
      dataframe[is.na(rescale_factor),rescale_factor:=1]

      dataframe[!is.na(get(subst1_amount_per_form)),(output_dd1):=get(disp_num_medicinal_product) * get(subst1_amount_per_form) * get(unit_of_presentation_num) / get(output_var)*rescale_factor]
      
      if (!is.null(subst1_concentration)) {
        dataframe[is.na(get(subst1_concentration)),(subst1_concentration):=0]

        dataframe[,(subst1_concentration):=as.numeric(get(subst1_concentration))]
      }

      dataframe[!is.na(get(subst1_concentration)) & is.na(get(output_dd1)),(output_dd1):=get(disp_num_medicinal_product) * get(subst1_concentration)* get(concentration_total_content) * get(unit_of_presentation_num) / get(output_var)*rescale_factor][,rescale_factor:=NULL]
      setcolorder(dataframe,col_order)
      
      dataframe[,(output_dd1_unit):=get(subst1_amount_per_form_unit)]
      
      
      #if the second active principle is present
      if (!is.null(subst2_amount_per_form)){
        if (is.null(subst2_amount_per_form_unit)) {
          stop("For Recipe 'Prescribed quantity-DD calculation', argument subst2_amount_per_form_unit must be specified")
        } else if (nrow(dataframe[get(subst2_amount_per_form_unit)  %!in% units_included,])>0 ) {
          stop("Not all the units of measurement inputted for the second active principle are  supported by the function.Please check the input data") 
        }
        # dataframe[,(output_dd2):=get(subst2_amount_per_form) * get(presc_quantity_per_day)]
        
        dataframe[!is.na(get(subst2_amount_per_form)),(output_dd2):=get(disp_num_medicinal_product) * get(subst2_amount_per_form) * get(unit_of_presentation_num) / get(output_var)*rescale_factor]
        
        if (!is.null(subst2_concentration)) {
          dataframe[is.na(get(subst2_concentration)),(subst2_concentration):=0]
          
          dataframe[,(subst2_concentration):=as.numeric(get(subst2_concentration))]
        }
        
        dataframe[!is.na(get(subst2_concentration)) & is.na(get(output_dd2)),(output_dd2):=get(disp_num_medicinal_product) * get(subst2_concentration)* get(concentration_total_content) * get(unit_of_presentation_num) / get(output_var)*rescale_factor][,rescale_factor:=NULL]
        setcolorder(dataframe,col_order)
        
        dataframe[,(output_dd2_unit):=get(subst2_amount_per_form_unit)]
      }
      
      
      
      
      
      
      
      
      
      
      #if the third active principle is present
      if (!is.null(subst3_amount_per_form)){
        if (is.null(subst3_amount_per_form_unit)) {
          stop("For Recipe 'Prescribed quantity-DD calculation', argument subst3_amount_per_form_unit must be specified")
        } else if (nrow(dataframe[get(subst3_amount_per_form_unit)  %!in% units_included,])>0 ) {
          stop("The units of measurement inputted for the third active principle are not supported by the function. Check the input data") 
        }
        
        dataframe[!is.na(get(subst3_amount_per_form)),(output_dd3):=get(disp_num_medicinal_product) * get(subst3_amount_per_form) * get(unit_of_presentation_num) / get(output_var)*rescale_factor]
        
        if (!is.null(subst3_concentration)) {
          dataframe[is.na(get(subst3_concentration)),(subst3_concentration):=0]
          
          dataframe[,(subst3_concentration):=as.numeric(get(subst3_concentration))]
        }
        
        dataframe[!is.na(get(subst3_concentration)) & is.na(get(output_dd3)),(output_dd3):=get(disp_num_medicinal_product) * get(subst3_concentration)* get(concentration_total_content) * get(unit_of_presentation_num) / get(output_var)*rescale_factor][,rescale_factor:=NULL]
        setcolorder(dataframe,col_order)
        
        dataframe[,(output_dd3_unit):=get(subst3_amount_per_form_unit)]
      }
    }
  }
  
  
  if (recipe == "Active substance amount per day") { 

    # dataframe<-tolower(dataframe[,..subst_amount_per_form_unit])
    #check that all mandatory variables are present and that all the units of measurement are supported
    if (is.null(disp_num_medicinal_product) | (is.null(subst1_amount_per_form) & is.null(subst1_concentration)) | (is.null(unit_of_presentation_num) & is.null(concentration_total_content)) | is.null(dd) | is.null(subst1_amount_per_form_unit) | is.null(dd_unit) ) {
      stop("For Recipe 'Active substance amount per day' all the arguments disp_num_medicinal_product, subst1_amount_per_form, unit_of_presentation_num,subst1_amount_per_form_unit dd and dd_unit must be specified") #are unit of measurement also mandatory right?
    }


    if (nrow(dataframe[!is.na(get(subst1_amount_per_form_unit)),][get(subst1_amount_per_form_unit)  %!in% units_included,])>0 |nrow(dataframe[!is.na(get(subst1_concentration_unit)),][get(subst1_concentration_unit)  %!in% units_included,])>0 |nrow(dataframe[!is.na(get(concentration_total_content_unit)),][get(concentration_total_content_unit)  %!in% units_included,])>0  | nrow(dataframe[get(dd_unit) %!in% units_included,])>0 ) {
      stop(paste0("The units of measurement inputted are not supported by the function (see the documentation for the complete list).Please check the input data"))
    } 
    #compute the specific recipe formula
    dataframe<-merge(dataframe,rescale_table,all.x=T,by.x=c(subst1_amount_per_form_unit,dd_unit),by.y=c("first_unit","second_unit"))
    if (sum(!is.na(dataframe[,rescale_factor]))>0) message("The units of measurement has been rescaled to compute the correct number of days of treatment")
    dataframe[is.na(rescale_factor),rescale_factor:=1]
    

    dataframe[,total_active_substance1_amount_per_medicinal_product:=fifelse(!is.na(get(subst1_amount_per_form)) & !is.na(get(unit_of_presentation_num)), get(subst1_amount_per_form) * get(unit_of_presentation_num),get(subst1_concentration) * get(concentration_total_content))]

    dataframe[,(output_var):= round((get(disp_num_medicinal_product) * total_active_substance1_amount_per_medicinal_product) / get(dd)*rescale_factor,2)][,rescale_factor:=NULL]
    dataframe[,(output_dd1_unit):=fifelse(!is.na(get(subst1_amount_per_form_unit)),get(subst1_amount_per_form_unit),get(concentration_total_content_unit))]
    
    setcolorder(dataframe,col_order)
  }
  
  
  
  if (recipe == "Amount of pharmaceutical dose form per day") {
    if (is.null(disp_num_medicinal_product) | is.null(concentration_total_content)  | is.null(dd) | is.null(concentration_total_content_unit) | is.null(dd_unit)) {
      stop("For Recipe 'Total substance amount', arguments disp_num_medicinal_product, concentration_total_content, unit_of_presentation_num, dd, concentration_total_content_unit and dd_unit must be specified")
    } else if (nrow(dataframe[get(concentration_total_content_unit)  %!in% units_included,])>0 | nrow(dataframe[get(dd_unit) %!in% units_included,])>0 ) {
      stop("The units of measurement inputted are not supported by the function (see the documentation for the complete list).Please check the input data") 
    } 
    #compute the specific recipe formula
    dataframe<-merge(dataframe,rescale_table,all.x=T,by.x=c(concentration_total_content_unit,dd_unit),by.y=c("first_unit","second_unit"))
    if (sum(!is.na(dataframe[,rescale_factor]))>0) message("The units of measurement has been rescaled to compute the correct number of days of treatment")
    dataframe[is.na(rescale_factor),rescale_factor:=1]
    dataframe[,(output_var):=get(disp_num_medicinal_product) * get(concentration_total_content) / get(dd)*rescale_factor]

    dataframe[,total_active_substance1_amount_per_medicinal_product:=fifelse(!is.na(get(subst1_amount_per_form)) & !is.na(get(unit_of_presentation_num)), get(subst1_amount_per_form) * get(unit_of_presentation_num),get(subst1_concentration) * get(concentration_total_content))]
    
    dataframe[,(output_dd1):=get(disp_num_medicinal_product) * total_active_substance1_amount_per_medicinal_product / get(output_var)*rescale_factor][,rescale_factor:=NULL]
    if (!is.null(subst1_amount_per_form_unit)) {
      dataframe[,(output_dd1_unit):=get(subst1_amount_per_form_unit)]
    }else{
      print("dentro")
      dataframe[,(output_dd1_unit):=get(subst1_concentration_unit)] 
    }

    
    setcolorder(dataframe,col_order)
    
  }
  
  if (recipe == "Prescribed quantity-DD calculation") {
    if (is.null(disp_num_medicinal_product) | is.null(unit_of_presentation_num)  | is.null(presc_quantity_per_day) | is.null(subst1_amount_per_form) |is.null(subst1_amount_per_form_unit)) {
      stop("For Recipe 'Prescribed quantity-DD calculation', arguments disp_num_medicinal_product, unit_of_presentation_num, presc_quantity_per_day, subst1_amount_per_form and subst1_amount_per_form_unit must be specified")
    } else if (nrow(dataframe[get(subst1_amount_per_form_unit)  %!in% units_included,])>0 ) {
      stop(paste0("The units of measurement inputted are not supported by the function (see the documentation for the complete list).Please check the input data"))
    } 
    #compute the specific recipe formula
    dataframe[,(output_var):=get(disp_num_medicinal_product) * get(unit_of_presentation_num) / get(presc_quantity_per_day)]
    
    #compute the specific recipe formula
    dataframe[,(output_dd1):=get(subst1_amount_per_form) * get(presc_quantity_per_day)]
  }
  # #if the second active principle is present
  # if (!is.null(subst2_amount_per_form)){
  #   if (is.null(subst2_amount_per_form_unit)) {
  #     stop("For Recipe 'Prescribed quantity-DD calculation', argument subst2_amount_per_form_unit must be specified")
  #   } else if (nrow(dataframe[get(subst2_amount_per_form_unit)  %!in% units_included,])>0 ) {
  #     stop("Not all the units of measurement inputted for the second active principle are  supported by the function.Please check the input data") 
  #   }
  #   dataframe[,(output_dd2):=get(subst2_amount_per_form) * get(presc_quantity_per_day)]
  # }
  # #if the third active principle is present
  # if (!is.null(subst3_amount_per_form)){
  #   if (is.null(subst3_amount_per_form_unit)) {
  #     stop("For Recipe 'Prescribed quantity-DD calculation', argument subst3_amount_per_form_unit must be specified")
  #   } else if (nrow(dataframe[get(subst3_amount_per_form_unit)  %!in% units_included,])>0 ) {
  #     stop("The units of measurement inputted for the third active principle are not supported by the function. Check the input data") 
  #   }
  #   dataframe[,(output_dd3):=get(subst3_amount_per_form) * get(presc_quantity_per_day)]
  # }
  
  for (col in colnames(dataframe)){
    if (str_detect(col,"999") ) {
      setnames(dataframe,col,sub("999", "", col) )
    }
  }
  return(dataframe) 
}


