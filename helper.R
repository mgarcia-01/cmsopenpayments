top_companies <- function(physician_specialty = NULL,  topN = 10){
  as.character(physician_specialty)

  if(is.null(physician_specialty) == FALSE)
  {
    toppayor <- aggregate(Total_Amount_of_Payment_USDollars ~ Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name
                          + Physician_Specialty
                          ,cmspayments,FUN = sum)
    colnames(toppayor) <- c("Company","TotalPayment", "Physician_Specialty")
    
    
    # used ddply but it gives same result
    #a <-  plyr::ddply(order_prod_prior_dept,.(department, aisle,product_name),summarize
    #             ,no_dist_orders=(length(unique(order_id))))
    names(toppayor) <- c("department", "aisle", "product_name", "no_orders")
    tsort <- toppayor[which(toppayor$department == department & toppayor$aisle == aisle),]
    asort <- head(tsort[order(das$no_orders, decreasing= TRUE),], n = topN)
    return(asort)
  } else if(is.null(department) == TRUE & is.null(aisle) ==  TRUE )
  {toppayor <- aggregate(order_prod_prior_dept$order_id
                     , list(order_prod_prior_dept$department
                            ,order_prod_prior_dept$aisle
                            ,order_prod_prior_dept$product_name)
                     , length)
  names(toppayor) <- c("department", "aisle", "product_name", "no_orders")
  das <- toppayor
  asort <- head(das[order(das$no_orders, decreasing= TRUE),], n = topN)
  return(asort)
  }
}



toppayor <- aggregate(Total_Amount_of_Payment_USDollars ~ Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name
                      + Physician_Specialty
                      ,cmspayments,FUN = sum)
colnames(toppayor) <- c("Company","TotalPayment", "Physician_Specialty")