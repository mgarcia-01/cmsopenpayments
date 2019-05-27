top_products <- function(department = NULL, aisle = NULL, topN = 10){
  as.character(department)
  as.character(aisle)
  
  if(is.null(department) == FALSE & is.null(aisle) == FALSE)
  {
    
    oppd <- aggregate(order_prod_prior_dept$order_id
                      , list(order_prod_prior_dept$department
                             ,order_prod_prior_dept$aisle
                             ,order_prod_prior_dept$product_name)
                      , length)
    # used ddply but it gives same result
    #a <-  plyr::ddply(order_prod_prior_dept,.(department, aisle,product_name),summarize
    #             ,no_dist_orders=(length(unique(order_id))))
    names(oppd) <- c("department", "aisle", "product_name", "no_orders")
    das <- oppd[which(oppd$department == department & oppd$aisle == aisle),]
    asort <- head(das[order(das$no_orders, decreasing= TRUE),], n = topN)
    return(asort)
  } else if(is.null(department) == TRUE & is.null(aisle) ==  TRUE )
  {oppd <- aggregate(order_prod_prior_dept$order_id
                     , list(order_prod_prior_dept$department
                            ,order_prod_prior_dept$aisle
                            ,order_prod_prior_dept$product_name)
                     , length)
  names(oppd) <- c("department", "aisle", "product_name", "no_orders")
  das <- oppd
  asort <- head(das[order(das$no_orders, decreasing= TRUE),], n = topN)
  return(asort)
  } 
}