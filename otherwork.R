### other work 



```{r topspecialtyother, echo=TRUE,include=TRUE}
#names(cmspayments)
#Companies is "Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name"

sdf <- aggregate(Total_Amount_of_Payment_USDollars ~ Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name
                 +Physician_Specialty
                 ,cmspayments
                 ,FUN = sum)
colnames(sdf) <- c("Company","Physician_Specialty","TotalPayment")
sdf[order("TotalPayment","Physician_Specialty","Company"),]

tspecial <- merge(toppayor,sdf, by = "Company")
tspecial[c("Company","Physician_Specialty","TotalPayment.y")]





toppayor <- aggregate(Total_Amount_of_Payment_USDollars ~ Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name
                      + Physician_Specialty
                      ,cmspayments,FUN = sum)
colnames(toppayor) <- c("Company", "Physician_Specialty","TotalPayment")
toppayor <- head(toppayor[order(toppayor$TotalPayment, decreasing= TRUE),], n = 100)
toppayor

```