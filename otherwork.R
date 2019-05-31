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



```{r histogram, echo=FALSE,include=FALSE}
x <- phys$Total_Amount_of_Payment_USDollars
d <- density(phys$Total_Amount_of_Payment_USDollars)
h<-hist(x, breaks=1000, col="dodger blue"
        , xlab="Payment Amount"
        ,freq = TRUE
        , main="Histogram Freq vs Normal Dist") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=1)
```



```{r visualizespecialty_copy, echo=FALSE,include=FALSE}
#plot_ly(x = c(topCompany$Company)
#       ,y = c(topCompany$TotalPayments)
#       ,type = "bar")
#par(mfrow - c(2,2))
ts <- aggregate(TotalPayments~Physician_Specialty,topspecialty,sum)


b <- ggplot(topspecialty,aes(Company,TotalPayments/1000000
                             ,fill = rep(unlist(Physician_Specialty)))
)

b+geom_bar(stat = "identity"
           ,colour = "dark blue"
           ,fill = "dodger blue") +theme(axis.text.x = element_text(angle=45
                                                                    ,hjust = 1)
                                         ,axis.text.y = element_text(angle = 45)
           )+ labs(y = "Total Payments")
```
