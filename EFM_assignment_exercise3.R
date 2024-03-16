# Exercise 3.


# Clear the workspace
rm(list=ls());    # Clears all objects
cat("\014");      # Clears the console screen
dev.off()   # Clears the Plots window

#Import M&A data from Excel
library(readxl)
# Dependent variable: M&A one day premium (%)
# Data on explanatory variables have been imported from FactSet
MAData <- read_excel("C:Slutlig M&A.xlsx")
View(MAData)

#Introduce the variables to R
Prem <- MAData$`One Day Premium (%)`
EV <- MAData$`Enterprise Value (MM)`
Margin <- MAData$`Profit Margin`   #Ends up not being used
EVEBIT <- MAData$`EV/EBIT`
PB <- MAData$`Price/Tangible Book`
DA <- MAData$`Debt/Assets`
Dstock <- MAData$Dstock
ROA <- MAData$ROA

#Restrictions on the data has been set in FactSet Screening
# Winsorize with the help of the script introduced on EMF lecture 3 by Dr. Jan Antell
library(DescTools)
#Upper bound & Lower bound
wins = c(0.02,0.98,7)

WEVEBIT   = Winsorize(EVEBIT, minval = NULL, maxval = NULL, probs = c(wins[1], wins[2]), na.rm = FALSE, type = wins[3])
WPB       = Winsorize(PB, minval = NULL, maxval = NULL, probs = c(wins[1], wins[2]), na.rm = FALSE, type = wins[3])


WEV   = Winsorize(EV, minval = NULL, maxval = NULL, probs = c(wins[1], wins[2]), na.rm = FALSE, type = wins[3])
WROA   = Winsorize(ROA, minval = NULL, maxval = NULL, probs = c(wins[1], wins[2]), na.rm = FALSE, type = wins[3])


#Adding the winsorized variables to the original data frame

MAData$WEVEBIT <- WEVEBIT
MAData$WPB <- WPB    #WPB ends up not being used
MAData$WEV <- WEV
MAData$WROA <- WROA
View(MAData)


#Descriptive statistics

library(fBasics)
options(scipen = 999)
basicStats(data.frame(Prem, WEV, DA, WEVEBIT, Dstock, WROA))


#OLS estimation with the help of the script introduced on EMF lecture 6 by Dr. Jan Antell

model1 = formula(Prem ~ WEV + DA + WEVEBIT + WROA + Dstock)


reg1   = lm(model1, x=TRUE);


#Illustrations to find data patterns (not relevant for the calculations but relevant for understanding)
library(ggplot2)
ggplot(MAData, aes(x = ROA, y = Prem)) + geom_point()


library(stargazer);
stargazer(reg1, 
          type="text", 
          dep.var.caption = "", 
          dep.var.labels = NULL, 
          intercept.bottom = FALSE,
          model.names=FALSE, 
          model.numbers=FALSE, 
          column.labels = c("Model 1"),
          keep.stat = c("n","f","rsq","adj.rsq"),
          notes.align = "l",
          notes.append = TRUE, report = "vc*st");

library(lmtest); library(sandwich);
res.robustHC  = coeftest(reg1, vcov=vcovHC(reg1, type=c("HC0")));   # HC0: Claasic White (1980), available: HC0, HC1, HC2, HC3, HC4, HC4m, HC5, const (OLS)
robustHCse    = sqrt(diag(vcovHC(reg1, type=c("HC0"))));

res.robustBS  = coeftest(reg1, vcov=vcovBS(reg1, type="residual"));   # Bootstrap
robustBSse    = sqrt(diag(vcovBS(reg1, type="residual")));

res.robustHAC = coeftest(reg1, vcov=vcovHAC(reg1));   # Het and autocorr corrected
robustHACse   = sqrt(diag(vcovHAC(reg1)));


library(stargazer);
stargazer(reg1,reg1,reg1,reg1, 
          type="text", 
          dep.var.caption = "", 
          dep.var.labels = NULL, 
          intercept.bottom = FALSE,
          model.names=FALSE,
          model.numbers=FALSE, 
          column.labels = c("Default SE","HC0SE","Bootstrap SE","HACSE"),
          keep.stat = c("n","f","rsq","adj.rsq"),
          notes.align = "l",
          notes.append = TRUE, 
          report = "vc*st",se=list(NULL,robustHCse,robustBSse,robustHACse));


#Test for normality
library(moments)
jarque.test(residuals(reg1))

#Test for heteroskedasticity
library(lmtest)
bptest(model1)

#Test for multicollinearity using a covariance - variance matrix
cor(as.data.frame(model.matrix(reg1)[,-1]))


#White's test (1980)
multvar  = c("WEV","Dstock","Margin","WROA","WEVEBIT")  
whitelevel  = paste(multvar, collapse=" + ",sep="");
whitesq     = paste("I(",multvar, "^2)", collapse=" + ",sep="");
frml        = lm(paste(paste("residuals(reg1)^2"," ~"),paste(whitelevel, whitesq, collapse=" + ",sep=" + ")),x=TRUE);
whitetestsq = summary(frml)$r.squared * nobs(reg1);
print("White (1980) test for heteroskedasticity (using squares)");
print(paste("Test statistic: ", whitetestsq, "with p-value: ", 1-pchisq(whitetestsq,ncol(model.matrix(frml)[,-1]))));
print(paste("Test statistic: ", whitetestsq, "with p-value: ", 1-pchisq(whitetestsq,ncol(model.matrix(frml)[,-1]))))


#Breusch-Godfrey test for autocorrelation
library(lmtest)
bgtest(model1)