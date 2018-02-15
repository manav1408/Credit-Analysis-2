library(readr)
x=read.csv("C:/Users/Manav Middha/Desktop/credit1.csv")

table(dd$Status)
table(dd$Home)
table(dd$Marital)
table(dd$Job)

good = dd$Status != 0 & dd$Home !=0 & dd$Marital !=0 & dd$Job != 0
dd = dd[good, ]

income = dd$Income
assets = dd$Assets
debt = dd$Debt
seniority = dd$Seniority

sum(income == 99999999)    
sum(assets == 99999999)    
sum(debt == 99999999)      
sum(seniority == 99999999) 

income[income == 99999999 | income == 0] <- NA
assets[assets == 99999999] <- NA
debt[debt == 99999999] <- NA

library(class)
dd.aux = dd[,-10]
aux.ok = dd.aux[!is.na(income),]
aux.na = dd.aux[is.na(income),]
knn.income = knn(aux.ok, aux.na, income[!is.na(income)])
income[is.na(income)] = knn.income

dd.aux = dd[,-11]
aux.ok = dd.aux[!is.na(assets),]
aux.na = dd.aux[is.na(assets),]
knn.assets = knn(aux.ok, aux.na, assets[!is.na(assets)])
assets[is.na(assets)] = knn.assets

dd.aux = dd[,-12]
aux.ok = dd.aux[!is.na(debt),]
aux.na = dd.aux[is.na(debt),]
knn.debt = knn(aux.ok, aux.na, debt[!is.na(debt)])
debt[is.na(debt)] = knn.debt

dd$Income = income
dd$Assets = assets
dd$Debt = debt

dd$Finrat = 100 * dd$Amount / dd$Price
dd$Savings = (dd$Income - dd$Expenses - (dd$Debt/100)) / (dd$Amount / dd$Time)