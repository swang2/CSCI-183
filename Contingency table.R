setwd("~/Desktop/kaggle/")
GSS <- read.csv("~/Documents/CSCI 183/GSS-gay.csv")
install.packages("vcd")
library(vcd)

newGSS <- GSS[GSS$Gss.year.for.this.respondent == 2010 | GSS$Gss.year.for.this.respondent == 2012,]
newGSS2 <- newGSS[,c("Should.govt.reduce.income.differences","Homosexuals.should.have.right.to.marry")]
newGSS2$row.names <- NULL
tableGSS <- table(newGSS2)
tableGSS
mosaic(tableGSS, shade= TRUE, legend= TRUE)
