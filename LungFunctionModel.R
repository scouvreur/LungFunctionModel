# Clear workspace variables
rm(list = ls())
cat("\014")

# Set working directory
setwd("//ad.ucl.ac.uk/homeo/zcemsco/Documents")

# Read CSV into R
baby <- read.csv(file="day2.csv", header=TRUE, sep=",")

baby$Female <- factor(baby$Female, levels = c(0,1),
               labels = c("Male", "Female"))

baby$Ethniticy <- factor(baby$Ethnicity, levels = c(0,1,2),
                         labels = c("Caucasian","African","Asian"))

baby$Pethidine <- factor(baby$Pethidine, levels = c(0,1),
                         labels = c("No", "Yes"))

# 2-Way Frequency Table 
attach(baby)
mytable <- table(baby$Pethidine, baby$Female) # A will be rows, B will be columns 
mytable # print table 

margin.table(mytable, 1) # A frequencies (summed over B)
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages

plot(baby$Heellen, baby$VmaxFRC)

pairs(~VmaxFRC+Heellen+Gestage+CD4+Birthweight,data=baby, 
      main="Simple Scatterplot Matrix")

# Basic dot plot from the vector "len"
ggplot2.dotplot(data=baby, xName='Pethidine',yName='Gestage')

plot(baby$Birthweight, baby$VmaxFRC)
abline(lm(baby$Birthweight ~ baby$VmaxFRC))

# Simple Bar Plot 
counts <- table(baby$Ethnicity)
barplot(counts, main="Ethnicity Distribution", names.arg=c("Caucasian", "African", "Asian"),
        xlab="Ethnic Group")

model2 <- lm(baby$Birthweight ~ baby$VmaxFRC + baby$Ethnicity + baby$Gestage)
hist(baby$Birthweight)
summary(model2)

ggplot2.scatterplot(data=baby, xName='VmaxFRC',yName='Birthweight',
                    groupName="Pethidine")

model3 <- lm(baby$Birthweight ~ baby$VmaxFRC + baby$Pethidine)
summary(model3)

model4 <- lm(baby$Birthweight ~ baby$VmaxFRC + baby$Pethidine + baby$Ethnicity)
summary(model4)
