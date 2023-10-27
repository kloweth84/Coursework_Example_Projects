library(beeswarm)
library(mice)
library(MASS)
library(ggplot2)
library(reshape)

school = read.csv('NC data.csv', header = T)
school2 = read.csv('NC data2.csv', header = T)


S = school[,seq(3,17)]
cor(S)

S2 = school2[, seq(3,16)]
cor(S2, na.rm = "T")

mice_all = mice(school2[,seq(3,12)], m=5)


NC1 = complete(mice_all,1)

NC2 = complete(mice_all, 2)

NC3 = complete(mice_all, 3)

NC1$grade <- as.factor(NC1$grade)
NC1$Region <- as.factor(NC1$Region)
NC2$grade <- as.factor(NC2$grade)
NC2$Region <- as.factor(NC2$Region)
NC3$grade <- as.factor(NC3$grade)
NC3$Region <- as.factor(NC3$Region)

#checking for normalities
qqnorm(NC1$gapblk_ela)
qqnorm(NC1$gaphsp_ela)
qqnorm(NC1$gapblk_math)
qqnorm(NC1$gaphsp_math)
#straight lines

#boxplots for NC demographics
ggplot(NC1, aes(x = Region, y = MFI, fill = Region)) + geom_boxplot() + ggtitle("Median Family Incomes in NC") + xlab("Region in NC") + ylab("Median Family Income (in 2009 Dollars)")
ggplot(NC1, aes(x = Region, y = BLPPer, fill = Region)) + geom_boxplot() + ggtitle("Black Population Percentages NC") + xlab("Region in NC") + ylab("Percent of District that is Black")
ggplot(NC1, aes(x = Region, y = HSPPer, fill = Region)) + geom_boxplot() + ggtitle("Hispanic Population Percentages NC") + xlab("Region in NC") + ylab("Percent of District that is Hispanic")
ggplot(NC1, aes(x=BLPPer, y=HSPPer)) + geom_point() + ggtitle ("School District Minority Populations") + xlab("African American Population Percentage") + ylab("Hispanic Population Percentage")

##black ELA
#grade and region
boxplot(gapblk_ela[grade == 3]~Region[grade == 3], data = NC1)
boxplot(gapblk_ela[grade == 4]~Region[grade == 4], data = NC1)
boxplot(gapblk_ela[grade == 5]~Region[grade == 5], data = NC1)
boxplot(gapblk_ela[grade == 6]~Region[grade == 6], data = NC1)
boxplot(gapblk_ela[grade == 7]~Region[grade == 7], data = NC1)
boxplot(gapblk_ela[grade == 8]~Region[grade == 8], data = NC1)

#boxplot by grade
ggplot(NC1, aes(x=grade, y = gapblk_ela, fill = grade)) + geom_boxplot()

#plots of gapblk_ela by MFI and BLLPer
ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = MFI, y = gapblk_ela, color = grade)) + geom_point() + ggtitle("Black-White ELA Gap vs. MFI") + xlab("2009 Median Family Income") + ylab("Black-White English Language Arts Score Gap")
ggplot(subset(NC1, Region %in% c(seq(1,8))), aes(x = MFI, y = gapblk_ela, color = Region)) + geom_point() + ggtitle("Black-White ELA Gap vs. MFI") + xlab("2009 Median Family Income") + ylab("Black-White English Language Arts Score Gap")

ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = BLPPer, y = gapblk_ela, color = grade)) + geom_point() + ggtitle("Black-White ELA Gap vs. Black Population") + xlab("Black Population Percentage") + ylab("Black-White English Language Arts Score Gap")
ggplot(subset(NC1, Region %in% c(seq(1,8))), aes(x = BLPPer, y = gapblk_ela, color = Region)) + geom_point() + ggtitle("Black-White ELA Gap vs. MFI") + xlab("2009 Median Family Income") + ylab("Black-White English Language Arts Score Gap")

#plot of gapblk_ela versus HSPPer
ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = HSPPer, y = gapblk_ela, color = grade)) + geom_point()+ggtitle("Black-White ELA Gap vs. Hispanic Population") + xlab("Hispanic Population Percentage") + ylab("Black-White English Language Arts Score Gap")

#Analysis of variance tests for different groups
summary(aov(gapblk_ela~Region, data = NC1))
#tested all grade levels but did not put code for all of them -- case for all 4 types of dependent variables
summary(aov(gapblk_ela[grade == 3]~Region[grade ==3], data = NC1))
#grade 7 aov had a pvalue of .0669
summary(aov(gapblk_ela[grade == 3]~Region[grade ==3], data = NC2))
#grade 4 aov had a pvalue of .0691
summary(aov(gapblk_ela~grade, data = NC1))
#pvalue of .0333
summary(aov(gapblk_ela~grade, data = NC2))
#pvalue of .155


##black math

#boxplots of region conditioned on grade level
boxplot(gapblk_math[grade == 3]~Region[grade == 3], data = NC1)
boxplot(gapblk_math[grade == 4]~Region[grade == 4], data = NC1)
boxplot(gapblk_math[grade == 5]~Region[grade == 5], data = NC1)
boxplot(gapblk_math[grade == 6]~Region[grade == 6], data = NC1)
boxplot(gapblk_math[grade == 7]~Region[grade == 7], data = NC1)
boxplot(gapblk_math[grade == 8]~Region[grade == 8], data = NC1)
ggplot(NC1, aes(x=grade, y = gapblk_math, fill = grade)) + geom_boxplot()

#plots of gapblk_math versus MFI, BLPPer, HSPPer
#positive correlation, regardless of grade
ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = MFI, y = gapblk_math, color = grade)) + geom_point()+ ggtitle("Black-White Math Gap vs. MFI") + xlab("2009 Median Family Income") + ylab("Black-White Math Achievement Score Gap")
ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = BLPPer, y = gapblk_math, color = grade)) + geom_point() + ggtitle("Black-White Math Gap vs. Black Population") + xlab("Black Population Percentage") + ylab("Black-White Math Achievement Score Gap")
ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = HSPPer, y = gapblk_math, color = grade)) + geom_point() + ggtitle("Black-White Math Gap vs. Hispanic Population") + xlab("Hispanic Population Percentage") + ylab("Black-White Math Achievement Score Gap")

#Analysis of Variance tests comparing groups
summary(aov(gapblk_math[grade ==3]~Region[grade ==3], data = NC1))
#grade 7 aov has a pvalue of .0135
summary(aov(gapblk_math[grade ==3]~Region[grade ==3], data = NC2))
#grade 7 has pvalue of .0449
summary(aov(gapblk_math~Region, data = NC1))

summary(aov(gapblk_math~grade, data = NC1))
#pvalue = 7.61e-05, suggests that gap changes over grade level
summary(aov(gapblk_math~grade, data = NC2))
#pvalue of .000255


##hispanic ELA
#boxplots for region and grade
boxplot(gaphsp_ela[grade == 3]~Region[grade == 3], data = NC1)
boxplot(gaphsp_ela[grade == 4]~Region[grade == 4], data = NC1)
boxplot(gaphsp_ela[grade == 5]~Region[grade == 5], data = NC1)
boxplot(gaphsp_ela[grade == 6]~Region[grade == 6], data = NC1)
boxplot(gaphsp_ela[grade == 7]~Region[grade == 7], data = NC1)
boxplot(gaphsp_ela[grade == 8]~Region[grade == 8], data = NC1)
boxplot(gaphsp_ela~grade, data = NC1)
ggplot(NC1, aes(x=grade, y = gaphsp_ela, fill = grade)) + geom_boxplot()
ggplot(NC2, aes(x=grade, y = gaphsp_ela, fill = grade)) + geom_boxplot()

##scatter plots of gaphsp_ela versus MFI, HSPPer, and BLPPer
ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = MFI, y = gaphsp_ela, color = grade)) + geom_point() + ggtitle("Hispanic-White ELA Gap vs. MFI")+ xlab("2009 Median Family Income") + ylab("Hispanic-White English Language Arts Gap")
ggplot(subset(NC1, Region %in% c(seq(1,8))), aes(x = MFI, y = gaphsp_ela, color = Region)) + geom_point() + ggtitle("Hispanic-White ELA Gap vs. MFI")+ xlab("2009 Median Family Income") + ylab("Hispanic-White English Language Arts Gap")
ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = HSPPer, y = gaphsp_ela, color = grade)) + geom_point() + ggtitle("Hispanic-White ELA Gap vs. Hispanic Population")+ xlab("Hispanic Population Percentage") + ylab("Hispanic-White English Language Arts Gap")
ggplot(subset(NC1, Region %in% c(seq(1,8))), aes(x = HSPPer, y = gaphsp_ela, color = Region)) + geom_point() + ggtitle("Hispanic-White ELA Gap vs. MFI")+ xlab("2009 Median Family Income") + ylab("Hispanic-White English Language Arts Gap")
ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = BLPPer, y = gaphsp_ela, color = grade)) + geom_point()+ ggtitle("Hispanic-White ELA Gap vs. Black Population")+ xlab("Black Population Percentage") + ylab("Hispanic-White English Language Arts Gap")

#analysis of variance tests for difference groups
summary(aov(gaphsp_ela[grade ==3]~Region[grade ==3], data = NC1))
#grade 4 had pvalue of .0499
#grade 5 had a pvalue of .0271
summary(aov(gaphsp_ela[grade ==3]~Region[grade ==3], data = NC2))
#grade 3 had pvalue of .0699
#grade 4 had pvalue of .0705
#grade 5 had pvalue of .0317
summary(aov(gaphsp_ela~grade, data = NC1))
#pvalue = .00657
summary(aov(gaphsp_ela~grade, data = NC2))
#pvalue = .000716


##hispanic math
#boxplots for region and grade
boxplot(gaphsp_math[grade == 3]~Region[grade == 3], data = NC1)
boxplot(gaphsp_math[grade == 4]~Region[grade == 4], data = NC1)
boxplot(gaphsp_math[grade == 5]~Region[grade == 5], data = NC1)
boxplot(gaphsp_math[grade == 6]~Region[grade == 6], data = NC1)
boxplot(gaphsp_math[grade == 7]~Region[grade == 7], data = NC1)
boxplot(gaphsp_math[grade == 8]~Region[grade == 8], data = NC1)
boxplot(gaphsp_math~grade, data = NC1)
ggplot(NC1, aes(x=grade, y = gaphsp_math, fill = grade)) + geom_boxplot()
ggplot(NC1, aes(x=Region, y = gaphsp_math, fill = Region)) + geom_boxplot()
ggplot(NC2, aes(x=grade, y = gaphsp_math, fill = grade)) + geom_boxplot()

#scatter plots of gaphsp_math versus MFI, HSPPer, and BLPPer
#positive correlation
ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = MFI, y = gaphsp_math, color = grade)) + geom_point() + ggtitle("Hispanic-White Math Gap vs. MFI")+ xlab("2009 Median Family Income") + ylab("Hispanic-White Math Achievement Score Gap")
#slightly quadratic as well
ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = HSPPer, y = gaphsp_math, color = grade)) + geom_point() + ggtitle("Hispanic-White Math Gap vs. Hispanic Population")+ xlab("2009 Hispanic Population Percentage") + ylab("Hispanic-White Math Achievement Score Gap")
ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = BLPPer, y = gaphsp_math, color = grade)) + geom_point() + ggtitle("Hispanic-White Math Gap vs. Black Population")+ xlab("2009 Black Population Percentage") + ylab("Hispanic-White Math Achievement Score Gap")

#Analysis of Variance Tests
summary(aov(gaphsp_math[grade ==3]~Region[grade ==3], data = NC1))
#grade 5 has pvalue of .0104
#grade 7 has pvalue of .0217

summary(aov(gaphsp_math[grade ==3]~Region[grade ==3], data = NC2))
#grade 3 has pvalue of .0741
#grade 5 has pvalue of .0205
#grade 7 has pvalue of .0512

summary(aov(gaphsp_math~grade, data = NC1))
#pvalue = .0451
summary(aov(gaphsp_math~grade, data = NC2))
#pvalue .194


##regression 
#making variables 
NC1$MFIcent = NC1$MFI - mean(NC1$MFI)
NC1$BLPPerC = NC1$BLPPer - mean(NC1$BLPPer)
NC1$HSPPerC = NC1$HSPPer - mean(NC1$HSPPer)
NC1$MFIBLP = NC1$MFIcent * NC1$BLPPerC
NC1$MFIHSP = NC1$MFIcent * NC1$HSPPerC

##black ELA
regblkela = lm(gapblk_ela~MFIcent + MFIBLP + MFIHSP + as.factor(grade) + as.factor(Region) + BLPPerC + HSPPerC, data = NC1)
summary(regblkela)

#reduced form -- one I use
regblkelaR = lm(gapblk_ela~MFIcent + BLPPerC + HSPPerC + as.factor(grade) + as.factor(Region), data = NC1)
summary(regblkelaR)
confint(regblkelaR)

NC2$MFIcent = NC2$MFI - mean(NC2$MFI)
NC2$BLPPerC = NC2$BLPPer - mean(NC2$BLPPer)
NC2$HSPPerC = NC2$HSPPer - mean(NC2$HSPPer)
NC2$MFIBLP = NC2$MFIcent * NC2$BLPPerC
NC2$MFIHSP = NC2$MFIcent * NC2$HSPPerC

regblkela2 = lm(gapblk_ela~MFIcent + MFIBLP + MFIHSP + as.factor(grade) + as.factor(Region) + BLPPerC + HSPPerC, data = NC2)
summary(regblkela)
#does not change with data set

confint(regblkela)

#checking residual plots
plot(regblkelaR$residuals~NC1$MFIcent)
plot(regblkelaR$residuals~NC1$BLPPerC)
plot(regblkelaR$residuals~NC1$HSPPerC)
boxplot(regblkelaR$residuals~NC1$grade, xlab = "Grade", ylab = "Residuals")
boxplot(regblkelaR$residuals~NC1$Region, xlab = "Region", ylab = "Residuals")

##black math 
#quadratic transformation of black pop bc of quadratic looking graph vs. gap
NC1$BLPPerC2 = NC1$BLPPerC^2
NC2$BLPPerC2 = NC2$BLPPerC^2

#used this one
regblkmath = lm(gapblk_math~MFIcent + MFIBLP + BLPPerC2 + MFIHSP+ as.factor(grade) + as.factor(Region) + BLPPerC + HSPPerC, data = NC1)
summary(regblkmath)
confint(regblkmath)

regblkmath2 = lm(gapblk_math~MFIcent + MFIBLP + BLPPerC2+ MFIHSP+ as.factor(grade) + as.factor(Region) + BLPPerC + HSPPerC, data = NC2)
summary(regblkmath2)
#some small changes in fit and significance

#checking residuals
plot(regblkmath$residuals~NC1$MFIcent)
#little strange
plot(regblkmath$residuals~NC1$BLPPerC)
plot(regblkmath$residuals~NC1$BLPPerC2)
plot(regblkmath$residuals~NC1$HSPPerC)
plot(regblkmath$residuals~NC1$MFIBLP)
plot(regblkmath$residuals~NC1$MFIHSP)
boxplot(regblkmath$residuals~NC1$Region, xlab = "Region", ylab = "Residuals")
boxplot(regblkmath$residuals~NC1$grade, xlab = "Grade", ylab = "Residuals")

##hispanic ELA

NC1$HSPPerC2 = NC1$HSPPerC^2
NC2$HSPPerC2 = NC2$HSPPerC^2

reghspela = lm(gaphsp_ela~MFIcent + MFIBLP + MFIHSP + HSPPerC2 + as.factor(grade) + as.factor(Region) + BLPPerC + HSPPerC, data = NC1)
summary(reghspela)

#one I used
reghspelaR = lm(gaphsp_ela~MFIcent + MFIHSP + as.factor(grade) + as.factor(Region) + BLPPerC + HSPPerC, data = NC1)
summary(reghspelaR)
confint(reghspelaR)

anova(reghspela, reghspelaR)

reghspela2 = lm(gaphsp_ela~MFIcent + MFIBLP + MFIHSP + HSPPerC2 + as.factor(grade) + as.factor(Region) + BLPPerC + HSPPerC, data = NC2)
summary(reghspela2)

#checking residuals
plot(reghspelaR$residuals~NC1$MFIcent)
plot(reghspelaR$residuals~NC1$HSPPerC)
plot(reghspelaR$residuals~NC1$BLPPerC)
plot(reghspelaR$residuals~NC1$MFIHSP)
boxplot(reghspelaR$residuals~NC1$grade, xlab = "Grade", ylab = "Residuals")
boxplot(reghspelaR$residuals~NC1$Region, xlab = "Region", ylab = "Residuals")

##hispanic math

#used this one
reghspmath = lm(gaphsp_math~MFIcent + MFIBLP + MFIHSP + HSPPerC2 + as.factor(grade) + as.factor(Region) + BLPPerC + HSPPerC, data = NC1)
summary(reghspmath)
confint(reghspmath)

reghspmath2 = lm(gaphsp_math~MFIcent + MFIBLP + MFIHSP + HSPPerC2 + as.factor(grade) + as.factor(Region) + BLPPerC + HSPPerC, data = NC2)
summary(reghspmath2)

plot(reghspmath$residuals~NC1$MFIcent)
plot(reghspmath$residuals~NC1$HSPPerC)
plot(reghspmath$residuals~NC1$BLPPerC)
plot(reghspmath$residuals~NC1$MFIBLP)
plot(reghspmath$residuals~NC1$MFIHSP)
boxplot(reghspmath$residuals~NC1$Region, xlab = "Region", ylab = "Residuals")
boxplot(reghspmath$residuals~NC1$grade, xlab = "Grade", ylab = "Residuals")

#correlations between dependent variables
cor(NC1$gapblk_ela,NC1$gapblk_math)
#.78
cor(NC1$gapblk_ela,NC1$gaphsp_ela)
#.66
cor(NC1$gaphsp_math,NC1$gaphsp_ela)
#.796
cor(NC1$gaphsp_math,NC1$gapblk_math)
#.696
#for NC2, cor = .719

#scatter plots showing correlation
ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = gaphsp_ela, y = gaphsp_math, color = grade)) + geom_point() + ggtitle("Hispanic-White ELA Gap vs. Math Gap")
ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = gapblk_ela, y = gapblk_math, color = grade)) + geom_point() + ggtitle("Black-White ELA Gap Vs. Math Gap")
ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = gaphsp_ela, y = gapblk_ela, color = grade)) + geom_point() + ggtitle("ELA Gap for Black and Hispanic Students")
ggplot(subset(NC1, grade %in% c(seq(3,8))), aes(x = gaphsp_math, y = gapblk_math, color = grade)) + geom_point() + ggtitle("Math Gap for Black and Hispanic Students")



#things I did not put on the trifold

#plots comparing gaps in each category for each grade
plot(gaphsp_math[grade == 3]~MFI[grade == 3], data = NC1, xlab = "Median Family Income", ylab = "Third Grade Gap")
points(gaphsp_ela[grade == 3]~MFI[grade == 3], data = NC1, col = "red", pch = 2)
points(gapblk_math[grade == 3]~MFI[grade ==3], data = NC1, col = "blue", pch = 3)
points(gapblk_ela[grade == 3]~MFI[grade ==3], data = NC1, col = "purple", pch = 4)
legend("bottomright", legend = c("Hispanic Math", "Hispanic ELA", "Black Math", "Black ELA"), col = c("Black", "Red", "Blue", "Purple"), pch = c(1,2,3,4), cex = .6)

plot(gaphsp_math[grade == 4]~MFI[grade == 4], data = NC1, xlab = "Median Family Income", ylab = "Fourth Grade Gap")
points(gaphsp_ela[grade == 4]~MFI[grade == 4], data = NC1, col = "red", pch = 2)
points(gapblk_math[grade == 4]~MFI[grade ==4], data = NC1, col = "blue", pch = 3)
points(gapblk_ela[grade == 4]~MFI[grade ==4], data = NC1, col = "purple", pch = 4)
legend("bottomright", legend = c("Hispanic Math", "Hispanic ELA", "Black Math", "Black ELA"), col = c("Black", "Red", "Blue", "Purple"), pch = c(1,2,3,4), cex = .6)

plot(gaphsp_math[grade == 5]~MFI[grade == 5], data = NC1, xlab = "Median Family Income", ylab = "Fifth Grade Gap")
points(gaphsp_ela[grade == 5]~MFI[grade == 5], data = NC1, col = "red", pch = 2)
points(gapblk_math[grade == 5]~MFI[grade ==5], data = NC1, col = "blue", pch = 3)
points(gapblk_ela[grade == 5]~MFI[grade ==5], data = NC1, col = "purple", pch = 4)
legend("bottomright", legend = c("Hispanic Math", "Hispanic ELA", "Black Math", "Black ELA"), col = c("Black", "Red", "Blue", "Purple"), pch = c(1,2,3,4), cex = .6)

plot(gaphsp_math[grade == 6]~MFI[grade == 6], data = NC1, xlab = "Median Family Income", ylab = "Sixth Grade Gap")
points(gaphsp_ela[grade == 6]~MFI[grade == 6], data = NC1, col = "red", pch = 2)
points(gapblk_math[grade == 6]~MFI[grade ==6], data = NC1, col = "blue", pch = 3)
points(gapblk_ela[grade == 6]~MFI[grade ==6], data = NC1, col = "purple", pch = 4)
legend("bottomright", legend = c("Hispanic Math", "Hispanic ELA", "Black Math", "Black ELA"), col = c("Black", "Red", "Blue", "Purple"), pch = c(1,2,3,4), cex = .6)

plot(gaphsp_math[grade == 7]~MFI[grade == 7], data = NC1, xlab = "Median Family Income", ylab = "Seventh Grade Gap")
points(gaphsp_ela[grade == 7]~MFI[grade == 7], data = NC1, col = "red", pch = 2)
points(gapblk_math[grade == 7]~MFI[grade ==7], data = NC1, col = "blue", pch = 3)
points(gapblk_ela[grade == 7]~MFI[grade ==7], data = NC1, col = "Purple", pch = 4)
legend("bottomright", legend = c("Hispanic Math", "Hispanic ELA", "Black Math", "Black ELA"), col = c("Black", "Red", "Blue", "Purple"), pch = c(1,2,3,4), cex = .6)

plot(gaphsp_math[grade == 8]~MFI[grade == 8], data = NC1, xlab = "Median Family Income", ylab = "Eighth Grade Gap")
points(gaphsp_ela[grade == 8]~MFI[grade == 8], data = NC1, col = "red", pch = 2)
points(gapblk_math[grade == 8]~MFI[grade ==8], data = NC1, col = "blue", pch = 3)
points(gapblk_ela[grade == 8]~MFI[grade ==8], data = NC1, col = "purple", pch = 4)
legend("bottomright", legend = c("Hispanic Math", "Hispanic ELA", "Black Math", "Black ELA"), col = c("Black", "Red", "Blue", "Purple"), pch = c(1,2,3,4), cex = .6)
#hispanic math always the smallest gap


t.test(NC1$gaphsp_math, NC1$gapblk_math)
t.test(NC1$gaphsp_ela, NC1$gaphsp_math)
t.test(NC1$gaphsp_ela, NC1$gapblk_ela)
t.test(NC1$gapblk_math, NC1$gapblk_ela)
#last t test has a pvalue of .7825

