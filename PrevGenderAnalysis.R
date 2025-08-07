#-----------------------------------------------------------------------------------------------------------------------------------------#
# Gender differences in tuberculosis burden and notifications in low- and middle-income countries: a systematic review and meta-analysis  #
# Katherine C. Horton, Peter MacPherson, Rein M.G.J. Houben, Richard G. White PhD, Elizabeth L. Corbett                                   #
#                                                                                                                                         #
# Last updated 31 March 2016                                                                                                              #
#-----------------------------------------------------------------------------------------------------------------------------------------#

## PREPARE DATASETS ##

# Import dataset

prevgenderdataset <- read.delim("~/Papers/Systematic review on gender & prevalence/R/PrevGenderDataset.txt")

# Subset data for different analyses - partdata for analysis of participation, bactydata for prevalence of bacteriologically-positive TB, smeardata for prevalence of smear-positive TB, agedata for prevalence of bacteriologically-positive TB by age group and pndata for P:N ratios 

partdata <- subset(prevgenderdataset, partratio != 'NA')
bactydata <- subset(prevgenderdataset, bactym != 'NA')
smeardata <- subset(prevgenderdataset, smearm != 'NA')
pndata <- subset(prevgenderdataset, logmfpn != 'NA')
agedata <- subset(prevgenderdataset, bactym15 != 'NA')

## INSTALL PACKAGES ##

library(meta)
library(metafor)

## M:F RATIOS IN PARTICIPATION ##

# Forest plot for M:F ratios in participation

tiff(filename="ParticipForestPlot.tiff", width=6.8, height=5.01, units='in', res=300, type="cairo")
par(mar=c(4,4,1,2))
part <- rma(ai=participm, bi=nonparticipm, ci=participf, di=nonparticipf, data=partdata, measure="RR", slab=paste(id), method="REML")
forest(part, xlim=c(-2,1.5), ylim=c(-1,33), atransf=exp, cex=.75, order=order(partdata$partratio), xlab="M:F ratio", mlab="Overall summary", psize=1, col="black", border="black")
par(cex=.75, font=2)
text(-2, 32, pos=4, "Survey country and year")
text(1.5, 32, pos=2, "M:F ratio for participation [95% CI]")
dev.off()

## TB PREVALENCE BY GENDER ##

# Overall random-effects weighted prevalence of bacteriologically-positive TB for men

propmb <- metaprop(bactym, participm, id, data=bactydata, comb.random=TRUE)
print(propmb, digits=5)

# Overall random-effects weighted prevalence of bacteriologically-positive TB for women

propfb <- metaprop(bactyf, participf, id, data=bactydata, comb.random=TRUE)
print(propfb, digits=5)

# Overall random-effects weighted prevalence of smear-positive TB for men

propms <- metaprop(smearm, participm, id, data=smeardata, comb.random=TRUE)
print(propms, digits=5)

# Overall random-effects weighted prevalence of smear-positive TB for women

propfs <- metaprop(smearf, participf, id, data=smeardata, comb.random=TRUE)
print(propfs, digits=5)

## Sub-group random-effects weighted prevalence of bacteriologically-positive TB for men

# Random-effects weighted prevalence of bacteriologically-positive TB for men by region

submb1 <- metaprop(bactym, participm, id, data=bactydata, byvar=region, comb.random=TRUE)
print(submb1, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for men for national vs. sub-national surveys

submb2data <- subset(bactydata, national != 'NA')
submb2 <- metaprop(bactym, participm, id, data=submb2data, byvar=national, comb.random=TRUE)
print(submb2, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for men for surveys in countries with high vs. low TB prevalence

submb3data <- subset(bactydata, tbprev != 'NA')
submb3 <- metaprop(bactym, participm, id, data=submb3data, byvar=tbprev, comb.random=TRUE)
print(submb3, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for men for surveys in countries with high vs. low HIV prevalence

submb4data <- subset(bactydata, hivprev != 'NA')
submb4 <- metaprop(bactym, participm, id, data=submb4data, byvar=hivprev, comb.random=TRUE)
print(submb4, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for men for surveys in countries with high vs. low HIV prevalence in incident TB

submb5data <- subset(bactydata, hivintb != 'NA')
submb5 <- metaprop(bactym, participm, id, data=submb5data, byvar=hivintb, comb.random=TRUE)
print(submb5, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for men for surveys with low vs. moderate or high risk of bias

submb6data <- subset(bactydata, lowrisk != 'NA')
submb6 <- metaprop(bactym, participm, id, data=submb6data, byvar=lowrisk, comb.random=TRUE)
print(submb6, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for men for surveys with initial screening procedures requiring self-report of signs/symptoms vs. broader initial screening procedures

submb7data <- subset(bactydata, symptomonly != 'NA')
submb7 <- metaprop(bactym, participm, id, data=submb7data, byvar=symptomonly, comb.random=TRUE)
print(submb7, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for men for surveys with diagnosis by smear microscopy vs. other diagnostic measures

submb8data <- subset(bactydata, smearonly != 'NA')
submb8 <- metaprop(bactym, participm, id, data=submb8data, byvar=smearonly, comb.random=TRUE)
print(submb8, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for men for surveys with low vs. high relative male participation

submb9data <- subset(bactydata, malepart != 'NA')
submb9 <- metaprop(bactym, participm, id, data=submb9data, byvar=malepart, comb.random=TRUE)
print(submb9, digits=5)

## Sub-group random-effects weighted prevalence of bacteriologically-positive TB for women

# Random-effects weighted prevalence of bacteriologically-positive TB for women by region

subfb1 <- metaprop(bactyf, participf, id, data=bactydata, byvar=region, comb.random=TRUE)
print(subfb1, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for women for national vs. sub-national surveys

subfb2data <- subset(bactydata, national != 'NA')
subfb2 <- metaprop(bactyf, participf, id, data=subfb2data, byvar=national, comb.random=TRUE)
print(subfb2, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for women for surveys in countries with high vs. low TB prevalence

subfb3data <- subset(bactydata, tbprev != 'NA')
subfb3 <- metaprop(bactyf, participf, id, data=subfb3data, byvar=tbprev, comb.random=TRUE)
print(subfb3, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for women for surveys in countries with high vs. low HIV prevalence

subfb4data <- subset(bactydata, hivprev != 'NA')
subfb4 <- metaprop(bactyf, participf, id, data=subfb4data, byvar=hivprev, comb.random=TRUE)
print(subfb4, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for women for surveys in countries with high vs. low HIV prevalence in incident TB

subfb5data <- subset(bactydata, hivintb != 'NA')
subfb5 <- metaprop(bactyf, participf, id, data=subfb5data, byvar=hivintb, comb.random=TRUE)
print(subfb5, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for women for surveys with low vs. moderate or high risk of bias

subfb6data <- subset(bactydata, lowrisk != 'NA')
subfb6 <- metaprop(bactyf, participf, id, data=subfb6data, byvar=lowrisk, comb.random=TRUE)
print(subfb6, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for women for surveys with initial screening procedures requiring self-report of signs/symptoms vs. broader initial screening procedures

subfb7data <- subset(bactydata, symptomonly != 'NA')
subfb7 <- metaprop(bactyf, participf, id, data=subfb7data, byvar=symptomonly, comb.random=TRUE)
print(subfb7, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for women for surveys with diagnosis by smear microscopy vs. other diagnostic measures

subfb8data <- subset(bactydata, smearonly != 'NA')
subfb8 <- metaprop(bactyf, participf, id, data=subfb8data, byvar=smearonly, comb.random=TRUE)
print(subfb8, digits=5)

# Random-effects weighted prevalence of bacteriologically-positive TB for women for surveys with low vs. high relative male participation

subfb9data <- subset(bactydata, malepart != 'NA')
subfb9 <- metaprop(bactyf, participf, id, data=subfb9data, byvar=malepart, comb.random=TRUE)
print(subfb9, digits=5)

## Sub-group random-effects weighted prevalence of smear-positive TB for men

# Random-effects weighted prevalence of smear-positive TB for men by region

subms1seardata <- subset(smeardata, region == '1-SEAR')
subms1sear <- metaprop(smearm, participm, id, data=subms1seardata, comb.random=TRUE)
print(subms1sear, digits=5)

subms1wprdata <- subset(smeardata, region == '2-WPR')
subms1wpr <- metaprop(smearm, participm, id, data=subms1wprdata, comb.random=TRUE)
print(subms1wpr, digits=5)

subms1afrdata <- subset(smeardata, region == '3-AFR')
subms1afr <- metaprop(smearm, participm, id, data=subms1afrdata, comb.random=TRUE)
print(subms1afr, digits=5)

subms1amrdata <- subset(smeardata, region == '5-AMR')
subms1amr <- metaprop(smearm, participm, id, data=subms1amrdata, comb.random=TRUE)
print(subms1amr, digits=5)

# Random-effects weighted prevalence of smear-positive TB for men for national vs. sub-national surveys

subms2data <- subset(smeardata, national != 'NA')
subms2 <- metaprop(smearm, participm, id, data=subms2data, byvar=national, comb.random=TRUE)
print(subms2, digits=5)

# Random-effects weighted prevalence of smear-positive TB for men for surveys in countries with high vs. low TB prevalence

subms3data <- subset(smeardata, tbprev != 'NA')
subms3 <- metaprop(smearm, participm, id, data=subms3data, byvar=tbprev, comb.random=TRUE)
print(subms3, digits=5)

# Random-effects weighted prevalence of smear-positive TB for men for surveys in countries with high vs. low HIV prevalence

subms4data <- subset(smeardata, hivprev != 'NA')
subms4 <- metaprop(smearm, participm, id, data=subms4data, byvar=hivprev, comb.random=TRUE)
print(subms4, digits=5)

# Random-effects weighted prevalence of smear-positive TB for men for surveys in countries with high vs. low HIV prevalence in incident TB

subms5data <- subset(smeardata, hivintb != 'NA')
subms5 <- metaprop(smearm, participm, id, data=subms5data, byvar=hivintb, comb.random=TRUE)
print(subms5, digits=5)

# Random-effects weighted prevalence of smear-positive TB for men for surveys with low vs. moderate or high risk of bias

subms6data <- subset(smeardata, lowrisk != 'NA')
subms6 <- metaprop(smearm, participm, id, data=subms6data, byvar=lowrisk, comb.random=TRUE)
print(subms6, digits=5)

# Random-effects weighted prevalence of smear-positive TB for men for surveys with initial screening procedures requiring self-report of signs/symptoms vs. broader initial screening procedures

subms7data <- subset(smeardata, symptomonly != 'NA')
subms7 <- metaprop(smearm, participm, id, data=subms7data, byvar=symptomonly, comb.random=TRUE)
print(subms7, digits=5)

# Random-effects weighted prevalence of smear-positive TB for men for surveys with diagnosis by smear microscopy vs. other diagnostic measures

subms8data <- subset(smeardata, smearonly != 'NA')
subms8 <- metaprop(smearm, participm, id, data=subms8data, byvar=smearonly, comb.random=TRUE)
print(subms8, digits=5)

# Random-effects weighted prevalence of smear-positive TB for men for surveys with low vs. high relative male participation

subms9data <- subset(smeardata, malepart != 'NA')
subms9 <- metaprop(smearm, participm, id, data=subms9data, byvar=malepart, comb.random=TRUE)
print(subms9, digits=5)

## Sub-group random-effects weighted prevalence of smear-positive TB for women

# Random-effects weighted prevalence of smear-positive TB for women by region

subfs1seardata <- subset(smeardata, region == '1-SEAR')
subfs1sear <- metaprop(smearf, participf, id, data=subfs1seardata, comb.random=TRUE)
print(subfs1sear, digits=5)

subfs1wprdata <- subset(smeardata, region == '2-WPR')
subfs1wpr <- metaprop(smearf, participf, id, data=subfs1wprdata, comb.random=TRUE)
print(subfs1wpr, digits=5)

subfs1afrdata <- subset(smeardata, region == '3-AFR')
subfs1afr <- metaprop(smearf, participf, id, data=subfs1afrdata, comb.random=TRUE)
print(subfs1afr, digits=5)

subfs1amrdata <- subset(smeardata, region == '5-AMR')
subfs1amr <- metaprop(smearf, participf, id, data=subfs1amrdata, comb.random=TRUE)
print(subfs1amr, digits=5)

# Random-effects weighted prevalence of smear-positive TB for women for national vs. sub-national surveys

subfs2data <- subset(smeardata, national != 'NA')
subfs2 <- metaprop(smearf, participf, id, data=subfs2data, byvar=national, comb.random=TRUE)
print(subfs2, digits=5)

# Random-effects weighted prevalence of smear-positive TB for women for surveys in countries with high vs. low TB prevalence

subfs3data <- subset(smeardata, tbprev != 'NA')
subfs3 <- metaprop(smearf, participf, id, data=subfs3data, byvar=tbprev, comb.random=TRUE)
print(subfs3, digits=5)

# Random-effects weighted prevalence of smear-positive TB for women for surveys in countries with high vs. low HIV prevalence

subfs4data <- subset(smeardata, hivprev != 'NA')
subfs4 <- metaprop(smearf, participf, id, data=subfs4data, byvar=hivprev, comb.random=TRUE)
print(subfs4, digits=5)

# Random-effects weighted prevalence of smear-positive TB for women for surveys in countries with high vs. low HIV prevalence in incident TB

subfs5data <- subset(smeardata, hivintb != 'NA')
subfs5 <- metaprop(smearf, participf, id, data=subfs5data, byvar=hivintb, comb.random=TRUE)
print(subfs5, digits=5)

# Random-effects weighted prevalence of smear-positive TB for women for surveys with low vs. moderate or high risk of bias

subfs6data <- subset(smeardata, lowrisk != 'NA')
subfs6 <- metaprop(smearf, participf, id, data=subfs6data, byvar=lowrisk, comb.random=TRUE)
print(subfs6, digits=5)

# Random-effects weighted prevalence of smear-positive TB for women for surveys with initial screening procedures requiring self-report of signs/symptoms vs. broader initial screening procedures

subfs7data <- subset(smeardata, symptomonly != 'NA')
subfs7 <- metaprop(smearf, participf, id, data=subfs7data, byvar=symptomonly, comb.random=TRUE)
print(subfs7, digits=5)

# Random-effects weighted prevalence of smear-positive TB for women for surveys with diagnosis by smear microscopy vs. other diagnostic measures

subfs8data <- subset(smeardata, smearonly != 'NA')
subfs8 <- metaprop(smearf, participf, id, data=subfs8data, byvar=smearonly, comb.random=TRUE)
print(subfs8, digits=5)

# Random-effects weighted prevalence of smear-positive TB for women for surveys with low vs. high relative male participation

subfs9data <- subset(smeardata, malepart != 'NA')
subfs9 <- metaprop(smearf, participf, id, data=subfs9data, byvar=malepart, comb.random=TRUE)
print(subfs9, digits=5)

## M:F RATIOS IN PREVALENCE ##

# Random-effects weighted meta-analysis and forest plot for M:F ratios in bacteriologically-positive TB

tiff(filename="BactyForestPlot.tiff", width=6.8, height=12, units='in', res=300, type="cairo")
par(mar=c(4,4,1,2))
bacty <- rma(ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, measure="RR", slab=paste(id), method="REML")
forest(bacty, xlim=c(-4, 4), ylim=c(0,79), at=log(c(.25, 1, 2, 4, 12)), atransf=exp, cex=.75, order=order(bactydata$region, bactydata$negbactyrank, decreasing=TRUE), rows=c(3:4,9:10,15:37,42:50,55:74), xlab="M:F ratio", mlab="Overall summary", psize=1, col="black", border="black")
par(cex=.75, font=2)
text(-4, c(78,75.5,51.5,38.5,11.5,5.5), pos=4, c("Survey country and year", "South-East Asia Region", "Western Pacific Region", "African Region", "Eastern Mediterranean Region", "Region of the Americas"))
text(3.5, 78, pos=2, "M:F ratio for bacteriologically-positive TB [95% CI]")
bacty.afr <- rma(ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, measure="RR", subset=(region=="3-AFR"), method="REML")
bacty.amr <- rma(ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, measure="RR", subset=(region=="5-AMR"), method="REML")
bacty.emr <- rma(ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, measure="RR", subset=(region=="4-EMR"), method="REML")
bacty.sear <- rma(ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, measure="RR", subset=(region=="1-SEAR"), method="REML")
bacty.wpr <- rma(ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, measure="RR", subset=(region=="2-WPR"), method="REML")
par(cex=1, font=2)
addpoly(bacty.sear, row=53.5, cex=.75, font=2, atransf=exp, mlab="Regional summary", col="white", border="black")
addpoly(bacty.wpr, row=40.5, cex=.75, font=2, atransf=exp, mlab="Regional summary", col="white", border="black")
addpoly(bacty.afr, row=13.5, cex=.75, font=2, atransf=exp, mlab="Regional summary", col="white", border="black")
addpoly(bacty.emr, row=7.5, cex=.75, font=2, atransf=exp, mlab="Regional summary", col="white", border="black")
addpoly(bacty.amr, row=1.5, cex=.75, font=2, atransf=exp, mlab="Regional summary", col="white", border="black")
dev.off()

# Random-effects weighted meta-analysis and forest plot for M:F ratios in smear-positive TB

tiff(filename="SmearForestPlot.tiff", width=6.8, height=12, units='in', res=300, type="cairo")
par(mar=c(4,4,1,2))
smear <- rma(ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, measure="RR", slab=paste(id), method="REML")
forest(smear, xlim=c(-4, 4), ylim=c(0,79), at=log(c(.25, 1, 2, 4, 12)), atransf=exp, cex=.75, order=order(smeardata$smearrank, decreasing=TRUE), rows=c(3,15,17,19:22,24:25,27,29:32,34,36:37,42:48,50,55,59,61:73), xlab="M:F ratio", mlab="Overall summary", psize=1, col="black", border="black")
par(cex=.75, font=2)
text(-4, c(78,75.5,51.5,38.5,5.5), pos=4, c("Survey country and year", "South-East Asia Region", "Western Pacific Region", "African Region", "Region of the Americas"))
text(3, 78, pos=2, "M:F ratio for smear-positive TB [95% CI]")
smear.afr <- rma(ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, measure="RR", subset=(region=="3-AFR"), method="REML")
smear.amr <- rma(ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, measure="RR", subset=(region=="5-AMR"), method="REML")
smear.sear <- rma(ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, measure="RR", subset=(region=="1-SEAR"), method="REML")
smear.wpr <- rma(ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, measure="RR", subset=(region=="2-WPR"), method="REML")
par(cex=1, font=2)
addpoly(smear.sear, row=53.5, cex=.75, atransf=exp, mlab="Regional summary", col="white", border="black")
addpoly(smear.wpr, row=40.5, cex=.75, atransf=exp, mlab="Regional summary", col="white", border="black")
addpoly(smear.afr, row=13.5, cex=.75, atransf=exp, mlab="Regional summary", col="white", border="black")
addpoly(smear.amr, row=1.5, cex=.75, atransf=exp, mlab="Regional summary", col="white", border="black")
dev.off()

## Random-effects weighted estimates for M:F ratios in bacteriologically-positive TB for countries with more than one survey

# Random-effects weighted estimates for M:F ratios in smear-positive TB for Ethiopia

ethb <- rma(ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, measure="RR", slab=paste(id), method="REML", subset=(country=="Ethiopia"))
forest(ethb, atransf=exp)

# Random-effects weighted estimates for M:F ratios in smear-positive TB for South Africa

rsab <- rma(ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, measure="RR", slab=paste(id), method="REML", subset=(country=="South Africa"))
forest(rsab, atransf=exp)

# Random-effects weighted estimates for M:F ratios in smear-positive TB for Viet Nam

vnb <- rma(ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, measure="RR", slab=paste(id), method="REML", subset=(country=="Viet Nam"))
forest(vnb, atransf=exp)

## Random-effects weighted estimates for M:F ratios in smear-positive TB for countries with more than one survey

# Random-effects weighted estimates for M:F ratios in smear-positive TB for Ethiopia

eths <- rma(ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, measure="RR", slab=paste(id), method="REML", subset=(country=="Ethiopia"))
forest(eths, atransf=exp)

# Random-effects weighted estimates for M:F ratios in smear-positive TB for South Africa

rsas <- rma(ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, measure="RR", slab=paste(id), method="REML", subset=(country=="South Africa"))
forest(rsas, atransf=exp)

# Random-effects weighted estimates for M:F ratios in smear-positive TB for Viet Nam

vns <- rma(ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, measure="RR", slab=paste(id), method="REML", subset=(country=="Viet Nam"))
forest(vns, atransf=exp)

## UNIVARIATE META-REGRESSION FOR M:F RATIOS IN PREVALENCE ##

## Univariate meta-regression for M:F ratios in bacteriologically-positive TB

# Univariate meta-regression for M:F ratios in bacteriologically-positive TB by region

metab1 <- rma(measure="RR", ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, mods = ~ amr + emr + sear + wpr, method="REML")
metab1$b <- exp(metab1$b)
metab1$ci.lb <- exp(metab1$ci.lb)
metab1$ci.ub <- exp(metab1$ci.ub)
summary(metab1)

# Univariate meta-regression for M:F ratios in bacteriologically-positive TB for national vs. sub-national surveys

metab2 <- rma(measure="RR", ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, mods = ~ national, method="REML")
metab2$b <- exp(metab2$b)
metab2$ci.lb <- exp(metab2$ci.lb)
metab2$ci.ub <- exp(metab2$ci.ub)
summary(metab2)

# Univariate meta-regression for M:F ratios in bacteriologically-positive TB by starting year of survey

metab3 <- rma(measure="RR", ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, mods = ~ startyear, method="REML")
metab3$b <- exp(metab3$b)
metab3$ci.lb <- exp(metab3$ci.lb)
metab3$ci.ub <- exp(metab3$ci.ub)
summary(metab3)

# Univariate meta-regression for M:F ratios in bacteriologically-positive TB for surveys in countries with high vs. low TB prevalence

metab4 <- rma(measure="RR", ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, mods = ~ tbprev, method="REML")
metab4$b <- exp(metab4$b)
metab4$ci.lb <- exp(metab4$ci.lb)
metab4$ci.ub <- exp(metab4$ci.ub)
summary(metab4)

# Univariate meta-regression for M:F ratios in bacteriologically-positive TB for surveys in countries with high vs. low HIV prevalence

metab5 <- rma(measure="RR", ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, mods = ~ hivprev, method="REML")
metab5$b <- exp(metab5$b)
metab5$ci.lb <- exp(metab5$ci.lb)
metab5$ci.ub <- exp(metab5$ci.ub)
summary(metab5)

# Univariate meta-regression for M:F ratios in bacteriologically-positive TB for surveys in countries with high vs. low HIV prevalence in incident TB

metab6 <- rma(measure="RR", ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, mods = ~ hivintb, method="REML")
metab6$b <- exp(metab6$b)
metab6$ci.lb <- exp(metab6$ci.lb)
metab6$ci.ub <- exp(metab6$ci.ub)
summary(metab6)

# Univariate meta-regression for M:F ratios in bacteriologically-positive TB for surveys with low vs. moderate or high risk of bias

metab7 <- rma(measure="RR", ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, mods = ~ lowrisk, method="REML")
metab7$b <- exp(metab7$b)
metab7$ci.lb <- exp(metab7$ci.lb)
metab7$ci.ub <- exp(metab7$ci.ub)
summary(metab7)

# Univariate meta-regression for M:F ratios in bacteriologically-positive TB for surveys with initial screening procedures requiring self-report of signs/symptoms vs. broader initial screening procedures

metab8 <- rma(measure="RR", ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, mods = ~ symptomonly, method="REML")
metab8$b <- exp(metab8$b)
metab8$ci.lb <- exp(metab8$ci.lb)
metab8$ci.ub <- exp(metab8$ci.ub)
summary(metab8)

# Univariate meta-regression for M:F ratios in bacteriologically-positive TB for surveys with diagnosis by smear microscopy vs. other diagnostic measures

metab9 <- rma(measure="RR", ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, mods = ~ smearonly, method="REML")
metab9$b <- exp(metab9$b)
metab9$ci.lb <- exp(metab9$ci.lb)
metab9$ci.ub <- exp(metab9$ci.ub)
summary(metab9)

# Univariate meta-regression for M:F ratios in bacteriologically-positive TB for surveys with low vs. high relative male participation

metab10 <- rma(measure="RR", ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, mods = ~ malepart, method="REML")
metab10$b <- exp(metab10$b)
metab10$ci.lb <- exp(metab10$ci.lb)
metab10$ci.ub <- exp(metab10$ci.ub)
summary(metab10)

## Univariate meta-regression for M:F ratios in smear-positive TB

# Univariate meta-regression for M:F ratios in smear-positive TB by region

metas1 <- rma(measure="RR", ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, mods = ~ amr + emr + sear + wpr, method="REML")
metas1$b <- exp(metas1$b)
metas1$ci.lb <- exp(metas1$ci.lb)
metas1$ci.ub <- exp(metas1$ci.ub)
summary(metas1)

# Univariate meta-regression for M:F ratios in smear-positive TB for national vs. sub-national surveys

metas2 <- rma(measure="RR", ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, mods = ~ national, method="REML")
metas2$b <- exp(metas2$b)
metas2$ci.lb <- exp(metas2$ci.lb)
metas2$ci.ub <- exp(metas2$ci.ub)
summary(metas2)

# Univariate meta-regression for M:F ratios in smear-positive TB by starting year of survey

metas3 <- rma(measure="RR", ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, mods = ~ startyear, method="REML")
metas3$b <- exp(metas3$b)
metas3$ci.lb <- exp(metas3$ci.lb)
metas3$ci.ub <- exp(metas3$ci.ub)
summary(metas3)

# Univariate meta-regression for M:F ratios in smear-positive TB for surveys in countries with high vs. low TB prevalence

metas4 <- rma(measure="RR", ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, mods = ~ tbprev, method="REML")
metas4$b <- exp(metas4$b)
metas4$ci.lb <- exp(metas4$ci.lb)
metas4$ci.ub <- exp(metas4$ci.ub)
summary(metas4)

# Univariate meta-regression for M:F ratios in smear-positive TB for surveys in countries with high vs. low HIV prevalence

metas5 <- rma(measure="RR", ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, mods = ~ hivprev, method="REML")
metas5$b <- exp(metas5$b)
metas5$ci.lb <- exp(metas5$ci.lb)
metas5$ci.ub <- exp(metas5$ci.ub)
summary(metas5)

# Univariate meta-regression for M:F ratios in smear-positive TB for surveys in countries with high vs. low HIV prevalence in incident TB

metas6 <- rma(measure="RR", ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, mods = ~ hivintb, method="REML")
metas6$b <- exp(metas6$b)
metas6$ci.lb <- exp(metas6$ci.lb)
metas6$ci.ub <- exp(metas6$ci.ub)
summary(metas6)

# Univariate meta-regression for M:F ratios in smear-positive TB for surveys with low vs. moderate or high risk of bias

metas7 <- rma(measure="RR", ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, mods = ~ lowrisk, method="REML")
metas7$b <- exp(metas7$b)
metas7$ci.lb <- exp(metas7$ci.lb)
metas7$ci.ub <- exp(metas7$ci.ub)
summary(metas7)

# Univariate meta-regression for M:F ratios in smear-positive TB for surveys with initial screening procedures requiring self-report of signs/symptoms vs. broader initial screening procedures

metas8 <- rma(measure="RR", ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, mods = ~ symptomonly, method="REML")
metas8$b <- exp(metas8$b)
metas8$ci.lb <- exp(metas8$ci.lb)
metas8$ci.ub <- exp(metas8$ci.ub)
summary(metas8)

# Univariate meta-regression for M:F ratios in smear-positive TB for surveys with diagnosis by smear microscopy vs. other diagnostic measures

metas9 <- rma(measure="RR", ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, mods = ~ smearonly, method="REML")
metas9$b <- exp(metas9$b)
metas9$ci.lb <- exp(metas9$ci.lb)
metas9$ci.ub <- exp(metas9$ci.ub)
summary(metas9)

# Univariate meta-regression for M:F ratios in smear-positive TB for surveys with low vs. high relative male participation

metas10 <- rma(measure="RR", ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, mods = ~ malepart, method="REML")
metas10$b <- exp(metas10$b)
metas10$ci.lb <- exp(metas10$ci.lb)
metas10$ci.ub <- exp(metas10$ci.ub)
summary(metas10)

## MULTIVARIATE META-REGRESSION FOR M:F RATIOS IN PREVALENCE ##

# Multivariate meta-regression for M:F ratios in bacteriologically-positive TB including region, high vs. low HIV prevalence, high vs. low HIV prevalence in incident TB and surveys with initial screening procedures requiring self-report of signs/symptoms vs. broader initial screening procedures 

multimetab <- rma(measure="RR", ai=bactym, bi=bactynegm, ci=bactyf, di=bactynegf, data=bactydata, mods = ~ amr + emr + sear + wpr + hivprev + hivintb + symptomonly, method="REML")
multimetab $b <- exp(multimetab $b)
multimetab $ci.lb <- exp(multimetab $ci.lb)
multimetab $ci.ub <- exp(multimetab $ci.ub)
summary(multimetab)

# Multivariate meta-regression for M:F ratios in smear-positive TB including region, high vs. low HIV prevalence, high vs. low HIV prevalence in incident TB and surveys with initial screening procedures requiring self-report of signs/symptoms vs. broader initial screening procedures

multimetas <- rma(measure="RR", ai=smearm, bi=smearnegm, ci=smearf, di=smearnegf, data=smeardata, mods = ~ amr + emr + sear + wpr + hivprev + hivintb + symptomonly, method="REML")
multimetas$b <- exp(multimetas$b)
multimetas$ci.lb <- exp(multimetas$ci.lb)
multimetas$ci.ub <- exp(multimetas$ci.ub)
summary(multimetas)

## M:F RATIOS IN PREVALENCE BY AGE GROUP ##

# Random-effects weighted M:F ratio in bacteriologically-positive TB for ages 15-24

agedata$bactynegm15 <- agedata$partm15 + agedata$bactym15
agedata$bactynegf15 <- agedata$partf15 + agedata$bactyf15
age15 <- rma(measure="RR", ai=bactym15, bi=bactynegm15, ci=bactyf15, di=bactynegf15, data=agedata, method="REML")
age15$b <- exp(age15$b)
age15$ci.lb <- exp(age15$ci.lb)
age15$ci.ub <- exp(age15$ci.ub)
summary(age15)

# Random-effects weighted M:F ratio in bacteriologically-positive TB for ages 25-34

agedata$bactynegm25 <- agedata$partm25 + agedata$bactym25
agedata$bactynegf25 <- agedata$partf25 + agedata$bactyf25
age25 <- rma(measure="RR", ai=bactym25, bi=bactynegm25, ci=bactyf25, di=bactynegf25, data=agedata, method="ML")
age25$b <- exp(age25$b)
age25$ci.lb <- exp(age25$ci.lb)
age25$ci.ub <- exp(age25$ci.ub)
summary(age25)

# Random-effects weighted M:F ratio in bacteriologically-positive TB for ages 35-44

agedata$bactynegm35 <- agedata$partm35 + agedata$bactym35
agedata$bactynegf35 <- agedata$partf35 + agedata$bactyf35
age35 <- rma(measure="RR", ai=bactym35, bi=bactynegm35, ci=bactyf35, di=bactynegf35, data=agedata, method="REML")
age35$b <- exp(age35$b)
age35$ci.lb <- exp(age35$ci.lb)
age35$ci.ub <- exp(age35$ci.ub)
summary(age35)

# Random-effects weighted M:F ratio in bacteriologically-positive TB for ages 45-54

agedata$bactynegm45 <- agedata$partm45 + agedata$bactym45
agedata$bactynegf45 <- agedata$partf45 + agedata$bactyf45
age45 <- rma(measure="RR", ai=bactym45, bi=bactynegm45, ci=bactyf45, di=bactynegf45, data=agedata, method="REML")
age45$b <- exp(age45$b)
age45$ci.lb <- exp(age45$ci.lb)
age45$ci.ub <- exp(age45$ci.ub)
summary(age45)

# Random-effects weighted M:F ratio in bacteriologically-positive TB for ages 55+

agedata$bactynegm55 <- agedata$partm55 + agedata$bactym55
agedata$bactynegf55 <- agedata$partf55 + agedata$bactyf55
age55 <- rma(measure="RR", ai=bactym55, bi=bactynegm55, ci=bactyf55, di=bactynegf55, data=agedata, method="REML")
age55$b <- exp(age55$b)
age55$ci.lb <- exp(age55$ci.lb)
age55$ci.ub <- exp(age55$ci.ub)
summary(age55)

## M:F RATIOS IN P:N RATIOS ##

# Define variable to order results and function to back-transform results (Note: analyses performed on the logscale)

pndata$pnrank = -pndata$logmfpn
transf.expten <- function(x) { 
  z <- 10^(x) 
  return(z) 
} 

# Forest plot for M:F ratios in P:N ratios

tiff(filename="PNForestPlot.tiff", width=6.8, height=7.9, units='in', res=300, type="cairo")
par(mar=c(4,4,1,2))
pn <- rma(logmfpn, varlogmfpn, data=pndata)
forest(pn, xlim=c(-4,4), ylim=c(0,52), slab=pndata$id, cex=.75, order=order(pndata$region, pndata$pnrank, decreasing=TRUE), rows=c(3,8:19,24:30,35:47), atransf=transf.expten, xlab="M:F ratio", mlab="Overall summary", psize=1, col="black", border="black")
par(cex=.75, font=2)
text(-4, c(51,48.5,31.5,20.5,4.5), pos=4, c("Survey country and year", "South-East Asia Region", "Western Pacific Region", "African Region", "Region of the Americas"))
text(4, 51, pos=2, "M:F ratio for P:N ratios [95% CI]")
pn.sear <- rma(logmfpn, varlogmfpn, data=pndata, subset=(region=="1-SEAR"))
pn.wpr <- rma(logmfpn, varlogmfpn, data=pndata, subset=(region=="2-WPR"))
pn.afr <- rma(logmfpn, varlogmfpn, data=pndata, subset=(region=="3-AFR"))
pn.amr <- rma(logmfpn, varlogmfpn, data=pndata, subset=(region=="5-AMR"))
par(cex=1, font=2)
addpoly(pn.sear, row=33.5, cex=.75, font=2, atransf=transf.expten, mlab="Regional summary", col="white", border="black")
addpoly(pn.wpr, row=22.5, cex=.75, font=2, atransf=transf.expten, mlab="Regional summary", col="white", border="black")
addpoly(pn.afr, row=6.5, cex=.75, font=2, atransf=transf.expten, mlab="Regional summary", col="white", border="black")
addpoly(pn.amr, row=1.5, cex=.75, font=2, atransf=transf.expten, mlab="Regional summary", col="white", border="black")
dev.off()

## META-REGRESSION FOR M:F RATIOS IN P:N RATIOS ##

## Univariate meta-regression for M:F ratios in P:N ratios

# Univariate meta-regression for M:F ratios in P:N ratios by region

pn1 <- rma(logmfpn, varlogmfpn, data=pndata, mods = ~ amr + sear + wpr)
pn1$b <- 10^(pn1$b)
pn1$ci.lb <- 10^(pn1$ci.lb)
pn1$ci.ub <- 10^(pn1$ci.ub)
summary(pn1)

# Univariate meta-regression for M:F ratios in P:N ratios for national vs. sub-national surveys

pn2 <- rma(logmfpn, varlogmfpn, data=pndata, mods = ~ national)
pn2$b <- 10^(pn2$b)
pn2$ci.lb <- 10^(pn2$ci.lb)
pn2$ci.ub <- 10^(pn2$ci.ub)
summary(pn2)

# Univariate meta-regression for M:F ratios in P:N ratios by starting year of survey

pn3 <- rma(logmfpn, varlogmfpn, data=pndata, mods = ~ startyear)
pn3$b <- 10^(pn3$b)
pn3$ci.lb <- 10^(pn3$ci.lb)
pn3$ci.ub <- 10^(pn3$ci.ub)
summary(pn3)

# Univariate meta-regression for M:F ratios in P:N ratios for surveys in countries with high vs. low TB prevalence

pn4 <- rma(logmfpn, varlogmfpn, data=pndata, mods = ~ tbprev)
pn4$b <- 10^(pn4$b)
pn4$ci.lb <- 10^(pn4$ci.lb)
pn4$ci.ub <- 10^(pn4$ci.ub)
summary(pn4)

# Univariate meta-regression for M:F ratios in P:N ratios for surveys in countries with high vs. low HIV prevalence

pn5 <- rma(logmfpn, varlogmfpn, data=pndata, mods = ~ hivprev)
pn5$b <- 10^(pn5$b)
pn5$ci.lb <- 10^(pn5$ci.lb)
pn5$ci.ub <- 10^(pn5$ci.ub)
summary(pn5)

# Univariate meta-regression for M:F ratios in P:N ratios for surveys in countries with high vs. low HIV prevalence in incident TB

pn6 <- rma(logmfpn, varlogmfpn, data=pndata, mods = ~ hivintb)
pn6$b <- 10^(pn6$b)
pn6$ci.lb <- 10^(pn6$ci.lb)
pn6$ci.ub <- 10^(pn6$ci.ub)
summary(pn6)

# Univariate meta-regression for M:F ratios in P:N ratios for surveys with low vs. moderate or high risk of bias

pn7 <- rma(logmfpn, varlogmfpn, data=pndata, mods = ~ lowrisk)
pn7$b <- 10^(pn7$b)
pn7$ci.lb <- 10^(pn7$ci.lb)
pn7$ci.ub <- 10^(pn7$ci.ub)
summary(pn7)

# Univariate meta-regression for M:F ratios in P:N ratios for surveys with initial screening procedures requiring self-report of signs/symptoms vs. broader initial screening procedures

pn8 <- rma(logmfpn, varlogmfpn, data=pndata, mods = ~ symptomonly)
pn8$b <- 10^(pn8$b)
pn8$ci.lb <- 10^(pn8$ci.lb)
pn8$ci.ub <- 10^(pn8$ci.ub)
summary(pn8)

# Univariate meta-regression for M:F ratios in P:N ratios for surveys with diagnosis by smear microscopy vs. other diagnostic measures

pn9 <- rma(logmfpn, varlogmfpn, data=pndata, mods = ~ smearonly)
pn9$b <- 10^(pn9$b)
pn9$ci.lb <- 10^(pn9$ci.lb)
pn9$ci.ub <- 10^(pn9$ci.ub)
summary(pn9)

# Univariate meta-regression for M:F ratios in P:N ratios for surveys with low vs. high relative male participation

pn10 <- rma(logmfpn, varlogmfpn, data=pndata, mods = ~ malepart)
pn10$b <- 10^(pn10$b)
pn10$ci.lb <- 10^(pn10$ci.lb)
pn10$ci.ub <- 10^(pn10$ci.ub)
summary(pn10)