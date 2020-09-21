## ---------------------------
##
## Script name: covid_rki_plot.r
##
## Purpose of script: The script plots
## levelplot of covid-cases and -deaths 
## by day and age group
##
## Author: Joerg Feuerhake
##
## Date Created: 2020-09-21
##
## Email: joerg.feuerhake@free-penguin.org
##
## ---------------------------
##
## Notes:
##  You need to download two csv-Files: 
## https://www-genesis.destatis.de/genesis//online?operation=table&code=12411-0005#astructure
## get the most recent number of inhabitants of germany
## copy it to file_path name the file BEV_COVID19.csv
##
## https://www.arcgis.com/home/item.html?id=f10774f1c63e40168479a1feb6c7ca74
## get the most recent number of inhabitants of germany
## copy it to file_path name the file RKI_COVID19.csv
##
## Change file_pthe according to your environment
## ---------------------------
file_path <- "~/Desktop/covid/"

library(lattice)
library(RColorBrewer)

# a moving average function
# we need it to smooth weekly reporting issues
ma <- function(x, n = 7){filter(x, rep(1 / n, n), sides = 1)}

## first we need the population of Germany
## we get it here
##https://www-genesis.destatis.de/genesis//online?operation=table&code=12411-0005#astructure
## and then download a csv of the most recent table
bevdat <- read.csv2(paste0(file_path,"BEV_COVID19.csv"))
bevdat$Altersgruppe <- "NONE"

# we have the german population for every age-year up to 80
# but we need the age groups according to the RKI
bevdat$Altersgruppe[bevdat$Altersjahr < 5] <- "A00-A04"
bevdat$Altersgruppe[bevdat$Altersjahr > 4 & bevdat$Altersjahr < 15] <- "A05-A14"
bevdat$Altersgruppe[bevdat$Altersjahr > 14 & bevdat$Altersjahr < 35] <- "A15-A34"
bevdat$Altersgruppe[bevdat$Altersjahr > 34 & bevdat$Altersjahr < 60] <- "A35-A59"
bevdat$Altersgruppe[bevdat$Altersjahr > 59 & bevdat$Altersjahr < 80] <- "A60-A79"
bevdat$Altersgruppe[bevdat$Altersjahr > 79] <- "A80+"

bevdat$Altersgruppe <- factor(bevdat$Altersgruppe)
summary(bevdat)
# just sum up by AGegroup
bev_age <- tapply(bevdat$Anzahl, 
                        INDEX=list(bevdat$Altersgruppe), 
                        FUN=sum)
bev_age <- bev_age/100000

## now get the daily reports on Covid cases
## https://www.arcgis.com/home/item.html?id=f10774f1c63e40168479a1feb6c7ca74
covidrki <- read.csv(paste0(file_path,"RKI_COVID19.csv"))
covidrki$NeuerFall <- factor(covidrki$NeuerFall)
covidrki$NeuerTodesfall <- factor(covidrki$NeuerTodesfall)
covidrki$NeuGenesen <- factor(covidrki$NeuGenesen)
covidrki$IstErkrankungsbeginn <- factor(covidrki$IstErkrankungsbeginn)
covidrki$FID <- factor(covidrki$FID)
covidrki$IdBundesland <- factor(covidrki$IdBundesland)

covidrki$Meldedatum <- as.Date(covidrki$Meldedatum)
summary(covidrki)

## some control plots
## cases by day
case_day <- tapply(covidrki$AnzahlFall, 
                   INDEX=list(covidrki$Meldedatum), 
                   FUN=sum)
case_day[is.na(case_day)]<-0
barplot(case_day)

## cases by age group
case_age <- tapply(covidrki$AnzahlFall, 
                    INDEX=list(covidrki$Altersgruppe), 
                    FUN=sum)
barplot(case_age)
barplot(case_age[1:6]/bev_age)

## deaths by day
deads_day <- tapply(covidrki$AnzahlTodesfall, 
                    INDEX=list(covidrki$Meldedatum), 
                    FUN=sum)
deads_day[is.na(deads_day)]<-0
barplot(deads_day)

## cases and deaths by day
plot(ma(case_day)/max(ma(case_day)[!is.na(ma(case_day))]), type="l", col="darkgreen")
lines(ma(deads_day)/max(ma(deads_day)[!is.na(ma(deads_day))]), col="darkred")

## deaths by day
deads_age <- tapply(covidrki$AnzahlTodesfall, 
                    INDEX=list(covidrki$Altersgruppe), 
                    FUN=sum)
barplot(deads_age)


# now for the levelplots
# cases first
cases_day_age <- tapply(covidrki$AnzahlFall, 
            INDEX=list(covidrki$Meldedatum, covidrki$Altersgruppe), 
            FUN=sum)


cases_day_age[is.na(cases_day_age)] <- 0
cases_day_age <- cases_day_age[,1:6]
cnames <- c(colnames(cases_day_age),paste0(colnames(cases_day_age),"_ma"))
rnames <- rownames(cases_day_age)
cases_day_age <- t(apply(cases_day_age, 1, "/", bev_age))
cases_day_age <- cbind(cases_day_age,ma(cases_day_age[,1:6]))
colnames(cases_day_age) <- cnames
rownames(cases_day_age) <- rnames

# now deaths
death_day_age <- tapply(covidrki$AnzahlTodesfall, 
                        INDEX=list(covidrki$Meldedatum, covidrki$Altersgruppe), 
                        FUN=sum)

death_day_age[is.na(death_day_age)] <- 0
death_day_age <- death_day_age[,1:6]
cnames <- c(colnames(death_day_age),paste0(colnames(death_day_age),"_ma"))
rnames <- rownames(death_day_age)
death_day_age <- t(apply(death_day_age, 1, "/", bev_age))
death_day_age <- cbind(death_day_age,ma(death_day_age[,1:6]))
colnames(death_day_age) <- cnames
rownames(death_day_age) <- rnames

## levelplots
mt = trellis.par.get("par.main.text")
mt$cex = 0.9
trellis.par.set("par.main.text",mt)

mt = trellis.par.get("par.sub.text")
mt$cex = 0.8
trellis.par.set("par.sub.text",mt)

mt = trellis.par.get("par.ylab.text")
mt$cex = 0.8
trellis.par.set("par.ylab.text",mt)

mt = trellis.par.get("par.xlab.text")
mt$cex = 0.8
trellis.par.set("par.xlab.text",mt)

coul <- colorRampPalette(brewer.pal(5, "BuPu"))(100)

lpc <- levelplot(cases_day_age[7:nrow(cases_day_age),7:12], 
                 aspect="fill", 
                 col.regions = coul,
                 scales=list(x=list(at=seq(7,nrow(cases_day_age),by=30),
                                    labels=rownames(cases_day_age)[seq(7,nrow(cases_day_age),by=30)]), 
                             y=list(labels=c("bis 4 Jahre", "5 bis 14 Jahre","15 bis 34 Jahre","35 bis 59 Jahre","60 bis 79 Jahre", "80 und mehr Jahre"))),
                 main="Corona-Neuinfektionen je 100.000 Personen der jeweiligen Altergruppe nach Tag\nin Deutschland (7-Tage gleitender Durchschnitt)",
                 sub="",
                 ylab="Altersgruppe",
                 xlab="Datum"
)


lpd <- levelplot(death_day_age[7:nrow(death_day_age),7:12], 
          aspect="fill", 
          col.regions = coul,
          scales=list(x=list(at=seq(7,nrow(death_day_age),by=30),
                             labels=rownames(death_day_age)[seq(7,nrow(death_day_age),by=30)]), 
                      y=list(labels=c("bis 4 Jahre", "5 bis 14 Jahre","15 bis 34 Jahre","35 bis 59 Jahre","60 bis 79 Jahre", "80 und mehr Jahre"))),
          main="Corona-Todesf?lle je 100.000 Personen der jeweiligen Altergruppe nach Tag\nin Deutschland (7-Tage gleitender Durchschnitt)",
          sub="Quellen: Robert Koch-Institut (RKI), dl-de/by-2-0; Statistisches Bundesamt (Destatis), 2020 | Stand: 21.09.2020",
          ylab="Altersgruppe",
          xlab="Datum"
)

## plot levelplots in one screen
plot(lpc, split=c(1, 1, 1, 2))
plot(lpd, split=c(1, 2, 1, 2), newpage = FALSE)

