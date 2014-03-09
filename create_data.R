# Read data from Quality of government
datorig <- read.csv("http://www.qogdata.pol.gu.se/data/qog_std_ts_20dec13.csv", sep=";")
# Read data on countries and continents from my github repository
library(RCurl)
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/world/countriesContinents.csv")
datCont <- read.csv(text = GHurl)
dat <- merge(datorig,datCont,by.x="ccode",by.y="cNr")
dat$group1 <- as.character(dat$group1)
dat$group1[dat$group1 %in% "other"] <- "X_other"
dat$group1 <- as.factor(dat$group1)
save(dat, file="data/standardCont.RData")