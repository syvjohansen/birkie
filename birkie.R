library(readxl)
library(ggplot2)
library(chron)
library(lubridate)
library(wCorr)
library(tidyr)
library(dplyr)
library(reshape2)
options(scipen = 999)
birkie <- read_excel("~/ski/birkie/birke.xlsx", 
                        sheet = "Birkie", col_names = FALSE, na = "NA")
birkiedf <- data.frame(birkie)

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

names(birkiedf) <- c("Name", "Bib", "Age", "Sex","Start", "D1", "T1", "D2", "T2", "D3", "T3", "D4", "T4", "D5", "T5")

t1minsec <- which(nchar(birkiedf$T1)<9)
t2minsec <- which(nchar(birkiedf$T2)<9)

birkiedf[t1minsec, ]$T1 <- paste("0:", birkiedf[t1minsec, ]$T1, sep="")
birkiedf[t2minsec, ]$T2 <- paste("0:", birkiedf[t2minsec, ]$T2, sep="")

birkiedf$T1 <- as.times(birkiedf$T1)
birkiedf$T2 <- as.times(birkiedf$T2)
birkiedf$T3 <- as.times(birkiedf$T3)
birkiedf$T4 <- as.times(birkiedf$T4)
birkiedf$T5 <- as.times(birkiedf$T5)
birkiedf$Start <- as.times(birkiedf$Start)
birkiedf$D1 <- as.numeric(birkiedf$D1)
birkiedf$D2 <- as.numeric(birkiedf$D2)
birkiedf$D3 <- as.numeric(birkiedf$D3)
birkiedf$D4 <- as.numeric(birkiedf$D4)
birkiedf$D5 <- as.numeric(birkiedf$D5)

mosqfinish <- birkiedf$T5 - birkiedf$T3
mosqfinishdf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, mosqfinish)
mosqfinishdf <- mosqfinishdf[order(mosqfinish), ]
mosqfinishdf$places <- 1:length(mosqfinishdf[,1])
namemosqfinishdf <- mosqfinishdf[order(mosqfinishdf$birkiedf.Name), ]

mosqfinishpace <- mosqfinish/(birkiedf$D5 - birkiedf$D3)


doubleofinish <- birkiedf$T5 - birkiedf$T2
doubleofinishdf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, doubleofinish)
doubleofinishdf <- doubleofinishdf[order(doubleofinish), ]
doubleofinishdf$places <- 1:length(doubleofinishdf[,1])
namedoubleofinishdf <- doubleofinishdf[order(doubleofinishdf$birkiedf.Name), ]

doubleofinishpace <- doubleofinish/(birkiedf$D5 - birkiedf$D2)

firefinish <- birkiedf$T5 - birkiedf$T1
firefinishdf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, firefinish)
firefinishdf <- firefinishdf[order(firefinish), ]
firefinishdf$places <- 1:length(firefinishdf[,1])
namefirefinishdf <- firefinishdf[order(firefinishdf$birkiedf.Name), ]

firefinishpace <- firefinish/(birkiedf$D5 - birkiedf$D1)

haywardfinish <- birkiedf$T5 - birkiedf$T4
haywardfinishdf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, haywardfinish)
haywardfinishdf <- haywardfinishdf[order(haywardfinish), ]
haywardfinishdf$places <- 1:length(haywardfinishdf[,1])
namehaywardfinishdf <- haywardfinishdf[order(haywardfinishdf$birkiedf.Name), ]

haywardfinishpace <- haywardfinish/(birkiedf$D5 - birkiedf$D4)

mosqhayward <- birkiedf$T4 - birkiedf$T3
mosqhaywarddf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, mosqhayward)
mosqhaywarddf <- mosqhaywarddf[order(mosqhayward), ]
mosqhaywarddf$places <- 1:length(mosqhaywarddf[,1])
namemosqhaywarddf <- mosqhaywarddf[order(mosqhaywarddf$birkiedf.Name), ]

mosqhaywardpace <- mosqhayward/(birkiedf$D4 - birkiedf$D3)

doubleohayward <- birkiedf$T4 - birkiedf$T2
doubleohaywarddf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, doubleohayward)
doubleohaywarddf <- doubleohaywarddf[order(doubleohayward), ]
doubleohaywarddf$places <- 1:length(doubleohaywarddf[,1])
namedoubleohaywarddf <- doubleohaywarddf[order(doubleohaywarddf$birkiedf.Name), ]

doubleohaywardpace <- doubleohayward/(birkiedf$D4 - birkiedf$D2)

firehayward <- birkiedf$T4 - birkiedf$T1
firehaywarddf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, firehayward)
firehaywarddf <- firehaywarddf[order(firehayward), ]
firehaywarddf$places <- 1:length(firehaywarddf[,1])
namefirehaywarddf <- firehaywarddf[order(firehaywarddf$birkiedf.Name), ]

firehaywardpace <- firehayward/(birkiedf$D4-birkiedf$D1)


doubleomosq <- birkiedf$T3 - birkiedf$T2
doubleomosqdf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, doubleomosq)
doubleomosqdf <- doubleomosqdf[order(doubleomosq), ]
doubleomosqdf$places <- 1:length(doubleomosqdf[,1])
namedoubleomosqdf <- doubleomosqdf[order(doubleomosqdf$birkiedf.Name), ]

doubleomosqpace <- doubleomosq/(birkiedf$D3-birkiedf$D2)

firemosq <- birkiedf$T3 - birkiedf$T1
firemosqdf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, firemosq)
firemosqdf <- firemosqdf[order(firemosq), ]
firemosqdf$places <- 1:length(firemosqdf[,1])
namefiremosqdf <- firemosqdf[order(firemosqdf$birkiedf.Name), ]

firemosqpace <- firemosq/(birkiedf$D3-birkiedf$D1)

firedoubleo <- birkiedf$T2 - birkiedf$T1
firedoubleodf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, firedoubleo)
firedoubleodf <- firedoubleodf[order(firedoubleo), ]
firedoubleodf$places <- 1:length(firedoubleodf[,1])
namefiredoubleodf <- firedoubleodf[order(firedoubleodf$birkiedf.Name), ]

firedoubleopace <- firedoubleo/(birkiedf$D2-birkiedf$D1)

fire <-  birkiedf$T1
firedf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, fire)
firedf <- firedf[order(fire), ]
firedf$places <- 1:length(firedf[,1])
namefiredf <- firedf[order(firedf$birkiedf.Name), ]

firepace <- fire/birkiedf$D1


doubleo <-  birkiedf$T2
doubleodf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, doubleo)
doubleodf <- doubleodf[order(doubleo), ]
doubleodf$places <- 1:length(doubleodf[,1])
namedoubleodf <- doubleodf[order(doubleodf$birkiedf.Name), ]

doubleopace <- doubleo/birkiedf$D2

mosq <-  birkiedf$T3
mosqdf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, mosq)
mosqdf <- mosqdf[order(mosq), ]
mosqdf$places <- 1:length(mosqdf[,1])
namemosqdf <- mosqdf[order(mosqdf$birkiedf.Name), ]

mosqpace <- mosq/birkiedf$D3

hayward <-  birkiedf$T4
haywarddf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, hayward)
haywarddf <- haywarddf[order(hayward), ]
haywarddf$places <- 1:length(haywarddf[,1])
namehaywarddf <- haywarddf[order(haywarddf$birkiedf.Name), ]

haywardpace <- hayward/birkiedf$D4

finish <-  birkiedf$T5
finishdf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, finish)
finishdf <- finishdf[order(finish), ]
finishdf$places <- 1:length(finishdf[,1])

finishpace <- finish/birkiedf$D5

namefinishdf <- finishdf[order(finishdf$birkiedf.Name), ]

namebirkiedf <- birkiedf[order(birkiedf$Name), ]


place_table <- data.frame(namebirkiedf$Name,namebirkiedf$Bib, namebirkiedf$Age, namebirkiedf$Sex,  namebirkiedf$Start, namefinishdf$places, namehaywarddf$places, 
                          namefirefinishdf$places, namemosqdf$places, namefirehaywarddf$places, 
                          namedoubleofinishdf$places, namedoubleodf$places, namefiremosqdf$places, namedoubleohaywarddf$places, 
                          namemosqfinishdf$places, namefiredf$places, namefiredoubleodf$places, namedoubleomosqdf$places, namemosqhaywarddf$places, 
                          namehaywardfinishdf$places)
names(place_table) <- c("Name","Bib", "Age", "Sex", "Start", "Finish", "Hayward", "firefinish", "mosq", "firehayward", "doubleofinish", "doubleo", "firemosq", "doubleohayward", "mosqfinish", "fire", "firedoubleo", "doubleomosq", "mosqhayward", "haywardfinish")
distance_intervals <- c(NA, NA, NA, NA, NA, 50, 47.2, 38, 36.3, 35.2, 28.9, 21.1, 24.3, 26.1, 13.7, 12, 9.1, 15.2, 8.9, 2.8)
for(a in 5:length(names(place_table))){
  print(cor(as.numeric(place_table[,a]), as.numeric(place_table$Finish)))#, #weights=rep(50/distance_intervals[a], length(place_table[,1]))))
}


ggplot(place_table, aes(x=fire, y=Finish)) + 
  geom_point()+geom_point(data=place_table[place_table$Name=="Syver Johansen", ], aes(x=fire, y=Finish), colour="red")+
  geom_point(data=place_table[place_table$Name=="Geir Johansen", ], aes(x=fire, y=Finish), colour="blue")+
  geom_point(data=place_table[place_table$Name=="Maija Johansen", ], aes(x=fire, y=Finish), colour="green")


# weighted_corrdf <- data.frame(rep(place_table$Name, each=15))
# names(weighted_corrdf) <- c("Name")
# weighted_corrdf$Distance <- rep(c(50, 47.2, 38, 36.3, 35.2, 28.9, 21.1, 24.3, 26.1, 13.7, 12, 9.1, 15.2, 8.9, 2.8), length(place_table[,1]))
# weighted_corrdf$Place <- as.vector(as.matrix(t(place_table[,5:19])))
# weighted_corrdf$final <- rep(c(place_table[,5]), each=15)
# #weighted_corr <- weightedCorr(weighted_corrdf$final, weighted_corrdf$Place, method="polyserial", weights=weighted_corrdf$Distance)
 weighted_corr <- cov.wt(weighted_corrdf[,3:4], wt=weighted_corrdf$Distance, cor=TRUE)

first_wavedf <- subset(place_table, as.numeric(as.character(Bib))>=1000 & as.numeric(as.character(Bib))<2000)
cor(first_wavedf$Start, first_wavedf$fire)

for(a in 5:length(names(first_wavedf))){
  print(cor(as.numeric(first_wavedf[,a]), as.numeric(first_wavedf$Finish)))
}
cor(place_table[,5:20])
cor(first_wavedf[,5:20])

ggplot(first_wavedf, aes(x=Start, fire)) + geom_point()+geom_point(data=first_wavedf[first_wavedf$Name=="Syver Johansen", ], aes(x=Start, y=fire), colour="red")


first_wavedfstart <- subset(first_wavedf, as.numeric(first_wavedf$Start)*24*60<536)
ggplot(first_wavedfstart, aes(x=Start, fire)) + geom_point()+geom_point(data=first_wavedf[first_wavedf$Name=="Syver Johansen", ], aes(x=Start, y=fire), colour="red")
cor(first_wavedfstart[,5:20])

w1start <- as.numeric(first_wavedfstart$Start)*24*60



third_wavedf <- subset(place_table, as.numeric(as.character(Bib))>=3000 & as.numeric(as.character(Bib))<4000)
third_wavedfstart <- subset(third_wavedf, as.numeric(third_wavedf$Start)*24*60<561)
ggplot(third_wavedfstart, aes(x=Start, fire)) + geom_point()+geom_point(data=third_wavedf[third_wavedf$Name=="Geir Johansen", ], aes(x=Start, y=fire), colour="red")
cor(third_wavedfstart[,5:20])




summary(glm(first_wavedfstart$fire~w1start))


birkiemendf <- subset(place_table, as.character(Sex)=="M")
elite_menqdf <- birkiemendf[order(as.numeric(as.character(birkiemendf$Finish))), ][1:200,]
for(a in 5:length(names(elite_menqdf))){
  print(cor(as.numeric(elite_menqdf[,a]), as.numeric(elite_menqdf$Finish)))
}

cor(elite_menqdf[,5:20])

ggplot(elite_menqdf, aes(x=mosqhayward, Finish)) + geom_point()+geom_point(data=first_wavedf[first_wavedf$Name=="Syver Johansen", ], aes(x=mosqhayward, y=Finish), colour="red")

w1elite_menqdf <- subset(elite_menqdf, as.numeric(as.character(Bib))>=1000)
cor(w1elite_menqdf[,5:20])

ggplot(w1elite_menqdf, aes(x=doubleohayward, Finish)) + geom_point()+geom_point(data=first_wavedf[first_wavedf$Name=="Syver Johansen", ], aes(x=doubleohayward, y=Finish), colour="red")



pace_table <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, birkiedf$Start, 
                         finishpace, haywardpace, firefinishpace, mosqpace, firehaywardpace, doubleofinishpace, doubleopace, firemosqpace, 
                         doubleohaywardpace, mosqfinishpace, firepace, firedoubleopace, doubleomosqpace, mosqhaywardpace, haywardfinishpace)
names(pace_table) <- c("Name","Bib", "Age", "Sex", "Start", "Finish", "Hayward", "firefinish", "mosq", "firehayward", "doubleofinish", "doubleo", "firemosq", "doubleohayward", "mosqfinish", "fire", "firedoubleo", "doubleomosq", "mosqhayward", "haywardfinish")

cor(na.omit(pace_table[,6:20]))

numpace_table <- pace_table

numpace_table[c(5:20)] <- lapply(numpace_table[c(5:20)], function(x) as.numeric(x)*60*24)

pacepercentage_table <- numpace_table
pacepercentage_table[c(7:20)] <- lapply(pacepercentage_table[c(7:20)], function(x) (x-pacepercentage_table[6])/pacepercentage_table[6])
names(pacepercentage_table) <- c("Name","Bib", "Age", "Sex", "Start", "Finish", "Hayward", "firefinish", "mosq", "firehayward", "doubleofinish", "doubleo", "firemosq", "doubleohayward", "mosqfinish", "fire", "firedoubleo", "doubleomosq", "mosqhayward", "haywardfinish")

elite_menqpacepercentagedf <- elite_menqdf
elite_menqpacepercentagedf <- pacepercentage_table[pacepercentage_table$Name %in% elite_menqpacepercentagedf$Name, ]
elite_menqpacepercentagedf <- elite_menqpacepercentagedf[1:200, ]

w1pacepercentagedf <- subset(pacepercentage_table, as.numeric(as.character(Bib))>=1000 & as.numeric(as.character(Bib))<2000)




elite_effort <- c(colMeans(elite_menqpacepercentagedf[,7:20])*-1)
elite_effort <- data.frame(elite_effort)
elite_effort$segment <- 7:20

w1elite_menqpacepercentagedf <- subset(elite_menqpacepercentagedf, as.numeric(as.character(Bib))>=1000)

w1elite_effort <- c(colMeans(w1elite_menqpacepercentagedf[,7:20])*-1)
w1elite_effort <- data.frame(w1elite_effort)
w1elite_effort$segment <- 7:20


w1_effort <- c(colMeans(w1pacepercentagedf[,7:20])*-1)
w1_effort <- data.frame(w1_effort)
w1_effort$segment <- 7:20


my_effort <- pacepercentage_table[pacepercentage_table$Name=="Syver Johansen", ]
my_effort <- c(my_effort[,7:20]*-1)
my_effort <- data.frame(my_effort)
my_effort <- t(my_effort)
my_effort <- data.frame(my_effort)
my_effort$segment <- 7:20
names(my_effort) <- c("effort", "segment")

ggplot(elite_effort, aes(x=segment, y=elite_effort)) + geom_point() + geom_point(data=my_effort, aes(x=segment, y=effort), colour="red") +
  geom_point(data=w1elite_effort, aes(x=segment, y=w1elite_effort), colour="green")+ geom_point(data=w1_effort, aes(x=segment, y=w1_effort), colour="purple")


###New Wave assignments####

##Elite men and women
elite_menqdf
elite_menqpacepercentagedf
elite_menqpacepercentagedf$wave = "me"
elite_menqpacepercentagedf.avg <- data.frame(names(elite_menqpacepercentagedf)[6:20],  c(colMeans(elite_menqpacepercentagedf[,6:20])), "me", row.names = NULL)
names(elite_menqpacepercentagedf.avg) = c("checkpoint", "percent", "wave")

birkieladiesdf <- subset(place_table, as.character(Sex)=="F")
elite_ladiesqdf <- birkieladiesdf[order(as.numeric(as.character(birkieladiesdf$Finish))), ][1:60,]
elite_ladiesqpacepercentagedf <- elite_ladiesqdf
elite_ladiesqpacepercentagedf <- pacepercentage_table[pacepercentage_table$Name %in% elite_ladiesqpacepercentagedf$Name, ]
elite_ladiesqpacepercentagedf <- elite_ladiesqpacepercentagedf[1:60, ]
elite_ladiesqpacepercentagedf$wave = "le"
elite_ladiesqpacepercentagedf.avg <- data.frame(names(elite_ladiesqpacepercentagedf)[6:20],  c(colMeans(elite_ladiesqpacepercentagedf[,6:20])), "le", row.names=NULL)
names(elite_ladiesqpacepercentagedf.avg) = c("checkpoint", "percent", "wave")

##w1 qualifiers
w1q <- subset(birkiedf, as.times(birkiedf$T5) < as.times("02:41:18"))
w1q <- subset(w1q, !as.character(Bib) %in% as.character(elite_menqdf$Bib) & !as.character(Bib) %in% as.character(elite_ladiesqdf$Bib))
w1qpacepercentagedf <- w1q
w1qpacepercentagedf <- pacepercentage_table[as.character(pacepercentage_table$Bib) %in% as.character(w1qpacepercentagedf$Bib), ]
w1qpacepercentagedf$wave = "w1"
w1qpacepercentagedf.avg <- data.frame(names(w1qpacepercentagedf)[6:20],  c(colMeans(w1qpacepercentagedf[,6:20])), "w1", row.names=NULL)
names(w1qpacepercentagedf.avg) = c("checkpoint", "percent", "wave")

#w2 qualifiers
w2q <- subset(birkiedf, as.times(birkiedf$T5) > as.times("02:41:17") & as.times(birkiedf$T5) < as.times("02:54:24"))
w2q <- subset(w2q, !as.character(Bib) %in% as.character(elite_menqdf$Bib) & !as.character(Bib) %in% as.character(elite_ladiesqdf$Bib))
w2qpacepercentagedf <- w2q
w2qpacepercentagedf <- pacepercentage_table[as.character(pacepercentage_table$Bib) %in% as.character(w2qpacepercentagedf$Bib), ]
w2qpacepercentagedf$wave = "w2"
w2qpacepercentagedf.avg <- data.frame(names(w2qpacepercentagedf)[6:20],  c(colMeans(w2qpacepercentagedf[,6:20])), "w2", row.names=NULL)
names(w2qpacepercentagedf.avg) = c("checkpoint", "percent", "wave")



#w3 qualifiers
w3q <- subset(birkiedf, as.times(birkiedf$T5) > as.times("02:54:23") & as.times(birkiedf$T5) < as.times("03:07:29"))
w3q <- subset(w3q, !as.character(Bib) %in% as.character(elite_menqdf$Bib) & !as.character(Bib) %in% as.character(elite_ladiesqdf$Bib))
w3qpacepercentagedf <- w3q
w3qpacepercentagedf <- pacepercentage_table[as.character(pacepercentage_table$Bib) %in% as.character(w3qpacepercentagedf$Bib), ]
w3qpacepercentagedf$wave = "w3"
w3qpacepercentagedf.avg <- data.frame(names(w3qpacepercentagedf)[6:20],  c(colMeans(w3qpacepercentagedf[,6:20])), "w3", row.names=NULL)
names(w3qpacepercentagedf.avg) = c("checkpoint", "percent", "wave")



#w4 qualifiers
w4q <- subset(birkiedf, as.times(birkiedf$T5) > as.times("03:07:28") & as.times(birkiedf$T5) < as.times("03:25:20"))
w4q <- subset(w4q, !as.character(Bib) %in% as.character(elite_menqdf$Bib) & !as.character(Bib) %in% as.character(elite_ladiesqdf$Bib))
w4qpacepercentagedf <- w4q
w4qpacepercentagedf <- pacepercentage_table[as.character(pacepercentage_table$Bib) %in% as.character(w4qpacepercentagedf$Bib), ]
w4qpacepercentagedf$wave = "w4"
w4qpacepercentagedf.avg <- (data.frame(names(w4qpacepercentagedf)[6:20],  c(colMeans(w4qpacepercentagedf[,6:20])), "w4", row.names=NULL))
names(w4qpacepercentagedf.avg) = c("checkpoint", "percent", "wave")



#w5 qualifiers
w5q <- subset(birkiedf, as.times(birkiedf$T5) > as.times("03:25:20") & as.times(birkiedf$T5) < as.times("03:44:23"))
w5q <- subset(w5q, !as.character(Bib) %in% as.character(elite_menqdf$Bib) & !as.character(Bib) %in% as.character(elite_ladiesqdf$Bib))
w5qpacepercentagedf <- w5q
w5qpacepercentagedf <- pacepercentage_table[as.character(pacepercentage_table$Bib) %in% as.character(w5qpacepercentagedf$Bib), ]
w5qpacepercentagedf$wave = "w5"
w5qpacepercentagedf.avg <- (data.frame(names(w5qpacepercentagedf)[6:20],  c(colMeans(w5qpacepercentagedf[,6:20])), "w5", row.names=NULL))
names(w5qpacepercentagedf.avg) = c("checkpoint", "percent", "wave")



#w6 qualifiers
w6q <- subset(birkiedf, as.times(birkiedf$T5) > as.times("03:44:22") & as.times(birkiedf$T5) < as.times("04:22:28"))
w6q <- subset(w6q, !as.character(Bib) %in% as.character(elite_menqdf$Bib) & !as.character(Bib) %in% as.character(elite_ladiesqdf$Bib))
w6qpacepercentagedf <- w6q
w6qpacepercentagedf <- pacepercentage_table[as.character(pacepercentage_table$Bib) %in% as.character(w6qpacepercentagedf$Bib), ]
w6qpacepercentagedf$wave = "w6"
w6qpacepercentagedf.avg <- (data.frame(names(w6qpacepercentagedf)[6:20],  c(colMeans(w6qpacepercentagedf[,6:20])), "w6", row.names=NULL))
names(w6qpacepercentagedf.avg) = c("checkpoint", "percent", "wave")



#w7 qualifiers
w7q <- subset(birkiedf, as.times(birkiedf$T5) > as.times("04:22:28"))
w7q <- subset(w7q, !as.character(Bib) %in% as.character(elite_menqdf$Bib) & !as.character(Bib) %in% as.character(elite_ladiesqdf$Bib))
w7qpacepercentagedf <- w7q
w7qpacepercentagedf <- pacepercentage_table[as.character(pacepercentage_table$Bib) %in% as.character(w7qpacepercentagedf$Bib), ]
w7qpacepercentagedf$wave = "w7"
w7qpacepercentagedf.avg <- (data.frame(names(w7qpacepercentagedf)[6:20],  c(colMeans(w7qpacepercentagedf[,6:20])), "w7", row.names=NULL))
names(w7qpacepercentagedf.avg) = c("checkpoint", "percent", "wave")


waveq <- do.call("rbind", list(elite_menqpacepercentagedf, elite_ladiesqpacepercentagedf, w1qpacepercentagedf, w2qpacepercentagedf, w3qpacepercentagedf, 
                               w4qpacepercentagedf, w5qpacepercentagedf, w6qpacepercentagedf, w7qpacepercentagedf))

waveq.avg <- do.call("rbind", list(elite_menqpacepercentagedf.avg, elite_ladiesqpacepercentagedf.avg, w1qpacepercentagedf.avg, w2qpacepercentagedf.avg, 
                                   w3qpacepercentagedf.avg, w4qpacepercentagedf.avg, w5qpacepercentagedf.avg, w6qpacepercentagedf.avg, w7qpacepercentagedf.avg))

for(a in 6:20){
  print(a)
  waveq[,a] = (as.vector(waveq[,a]))
}
waveq_select = waveq
names(waveq_select[6:20]) = 1:15
waveq_select = gather(waveq_select, "checkpoint", "percent", 16:20)
#waveq = gather(waveq, "checkpoint", "percent", 7:20)

waveq_select.avg <- subset(waveq.avg, checkpoint!="Finish")
waveq_select.avg$checkpoint <- as.character(waveq_select.avg$checkpoint)

for(a in 1:length(unique(waveq_select$checkpoint))){
  waveq_select$checkpoint[(which(waveq_select$checkpoint == unique(waveq_select$checkpoint)[a]))] = as.character(a)
}

for(a in 1:length(unique(waveq_select.avg$checkpoint))){
  waveq_select.avg$checkpoint[(which(waveq_select.avg$checkpoint == unique(waveq_select.avg$checkpoint)[a]))] = as.character(a)
}

waveq_select.avg$checkpoint = as.numeric(waveq_select.avg$checkpoint)
waveq_select.avg = subset(waveq_select.avg, checkpoint >=10)
waveq_select.avg$checkpoint = waveq_select.avg$checkpoint-9

waveq_select$checkpoint = as.numeric(waveq_select$checkpoint)
ggplot(waveq_select, aes(x=checkpoint, y=percent, color=wave))+geom_point()+geom_smooth(method=lm, aes(fill=wave))

my_effort <- pacepercentage_table[pacepercentage_table$Name=="Nick Sokolowski", ]
my_effort <- c(my_effort[,16:20])
my_effort <- data.frame(my_effort)
my_effort <- t(my_effort)
my_effort <- data.frame(my_effort)
my_effort$segment <- 1:5
names(my_effort) <- c("effort", "segment")


ggplot(waveq_select.avg, aes(x=checkpoint, y=percent, color=wave)) + geom_point() + 
  geom_point(data=my_effort, aes(x=segment, y=effort), colour="black")#+ geom_smooth()




##People who qualified for wave two from wave three
w2qw3 <- subset(w2q, as.numeric(as.character(Bib))>=3000 & as.numeric(as.character(Bib))<=4000)
w2qw3pacepercentagedf <- w2qw3
w2qw3pacepercentagedf <- pacepercentage_table[as.character(pacepercentage_table$Bib) %in% as.character(w2qw3pacepercentagedf$Bib), ]
w2qw3pacepercentagedf$wave = "w2w3"
w2qw3pacepercentagedf.avg <- data.frame(names(w2qw3pacepercentagedf)[6:20],  c(colMeans(w2qw3pacepercentagedf[,6:20])), "w2qw3", row.names=NULL)
names(w2qw3pacepercentagedf.avg) = c("checkpoint", "percent", "wave")
w2qw3pacepercentagedf.avg <- subset(w2qw3pacepercentagedf.avg, checkpoint!="Finish")
w2qw3pacepercentagedf.avg$checkpoint <- as.character(w2qw3pacepercentagedf.avg$checkpoint)
for(a in 1:length(unique(w2qw3pacepercentagedf.avg$checkpoint))){
  w2qw3pacepercentagedf.avg$checkpoint[(which(w2qw3pacepercentagedf.avg$checkpoint == unique(w2qw3pacepercentagedf.avg$checkpoint)[a]))] = as.character(a)
}

w2qw3pacepercentagedf.avg$checkpoint = as.numeric(w2qw3pacepercentagedf.avg$checkpoint)
w2qw3pacepercentagedf.avg = subset(w2qw3pacepercentagedf.avg, checkpoint >=10)
w2qw3pacepercentagedf.avg$checkpoint = w2qw3pacepercentagedf.avg$checkpoint-9


ggplot(w2qw3pacepercentagedf.avg, aes(x=checkpoint, y=percent)) + geom_point() +
  geom_point(data=my_effort, aes(x=segment, y=effort), colour="red")






###########
#Find people who have bonked at mosquito brook
bonk <- waveq
bonk <- subset(bonk, mosqhayward >= 0.1177993)



##View overall mosquito to hayward graph
hist(as.numeric(unlist(waveq$mosqhayward)))

hist(as.numeric(unlist(elite_menqpacepercentagedf$mosqhayward)))
summary(as.numeric(unlist(elite_menqpacepercentagedf$mosqhayward)))
hist(as.numeric(unlist(w1qpacepercentagedf$mosqhayward)))
summary(as.numeric(unlist(w1qpacepercentagedf$mosqhayward)))
hist(as.numeric(unlist(w2qpacepercentagedf$mosqhayward)))
summary(as.numeric(unlist(w2qpacepercentagedf$mosqhayward)))
hist(as.numeric(unlist(w3qpacepercentagedf$mosqhayward)))
summary(as.numeric(unlist(w3qpacepercentagedf$mosqhayward)))
hist(as.numeric(unlist(w4qpacepercentagedf$mosqhayward)))
summary(as.numeric(unlist(w4qpacepercentagedf$mosqhayward)))
hist(as.numeric(unlist(w5qpacepercentagedf$mosqhayward)))
summary(as.numeric(unlist(w5qpacepercentagedf$mosqhayward)))
hist(as.numeric(unlist(w6qpacepercentagedf$mosqhayward)))
summary(as.numeric(unlist(w6qpacepercentagedf$mosqhayward)))
hist(as.numeric(unlist(w7qpacepercentagedf$mosqhayward)))
summary(as.numeric(unlist(w7qpacepercentagedf$mosqhayward)))

hist(as.numeric(unlist(w1qpacepercentagedf$fire)))
hist(as.numeric(unlist(w1qpacepercentagedf$mosqhayward)))
hist(as.numeric(unlist(waveq$mosqhayward)))
boxplot(as.numeric(unlist(waveq$mosqhayward)))$out
boxplot(as.numeric(unlist(elite_menqpacepercentagedf$mosqhayward)))$out
boxplot(as.numeric(unlist(w1qpacepercentagedf$mosqhayward)))$out


hist(as.numeric(unlist(bonk$fire)))
boxplot(as.numeric(unlist(bonk$fire)))
summary(as.numeric(unlist(bonk$fire)))

hist(as.numeric(unlist(bonk$firedoubleo)))
boxplot(as.numeric(unlist(bonk$firedoubleo)))
summary(as.numeric(unlist(bonk$firedoubleo)))

hist(as.numeric(unlist(bonk$doubleomosq)))
boxplot(as.numeric(unlist(bonk$doubleomosq)))
summary(as.numeric(unlist(bonk$doubleomosq)))


hist(as.numeric(unlist(bonk$doubleo)))
boxplot(as.numeric(unlist(bonk$doubleo)))
summary(as.numeric(unlist(bonk$doubleo)))

table(bonk$wave)
length(bonk$wave)/length(waveq$wave)
table(bonk$wave)/table(waveq$wave)
table(bonk$wave)/table(waveq$wave)[c(1, 3:9)]

####Find how to hit that perfect range
perf_range <- subset(waveq, mosqhayward >= 0.016003 & mosqhayward <=0.041375)

hist(as.numeric(unlist(perf_range$fire)))
boxplot(as.numeric(unlist(perf_range$fire)))
summary(as.numeric(unlist(perf_range$fire)))

hist(as.numeric(unlist(perf_range$firedoubleo)))
boxplot(as.numeric(unlist(perf_range$firedoubleo)))
summary(as.numeric(unlist(perf_range$firedoubleo)))

hist(as.numeric(unlist(perf_range$doubleomosq)))
boxplot(as.numeric(unlist(perf_range$doubleomosq)))

hist(as.numeric(unlist(perf_range$doubleo)))
boxplot(as.numeric(unlist(perf_range$doubleo)))
summary(as.numeric(unlist(perf_range$doubleo)))

summary(perf_range)
table(perf_range$wave)/table(waveq$wave)


##Find how to energy surplus
boxplot(as.numeric(unlist(waveq$mosqhayward)))$out
sd(as.numeric(unlist(waveq$mosqhayward)))*2
summary(as.numeric(unlist(waveq$mosqhayward)))
hist(as.numeric(unlist(waveq$mosqhayward)))

too_slow <- subset(waveq, mosqhayward < mean(as.numeric(unlist(waveq$mosqhayward))) - sd(as.numeric(unlist(waveq$mosqhayward))))
table(too_slow$wave)/table(waveq$wave)

too_slow <- subset(waveq, mosqhayward < mean(as.numeric(unlist(waveq$mosqhayward))) - sd(as.numeric(unlist(waveq$mosqhayward))))
table(too_slow$wave)/table(waveq$wave)


summary(as.numeric(unlist(elite_menqpacepercentagedf$mosqhayward)))
sd(as.numeric(unlist(elite_menqpacepercentagedf$mosqhayward)))
too_slow <- subset(waveq, mosqhayward < mean(as.numeric(unlist(elite_menqpacepercentagedf$mosqhayward))) - sd(as.numeric(unlist(elite_menqpacepercentagedf$mosqhayward))))
summary(too_slow)
table(too_slow$wave)/table(waveq$wave)

too_slow <- subset(waveq, mosqhayward < min(as.numeric(unlist(elite_menqpacepercentagedf$mosqhayward))))
summary(too_slow)
View(too_slow)

too_slow <- subset(waveq, mosqhayward < quantile(as.numeric(unlist(elite_menqpacepercentagedf$mosqhayward)))[2])
table(too_slow$wave)/table(waveq$wave)

summary(too_slow)


slow_start <- subset(waveq, fire > mean(as.numeric(unlist(waveq$fire))) + sd(as.numeric(unlist(waveq$fire))))
table(slow_start$wave)/table(waveq$wave)


slow_start <- subset(waveq, fire > mean(as.numeric(unlist(elite_menqpacepercentagedf$fire))) + sd(as.numeric(unlist(elite_menqpacepercentagedf$fire))))
table(slow_start$wave)/table(waveq$wave)

###Now make one for too fast

boxplot(as.numeric(unlist(waveq$mosqhayward)))$out
sd(as.numeric(unlist(waveq$mosqhayward)))*2
summary(as.numeric(unlist(waveq$mosqhayward)))
hist(as.numeric(unlist(waveq$mosqhayward)))

too_fast <- subset(waveq, mosqhayward > mean(as.numeric(unlist(waveq$mosqhayward))) + sd(as.numeric(unlist(waveq$mosqhayward))))
table(too_fast$wave)/table(waveq$wave)[4:9]

summary(as.numeric(unlist(elite_menqpacepercentagedf$mosqhayward)))
sd(as.numeric(unlist(elite_menqpacepercentagedf$mosqhayward)))
too_fast <- subset(waveq, mosqhayward > mean(as.numeric(unlist(elite_menqpacepercentagedf$mosqhayward))) + sd(as.numeric(unlist(elite_menqpacepercentagedf$mosqhayward))))
summary(too_fast)
table(too_fast$wave)/table(waveq$wave)

too_fast <- subset(waveq, mosqhayward > max(as.numeric(unlist(elite_menqpacepercentagedf$mosqhayward))))
summary(too_fast)
View(too_fast)
table(too_fast$wave)/table(waveq$wave)[3:9]

fast_start <- subset(waveq, fire < mean(as.numeric(unlist(waveq$fire))) - sd(as.numeric(unlist(waveq$fire))))
table(fast_start$wave)/table(waveq$wave)[c(3, 5:9)]


fast_start <- subset(waveq, fire < mean(as.numeric(unlist(elite_menqpacepercentagedf$fire))) - sd(as.numeric(unlist(elite_menqpacepercentagedf$fire))))
table(fast_start$wave)/table(waveq$wave)


#Linear models
lm(as.numeric(unlist(mosqhayward))~as.numeric(unlist(fire)), data=waveq)
lm(as.numeric(unlist(mosqhayward))~as.numeric(unlist(doubleo)), data=waveq)

