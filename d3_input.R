library(readxl)
library(ggplot2)
library(chron)
library(lubridate)
library(wCorr)
library(tidyr)
library(dplyr)
library(reshape2)
options(scipen = 999)

####################
## Load Syver's Excel File:
####################
getwd()
setwd("/Users/syverjohansen/ski/birkie2") ## NEED TO SET YOUR WORKING DIRECTORY

birkie <- read_excel("birke.xlsx", 
                     sheet = "Birkie", col_names = FALSE, na = "NA")
birkiedf <- data.frame(birkie)

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

####################
## Format Excel File:
####################

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

####################
## Pace Split Calcs:
####################

## Start -> FireTower
fire <-  birkiedf$T1
firedf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, fire)
firedf <- firedf[order(fire), ]
firedf$places <- 1:length(firedf[,1])
namefiredf <- firedf[order(firedf$birkiedf.Name), ]

firepace <- fire/birkiedf$D1

## FireTower -> HighwayOO
firedoubleo <- birkiedf$T2 - birkiedf$T1
firedoubleodf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, firedoubleo)
firedoubleodf <- firedoubleodf[order(firedoubleo), ]
firedoubleodf$places <- 1:length(firedoubleodf[,1])
namefiredoubleodf <- firedoubleodf[order(firedoubleodf$birkiedf.Name), ]

firedoubleopace <- firedoubleo/(birkiedf$D2-birkiedf$D1)

## HighwayOO -> Mosquito
doubleomosq <- birkiedf$T3 - birkiedf$T2
doubleomosqdf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, doubleomosq)
doubleomosqdf <- doubleomosqdf[order(doubleomosq), ]
doubleomosqdf$places <- 1:length(doubleomosqdf[,1])
namedoubleomosqdf <- doubleomosqdf[order(doubleomosqdf$birkiedf.Name), ]

doubleomosqpace <- doubleomosq/(birkiedf$D3-birkiedf$D2)

## Mosquito -> Lake Hayward
mosqhayward <- birkiedf$T4 - birkiedf$T3
mosqhaywarddf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, mosqhayward)
mosqhaywarddf <- mosqhaywarddf[order(mosqhayward), ]
mosqhaywarddf$places <- 1:length(mosqhaywarddf[,1])
namemosqhaywarddf <- mosqhaywarddf[order(mosqhaywarddf$birkiedf.Name), ]

mosqhaywardpace <- mosqhayward/(birkiedf$D4 - birkiedf$D3)

## Lake Hayward -> Finish
haywardfinish <- birkiedf$T5 - birkiedf$T4
haywardfinishdf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, haywardfinish)
haywardfinishdf <- haywardfinishdf[order(haywardfinish), ]
haywardfinishdf$places <- 1:length(haywardfinishdf[,1])
namehaywardfinishdf <- haywardfinishdf[order(haywardfinishdf$birkiedf.Name), ]

haywardfinishpace <- haywardfinish/(birkiedf$D5 - birkiedf$D4)

## Overall Pace:
finish <-  birkiedf$T5
finishdf <- data.frame(birkiedf$Name, birkiedf$Bib, birkiedf$Age, birkiedf$Sex, finish)
finishdf <- finishdf[order(finish), ]
finishdf$places <- 1:length(finishdf[,1])

finishpace <- finish/birkiedf$D5

####################
## Dataframe 4 Visuals:
####################
birkiedf_vis = birkiedf %>% 
        select("Name","Bib","Age","Sex","Start") %>% #,"D1","D2","D3","D4","D5") %>% 
        mutate(Pall = finishpace) %>% 
        mutate(P1 = firepace) %>% 
        mutate(P2 = firedoubleopace) %>% 
        mutate(P3 = doubleomosqpace) %>% 
        mutate(P4 = mosqhaywardpace) %>% 
        mutate(P5 = haywardfinishpace)  
       
birkiedf_vis = birkiedf_vis %>% 
  mutate(S1 = ((P1 - Pall)/Pall)*100) %>% 
  mutate(S2 = ((P2 - Pall)/Pall)*100) %>% 
  mutate(S3 = ((P3 - Pall)/Pall)*100) %>% 
  mutate(S4 = ((P4 - Pall)/Pall)*100) %>% 
  mutate(S5 = ((P5 - Pall)/Pall)*100) %>% 
  select(-P1,-P2,-P3,-P4,-P5)
  
print(dim(birkiedf_vis))
birkiedf_vis = birkiedf_vis[complete.cases(birkiedf_vis),] ## Remove DNFs
print(dim(birkiedf_vis))

checkpoint_distances = c(12, 21.1, 36.3, 47.2, 50)
distances <- function(pace_str) {
  checkpoint_distances[as.numeric(strsplit(pace_str, split="")[[1]][2])]
}

waves <- function(start_time) {
  if(start_time < as.times("08:30:00")){ return("Oldies")}
  if(start_time < as.times("08:50:00")){ return("WE")}
  if(start_time < as.times("08:55:00")){ return("ME")}
  if(start_time < as.times("09:05:00")){ return("W1")}
  if(start_time < as.times("09:20:00")){ return("W2")}
  if(start_time < as.times("09:30:00")){ return("W3")}
  if(start_time < as.times("09:40:00")){ return("W4")}
  if(start_time < as.times("09:50:00")){ return("W5")}
  if(start_time < as.times("09:55:00")){ return("W6")}
  if(start_time < as.times("10:00:00")){ return("W7")}
  if(start_time < as.times("10:05:00")){ return("W8")}
  else{ return("Late Start") }
}
as.times("08:30:30") < as.times("08:30:00")
birkiedf_vis_gath = birkiedf_vis %>% 
        gather(key = "checkpoint",value = "pace","S1","S2","S3","S4","S5") %>% 
        mutate(distance = unlist(lapply(checkpoint,distances))) %>% 
        mutate(wave = unlist(lapply(Start,waves)))
        
birkiedf_vis_gath %>% 
  filter(checkpoint == "S5") %>% 
  group_by(wave) %>% 
  summarise(n())

####################
## Visualizations:
####################

birkiedf_vis_gath %>% 
  filter(wave != "Oldies") %>% 
  ggplot(aes(x=distance,y=pace,color=wave))+
  geom_point()+
  geom_smooth(se = FALSE,size=1,method="loess")

birkiedf_vis_gath %>% 
  filter(wave != "Oldies") %>% 
  group_by(wave,distance) %>% 
  summarise(pace_agg = mean(pace)) %>% 
  ggplot()+
  geom_point(aes(x=distance,y=pace_agg,color=wave))

#write.csv(birkiedf_vis_gath,'d3_input.csv',row.names=FALSE)
#write.csv(birkiedf_vis_gath[sample(19000,1000,replace = FALSE),],'d3_input_small.csv',row.names=FALSE)

####################
## Add wave shifts...
####################

shift <- function(wave,checkpoint) {
  if(wave == "Oldies" ){s = -5}
  else if(wave == "WE"){s = 4}
  else if(wave == "ME"){s = 5}
  else if(wave == "W1"){s = 3}
  else if(wave == "W2"){s = 2}
  else if(wave == "W3"){s = 1}
  else if(wave == "W4"){s = 0}
  else if(wave == "W5"){s = -1}
  else if(wave == "W6"){s = -2}
  else if(wave == "W7"){s = -3}
  else if(wave == "W8"){s = -4}
  else{ print("ERROR in Wave Assignment") }
  factor = 0.2
  return(factor*s + checkpoint)
}

birkiedf_vis_gath2 = birkiedf_vis_gath %>% 
  mutate(distance = mapply(shift,wave,distance))

birkiedf_vis_gath2 %>% 
  #filter(wave != "Oldies") %>% 
  ggplot(aes(x=distance,y=pace,color=wave))+
  geom_point()#+
  #geom_smooth(se = FALSE,size=1,method="loess")

write.csv(birkiedf_vis_gath2,'d3_inputA.csv',row.names=FALSE)

####################
## Splits DataFrame
####################

birkiedf_vis2 = birkiedf %>% 
  select("Name","Bib","Age","Sex","Start","T1","T2","T3","T4","T5") %>% 
  mutate(wave = unlist(lapply(Start,waves)))
birkiedf_vis2 = birkiedf_vis2[complete.cases(birkiedf_vis2),] 

x_pos <- function(times_vec) {
  pos_vec = (rank(times_vec)-1) %% 5
  
  pos_vec[pos_vec == 0] = 0.5
  pos_vec[pos_vec == 1] = 1.5
  pos_vec[pos_vec == 2] = 2.5
  pos_vec[pos_vec == 3] = 3.5
  pos_vec[pos_vec == 4] = 4.5
  
  return(pos_vec)
}

birkiedf_vis2$X1 = x_pos(birkiedf_vis2$T1)
birkiedf_vis2$X2 = x_pos(birkiedf_vis2$T2)
birkiedf_vis2$X3 = x_pos(birkiedf_vis2$T3)
birkiedf_vis2$X4 = x_pos(birkiedf_vis2$T4)
birkiedf_vis2$X5 = x_pos(birkiedf_vis2$T5)

write.csv(birkiedf_vis2,'d3_inputB.csv',row.names=FALSE)


