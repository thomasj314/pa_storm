if(!file.exists("data")){
  dir.create("data")
}

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl,destfile = "./data/storm1.csv.bz2")

install.packages("R.utils")
library("R.utils")
library("knitr")
library("dplyr")

dfstorm <- read.csv(("./data/storm1.csv.bz2"), header=TRUE, sep = ",")

df <- subset(dfstorm,select = c("STATE__","STATE","EVTYPE","FATALITIES","INJURIES","PROPDMG",
                                "PROPDMGEXP","CROPDMG","CROPDMGEXP"))

## There were strings that don't need

df <- df[1:547363,]



df_storm <- dfstorm[1:547363,]


h_storm <- subset(df,select = c("EVTYPE","FATALITIES","INJURIES"))

h_storm$FATALITIES <- as.numeric(h_storm$FATALITIES)
h_storm$INJURIES <- as.numeric(h_storm$INJURIES)

h_storm$EVTYPE <- tolower(h_storm$EVTYPE)


##Storm Event.

storm_event <- c("astronomical low tide","avalanche","blizzard","coastal flood",
                 "debris flow","dense fog","dense smoke","drought","dust devil","dust storm","excessive heat",
                 "frost/freeze","flash flood","flood","cold wind chill","extreme cold/wind chill",
                 "funnel cloud","freezing fog","hail","heat","heavy snow","high surf","high wind",
                 "marine hail","marine high wind","marine strong wind","marine thunderstorm wind",
                 "hurricane","ice storm", "lake effect snow","lakeshore flood","lightning","marine hail",
                 "rip current","seiche","sleet","storm surge","strong wind","thunderstorm wind","tornado",
                 "tropical depression","tropical storm","tsunami","volcanic ash","waterspout","wildfire",
                 "winter storm","winter weather")

## making an empty data frame 
allresult <- data.frame()

for(i in 1:length(storm_event)){
  storm_grep <- grep(storm_event[i],h_storm$EVTYPE)
  storm_ls <- h_storm[storm_grep,]
  
  storm_result <- summarise(storm_ls,EVTYPE=storm_event[i],
                            FATALITIES=sum(FATALITIES),
                            INJURIES=sum(INJURIES))
  allresult <- rbind(allresult,storm_result)
}



## making a overall health value.

allresult$HEALTH <- allresult$FATALITIES + allresult$INJURIES


###############################################################
prop_df <- df[,c("EVTYPE","PROPDMG","PROPDMGEXP")]

prop_df$EVTYPE <- tolower(prop_df$EVTYPE)

pexp_label <- unique(df$PROPDMGEXP)
pexp_cost <- c(1000,1000000,0,1000000000,1000000,1,10,10,10,0,10,10,10,100,10,100,0,10,10)

prop_table <- data.frame(label = pexp_label, cost = pexp_cost)


for(i in 1:nrow(prop_table)){
prop_df$PROPDMGEXP[prop_df$PROPDMGEXP == prop_table[i,1]] <- prop_table[i,2]
}
prop_df$PROPDMG <- as.numeric(prop_df$PROPDMG)
prop_df$PROPDMGEXP <- as.numeric(prop_df$PROPDMGEXP)
prop_df$PROPTOTAL <- prop_df$PROPDMG*prop_df$PROPDMGEXP



prop_result <- data.frame()

for(i in 1:length(storm_event)){
  ev_type <- grep(storm_event[i],prop_df$EVTYPE)
  ev_typels <- prop_df[ev_type,]
  
  ev_result <- summarise(ev_typels,EVTYPE= storm_event[i],
                         PROPTOTAL=sum(PROPTOTAL))
  
  prop_result <- rbind(prop_result,ev_result)
}

par(mar=c(8,4,3,2),cex=0.8)

or_propdmg <- prop_result[order(-prop_result$PROPTOTAL),][1:10,]

with(or_propdmg,barplot(PROPTOTAL/1000000, names.arg = EVTYPE,
                       main = "Events that cause Property Damage",
                       ylab= "Damage Cost($million)", las=3, col="lightblue"))



#################################################################

crop_df <- df[,c("EVTYPE","CROPDMG","CROPDMGEXP")]

crop_df$EVTYPE <- tolower(crop_df$EVTYPE)

crop_label <- unique(crop_df$CROPDMGEXP)
crop_cost <- c(0,1000000,1000,1000000,1000000000,0,10,1000,10)

crop_table <- data.frame(label = crop_label,cost = crop_cost)
crop_table$cost <- as.numeric(crop_table$cost)

crop_df$CROPDMGEXP[crop_df$CROPDMGEXP == crop_table[1,1]] <- 0

for(i in 2:nrow(crop_table)){
  crop_df$CROPDMGEXP[crop_df$CROPDMGEXP == crop_table[i,1]] <- crop_table[i,2]
}



crop_df$CROPDMG <- as.numeric(crop_df$CROPDMG)
crop_df$CROPDMGEXP<- as.numeric(crop_df$CROPDMGEXP)
crop_df$CROPDMGTOTAL <- crop_df$CROPDMG*crop_df$CROPDMGEXP

crpdmg_result <- data.frame()

for(i in 1:length(storm_event)){
  cp_grep <- grep(storm_event[i],crop_df$EVTYPE)
  cp_ls <- crop_df[cp_grep,]
  
  cp_result <- summarise(cp_ls,EVTYPE = storm_event[i],
                         CROPDMGTOTAL=sum(CROPDMGTOTAL))
  crpdmg_result<- rbind(crpdmg_result,cp_result)  
}

or_crpdmg <- crpdmg_result[order(-crpdmg_result$CROPDMGTOTAL),][1:10,]

par(mar=c(8,4,3,2),cex=0.8)

with(or_crpdmg,barplot(CROPDMGTOTAL/1000000, names.arg = EVTYPE,
                           main = "Events that cause Crop Damage",
                           ylab= "Damage Cost($million)", las=3, col="lightgreen"))


with(or_propdmg,barplot(PROPTOTAL/1000000, names.arg = EVTYPE,
                        main = "Events that cause Property Damage",
                        ylab= "Damage Cost($million)", las=3, col="lightblue"))
or_allresult <- allresult[order(-allresult$HEALTH),][1:10,]

with(or_allresult,barplot(HEALTH,names.arg = EVTYPE,
                       main = "Events that are harmful to our health",col="blue",las=3))
