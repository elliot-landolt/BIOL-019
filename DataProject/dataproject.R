library(ggplot2)
library(plyr)
library(httpgd)

dolphins <- read.csv("/Users/Elliot/Desktop/BIOL-019/DataProject/dolphins.csv")
sightings <- read.csv("/Users/Elliot/Desktop/BIOL-019/DataProject/sightings.csv")
shark_bites <- read.csv("/Users/Elliot/Desktop/BIOL-019/DataProject/shark_bites.csv")
test <- 1

#Preview

head(sightings, 10)
tail(sightings, 2)

table(dolphins$Sex)

sightings[1:5, 1:3]

max(sightings$Age)
range(sightings$Age)
summary(sightings$Age)

head(shark_bites)

#Combine and Filter

shark_bites$Sex <- dolphins$Sex[match(shark_bites$Dolphin.ID, dolphins$Dolphin.ID)]

adult_sightings <- sightings[which(sightings$Life.History.Status == "adult"),]

#Calculate age at time of bite

shark_bites$Birth.Date <- dolphins$Birth.Date[match(shark_bites$Dolphin.ID, dolphins$Dolphin.ID)]

shark_bites$Birth.Date <- as.Date(shark_bites$Birth.Date)
shark_bites$Shark.Bite.Date <- as.Date(shark_bites$Shark.Bite.Date)

shark_bites$Age <- shark_bites$Shark.Bite.Date - shark_bites$Birth.Date

shark_bites$Age <- as.numeric(shark_bites$Age/365)

#Analyze

shark_bites$age_category <- ifelse(shark_bites$Age >= 12, "Adult", "Juvenile")

sb_results <- table(shark_bites$Sex, shark_bites$age_category)

print(test)