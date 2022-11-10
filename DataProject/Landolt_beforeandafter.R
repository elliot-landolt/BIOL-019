# Elliot Landolt Research Question

# Do dolphins change their behavior before and after receiving a shark bite?
# Let's run through an example with rest

dolphins <- read.csv("dolphins.csv")
shark_bites <- read.csv("shark_bites.csv")
sightings <- read.csv("sightings.csv")

# Filter data for all dolphins that have experienced a shark attack from 'sightings.csv' by comparing Dolphin.ID column to 'shark_bites.csv' to create a new table 'bit_dolphins' for only dolphins that have been bit

bit_dolphins <- sightings[which(sightings$Dolphin.ID %in% shark_bites$Dolphin.ID), ]

# Bit dolphins that do not have data for dates occurring both before and after the Shark.Bite.Date should be removed from the 'bit_dolphins' table. This can be done by only filtering for dolphins that have data both before and after the Shark.Bite.Date

# Note - There are some dolphins who have been bit more than once, but for this example, we'll only consider before and after the first recorded bite

shark_bites$Shark.Bite.Date <- as.Date(shark_bites$Shark.Bite.Date)
shark_bites <- shark_bites[order(shark_bites$Shark.Bite.Date), ]
first_shark_bites <- shark_bites[!duplicated(shark_bites$Dolphin.ID), ]

# In the sightings data, we'll add a column with the date of the first shark bite for that individual

bit_dolphins$first_shark_bite_date <-
  first_shark_bites$Shark.Bite.Date[match(bit_dolphins$Dolphin.ID, first_shark_bites$Dolphin.ID)]

# Classify each sighting as before or after that individual's first shark bite

bit_dolphins$Observation.Date <- as.Date(bit_dolphins$Observation.Date)

bit_dolphins$bite_time_frame <- ifelse(bit_dolphins$Observation.Date >=
                                         bit_dolphins$first_shark_bite_date, "after",
                                       "before")

# Filter for those seen at least 5 times before and after the shark bite
# Could also try setting a higher threshold here

counts <-
  table(bit_dolphins$Dolphin.ID, bit_dolphins$bite_time_frame) |>
  as.data.frame.matrix()

both_time_frames <- rownames(counts)[which(counts$after > 5 &
                                             counts$before > 5)]

bit_dolphins <- bit_dolphins[which(bit_dolphins$Dolphin.ID %in% both_time_frames), ]

#Calculate frequency of each activity before and after

activity_budget <- table(bit_dolphins[,c("Dolphin.ID", 
                                   "bite_time_frame", 
                                   "Activity")]) |> prop.table(margin = 1) |> as.data.frame() 

activity_budget <- reshape(activity_budget, direction = "wide", 
                           idvar = c("Dolphin.ID", "bite_time_frame"), timevar = "Activity")

# Use some packages to help reshape and plot paired data
# install.packages(c("tidyr", "ggplot2", "ggpubr"))

library(tidyr)
library(ggpubr)

# Pull out just the frequency of rest to visualize first 
paired_props <- activity_budget |>
  pivot_wider(id_cols = Dolphin.ID, names_from = bite_time_frame, 
              values_from = c(Freq.REST))


# Find the rest change across all dolphins remaining in the bit_dolphins table
paired_props$rest.change <- paired_props$after - paired_props$before

head(paired_props)
colMeans(paired_props[, -1])

# Plot individual paired data
ggpaired(data = paired_props, cond1 = "before", cond2 = "after",
         color = "condition", palette = "npg",
         line.color = adjustcolor("gray", alpha.f = 0.4),
         line.size = 0.4,
         xlab = "Time Frame",
         ylab = "Percent rest Use")
