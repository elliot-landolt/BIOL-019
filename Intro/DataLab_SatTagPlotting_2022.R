
################################
#### Part I: Load Tag Data ####
################################

# Install packages before starting ('Tools'--> 'Install Packages')
# You'll only need to install once, but then you'll still need to
# load the library every time you restart R or RStudio

# Load in package libraries
library(ggplot2)
library(plyr)

# Load in the data (make sure to set your working directory to the file location)
load("/Users/Elliot/Desktop/BIOL-019/Intro/alltags_clean.RData")

# This is a collection of 30 tags from both pilot whales (Gm)
# And beaked whales (Zc)

##############################################
#### Part II: Exploring An Individual Tag ####
##############################################

# Extract a single tag from the dataset
GmTag085 <- alltags_clean[[1]] # makes into normal data frame that you can work with

GmTag085$DepthMin <- as.numeric(GmTag085$DepthMin) # changes data in this column from character to numeric (change occurs in GmTag085 data frame, not in the larger list)

GmTag085 <- subset(GmTag085, GmTag085$What == "Dive" | GmTag085$What == "Surface" | GmTag085$What == "GAP") # Pulls out messages, leaving just Dive and Surface data (and gaps)

GmTag085$DepthMin[is.na(GmTag085$DepthMin)] <- 0 # Turns all surface data with no depth to 0

GmDepth = -1 * GmTag085$DepthMin # Makes depth data negative so it's intuitive when graphed

Gm085gaps <- subset(GmTag085, GmTag085$What == "GAP")

## Plot 1
# Date vs. dive depth for GmTag085
dev.new() # this command tells R to open your next plot in a new window, you'll need to highlight this line and the next 6 lines, then hit 'command/ctrl' + 'enter'
ggplot() +
  geom_line(data = GmTag085, size = 0.25, aes(x = Start, y = GmDepth)) +
  geom_rect(data = Gm085gaps, alpha = 0.5, 
            aes(xmin = Start, xmax = End, ymin = -1250, ymax = 0)) +
  geom_point(data = GmTag085, alpha = 0.6, 
             aes(x = Start, y = GmDepth, colour = GmDepth)) +
  ggtitle("GmTag085") +
  xlab("Date") +
  ylab("Depth (m)") +
  labs(color = "Duration (min)")

### 2 Plotting Dive Depth versus Dive Duration for GmTag085 ###

## Plot 2
dev.new()
ggplot(data = GmTag085, aes(x = DepthMin, y = DurationMin)) +
  geom_point(aes(color = DeployID)) +
  ggtitle("GmTag085 Depth Duration Distribution") +
  xlab("Dive Depth(m)") +
  ylab("Dive Duration(min)") +
  ylim(0, 30) +
  labs(color = "Tag ID")

##########################################################################
#### Part III: Visualizing Individual Variation and Comparing Species ####
##########################################################################

### To do this, we'll graph multiple tags together

# First, separate pilot whale and beaked whale tags into 2 objects
GmTaglist <- alltags_clean[grep("GmTag", alltags_clean)]
ZcTaglist <- alltags_clean[grep("ZcTag", alltags_clean)]

# Then turn these lists into single data frames
GmTags <- ldply(GmTaglist) # moves list into single data frame
ZcTags <- ldply(ZcTaglist)

### 3 Plotting Dive Depth and Duration distribution for all pilot whales ###

# Make sure all the columns you plan to work with are considered numeric data
GmTags$DepthMin <- as.numeric(GmTags$DepthMin)
GmTags$DurationMin <- as.numeric(GmTags$DurationMin)

# Only select rows without missing data
GmTags <- GmTags[complete.cases(GmTags[, c("DepthMin", "DurationMin")]), ]

# Plot 3 Pilot Whale Point Plot

ggplot(data = GmTags, aes(x = DepthMin, y = DurationMin)) +
  geom_point(aes(color = DeployID), alpha = 0.5) +
  ggtitle("Pilot Whale Depth Duration Distribution") +
  xlab("Dive Depth(m)") +
  ylab("Dive Duration(min)") +
  ylim(0, 30) +
  labs(color = "Tag ID")

### Now repeat previous step for beaked whales ###

ZcTags$DepthMin <- as.numeric(ZcTags$DepthMin)
ZcTags$DurationMin <- as.numeric(ZcTags$DurationMin)

ZcTags <- ZcTags[complete.cases(ZcTags[, c("DepthMin", "DurationMin")]), ]

# Plot 4 Beaked Whale Point Plot

ggplot(data = ZcTags, aes(x = DepthMin, y = DurationMin)) +
  geom_point(aes(color = DeployID), alpha = 0.5) +
  ggtitle("Beaked Whale Depth Duration Distribution") +
  xlab("Dive Depth(m)") +
  ylab("Dive Duration(min)") +
  ylim(0, 150) +
  labs(color = "Tag ID")

## Exercise - can you re-run the pilot whale plot, changing the scale of the axes to match the beaked whale data?
