library(plyr)
library(car)
library(gvlma)

setwd("~/School/UW/Dissertation/Data")

############################
# VOIS3D/Advancement Stats #
############################

speaker.data <- read.delim("Speaker-Data-R.txt", na.strings = c("NA", ""), encoding = "UTF-8")

# Fix column names
colnames(speaker.data)[12:23] <- c("aeg_ehg.3d", "aeg_eyg.3d", "aeg_ey.3d", "ehg_eyg.3d", "ehg_ey.3d", "eyg_ey.3d", "aeg_ehg.2d", "aeg_eyg.2d", "aeg_ey.2d", "ehg_eyg.2d", "ehg_ey.2d", "eyg_ey.2d")

# Reorder SEC
speaker.data$SEC.Birth <- factor(speaker.data$SEC.Now, levels = c("Working", "Lower-middle", "Middle", "Upper-middle"))
speaker.data$SEC.Now <- factor(speaker.data$SEC.Now, levels = c("Working", "Lower-middle", "Middle", "Upper-middle"))


# Recode generation to make gen 2 baseline
speaker.data$Generation.2 <- as.character(speaker.data$Generation)
speaker.data$Generation.2 <- gsub("2", "0", speaker.data$Generation.2)

# Recode ethnicity to make Caucasian baseline
speaker.data$Ethnicity.2 <- as.character(speaker.data$Ethnicity)
speaker.data$Ethnicity.2 <- gsub("Caucasian", "a", speaker.data$Ethnicity.2)

# Recode region to east/west, make west baseline
speaker.data$Region.2 <- as.character(speaker.data$Region)
speaker.data$Region.2 <- gsub("Central", "East", speaker.data$Region.2)
speaker.data$Region.2 <- gsub("West", "a", speaker.data$Region.2)

# Set variables to factors
speaker.data$Generation <- as.factor(speaker.data$Generation)
speaker.data$Regionality <- as.factor(speaker.data$Regionality)
speaker.data$Generation.2 <- as.factor(speaker.data$Generation.2)
speaker.data$Ethnicity.2 <- as.factor(speaker.data$Ethnicity.2)
speaker.data$Region.2 <- as.factor(speaker.data$Region.2)


### Build up model ###
advancement.lm <- lm(Advancement.3 ~ Generation.2, data = speaker.data)
print(summary(advancement.lm))

advancement.lm <- lm(Advancement.3 ~ Generation.2 + Gender, data = speaker.data)
print(summary(advancement.lm))

advancement.lm <- lm(Advancement.3 ~ Generation.2 + Gender + Ethnicity.2, data = speaker.data)
print(summary(advancement.lm))

advancement.lm <- lm(Advancement.3 ~ Generation.2 + Gender + Region, data = speaker.data)
print(summary(advancement.lm))

advancement.lm <- lm(Advancement.3 ~ Generation.2 + Gender + Region.2, data = speaker.data)
print(summary(advancement.lm))

advancement.lm <- lm(Advancement.3 ~ Generation.2 + Gender + Regionality, data = speaker.data)
print(summary(advancement.lm))

advancement.lm <- lm(Advancement.3 ~ Generation.2 + Gender + NSS, data = speaker.data)
print(summary(advancement.lm))

advancement.lm <- lm(Advancement.3 ~ Generation.2 + Gender + NSS, data = speaker.data)
print(summary(advancement.lm))

qqPlot(advancement.lm, main="QQ Plot")
gvmodel <- gvlma(advancement.lm)
summary(gvmodel)


#####################
# Vowel-based Stats #
#####################

# Read in data as UTF-8
data <- read.delim("Vowel-Data-R.txt", encoding = "UTF-8")

# Alter following class codings
data$Following.Class <- gsub("#", "C", data$Following.Class)
data$Following.Class <- gsub("G", "g", data$Following.Class)

# Recode region to east/west, make west baseline
data$Region.2 <- as.character(data$Region)
data$Region.2 <- gsub("Central", "East", data$Region.2)
data$Region.2 <- gsub("West", "a", data$Region.2)

# Set generation and following class to factor
data$Generation <- as.numeric(data$Generation)
data$Following.Class <- as.factor(data$Following.Class)
data$Region.2 <- as.factor(data$Region.2)

# Lowercase gender
data$Gender <- tolower(data$Gender)
data$Gender <- as.factor(data$Gender)

# Reorder vowel factor
data$Vowel <- factor(data$Vowel, levels = c("i", "\U026A", "e", "\U025B", "æ", "\U0251", "\U0254", "o", "\U028A", "u"))

# Drop levels
data <- droplevels(data)

# Subset to front vowels
data.stats <- droplevels(subset(data, Vowel == "e" | Vowel == "\U025B" | Vowel == "æ"))

# Calculate means by ethnicity and wordclass
data.means <- ddply(data.stats, .(Speaker, Region.2, Gender, Ethnicity, Generation, Vowel, Following.Class), summarize, F1_20 = mean(Normalized.F1.20.), F1_50 = mean(Normalized.F1.50.), F1_80 = mean(Normalized.F1.80.), F2_20 = mean(Normalized.F2.20.), F2_50 = mean(Normalized.F2.50.), F2_80 = mean(Normalized.F2.80.))

# Get euclidean distance between wordclasses by vowel
data.distances <- ddply(data.means, .(Speaker, Region.2, Gender, Ethnicity, Generation, Vowel), summarize, dist_20 = sqrt((F1_20[1] - F1_20[2]) ^ 2 + (F2_20[1] - F2_20[2]) ^ 2), dist_50 = sqrt((F1_50[1] - F1_50[2]) ^ 2 + (F2_50[1] - F2_50[2]) ^ 2), dist_80 = sqrt((F1_80[1] - F1_80[2]) ^ 2 + (F2_80[1] - F2_80[2]) ^ 2), dist_sum = dist_20 + dist_50 + dist_80)

# Get temporal displacement by vowel (subset to NPV classes)
data.displacements <- ddply(droplevels(subset(data.means, Following.Class == "C")), .(Speaker, Region.2, Gender, Ethnicity, Generation, Vowel), summarize, F1_tdisp = abs(F1_20 - F1_80), F2_tdisp = abs(F2_20 - F2_80), total_tdisp = F1_tdisp + F2_tdisp)

# Get z-scores for duration
data.dur <- data.stats
data.dur$Duration <- scale(data.dur$Duration..ms.)

# Calculate mean duration by speaker and wordclass
data.speaker_dur_means <- ddply(droplevels(subset(data.dur, Vowel == "e" | Vowel == "\U025B" | Vowel == "æ")), .(Speaker, Region, Gender, Ethnicity, Generation, Vowel, Following.Class), summarize, Duration = mean(Duration))

# Calculate mean duration by wordclass
data.dur_means <- ddply(data.speaker_dur_means, .(Vowel, Following.Class), summarize, Duration = mean(Duration))


### Models ###

# Euclidean distance by generation
distances.lm <- lm(dist_sum ~ Generation, data = data.distances)
summary(distances.lm)

distances.lm <- lm(dist_sum ~ Generation + Ethnicity, data = data.distances)
summary(distances.lm)

# TD by generation (speaker totals)
displacements.lm <- lm(total_tdisp ~ Generation, data = data.displacements)
summary(displacements.lm)

# Duration by wordclass
dur.lm <- lm(Duration ~ Vowel * Following.Class, data = data.speaker_dur_means)
summary(dur.lm)


########################
# Social Network Stats #
########################
social_network_data <- ddply(speaker.data, .(Ethnicity), summarize, NSS_mean = mean(NSS, na.rm = T), NSS_median = median(NSS, na.rm = T), Same_friends_mean = mean(Same.Proportion, na.rm = T), Caucasian_friends_mean = mean(Caucasian.Proportion, na.rm = T), Bias = mean(Caucasian.Bias, na.rm = T))
