library(phonR)
library(plyr)

setwd("~/School/UW/Dissertation/Graphics")

# Read in data as UTF-8
data <- read.delim("../Data/Vowel-Data-R.txt", encoding = "UTF-8")

# Alter following class codings
data$Following.Class <- gsub("#", "C", data$Following.Class)
data$Following.Class <- gsub("G", "g", data$Following.Class)

# Set generation and following class to factor
data$Generation <- as.factor(data$Generation)
data$Following.Class <- as.factor(data$Following.Class)

# Lowercase gender
data$Gender <- tolower(data$Gender)
data$Gender <- as.factor(data$Gender)

# Reorder vowel factor
data$Vowel <- factor(data$Vowel, levels = c("i", "\U026A", "e", "\U025B", "æ", "\U0251", "\U0254", "o", "\U028A", "u"))

# Sort data (for phonR)
data <- data[order(data$Vowel),]

# Drop levels
data <- droplevels(data)

# Set raw limits
f1_lim = c(1100, 200)
f2_lim = c(3200, 700)

# Set normalized limits
norm_f1_lim = c(0.2, -1.4)
norm_f2_lim = c(1.1, 0.3)

# Select pre-velar raising/lowering vowels
pv_data <- droplevels(subset(data, Vowel == "æ" | Vowel == "\U025B" | Vowel == "e"))


#########################
# All speakers (tokens) #
#########################

cairo_pdf("Plots/Individual/All speakers (tokens).pdf", width = 11, height = 8.5, onefile = T)

# Set margins to account for title
par(cex = 1.5, lwd = 2)

# Plot all speakers (tokens)
for (i in 1:nlevels(data$Speaker)) {

	# Subset data by current speaker
	data.speaker <- droplevels(subset(data, Speaker == levels(data$Speaker)[i]))

	plotVowels(f1 = data.speaker$F1.50., f2 = data.speaker$F2.50., vowel = data.speaker$Vowel, group = data.speaker$Following.Class, var.col.by = data.speaker$Vowel, var.style.by = data.speaker$Following.Class, plot.tokens = T, pch.tokens = paste(data.speaker$Vowel, data.speaker$Following.Class, sep = ""), cex.tokens = 1, cex.lab = 1.5, xlim = f2_lim, ylim = f1_lim, ellipse.line = T, pretty = T, main = paste(levels(data$Speaker)[i]), cex.main = 1.5, font.main = 2, xlab = "F2 (Hz)", ylab = "F1 (Hz)")
}

dev.off()


########################
# All speakers (means) #
########################

cairo_pdf("Plots/Individual/All speakers (means).pdf", width = 11, height = 8.5, onefile = T)

# Set margins to account for title
par(cex = 1.5, lwd = 2)

# Plot all speakers (means)
for (i in 1:nlevels(data$Speaker)) {

	# Subset data by current speaker
	data.speaker <- subset(data, Speaker == levels(data$Speaker)[i])

	plotVowels(f1 = data.speaker$F1.50., f2 = data.speaker$F2.50., vowel = data.speaker$Vowel, group = data.speaker$Following.Class, var.col.by = data.speaker$Vowel, var.style.by = data.speaker$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.speaker$Vowel, data.speaker$Following.Class, sep = ""), cex.means = 1, cex.lab = 1.5, xlim = f2_lim, ylim = f1_lim, ellipse.line = T, pretty = T, main = paste(levels(data$Speaker)[i]), cex.main = 1.5, font.main = 2, xlab = "F2 (Hz)", ylab = "F1 (Hz)")
}

dev.off()


############################
# All speakers (NPV means) #
############################

cairo_pdf("Plots/Individual/All speakers (NPV means).pdf", width = 8.5, height = 11, onefile = T)

# Set margins to account for title, plot in 3x2 grid
par(cex = 1.5, lwd = 1, oma = c(0, 0, 1, 0), mfrow = c(3, 2))

# Plot all speakers (means)
for (i in 1:nlevels(data$Speaker)) {

	# Hide y axis labels for left-hand plots
	if (i %% 2 == 1) {
		f1_lab <- ""
		par(yaxt = "n")
	} else {
		f1_lab <- "F1 (Hz)"
		par(yaxt = "s")
	}

	# Subset data by current speaker and non-g tokens
	data.speaker <- subset(data, Speaker == levels(data$Speaker)[i] & Following.Class != "g")

	plotVowels(f1 = data.speaker$F1.50., f2 = data.speaker$F2.50., vowel = data.speaker$Vowel, var.col.by = data.speaker$Vowel, plot.tokens = F, plot.means = T, pch.means = data.speaker$Vowel, cex.means = 1, cex.lab = 1, xlim = f2_lim, ylim = f1_lim, poly.line = T, poly.order = c("i", "\U026A", "e", "\U025B", "æ", "\U0251", "\U0254", "o", "\U028A", "u"), pretty = T, main = paste(levels(data$Speaker)[i]), cex.main = 1.25, font.main = 1.5, xlab = "F2 (Hz)", ylab = f1_lab)
}

dev.off()


#############
# Ethnicity #
#############

cairo_pdf("Plots/Group/Ethnicity.pdf", width = 11, height = 8.5, onefile = T)

# Set margins to account for title
par(cex = 1.5, lwd = 2)

# Plot ethnicities (means)
for (i in 1:nlevels(pv_data$Ethnicity)) {

	# Subset data by current ethnicity
	data.ethnicity <- droplevels(subset(pv_data, Ethnicity == levels(pv_data$Ethnicity)[i]))

	plotVowels(f1 = data.ethnicity$Normalized.F1.50., f2 = data.ethnicity$Normalized.F2.50., vowel = data.ethnicity$Vowel, group = data.ethnicity$Following.Class, var.col.by = data.ethnicity$Vowel, col = c("green3", "firebrick2", "blueviolet"), var.style.by = data.ethnicity$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.ethnicity$Vowel, data.ethnicity$Following.Class, sep = ""), cex.means = 1, cex.lab = 1.5, xlim = norm_f2_lim, ylim = norm_f1_lim, ellipse.line = T, pretty = T, main = paste(levels(pv_data$Ethnicity)[i]), cex.main = 1.5, font.main = 2, xlab = "F2 (Nearey2)", ylab = "F1 (Nearey2)")

	# Plot ns
	text(0.375, 0.1, paste("n = ", nlevels(data.ethnicity$Speaker), sep = ""))
}

dev.off()


######################
# Ethnicity Combined #
######################

cairo_pdf("Plots/Group/Ethnicity (Combined).pdf", width = 12.75, height = 7.33, onefile = T)

# Set margins to account for title
par(cex = 1.5, lwd = 2, oma=c(0, 0, 1, 0), mfrow = c(2, 3))

# Plot ethnicities (means)
for (i in 1:nlevels(pv_data$Ethnicity)) {
	# Increment plot count
	plot.count = plot.count + 1

	# Hide y axis labels for left-hand plots
	if (plot.count == 3 | plot.count == 5) {
		f1_lab <- "F1 (Nearey2)"
		par(yaxt = "s")
	} else {
		f1_lab <- ""
		par(yaxt = "n")
	}

	# Hide x axis labels for bottom plots
	if (plot.count <= 3) {
		f2_lab <- "F2 (Nearey2)"
		par(xaxt = "s")
	} else {
		f2_lab <- ""
		par(xaxt = "n")
	}

	# Subset data by current ethnicity
	data.ethnicity <- droplevels(subset(pv_data, Ethnicity == levels(pv_data$Ethnicity)[i]))

	plotVowels(f1 = data.ethnicity$Normalized.F1.50., f2 = data.ethnicity$Normalized.F2.50., vowel = data.ethnicity$Vowel, group = data.ethnicity$Following.Class, var.col.by = data.ethnicity$Vowel, col = c("darkolivegreen", "firebrick2", "blueviolet"), var.style.by = data.ethnicity$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.ethnicity$Vowel, data.ethnicity$Following.Class, sep = ""), cex.means = 1.5, cex.lab = 1.5, xlim = norm_f2_lim, ylim = norm_f1_lim, ellipse.line = T, pretty = T, main = paste(levels(pv_data$Ethnicity)[i]), cex.main = 1.5, font.main = 2, xlab = "F2 (Nearey2)", ylab = "F1 (Nearey2)")

	# Plot ns
	text(0.375, 0.1, paste("n = ", nlevels(data.ethnicity$Speaker), sep = ""))
}

dev.off()


##########
# Gender #
##########

cairo_pdf("Plots/Group/Gender.pdf", width = 8.5, height = 11, onefile = T)

# Set margins to account for title, plot in 3x2 grid
par(cex = 1.5, lwd = 1, oma=c(0, 0, 1, 0), mfrow = c(3, 2))

# Plot genders (means)
for (i in 1:nlevels(pv_data$Gender)) {

	# Hide y axis labels for left-hand plot
	if (i == 1) {
		f1_lab <- ""
		par(yaxt = "n")
	} else {
		f1_lab <- "F1 (Nearey2)"
		par(yaxt = "s")
	}

	# Subset data by gender
	data.gender <- droplevels(subset(pv_data, Gender == levels(pv_data$Gender)[i]))

	plotVowels(f1 = data.gender$Normalized.F1.50., f2 = data.gender$Normalized.F2.50., vowel = data.gender$Vowel, group = data.gender$Following.Class, var.col.by = data.gender$Vowel, col = c("green3", "firebrick2", "blueviolet"), var.style.by = data.gender$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.gender$Vowel, data.gender$Following.Class, sep = ""), cex.means = 1, cex.lab = 1, xlim = norm_f2_lim, ylim = norm_f1_lim, ellipse.line = T, pretty = T, main = paste(levels(pv_data$Gender)[i]), cex.main = 1.25, font.main = 1.5, xlab = "F2 (Nearey2)", ylab = f1_lab)

	# Plot ns
	text(0.375, 0.1, paste("n = ", nlevels(data.gender$Speaker), sep = ""))
}

dev.off()


#########################
# Gender (presentation) #
#########################

cairo_pdf("Plots/Group/Gender (presentation).pdf", width = 8.5, height = 11, onefile = T)

# Set margins to account for title, plot in 3x2 grid
par(cex = 1.5, lwd = 2, oma=c(0, 0, 1, 0), mfrow = c(3, 2))

# Plot genders (means)
for (i in 1:nlevels(pv_data$Gender)) {

	# Hide y axis labels for left-hand plot
	if (i == 1) {
		f1_lab <- ""
		par(yaxt = "n")
	} else {
		f1_lab <- "F1 (Nearey2)"
		par(yaxt = "s")
	}

	# Subset data by gender
	data.gender <- droplevels(subset(pv_data, Gender == levels(pv_data$Gender)[i]))

	plotVowels(f1 = data.gender$Normalized.F1.50., f2 = data.gender$Normalized.F2.50., vowel = data.gender$Vowel, group = data.gender$Following.Class, var.col.by = data.gender$Vowel, col = c("darkolivegreen", "firebrick2", "blueviolet"), var.style.by = data.gender$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.gender$Vowel, data.gender$Following.Class, sep = ""), cex.means = 1.5, cex.lab = 1.5, xlim = norm_f2_lim, ylim = norm_f1_lim, ellipse.line = T, pretty = T, main = paste(levels(pv_data$Gender)[i]), cex.main = 1.5, font.main = 2, xlab = "F2 (Nearey2)", ylab = f1_lab)

	# Plot ns
	text(0.375, 0.1, paste("n = ", nlevels(data.gender$Speaker), sep = ""))
}

dev.off()


##############
# Generation #
##############

cairo_pdf("Plots/Group/Generation.pdf", width = 8.5, height = 11, onefile = T)

# Reset counter to allow for axis only on right-most plot
plot.count = 0

# Set margins to account for title, plot in 3x2 grid
par(cex = 1.5, lwd = 1, oma=c(0, 0, 1, 0), mfrow = c(3, 2))

# Plot generations (means)
for (i in 1:nlevels(pv_data$Generation)) {

	# Subset data by current ethnicity
	data.generation <- droplevels(subset(pv_data, Generation == levels(pv_data$Generation)[i]))

	for (j in 1:nlevels(data.generation$Gender)) {

		# Increment plot count
		plot.count = plot.count + 1

		# Hide y axis labels for left-hand plots
		if (plot.count %% 2 == 0) {
			f1_lab <- "F1 (Nearey2)"
			par(yaxt = "s")
		} else {
			f1_lab <- ""
			par(yaxt = "n")
		}

		# Subset by gender
		data.gender <- droplevels(subset(data.generation, Gender == levels(data.generation$Gender)[j]))

		plotVowels(f1 = data.gender$Normalized.F1.50., f2 = data.gender$Normalized.F2.50., vowel = data.gender$Vowel, group = data.gender$Following.Class, var.col.by = data.gender$Vowel, col = c("green3", "firebrick2", "blueviolet"), var.style.by = data.gender$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.gender$Vowel, data.gender$Following.Class, sep = ""), cex.means = 1, cex.lab = 1, xlim = norm_f2_lim, ylim = norm_f1_lim, ellipse.line = T, pretty = T, main = paste("Generation ", levels(pv_data$Generation)[i], " ", levels(data.generation$Gender)[j], "s", sep = ""), cex.main = 1.25, font.main = 1.5, xlab = "F2 (Nearey2)", ylab = f1_lab)

		# Plot ns
		text(0.375, 0.1, paste("n = ", nlevels(data.gender$Speaker), sep = ""))
	}
}

dev.off()


#######################
# Generation Combined #
#######################

cairo_pdf("Plots/Group/Generation (Combined).pdf", width = 8.5, height = 7.33, onefile = T)

# Set margins to account for title, plot in 3x2 grid
par(cex = 1.5, lwd = 2, oma=c(0, 0, 1, 0), mfrow = c(2, 2))

# Plot generations (means)
for (i in 1:nlevels(pv_data$Generation)) {

	# Subset data by current ethnicity
	data.generation <- droplevels(subset(pv_data, Generation == levels(pv_data$Generation)[i]))

	plotVowels(f1 = data.generation$Normalized.F1.50., f2 = data.generation$Normalized.F2.50., vowel = data.generation$Vowel, group = data.generation$Following.Class, var.col.by = data.generation$Vowel, col = c("darkolivegreen", "firebrick2", "blueviolet"), var.style.by = data.generation$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.generation$Vowel, data.generation$Following.Class, sep = ""), cex.means = 1.25, cex.lab = 1.5, xlim = norm_f2_lim, ylim = norm_f1_lim, ellipse.line = T, pretty = T, main = paste("Generation ", levels(pv_data$Generation)[i], sep = ""), cex.main = 1.5, font.main = 2, xlab = "F2 (Nearey2)", ylab = "F1 (Nearey2)")

	# Plot ns
	text(0.375, 0.1, paste("n = ", nlevels(data.generation$Speaker), sep = ""))
}

dev.off()


##########
# Region #
##########

cairo_pdf("Plots/Group/Region.pdf", width = 11, height = 8.5, onefile = T)

# Set margins to account for title
par(cex = 1.5, lwd = 2)

# Plot regions (means)
for (i in 1:nlevels(pv_data$Region)) {

	# Subset data by current ethnicity
	data.region <- droplevels(subset(pv_data, Region == levels(pv_data$Region)[i]))

	plotVowels(f1 = data.region$Normalized.F1.50., f2 = data.region$Normalized.F2.50., vowel = data.region$Vowel, group = data.region$Following.Class, var.col.by = data.region$Vowel, col = c("green3", "firebrick2", "blueviolet"), var.style.by = data.region$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.region$Vowel, data.region$Following.Class, sep = ""), cex.means = 1, cex.lab = 1.5, xlim = norm_f2_lim, ylim = norm_f1_lim, ellipse.line = T, pretty = T, main = paste(levels(pv_data$Region)[i]), cex.main = 1.5, font.main = 2, xlab = "F2 (Nearey2)", ylab = "F1 (Nearey2)")

	text(0.375, 0.1, paste("n = ", nlevels(data.region$Speaker), sep = ""))
}

dev.off()


###################
# Breakdown (raw) #
###################

cairo_pdf("Plots/Group/Breakdown (raw).pdf", width = 8.5, height = 11, onefile = T)

# Set margins to account for title, plot in 3x2 grid
par(cex = 1.5, lwd = 1, oma=c(0, 0, 1, 0), mfrow = c(3, 2))

# Plot ethnicities by gender and generation (means)
for (i in 1:nlevels(pv_data$Ethnicity)) {

	# Subset data by current ethnicity
	data.ethnicity <- droplevels(subset(pv_data, Ethnicity == levels(pv_data$Ethnicity)[i]))

	# Plot genders
	for (j in 1:nlevels(data.ethnicity$Gender)) {

		# Subset by gender
		data.ethnicity.subset <- droplevels(subset(data.ethnicity, Gender == levels(data.ethnicity$Gender)[j]))

		plotVowels(f1 = data.ethnicity.subset$F1.50., f2 = data.ethnicity.subset$F2.50., vowel = data.ethnicity.subset$Vowel, group = data.ethnicity.subset$Following.Class, var.col.by = data.ethnicity.subset$Vowel, col = c("green3", "firebrick2", "blueviolet"), var.style.by = data.ethnicity.subset$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.ethnicity.subset$Vowel, data.ethnicity.subset$Following.Class, sep = ""), cex.means = 1, cex.lab = 1, xlim = f2_lim, ylim = f1_lim, ellipse.line = T, pretty = T, main = paste(levels(pv_data$Ethnicity)[i], levels(data.ethnicity$Gender)[j]), cex.main = 1.25, font.main = 1.5, xlab = "F2 (Hz)", ylab = "F1 (Hz)")

		# Plot ns
		text(1000, 950, paste("n = ", nlevels(data.ethnicity.subset$Speaker), sep = ""))
	}

	# Plot generations
	for (j in 1:nlevels(data.ethnicity$Generation)) {

		# Subset by generation
		data.ethnicity.subset <- droplevels(subset(data.ethnicity, Generation == levels(data.ethnicity$Generation)[j]))

		plotVowels(f1 = data.ethnicity.subset$F1.50., f2 = data.ethnicity.subset$F2.50., vowel = data.ethnicity.subset$Vowel, group = data.ethnicity.subset$Following.Class, var.col.by = data.ethnicity.subset$Vowel, col = c("green3", "firebrick2", "blueviolet"), var.style.by = data.ethnicity.subset$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.ethnicity.subset$Vowel, data.ethnicity.subset$Following.Class, sep = ""), cex.means = 1, cex.lab = 1, xlim = f2_lim, ylim = f1_lim, ellipse.line = T, pretty = T, main = paste(levels(pv_data$Ethnicity)[i], "generation", levels(data.ethnicity$Generation)[j]), cex.main = 1.25, font.main = 1.5, xlab = "F2 (Hz)", ylab = "F1 (Hz)")

		# Plot ns
		text(1000, 950, paste("n = ", nlevels(data.ethnicity.subset$Speaker), sep = ""))
	}
}

dev.off()


####################################
# Breakdown (normalized, midpoint) #
####################################

cairo_pdf("Plots/Group/Breakdown (normalized, midpoint).pdf", width = 8.5, height = 11, onefile = T)

for (i in 1:nlevels(pv_data$Ethnicity)) {

	# Reset counter to allow for axis only on right-most plot
	plot.count = 0

	# Set margins to account for title, plot in 3x2 grid
	par(cex = 1.5, lwd = 1, oma=c(0, 0, 1, 0), mfrow = c(3, 2))

	# Subset by ethnicity
	data.ethnicity <- droplevels(subset(pv_data, Ethnicity == levels(pv_data$Ethnicity)[i]))

	for (j in 1:nlevels(data.ethnicity$Generation)) {

		# Subset by generation
		data.generation <- droplevels(subset(data.ethnicity, Generation == levels(data.ethnicity$Generation)[j]))

		for (k in 1:nlevels(data.generation$Gender)) {

			# Increment plot count
			plot.count = plot.count + 1

			# Hide y axis labels for left-hand plots
			if (plot.count %% 2 == 0 | (plot.count %% 2 == 1 & j == nlevels(data.ethnicity$Generation) & k == nlevels(data.generation$Gender))) {
				f1_lab <- "F1 (Nearey2)"
				par(yaxt = "s")
			} else {
				f1_lab <- ""
				par(yaxt = "n")
			}

			# Subset by gender
			data.gender <- droplevels(subset(data.generation, Gender == levels(data.generation$Gender)[k]))

			plotVowels(f1 = data.gender$Normalized.F1.50., f2 = data.gender$Normalized.F2.50., vowel = data.gender$Vowel, group = data.gender$Following.Class, var.col.by = data.gender$Vowel, col = c("green3", "firebrick2", "blueviolet"), var.style.by = data.gender$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.gender$Vowel, data.gender$Following.Class, sep = ""), cex.means = 1, cex.lab = 1, xlim = norm_f2_lim, ylim = norm_f1_lim, ellipse.line = T, pretty = T, main = paste(levels(pv_data$Ethnicity)[i], " gen ", levels(data.ethnicity$Generation)[j], " ", levels(data.generation$Gender)[k], "s", sep = ""), cex.main = 1.25, font.main = 1.5, xlab = "F2 (Nearey2)", ylab = f1_lab)

			# Plot ns
			text(0.375, 0.1, paste("n = ", nlevels(data.gender$Speaker), sep = ""))
		}
	}
}

dev.off()


#################################
# Breakdown (normalized, onset) #
#################################

cairo_pdf("Plots/Group/Breakdown (normalized, onset).pdf", width = 11, height = 8.5, onefile = T)

# Set font and line size
par(cex = 1.5, lwd = 2)

# Plot ethnicities by gender and generation (means)
for (i in 1:nlevels(pv_data$Ethnicity)) {

	# Subset data by current ethnicity
	data.ethnicity <- droplevels(subset(pv_data, Ethnicity == levels(pv_data$Ethnicity)[i]))

	# Plot genders
	for (j in 1:nlevels(data.ethnicity$Gender)) {

		# Subset by gender
		data.ethnicity.subset <- droplevels(subset(data.ethnicity, Gender == levels(data.ethnicity$Gender)[j]))

		plotVowels(f1 = data.ethnicity.subset$Normalized.F1.20., f2 = data.ethnicity.subset$Normalized.F2.20., vowel = data.ethnicity.subset$Vowel, group = data.ethnicity.subset$Following.Class, var.col.by = data.ethnicity.subset$Vowel, col = c("green3", "firebrick2", "blueviolet"), var.style.by = data.ethnicity.subset$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.ethnicity.subset$Vowel, data.ethnicity.subset$Following.Class, sep = ""), cex.means = 1, cex.lab = 1.5, xlim = norm_f2_lim, ylim = norm_f1_lim, ellipse.line = T, pretty = T, main = paste(levels(pv_data$Ethnicity)[i], levels(data.ethnicity$Gender)[j], "onset"), cex.main = 1.5, font.main = 2, xlab = "F2 (Nearey2)", ylab = "F1 (Nearey2)")
	}

	# Plot generations
	for (j in 1:nlevels(data.ethnicity$Generation)) {

		# Subset by generation
		data.ethnicity.subset <- droplevels(subset(data.ethnicity, Generation == levels(data.ethnicity$Generation)[j]))

		plotVowels(f1 = data.ethnicity.subset$Normalized.F1.20., f2 = data.ethnicity.subset$Normalized.F2.20., vowel = data.ethnicity.subset$Vowel, group = data.ethnicity.subset$Following.Class, var.col.by = data.ethnicity.subset$Vowel, col = c("green3", "firebrick2", "blueviolet"), var.style.by = data.ethnicity.subset$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.ethnicity.subset$Vowel, data.ethnicity.subset$Following.Class, sep = ""), cex.means = 1, cex.lab = 1.5, xlim = norm_f2_lim, ylim = norm_f1_lim, ellipse.line = T, pretty = T, main = paste(levels(pv_data$Ethnicity)[i], "generation", levels(data.ethnicity$Generation)[j], "onset"), cex.main = 1.5, font.main = 2, xlab = "F2 (Nearey2)", ylab = "F1 (Nearey2)")
	}
}

dev.off()


##################################
# Breakdown (normalized, offset) #
##################################

cairo_pdf("Plots/Group/Breakdown (normalized, offset).pdf", width = 11, height = 8.5, onefile = T)

# Set font and line size
par(cex = 1.5, lwd = 2)

# Plot ethnicities by gender and generation (means)
for (i in 1:nlevels(pv_data$Ethnicity)) {

	# Subset data by current ethnicity
	data.ethnicity <- droplevels(subset(pv_data, Ethnicity == levels(pv_data$Ethnicity)[i]))

	# Plot genders
	for (j in 1:nlevels(data.ethnicity$Gender)) {

		# Subset by gender
		data.ethnicity.subset <- droplevels(subset(data.ethnicity, Gender == levels(data.ethnicity$Gender)[j]))

		plotVowels(f1 = data.ethnicity.subset$Normalized.F1.80., f2 = data.ethnicity.subset$Normalized.F2.80., vowel = data.ethnicity.subset$Vowel, group = data.ethnicity.subset$Following.Class, var.col.by = data.ethnicity.subset$Vowel, col = c("green3", "firebrick2", "blueviolet"), var.style.by = data.ethnicity.subset$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.ethnicity.subset$Vowel, data.ethnicity.subset$Following.Class, sep = ""), cex.means = 1, cex.lab = 1.5, xlim = norm_f2_lim, ylim = norm_f1_lim, ellipse.line = T, pretty = T, main = paste(levels(pv_data$Ethnicity)[i], levels(data.ethnicity$Gender)[j], "offset"), cex.main = 1.5, font.main = 2, xlab = "F2 (Nearey2)", ylab = "F1 (Nearey2)")

		# Plot ns
		text(0.375, 0.1, paste("n = ", nlevels(data.ethnicity.subset$Speaker), sep = ""))
	}

	# Plot generations
	for (j in 1:nlevels(data.ethnicity$Generation)) {

		# Subset by generation
		data.ethnicity.subset <- droplevels(subset(data.ethnicity, Generation == levels(data.ethnicity$Generation)[j]))

		plotVowels(f1 = data.ethnicity.subset$Normalized.F1.80., f2 = data.ethnicity.subset$Normalized.F2.80., vowel = data.ethnicity.subset$Vowel, group = data.ethnicity.subset$Following.Class, var.col.by = data.ethnicity.subset$Vowel, col = c("green3", "firebrick2", "blueviolet"), var.style.by = data.ethnicity.subset$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.ethnicity.subset$Vowel, data.ethnicity.subset$Following.Class, sep = ""), cex.means = 1, cex.lab = 1.5, xlim = norm_f2_lim, ylim = norm_f1_lim, ellipse.line = T, pretty = T, main = paste(levels(pv_data$Ethnicity)[i], "generation", levels(data.ethnicity$Generation)[j], "offset"), cex.main = 1.5, font.main = 2, xlab = "F2 (Nearey2)", ylab = "F1 (Nearey2)")

		# Plot ns
		text(0.375, 0.1, paste("n = ", nlevels(data.ethnicity.subset$Speaker), sep = ""))
	}
}

dev.off()


############################
# Breakdown (presentation) #
############################

cairo_pdf("Plots/Group/Breakdown (presentation).pdf", width = 11, height = 7.33, onefile = T)

for (i in 1:nlevels(pv_data$Ethnicity)) {

	# Reset counter to allow for axis only on right-most plot
	plot.count = 0

	# Set margins to account for title, plot in 3x2 grid
	par(cex = 1.5, lwd = 1, oma=c(0, 0, 1, 0), mfrow = c(2, 3))

	# Subset by ethnicity
	data.ethnicity <- droplevels(subset(pv_data, Ethnicity == levels(pv_data$Ethnicity)[i]))

	for (j in 1:nlevels(data.ethnicity$Gender)) {

		# Subset by generation
		data.gender <- droplevels(subset(data.ethnicity, Gender == levels(data.ethnicity$Gender)[j]))

		for (k in 1:nlevels(data.gender$Generation)) {

			# Increment plot count
			plot.count = plot.count + 1

			# Subset by gender
			data.generation <- droplevels(subset(data.gender, Generation == levels(data.gender$Generation)[k]))

			plotVowels(f1 = data.generation$Normalized.F1.50., f2 = data.generation$Normalized.F2.50., vowel = data.generation$Vowel, group = data.generation$Following.Class, var.col.by = data.generation$Vowel, col = c("darkolivegreen", "firebrick2", "blueviolet"), var.style.by = data.generation$Following.Class, plot.tokens = F, plot.means = T, pch.means = paste(data.generation$Vowel, data.generation$Following.Class, sep = ""), cex.means = 1, cex.lab = 1, xlim = norm_f2_lim, ylim = norm_f1_lim, ellipse.line = T, pretty = T, main = paste(levels(pv_data$Ethnicity)[i], " gen ", levels(data.ethnicity$Generation)[k], " ", levels(data.ethnicity$Gender)[j], "s", sep = ""), cex.main = 1.25, font.main = 1.5, xlab = "F2 (Nearey2)", ylab = "F1 (Nearey2)")

			# Plot ns
			text(0.375, 0.1, paste("n = ", nlevels(data.generation$Speaker), sep = ""))
		}
	}
}

dev.off()


############
# Overview #
############

cairo_pdf("Plots/Group/Overview.pdf", width = 8.5, height = 11, onefile = T)

# Subset data to non-/g/ environments
data.overview <- droplevels(subset(data, Following.Class != "g"))

# Modify limits for plots
f1_lim = c(1000, 200)
f2_lim = c(3000, 700)

for (i in 1:nlevels(data.overview$Ethnicity)) {

	# Reset counter to allow for axis only on right-most plot
	plot.count = 0

	# Set margins to account for title, plot in 3x2 grid
	par(cex = 1.5, lwd = 1, oma=c(0, 0, 1, 0), mfrow = c(3, 2))

	# Subset by ethnicity
	data.overview.ethnicity <- droplevels(subset(data.overview, Ethnicity == levels(data.overview$Ethnicity)[i]))

	for (j in 1:nlevels(data.overview.ethnicity$Generation)) {

		# Subset by generation
		data.overview.generation <- droplevels(subset(data.overview.ethnicity, Generation == levels(data.overview.ethnicity$Generation)[j]))

		for (k in 1:nlevels(data.overview.generation$Gender)) {

			# Increment plot count
			plot.count = plot.count + 1

			# Hide y axis labels for left-hand plots
			if (plot.count %% 2 == 0 | (plot.count %% 2 == 1 & j == nlevels(data.overview.ethnicity$Generation) & k == nlevels(data.overview.generation$Gender))) {
				f1_lab <- "F1 (Hz)"
				par(yaxt = "s")
			} else {
				f1_lab <- ""
				par(yaxt = "n")
			}

			# Subset by gender
			data.overview.gender <- droplevels(subset(data.overview.generation, Gender == levels(data.overview.generation$Gender)[k]))

			plotVowels(f1 = data.overview.gender$F1.50., f2 = data.overview.gender$F2.50., vowel = data.overview.gender$Vowel, var.col.by = data.overview.gender$Vowel, plot.tokens = F, plot.means = T, pch.means = data.overview.gender$Vowel, cex.means = 1, cex.lab = 1, xlim = f2_lim, ylim = f1_lim, poly.line = T, poly.order = c("i", "\U026A", "e", "\U025B", "æ", "\U0251", "\U0254", "o", "\U028A", "u"), pretty = T, main = paste(levels(data.overview$Ethnicity)[i], " gen ", levels(data.overview.ethnicity$Generation)[j], " ", levels(data.overview.generation$Gender)[k], "s", sep = ""), cex.main = 1.25, font.main = 1.5, xlab = "F2 (Hz)", ylab = f1_lab)

			# Plot ns
			text(1000, 950, paste("n = ", nlevels(data.overview.gender$Speaker), sep = ""))
		}
	}
}

dev.off()


#####################
# Overview Combined #
#####################

cairo_pdf("Plots/Group/Overview (Combined).pdf", width = 12.75, height = 7.33, onefile = T)

# Reset counter to allow for axis only on right-most plot
plot.count = 0

# Set margins to account for title
par(cex = 1.5, lwd = 2, oma=c(0, 0, 1, 0), mfrow = c(2, 3))

for (i in 1:nlevels(data.overview$Ethnicity)) {
	# Increment plot count
	plot.count = plot.count + 1

	# Hide y axis labels for left-hand plots
	if (plot.count == 3 | plot.count == 5) {
		f1_lab <- "F1 (Nearey2)"
		par(yaxt = "s")
	} else {
		f1_lab <- ""
		par(yaxt = "n")
	}

	# Hide x axis labels for bottom plots
	if (plot.count <= 3) {
		f2_lab <- "F2 (Nearey2)"
		par(xaxt = "s")
	} else {
		f2_lab <- ""
		par(xaxt = "n")
	}

	# Subset by ethnicity
	data.overview.ethnicity <- droplevels(subset(data.overview, Ethnicity == levels(data.overview$Ethnicity)[i]))

	plotVowels(f1 = data.overview.ethnicity$Normalized.F1.50., f2 = data.overview.ethnicity$Normalized.F2.50., vowel = data.overview.ethnicity$Vowel, group = data.overview.ethnicity$Ethnicity, var.col.by = data.overview.ethnicity$Vowel, plot.tokens = F, plot.means = T, pch.means = data.overview.ethnicity$Vowel, cex.means = 2, cex.lab = 1.5, xlim = c(1.1, -0.2), ylim = c(-0.2, -1.4), poly.line = T, poly.order = c("i", "\U026A", "e", "\U025B", "æ", "\U0251", "\U0254", "o", "\U028A", "u"), pretty = T, main = levels(data.overview$Ethnicity)[i], cex.main = 1.5, font.main = 2, xlab = f2_lab, ylab = f1_lab)

}

dev.off()


##################
# Dialect Region #
##################

# Read in data as UTF-8
dialect_data <- read.delim("../US Dialects/Dialect-Data-R.txt", encoding = "UTF-8")

# Subset to males
dialect_data <- droplevels(subset(dialect_data, Gender == "M"))

# Subset data to white, male, and non-/g/ environments
nwm_data <- droplevels(subset(data, Ethnicity == "Caucasian" & Gender == "male" & Following.Class != "g"))

# Calculate means
nwm_data <- ddply(nwm_data, .(Vowel), summarize, F1 = mean(F1.50.), F2 = mean(F2.50.))

# Add cols and reorganize
nwm_data$Reference <- "PNWEP"
nwm_data$Region <- "Northwest"
nwm_data$Gender <- "M"
nwm_data <- cbind(nwm_data[1], nwm_data[5:6], nwm_data[2:4])

# Modify limits for plots
f1_lim = c(900, 200)
f2_lim = c(2500, 700)

cairo_pdf("Plots/Group/Dialect Region.pdf", width = 11, height = 8.5, onefile = T)

for (i in 1:nlevels(dialect_data$Region)) {
	# Set margins to account for title
	par(cex = 1.5, lwd = 1.5, oma=c(0, 0, 1, 0))

	# Subset by region
	dialect_data.region <- droplevels(subset(dialect_data, Region == levels(dialect_data$Region)[i]))

	# Append NWE data
	dialect_data.region <- rbind(dialect_data.region, nwm_data)

	plotVowels(f1 = dialect_data.region$F1, f2 = dialect_data.region$F2, vowel = dialect_data.region$Vowel, group = dialect_data.region$Region, var.col.by = dialect_data.region$Region, col = c("firebrick2", "blueviolet"), plot.tokens = F, plot.means = T, pch.means = dialect_data.region$Vowel, cex.means = 1.25, cex.lab = 2, xlim = f2_lim, ylim = f1_lim, poly.line = T, poly.order = c("i", "\U026A", "e", "\U025B", "æ", "\U0251", "\U0254", "o", "\U028A", "u"), pretty = T, main = levels(dialect_data$Region)[i], cex.main = 2.5, font.main = 1.5, xlab = "F2 (Hz)", ylab = "F1 (Hz)", legend.kwd = "bottomright", legend.args = c(lwd = 2))

}

dev.off()


###################
# Northwest Males #
###################

f1_lim = c(700, 200)
f2_lim = c(2500, 900)

cairo_pdf("Plots/Group/NW males.pdf", width = 11, height = 8.5, onefile = T)

# Set margins to account for title
par(cex = 1.5, lwd = 2, oma=c(0, 0, 1, 0))

plotVowels(f1 = nwm_data$F1, f2 = nwm_data$F2, vowel = nwm_data$Vowel, var.col.by = nwm_data$Vowel, plot.tokens = F, plot.means = T, pch.means = nwm_data$Vowel, cex.means = 1.5, cex.lab = 2, xlim = f2_lim, ylim = f1_lim, poly.line = T, poly.order = c("i", "\U026A", "e", "\U025B", "æ", "\U0251", "\U0254", "o", "\U028A", "u"), pretty = T, main = "", cex.main = 2.5, font.main = 1.5, xlab = "F2 (Hz)", ylab = "F1 (Hz)")

dev.off()


# Clear workspace and finish
rm(list = ls (all = T))
