# Imports for reshaping data, SS-ANOVA functions, and ggplot
library(ggplot2)
library(grid)
library(gss)
library(reshape2)


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist = NULL, cols = 1, layout = NULL, title = "default") {
	# Make a list from the ... arguments and plotlist
	plots <- c(list(...), plotlist)

	numPlots = length(plots)

	# If layout is NULL, then use 'cols' to determine layout
	if (is.null(layout)) {
		# Make the panel
		# ncol: Number of columns of plots
		# nrow: Number of rows needed, calculated from # of cols
		layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
						 ncol = cols, nrow = ceiling(numPlots / cols))
	}

	if (numPlots==1) {
		print(plots[[1]])

	} else {
		# Set up the page
		grid.newpage()

		# Add extra row for title, set heights so that title row is smaller
		pushViewport(viewport(layout = grid.layout(nrow(layout) + 1, ncol(layout), heights = unit(c(0.5, rep(5, nrow(layout))), "null"))))

		# Make each plot, in the correct location
		for (i in 1:numPlots) {
			# Get the i,j matrix positions of the regions that contain this subplot
			matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

			print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row + 1,
											layout.pos.col = matchidx$col))
		}

		# Add title (defaults to "default")
		grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)), gp = gpar(fontsize = 22))
	}
}


# Extract a legend from a plot
g_legend <- function(a.gplot) {
	tmp <- ggplot_gtable(ggplot_build(a.gplot))
	leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
	legend <- tmp$grobs[[leg]]
	return(legend)
}


setwd("~/School/UW/Dissertation/Graphics")

# Read in data
data <- read.delim(file = "../Data/Vowel-Data-R.txt", header = T, encoding = "UTF-8")

# Reorder vowel factor
data$Vowel <- factor(data$Vowel, levels = c("i", "\U026A", "e", "\U025B", "æ", "\U0251", "\U0254", "o", "\U028A", "u"))

# Lowercase gender
data$Gender <- tolower(data$Gender)
data$Gender <- as.factor(data$Gender)

# Set generation to factor
data$Generation <- as.factor(data$Generation)

# Select wordlist, unaspirated, non-outlier tokens from PVR front vowels
data <- droplevels(subset(data, Task == "wordlist" & Aspirated == "n"))

# Collapse # and C environments, set following class to factor
data$Following.Class <- gsub("#", "C", data$Following.Class)
data$Following.Class <- gsub("G", "g", data$Following.Class)
data$Following.Class <- as.factor(data$Following.Class)

# Create token number
data$Token <- 1:nrow(data)

# Change column names so interval will have correct factors after reshape
colnames(data)[c(29, 30, 31)] = c("20", "50", "80")

# Reshape F1 & F2 values, merge, and fix column name
data.f1 <- melt(data, id.vars = c("Speaker", "Region", "Gender", "Ethnicity", "Generation", "Vowel", "Following.Class", "Token"), measure.vars = c("20", "50", "80"), variable.name = "Interval", value.name = "F1")
data.f2 <- melt(data, id.vars = c("Speaker", "Region", "Gender", "Ethnicity", "Generation", "Vowel", "Following.Class", "Token"), measure.vars = c("Normalized.F2.20.", "Normalized.F2.50.", "Normalized.F2.80."), variable.name = "Interval", value.name = "F2")
data <- cbind(data.f1, data.f2$F2)
colnames(data)[11] <- "F2"

# Set interval to numeric
data$Interval <- as.numeric(levels(data$Interval))[data$Interval]

# Set line colors
colors <- c("green3", "firebrick2", "blueviolet")
alt_colors <- c("darkolivegreen", "firebrick2", "blueviolet")

# Select only vowels involved in pre-velar raising/lowering
data.pv <- droplevels(subset(data, (Vowel == "æ" | Vowel == "\U025B" | Vowel == "e")))

# Select only vowels involved in pre-velar raising/lowering
data.npv <- droplevels(subset(data, Following.Class != "g"))


#######################
# Overall (pre-velar) #
#######################

# Generate SS-ANOVA model for F2
f2.model <- ssanova(F2 ~ Interval * Vowel * Following.Class, data = data.pv)

# Make sure F1 SE doesn't get crazy. Recreate model if it does
repeat {
	# Generate SS-ANOVA model for F1
	f1.model <- ssanova(F1 ~ Interval * Vowel * Following.Class, data = data.pv)

	# Create dummy data
	grid <- expand.grid(Interval = seq(20, 80, length = 100), Vowel = levels(data.pv$Vowel), Following.Class = levels(data.pv$Following.Class))

	# Predict F1/F2, standard error
	grid$F1.Fit <- predict(f1.model, newdata = grid, se = T)$fit
	grid$F1.SE <- predict(f1.model, newdata = grid, se = T)$se.fit
	grid$F2.Fit <- predict(f2.model, newdata = grid, se = T)$fit
	grid$F2.SE <- predict(f2.model, newdata = grid, se = T)$se.fit

	if (max(grid$F1.SE) < 0.1) {
		break
	}
}

# Set output file
cairo_pdf("Plots/Group/SS-ANOVA (all).pdf", width = 10, height = 10)

# Set font size
theme_set(theme_gray(base_size = 20))

# Generate plot
formant.comparison <- ggplot(grid, aes(x = Interval, color = Vowel, fill = Vowel, linetype = Following.Class))

# Layer trajectories
formant.comparison <- formant.comparison + geom_line(aes(y = F1.Fit), lwd = 2) + geom_line(aes(y = F2.Fit), lwd = 2)

# Plot error bars
formant.comparison <- formant.comparison + geom_ribbon(aes(ymin = F1.Fit - (1.96 * F1.SE), ymax = F1.Fit + (1.96 * F1.SE), fill = Vowel), alpha = 0.2, color = NA, show_guide = F) + geom_ribbon(aes(ymin = F2.Fit - (1.96 * F2.SE), ymax = F2.Fit + (1.96 * F2.SE), fill = Vowel), alpha = 0.2, color = NA, show_guide = F)

# Scale axes
formant.comparison <- formant.comparison + scale_x_continuous(breaks = c(20, 50, 80), labels = c("20%", "50%", "80%"))

# Label plot and axes
formant.comparison <- formant.comparison + xlab("Vowel Duration") + ylab("F1                         Nearey2                         F2") + ggtitle("All Speakers")

# Set colors
formant.comparison <- formant.comparison + scale_colour_manual(labels = levels(grid$Vowel), values = colors) + scale_fill_manual(labels = levels(grid$Vowel), values = colors) + scale_linetype_manual(name = "Envir.", values = c("solid", "dashed"))

# Tweak legend
formant.comparison <- formant.comparison + theme(legend.justification = c(1, 0.5), legend.position = c(1, 0.5), legend.background = element_rect(fill = "transparent"), legend.key = element_blank()) + guides(colour = guide_legend(override.aes = list(size = 3)), linetype = guide_legend(override.aes = list(size = 1))) + theme(legend.title = element_text(size = 18), legend.text = element_text(size = 18), legend.key.width = unit(2, "line"))

print(formant.comparison)

# Close device
dev.off()


#################
# Overall (NPV) #
#################

# Generate SS-ANOVA model for F2
f2.model <- ssanova(F2 ~ Vowel * Interval, data = data.npv)

# Make sure F1 SE doesn't get crazy. Recreate model if it does
repeat {
	# Generate SS-ANOVA model for F1
	f1.model <- ssanova(F1 ~ Vowel * Interval, data = data.npv)

	# Create dummy data
	grid <- expand.grid(Interval = seq(20, 80, length = 100), Vowel = levels(data.npv$Vowel))

	# Predict F1/F2, standard error
	grid$F1.Fit <- predict(f1.model, newdata = grid, se = T)$fit
	grid$F1.SE <- predict(f1.model, newdata = grid, se = T)$se.fit
	grid$F2.Fit <- predict(f2.model, newdata = grid, se = T)$fit
	grid$F2.SE <- predict(f2.model, newdata = grid, se = T)$se.fit

	if (max(grid$F1.SE) < 0.1) {
		break
	}
}

# Set output file
cairo_pdf("Plots/Group/SS-ANOVA (all, NPV).pdf", width = 10, height = 10)

# Set font size
theme_set(theme_gray(base_size = 20))

# Generate plot
formant.comparison <- ggplot(grid, aes(x = Interval, color = Vowel))

# Layer trajectories
formant.comparison <- formant.comparison + geom_line(aes(y = F1.Fit), lwd = 2) + geom_line(aes(y = F2.Fit), lwd = 2)

# Plot error bars
formant.comparison <- formant.comparison + geom_ribbon(aes(ymin = F1.Fit - (1.96 * F1.SE), ymax = F1.Fit + (1.96 * F1.SE), fill = Vowel), alpha = 0.2, color = NA, show_guide = F) + geom_ribbon(aes(ymin = F2.Fit - (1.96 * F2.SE), ymax = F2.Fit + (1.96 * F2.SE), fill = Vowel), alpha = 0.2, color = NA, show_guide = F)

# Scale axes
formant.comparison <- formant.comparison + scale_x_continuous(breaks = c(20, 50, 80), labels = c("20%", "50%", "80%"))

# Label plot and axes
formant.comparison <- formant.comparison + xlab("Vowel Duration") + ylab("F1                         Nearey2                         F2") + ggtitle("All Speakers")

# Tweak legend
formant.comparison <- formant.comparison + theme(legend.justification = c(1, 0.5), legend.position = c(1, 0.5), legend.background = element_rect(fill = "transparent"), legend.key = element_blank()) + guides(colour = guide_legend(override.aes = list(size = 3)), linetype = guide_legend(override.aes = list(size = 1))) + theme(legend.title = element_text(size = 18), legend.text = element_text(size = 18), legend.key.width = unit(2, "line"))

print(formant.comparison)

# Close device
dev.off()


#############
# Breakdown #
#############

# Set output file
cairo_pdf("Plots/Group/SS-ANOVA (breakdown).pdf", width = 10, height = 15, onefile = T)

# Set font size
theme_set(theme_gray(base_size = 12))

for (i in 1:nlevels(data.pv$Ethnicity)) {

	# Reset plot list and counter
	plots <- vector("list")
	plot.count = 0

	# Subset by ethnicity
	data.ethnicity <- droplevels(subset(data.pv, Ethnicity == levels(data.pv$Ethnicity)[i]))

	for (j in 1:nlevels(data.ethnicity$Generation)) {

		# Subset by generation
		data.generation <- droplevels(subset(data.ethnicity, Generation == levels(data.ethnicity$Generation)[j]))

		for (k in 1:nlevels(data.generation$Gender)) {

			# Increment plot count
			plot.count = plot.count + 1

			# Subset by gender
			data.gender <- droplevels(subset(data.generation, Gender == levels(data.generation$Gender)[k]))

			# Generate SS-ANOVA model for F2
			f2.model <- ssanova(F2 ~ Interval * Vowel * Following.Class, data = data.gender)

			# Make sure F1 SE doesn't get crazy. Recreate model if it does
			repeat {
				# Generate SS-ANOVA model for F1
				f1.model <- ssanova(F1 ~ Interval * Vowel * Following.Class, data = data.gender)

				# Create dummy data
				grid <- expand.grid(Interval = seq(20, 80, length = 100), Vowel = levels(data.pv$Vowel), Following.Class = levels(data.pv$Following.Class))

				# Predict F1/F2, standard error
				grid$F1.Fit <- predict(f1.model, newdata = grid, se = T)$fit
				grid$F1.SE <- predict(f1.model, newdata = grid, se = T)$se.fit
				grid$F2.Fit <- predict(f2.model, newdata = grid, se = T)$fit
				grid$F2.SE <- predict(f2.model, newdata = grid, se = T)$se.fit

				if (max(grid$F1.SE) < 0.1) {
					break
				}
			}

			# Generate plot
			formant.comparison <- ggplot(grid, aes(x = Interval, color = Vowel, fill = Vowel, linetype = Following.Class))

			# Layer trajectories
			formant.comparison <- formant.comparison + geom_line(aes(y = F1.Fit), lwd = 1) + geom_line(aes(y = F2.Fit), lwd = 1)

			# Plot error bars
			formant.comparison <- formant.comparison + geom_ribbon(aes(ymin = F1.Fit - (1.96 * F1.SE), ymax = F1.Fit + (1.96 * F1.SE), fill = Vowel), alpha = 0.2, color = NA, show_guide = F) + geom_ribbon(aes(ymin = F2.Fit - (1.96 * F2.SE), ymax = F2.Fit + (1.96 * F2.SE), fill = Vowel), alpha = 0.2, color = NA, show_guide = F)

			# Scale axes
			formant.comparison <- formant.comparison + scale_x_continuous(breaks = c(20, 50, 80), labels = c("20%", "50%", "80%"))

			# Label plot and axes
			formant.comparison <- formant.comparison + xlab("Vowel Duration") + ylab("F1                         Nearey2                         F2") + ggtitle(paste(levels(data.pv$Ethnicity)[i], " gen ", levels(data.ethnicity$Generation)[j], " ", levels(data.generation$Gender)[k], "s", sep = ""))

			# Set colors
			formant.comparison <- formant.comparison + scale_colour_manual(labels = levels(grid$Vowel), values = colors) + scale_fill_manual(labels = levels(grid$Vowel), values = colors) + scale_linetype_manual(name = "Envir.", values = c("solid", "dashed"))

			# Tweak legend
			formant.comparison <- formant.comparison + theme(legend.justification = c(1, 0.5), legend.position = c(1, 0.5), legend.background = element_rect(fill = "transparent"), legend.key = element_blank()) + guides(colour = guide_legend(override.aes = list(size = 1)), linetype = guide_legend(override.aes = list(size = 1))) + theme(legend.title = element_text(size = 10), legend.text = element_text(size = 10), legend.key.height = unit(0.75, "line"), legend.key.width = unit(1, "line"))

			plots[[plot.count]] <- formant.comparison
		}
	}

	# Print page
	multiplot(plotlist = plots, layout = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = T), title = levels(data.pv$Ethnicity)[i])

}

dev.off()


##########################
# Breakdown presentation #
##########################

# Set output file
cairo_pdf("Plots/Group/SS-ANOVA (breakdown, presentation).pdf", width = 15, height = 10, onefile = T)

# Set font size
theme_set(theme_gray(base_size = 12))

for (i in 1:nlevels(data.pv$Ethnicity)) {

	# Reset plot list and counter
	plots <- vector("list")
	plot.count = 0

	# Subset by ethnicity
	data.ethnicity <- droplevels(subset(data.pv, Ethnicity == levels(data.pv$Ethnicity)[i]))

	for (j in 1:nlevels(data.ethnicity$Gender)) {

		# Subset by generation
		data.gender <- droplevels(subset(data.ethnicity, Gender == levels(data.ethnicity$Gender)[j]))

		for (k in 1:nlevels(data.gender$Generation)) {

			# Increment plot count
			plot.count = plot.count + 1

			# Subset by gender
			data.generation <- droplevels(subset(data.gender, Generation == levels(data.gender$Generation)[k]))

			# Generate SS-ANOVA model for F2
			f2.model <- ssanova(F2 ~ Interval * Vowel * Following.Class, data = data.generation)

			# Make sure F1 SE doesn't get crazy. Recreate model if it does
			repeat {
				# Generate SS-ANOVA model for F1
				f1.model <- ssanova(F1 ~ Interval * Vowel * Following.Class, data = data.generation)

				# Create dummy data
				grid <- expand.grid(Interval = seq(20, 80, length = 100), Vowel = levels(data.pv$Vowel), Following.Class = levels(data.pv$Following.Class))

				# Predict F1/F2, standard error
				grid$F1.Fit <- predict(f1.model, newdata = grid, se = T)$fit
				grid$F1.SE <- predict(f1.model, newdata = grid, se = T)$se.fit
				grid$F2.Fit <- predict(f2.model, newdata = grid, se = T)$fit
				grid$F2.SE <- predict(f2.model, newdata = grid, se = T)$se.fit

				if (max(grid$F1.SE) < 0.1) {
					break
				}
			}

			# Generate plot
			formant.comparison <- ggplot(grid, aes(x = Interval, color = Vowel, fill = Vowel, linetype = Following.Class))

			# Layer trajectories
			formant.comparison <- formant.comparison + geom_line(aes(y = F1.Fit), lwd = 1) + geom_line(aes(y = F2.Fit), lwd = 1)

			# Scale axes
			formant.comparison <- formant.comparison + scale_x_continuous(breaks = c(20, 50, 80), labels = c("20%", "50%", "80%"))

			# Label plot and axes
			formant.comparison <- formant.comparison + xlab("Vowel Duration") + ylab("F1                         Nearey2                         F2") + ggtitle(paste("Generation ", levels(data.ethnicity$Generation)[k], " ", levels(data.ethnicity$Gender)[j], "s", sep = ""))

			# Set colors
			formant.comparison <- formant.comparison + scale_colour_manual(labels = levels(grid$Vowel), values = alt_colors) + scale_fill_manual(labels = levels(grid$Vowel), values = alt_colors) + scale_linetype_manual(name = "Envir.", values = c("solid", "dashed"))

			formant.comparison <- formant.comparison + theme(legend.position = "none")

			plots[[plot.count]] <- formant.comparison
		}
	}

	# Print page
	multiplot(plotlist = plots, layout = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = T), title = levels(data.pv$Ethnicity)[i])

}

dev.off()


##########
# Gender #
##########

cairo_pdf("Plots/Group/SS-ANOVA (gender).pdf", width = 10, height = 15, onefile = T)

# Reset plot list and counter
plots <- vector("list")
plot.count = 0

# Plot genders (means)
for (i in 1:nlevels(data.pv$Gender)) {

	# Increment plot count
	plot.count = plot.count + 1

	# Subset by gender
	data.gender <- droplevels(subset(data.pv, Gender == levels(data.pv$Gender)[i]))

	# Generate SS-ANOVA model for F2
	f2.model <- ssanova(F2 ~ Interval * Vowel * Following.Class, data = data.gender)

	# Make sure F1 SE doesn't get crazy. Recreate model if it does
	repeat {
		# Generate SS-ANOVA model for F1
		f1.model <- ssanova(F1 ~ Interval * Vowel * Following.Class, data = data.gender)

		# Create dummy data
		grid <- expand.grid(Interval = seq(20, 80, length = 100), Vowel = levels(data.pv$Vowel), Following.Class = levels(data.pv$Following.Class))

		# Predict F1/F2, standard error
		grid$F1.Fit <- predict(f1.model, newdata = grid, se = T)$fit
		grid$F1.SE <- predict(f1.model, newdata = grid, se = T)$se.fit
		grid$F2.Fit <- predict(f2.model, newdata = grid, se = T)$fit
		grid$F2.SE <- predict(f2.model, newdata = grid, se = T)$se.fit

		if (max(grid$F1.SE) < 0.1) {
			break
		}
	}

	# Generate plot
	formant.comparison <- ggplot(grid, aes(x = Interval, color = Vowel, fill = Vowel, linetype = Following.Class))

	# Layer trajectories
	formant.comparison <- formant.comparison + geom_line(aes(y = F1.Fit), lwd = 1) + geom_line(aes(y = F2.Fit), lwd = 1)

	# Plot error bars
	formant.comparison <- formant.comparison + geom_ribbon(aes(ymin = F1.Fit - (1.96 * F1.SE), ymax = F1.Fit + (1.96 * F1.SE), fill = Vowel), alpha = 0.2, color = NA, show_guide = F) + geom_ribbon(aes(ymin = F2.Fit - (1.96 * F2.SE), ymax = F2.Fit + (1.96 * F2.SE), fill = Vowel), alpha = 0.2, color = NA, show_guide = F)

	# Scale axes
	formant.comparison <- formant.comparison + scale_x_continuous(breaks = c(20, 50, 80), labels = c("20%", "50%", "80%"))

	# Label plot and axes
	formant.comparison <- formant.comparison + xlab("Vowel Duration") + ylab("F1                         Nearey2                         F2") + ggtitle(paste(levels(data.pv$Gender)[i], "s", sep = ""))

	# Set colors
	formant.comparison <- formant.comparison + scale_colour_manual(labels = levels(grid$Vowel), values = colors) + scale_fill_manual(labels = levels(grid$Vowel), values = colors) + scale_linetype_manual(name = "Envir.", values = c("solid", "dashed"))

	# Tweak legend
	formant.comparison <- formant.comparison + theme(legend.justification = c(1, 0.5), legend.position = c(1, 0.5), legend.background = element_rect(fill = "transparent"), legend.key = element_blank()) + guides(colour = guide_legend(override.aes = list(size = 1)), linetype = guide_legend(override.aes = list(size = 1))) + theme(legend.title = element_text(size = 10), legend.text = element_text(size = 10), legend.key.height = unit(0.75, "line"), legend.key.width = unit(1, "line"))

	plots[[plot.count]] <- formant.comparison
}

# Print page
multiplot(plotlist = plots, layout = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = T), title = "Gender")

dev.off()


#######################
# Gender Presentation #
#######################

cairo_pdf("Plots/Group/SS-ANOVA (gender, presentation).pdf", width = 10, height = 5, onefile = T)

# Reset plot list and counter
plots <- vector("list")
plot.count = 0

# Plot genders (means)
for (i in 1:nlevels(data.pv$Gender)) {

	# Increment plot count
	plot.count = plot.count + 1

	# Subset by gender
	data.gender <- droplevels(subset(data.pv, Gender == levels(data.pv$Gender)[i]))

	# Generate SS-ANOVA model for F2
	f2.model <- ssanova(F2 ~ Interval * Vowel * Following.Class, data = data.gender)

	# Make sure F1 SE doesn't get crazy. Recreate model if it does
	repeat {
		# Generate SS-ANOVA model for F1
		f1.model <- ssanova(F1 ~ Interval * Vowel * Following.Class, data = data.gender)

		# Create dummy data
		grid <- expand.grid(Interval = seq(20, 80, length = 100), Vowel = levels(data.pv$Vowel), Following.Class = levels(data.pv$Following.Class))

		# Predict F1/F2, standard error
		grid$F1.Fit <- predict(f1.model, newdata = grid, se = T)$fit
		grid$F1.SE <- predict(f1.model, newdata = grid, se = T)$se.fit
		grid$F2.Fit <- predict(f2.model, newdata = grid, se = T)$fit
		grid$F2.SE <- predict(f2.model, newdata = grid, se = T)$se.fit

		if (max(grid$F1.SE) < 0.1) {
			break
		}
	}

	# Generate plot
	formant.comparison <- ggplot(grid, aes(x = Interval, color = Vowel, fill = Vowel, linetype = Following.Class))

	# Layer trajectories
	formant.comparison <- formant.comparison + geom_line(aes(y = F1.Fit), lwd = 1) + geom_line(aes(y = F2.Fit), lwd = 1)

	# Scale axes
	formant.comparison <- formant.comparison + scale_x_continuous(breaks = c(20, 50, 80), labels = c("20%", "50%", "80%"))

	# Label plot and axes
	formant.comparison <- formant.comparison + xlab("Vowel Duration") + ylab("F1                         Nearey2                         F2") + ggtitle(paste(levels(data.pv$Gender)[i], "s", sep = ""))

	# Set colors
	formant.comparison <- formant.comparison + scale_colour_manual(labels = levels(grid$Vowel), values = alt_colors) + scale_fill_manual(labels = levels(grid$Vowel), values = alt_colors) + scale_linetype_manual(name = "Envir.", values = c("solid", "dashed"))

	# Tweak legend
	#formant.comparison <- formant.comparison + theme(legend.justification = c(1, 0.5), legend.position = c(1, 0.5), legend.background = element_rect(fill = "transparent"), legend.key = element_blank()) + guides(colour = guide_legend(override.aes = list(size = 1)), linetype = guide_legend(override.aes = list(size = 1))) + theme(legend.title = element_text(size = 10), legend.text = element_text(size = 10), legend.key.height = unit(0.75, "line"), legend.key.width = unit(1, "line"))
	formant.comparison <- formant.comparison + theme(legend.position = "none")

	plots[[plot.count]] <- formant.comparison
}

# Print page
multiplot(plotlist = plots, layout = matrix(c(1, 2), nrow = 1, byrow = T), title = "Gender")

dev.off()


##############
# Generation #
##############

cairo_pdf("Plots/Group/SS-ANOVA (generation).pdf", width = 10, height = 15, onefile = T)

# Reset plot list and counter
plots <- vector("list")
plot.count = 0

# Plot generations (means)
for (i in 1:nlevels(data.pv$Generation)) {

	# Subset data by current ethnicity
	data.generation <- droplevels(subset(data.pv, Generation == levels(data.pv$Generation)[i]))

	for (j in 1:nlevels(data.generation$Gender)) {

		# Increment plot count
		plot.count = plot.count + 1

		# Subset by gender
		data.gender <- droplevels(subset(data.generation, Gender == levels(data.generation$Gender)[j]))

		# Generate SS-ANOVA model for F2
		f2.model <- ssanova(F2 ~ Interval * Vowel * Following.Class, data = data.gender)

		# Make sure F1 SE doesn't get crazy. Recreate model if it does
		repeat {
			# Generate SS-ANOVA model for F1
			f1.model <- ssanova(F1 ~ Interval * Vowel * Following.Class, data = data.gender)

			# Create dummy data
			grid <- expand.grid(Interval = seq(20, 80, length = 100), Vowel = levels(data.pv$Vowel), Following.Class = levels(data.pv$Following.Class))

			# Predict F1/F2, standard error
			grid$F1.Fit <- predict(f1.model, newdata = grid, se = T)$fit
			grid$F1.SE <- predict(f1.model, newdata = grid, se = T)$se.fit
			grid$F2.Fit <- predict(f2.model, newdata = grid, se = T)$fit
			grid$F2.SE <- predict(f2.model, newdata = grid, se = T)$se.fit

			if (max(grid$F1.SE) < 0.1) {
				break
			}
		}

		# Generate plot
		formant.comparison <- ggplot(grid, aes(x = Interval, color = Vowel, fill = Vowel, linetype = Following.Class))

		# Layer trajectories
		formant.comparison <- formant.comparison + geom_line(aes(y = F1.Fit), lwd = 1) + geom_line(aes(y = F2.Fit), lwd = 1)

		# Plot error bars
		formant.comparison <- formant.comparison + geom_ribbon(aes(ymin = F1.Fit - (1.96 * F1.SE), ymax = F1.Fit + (1.96 * F1.SE), fill = Vowel), alpha = 0.2, color = NA, show_guide = F) + geom_ribbon(aes(ymin = F2.Fit - (1.96 * F2.SE), ymax = F2.Fit + (1.96 * F2.SE), fill = Vowel), alpha = 0.2, color = NA, show_guide = F)

		# Scale axes
		formant.comparison <- formant.comparison + scale_x_continuous(breaks = c(20, 50, 80), labels = c("20%", "50%", "80%"))

		# Label plot and axes
		formant.comparison <- formant.comparison + xlab("Vowel Duration") + ylab("F1                         Nearey2                         F2") + ggtitle(paste("Generation ", levels(data.pv$Generation)[i], " ", levels(data.generation$Gender)[j], "s", sep = ""))

		# Set colors
		formant.comparison <- formant.comparison + scale_colour_manual(labels = levels(grid$Vowel), values = colors) + scale_fill_manual(labels = levels(grid$Vowel), values = colors) + scale_linetype_manual(name = "Envir.", values = c("solid", "dashed"))

		# Tweak legend
		formant.comparison <- formant.comparison + theme(legend.justification = c(1, 0.5), legend.position = c(1, 0.5), legend.background = element_rect(fill = "transparent"), legend.key = element_blank()) + guides(colour = guide_legend(override.aes = list(size = 1)), linetype = guide_legend(override.aes = list(size = 1))) + theme(legend.title = element_text(size = 10), legend.text = element_text(size = 10), legend.key.height = unit(0.75, "line"), legend.key.width = unit(1, "line"))

		plots[[plot.count]] <- formant.comparison
	}
}

# Print page
multiplot(plotlist = plots, layout = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = T), title = "Generation")

dev.off()


######################
# Ethnicity Combined #
######################

# Set output file
cairo_pdf("Plots/Group/SS-ANOVA (ethnicity combined).pdf", width = 15, height = 10, onefile = T)

# Set font size
theme_set(theme_gray(base_size = 12))

# Reset plot list and counter
plots <- vector("list")
plot.count = 0

for (i in 1:nlevels(data.pv$Ethnicity)) {
	# Increment plot count
	plot.count = plot.count + 1

	# Subset by ethnicity
	data.ethnicity <- droplevels(subset(data.pv, Ethnicity == levels(data.pv$Ethnicity)[i]))

	# Generate SS-ANOVA model for F2
	f2.model <- ssanova(F2 ~ Interval * Vowel * Following.Class, data = data.ethnicity)

	# Make sure F1 SE doesn't get crazy. Recreate model if it does
	repeat {
		# Generate SS-ANOVA model for F1
		f1.model <- ssanova(F1 ~ Interval * Vowel * Following.Class, data = data.ethnicity)

		# Create dummy data
		grid <- expand.grid(Interval = seq(20, 80, length = 100), Vowel = levels(data.pv$Vowel), Following.Class = levels(data.pv$Following.Class))

		# Predict F1/F2, standard error
		grid$F1.Fit <- predict(f1.model, newdata = grid, se = T)$fit
		grid$F1.SE <- predict(f1.model, newdata = grid, se = T)$se.fit
		grid$F2.Fit <- predict(f2.model, newdata = grid, se = T)$fit
		grid$F2.SE <- predict(f2.model, newdata = grid, se = T)$se.fit

		if (max(grid$F1.SE) < 0.1) {
			break
		}
	}

	# Generate plot
	formant.comparison <- ggplot(grid, aes(x = Interval, color = Vowel, fill = Vowel, linetype = Following.Class))

	# Layer trajectories
	formant.comparison <- formant.comparison + geom_line(aes(y = F1.Fit), lwd = 1) + geom_line(aes(y = F2.Fit), lwd = 1)

	# Scale axes
	formant.comparison <- formant.comparison + scale_x_continuous(breaks = c(20, 50, 80), labels = c("20%", "50%", "80%"))

	# Label plot and axes
	formant.comparison <- formant.comparison + xlab("Vowel Duration") + ylab("F1                         Nearey2                         F2") + ggtitle(levels(data.pv$Ethnicity)[i])

	# Set colors
	formant.comparison <- formant.comparison + scale_colour_manual(labels = levels(grid$Vowel), values = alt_colors) + scale_fill_manual(labels = levels(grid$Vowel), values = alt_colors) + scale_linetype_manual(name = "Envir.", values = c("solid", "dashed"))

	# Tweak legend
	#formant.comparison <- formant.comparison + theme(legend.justification = c(1, 0.5), legend.position = c(1, 0.5), legend.background = element_rect(fill = "transparent"), legend.key = element_blank()) + guides(colour = guide_legend(override.aes = list(size = 1)), linetype = guide_legend(override.aes = list(size = 1))) + theme(legend.title = element_text(size = 10), legend.text = element_text(size = 10), legend.key.height = unit(0.75, "line"), legend.key.width = unit(1, "line"))
	formant.comparison <- formant.comparison + theme(legend.position = "none")

	# Print page
	plots[[plot.count]] <- formant.comparison

}

# Print page
multiplot(plotlist = plots, layout = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = T), title = "Ethnicity")

dev.off()


#######################
# Generation Combined #
#######################

cairo_pdf("Plots/Group/SS-ANOVA (generation combined).pdf", width = 10, height = 10, onefile = T)

# Reset plot list and counter
plots <- vector("list")
plot.count = 0

# Plot generations (means)
for (i in 1:nlevels(data.pv$Generation)) {

	# Subset data by current ethnicity
	data.generation <- droplevels(subset(data.pv, Generation == levels(data.pv$Generation)[i]))

	# Increment plot count
	plot.count = plot.count + 1

	# Generate SS-ANOVA model for F2
	f2.model <- ssanova(F2 ~ Interval * Vowel * Following.Class, data = data.generation)

	# Make sure F1 SE doesn't get crazy. Recreate model if it does
	repeat {
		# Generate SS-ANOVA model for F1
		f1.model <- ssanova(F1 ~ Interval * Vowel * Following.Class, data = data.generation)

		# Create dummy data
		grid <- expand.grid(Interval = seq(20, 80, length = 100), Vowel = levels(data.pv$Vowel), Following.Class = levels(data.pv$Following.Class))

		# Predict F1/F2, standard error
		grid$F1.Fit <- predict(f1.model, newdata = grid, se = T)$fit
		grid$F1.SE <- predict(f1.model, newdata = grid, se = T)$se.fit
		grid$F2.Fit <- predict(f2.model, newdata = grid, se = T)$fit
		grid$F2.SE <- predict(f2.model, newdata = grid, se = T)$se.fit

		if (max(grid$F1.SE) < 0.1) {
			break
		}
	}

	# Generate plot
	formant.comparison <- ggplot(grid, aes(x = Interval, color = Vowel, fill = Vowel, linetype = Following.Class))

	# Layer trajectories
	formant.comparison <- formant.comparison + geom_line(aes(y = F1.Fit), lwd = 1) + geom_line(aes(y = F2.Fit), lwd = 1)

	# Scale axes
	formant.comparison <- formant.comparison + scale_x_continuous(breaks = c(20, 50, 80), labels = c("20%", "50%", "80%"))

	# Label plot and axes
	formant.comparison <- formant.comparison + xlab("Vowel Duration") + ylab("F1                         Nearey2                         F2") + ggtitle(paste("Generation ", levels(data.pv$Generation)[i]))

	# Set colors
	formant.comparison <- formant.comparison + scale_colour_manual(labels = levels(grid$Vowel), values = alt_colors) + scale_fill_manual(labels = levels(grid$Vowel), values = alt_colors) + scale_linetype_manual(name = "Envir.", values = c("solid", "dashed"))

	# Tweak legend
	#formant.comparison <- formant.comparison + theme(legend.justification = c(1, 0.5), legend.position = c(1, 0.5), legend.background = element_rect(fill = "transparent"), legend.key = element_blank()) + guides(colour = guide_legend(override.aes = list(size = 1)), linetype = guide_legend(override.aes = list(size = 1))) + theme(legend.title = element_text(size = 10), legend.text = element_text(size = 10), legend.key.height = unit(0.75, "line"), legend.key.width = unit(1, "line"))
	formant.comparison <- formant.comparison + theme(legend.position = "none")

	plots[[plot.count]] <- formant.comparison

}

# Print page
multiplot(plotlist = plots, layout = matrix(c(1, 2, 3, 4), nrow = 2, byrow = T), title = "Generation")

dev.off()


# Extract and save legend
#cairo_pdf("Plots/Group/SS-ANOVA legend.pdf", width = 5, height = 5)
#legend <- g_legend(formant.comparison)
#grid.draw(legend)
#dev.off()


# Clear workspace and finish
rm(data, data.ethnicity, data.f1, data.f2, data.gender, data.generation, data.npv, data.pv, grid, colors, f1.model, f2.model, formant.comparison, i, j, k, multiplot, plots, plot.count)
