# Load required libraries
library(pracma)
library(seqinr)
library(dplyr)
library(tidyr)

# Read data from CSV file
data <- read.csv('data.csv')

# Display summary and head of the data
summary(data)
head(data)

# Subset data for different conditions
t0.ctrl.1 <-
  subset(data, timepoint == 'T0' & group == 'CTRL' & well == '#1')
t0.ctrl.2 <-
  subset(data, timepoint == 'T0' & group == 'CTRL' & well == '#2')
t0.ctrl.3 <-
  subset(data, timepoint == 'T0' & group == 'CTRL' & well == '#3')

t30.ctrl.1 <-
  subset(data, timepoint == 'T30' & group == 'CTRL' & well == '#1')
t30.ctrl.2 <-
  subset(data, timepoint == 'T30' & group == 'CTRL' & well == '#2')
t30.ctrl.3 <-
  subset(data, timepoint == 'T30' & group == 'CTRL' & well == '#3')

t0.pe.1 <-
  subset(data, timepoint == 'T0' & group == 'PE' & well == '#1')
t0.pe.2 <-
  subset(data, timepoint == 'T0' & group == 'PE' & well == '#2')
t0.pe.3 <-
  subset(data, timepoint == 'T0' & group == 'PE' & well == '#3')

t30.pe.1 <-
  subset(data, timepoint == 'T30' & group == 'PE' & well == '#1')
t30.pe.2 <-
  subset(data, timepoint == 'T30' & group == 'PE' & well == '#2')
t30.pe.3 <-
  subset(data, timepoint == 'T30' & group == 'PE' & well == '#3')

# Create a list of subsets
our_subsets <- list(
  t0.ctrl.1,
  t0.ctrl.2,
  t0.ctrl.3,
  t30.ctrl.1,
  t30.ctrl.2,
  t30.ctrl.3,
  t0.pe.1,
  t0.pe.2,
  t0.pe.3,
  t30.pe.1,
  t30.pe.2,
  t30.pe.3
)

# Initialize lists for storing results
names <- list()
mean_heights <- list()
sd_heights <- list()
beats <- list()

# Iterate over subsets
for (this_subset in our_subsets) {
  # Extract unique identifiers for the subset
  time_point <- unique(this_subset['timepoint'])
  g_roup <- unique(this_subset['group'])
  w_ell <- unique(this_subset['well'])
  title <- paste(time_point, g_roup, w_ell)
  
  # Append title to the names list
  names <- append(names, title)
  
  # Plot the subset data
  plot(
    value ~ time,
    data = this_subset,
    type = 'l',
    xlim = c(0.5, 14.5),
    main = title
  )
  
  # Extract values and calculate peak-related measures
  y <- this_subset[['value']]
  threshold <- mean(y) + ((max(y) - min(y)) / 3)
  peaks <-
    findpeaks(y, minpeakheight = threshold, minpeakdistance = 50)
  baseline <- baselineabif(y, maxrfu = threshold)
  
  # Append number of peaks to the beats list
  beats <- append(beats, nrow(peaks))
  
  colnames(peaks) <- c('Intensity', 'Position', 'Begin', 'End')
  peakheight <- peaks[1:nrow(peaks)] - baseline
  peaks[1:nrow(peaks)] <- peakheight
  
  # Calculate and store mean and standard deviation of peak heights
  mean_heights <- append(mean_heights, mean(peakheight))
  sd_heights <- append(sd_heights, sd(peakheight))
}

# Create data frame for mean peak heights
mean_heights_df <- data.frame(unlist(names), unlist(mean_heights))
colnames(mean_heights_df) <- c('archgroup', 'value')
mean_heights_df <-
  mean_heights_df %>% separate(archgroup, c('timepoint', 'group', 'well'))

# Create data frame for standard deviation of peak heights
sd_heights_df <- data.frame(unlist(names), unlist(sd_heights))
colnames(sd_heights_df) <- c('archgroup', 'value')
sd_heights_df <-
  sd_heights_df %>% separate(archgroup, c('timepoint', 'group', 'well'))

# Subset mean peak heights for different conditions
t0.ctrl.1.heights <-
  subset(mean_heights_df, timepoint == 'T0' &
           group == 'CTRL' & well == '1')$value
t0.ctrl.2.heights <-
  subset(mean_heights_df, timepoint == 'T0' &
           group == 'CTRL' & well == '2')$value
t0.ctrl.3.heights <-
  subset(mean_heights_df, timepoint == 'T0' &
           group == 'CTRL' & well == '3')$value

t30.ctrl.1.heights <-
  subset(mean_heights_df, timepoint == 'T30' &
           group == 'CTRL' & well == '1')$value
t30.ctrl.2.heights <-
  subset(mean_heights_df, timepoint == 'T30' &
           group == 'CTRL' & well == '2')$value
t30.ctrl.3.heights <-
  subset(mean_heights_df, timepoint == 'T30' &
           group == 'CTRL' & well == '3')$value

t0.pe.1.heights <-
  subset(mean_heights_df, timepoint == 'T0' &
           group == 'PE' & well == '1')$value
t0.pe.2.heights <-
  subset(mean_heights_df, timepoint == 'T0' &
           group == 'PE' & well == '2')$value
t0.pe.3.heights <-
  subset(mean_heights_df, timepoint == 'T0' &
           group == 'PE' & well == '3')$value

t30.pe.1.heights <-
  subset(mean_heights_df, timepoint == 'T30' &
           group == 'PE' & well == '1')$value
t30.pe.2.heights <-
  subset(mean_heights_df, timepoint == 'T30' &
           group == 'PE' & well == '2')$value
t30.pe.3.heights <-
  subset(mean_heights_df, timepoint == 'T30' &
           group == 'PE' & well == '3')$value

# Calculate ratios of peak heights
ctrl.1.heights <- t30.ctrl.1.heights / t0.ctrl.1.heights
ctrl.2.heights <- t30.ctrl.2.heights / t0.ctrl.2.heights
ctrl.3.heights <- t30.ctrl.3.heights / t0.ctrl.3.heights

pe.1.heights <- t30.pe.1.heights / t0.pe.1.heights
pe.2.heights <- t30.pe.2.heights / t0.pe.2.heights
pe.3.heights <- t30.pe.3.heights / t0.pe.3.heights

ctrl.heights <- c(ctrl.1.heights, ctrl.2.heights, ctrl.3.heights)
pe.heights <- c(pe.1.heights, pe.2.heights, pe.3.heights)

# Standard deviation of ratios
ctrl.1.heights.error <-
  sqrt(ctrl.1.heights ^ 2 * (((t30.ctrl.1.heights.sd ^ 2) / (t30.ctrl.1.heights ^
                                                               2)
  ) + ((t0.ctrl.1.heights.sd ^ 2) / (t0.ctrl.1.heights ^ 2)
  )))
ctrl.2.heights.error <-
  sqrt(ctrl.2.heights ^ 2 * (((t30.ctrl.2.heights.sd ^ 2) / (t30.ctrl.2.heights ^
                                                               2)
  ) + ((t0.ctrl.3.heights.sd ^ 2) / (t0.ctrl.2.heights ^ 2)
  )))
ctrl.3.heights.error <-
  sqrt(ctrl.3.heights ^ 2 * (((t30.ctrl.3.heights.sd ^ 2) / (t30.ctrl.3.heights ^
                                                               2)
  ) + ((t0.ctrl.3.heights.sd ^ 2) / (t0.ctrl.3.heights ^ 2)
  )))

pe.1.heights.error <-
  sqrt(pe.1.heights ^ 2 * (((t30.pe.1.heights.sd ^ 2) / (t30.pe.1.heights ^
                                                           2)
  ) + ((t0.pe.1.heights.sd ^ 2) / (t0.pe.1.heights ^ 2)
  )))
pe.2.heights.error <-
  sqrt(pe.2.heights ^ 2 * (((t30.pe.2.heights.sd ^ 2) / (t30.pe.2.heights ^
                                                           2)
  ) + ((t0.pe.2.heights.sd ^ 2) / (t0.pe.2.heights ^ 2)
  )))
pe.3.heights.error <-
  sqrt(pe.3.heights ^ 2 * (((t30.pe.3.heights.sd ^ 2) / (t30.pe.3.heights ^
                                                           2)
  ) + ((t0.pe.3.heights.sd ^ 2) / (t0.pe.3.heights ^ 2)
  )))

ctrl.heights.errors <-
  c(ctrl.1.heights.error,
    ctrl.2.heights.error,
    ctrl.3.heights.error)
pe.heights.errors <-
  c(pe.1.heights.error, pe.2.heights.error, pe.3.heights.error)

# Calculate means and errors
ctrl.heights.mean <- mean(ctrl.heights)
pe.heights.mean <- mean(pe.heights)
ctrl.heights.error <- 1 / 3 * sqrt(sum(ctrl.heights.errors ^ 2))
pe.heights.error <- 1 / 3 * sqrt(sum(pe.heights.errors ^ 2))

# Plot barplot for mean peak heights
heights.means <- c(ctrl.heights.mean, pe.heights.mean)
heights.errors <- c(ctrl.heights.error, pe.heights.error)
heights.barplot <-
  barplot(
    heights.means,
    names = c('CTRL', 'PE'),
    ylim = c(min(0, min(
      heights.means - heights.errors
    )), max(heights.means + heights.errors) + 0.15),
    main = 'Peak Heights'
  )
arrows(
  x0 = heights.barplot,
  y0 = heights.means + heights.errors,
  y1 = heights.means - heights.errors,
  angle = 90,
  code = 3,
  length = 0.1
)

# Create data frame for beats
beats_df <- data.frame(unlist(names), unlist(beats))
colnames(beats_df) <- c('archgroup', 'value')
beats_df$bpm <- beats_df$value * 4
beats_df <-
  beats_df %>% separate(archgroup, c('timepoint', 'group', 'well'))

# Subset beats for different conditions
t0.ctrl.1.beats <-
  subset(beats_df, timepoint == 'T0' &
           group == 'CTRL' & well == '1')$bpm
t0.ctrl.2.beats <-
  subset(beats_df, timepoint == 'T0' &
           group == 'CTRL' & well == '2')$bpm
t0.ctrl.3.beats <-
  subset(beats_df, timepoint == 'T0' &
           group == 'CTRL' & well == '3')$bpm

t30.ctrl.1.beats <-
  subset(beats_df, timepoint == 'T30' &
           group == 'CTRL' & well == '1')$bpm
t30.ctrl.2.beats <-
  subset(beats_df, timepoint == 'T30' &
           group == 'CTRL' & well == '2')$bpm
t30.ctrl.3.beats <-
  subset(beats_df, timepoint == 'T30' &
           group == 'CTRL' & well == '3')$bpm

t0.pe.1.beats <-
  subset(beats_df, timepoint == 'T0' &
           group == 'PE' & well == '1')$bpm
t0.pe.2.beats <-
  subset(beats_df, timepoint == 'T0' &
           group == 'PE' & well == '2')$bpm
t0.pe.3.beats <-
  subset(beats_df, timepoint == 'T0' &
           group == 'PE' & well == '3')$bpm

t30.pe.1.beats <-
  subset(beats_df, timepoint == 'T30' &
           group == 'PE' & well == '1')$bpm
t30.pe.2.beats <-
  subset(beats_df, timepoint == 'T30' &
           group == 'PE' & well == '2')$bpm
t30.pe.3.beats <-
  subset(beats_df, timepoint == 'T30' &
           group == 'PE' & well == '3')$bpm

# Calculate ratios of beats
ctrl.1.beats <- t30.ctrl.1.beats / t0.ctrl.1.beats
ctrl.2.beats <- t30.ctrl.2.beats / t0.ctrl.2.beats
ctrl.3.beats <- t30.ctrl.3.beats / t0.ctrl.3.beats

pe.1.beats <- t30.pe.1.beats / t0.pe.1.beats
pe.2.beats <- t30.pe.2.beats / t0.pe.2.beats
pe.3.beats <- t30.pe.3.beats / t0.pe.3.beats

ctrl.beats <- c(ctrl.1.beats, ctrl.2.beats, ctrl.3.beats)
pe.beats <- c(pe.1.beats, pe.2.beats, pe.3.beats)

# Calculate mean and standard deviation of beats
ctrl.beats.mean <- mean(ctrl.beats)
ctrl.beats.sd <- sd(ctrl.beats)
pe.beats.mean <- mean(pe.beats)
pe.beats.sd <- sd(pe.beats)

# Plot barplot for heart rate
beats.means <- c(ctrl.beats.mean, pe.beats.mean)
beats.sds <- c(ctrl.beats.sd, pe.beats.sd)
beats.barplot <-
  barplot(
    beats.means,
    names = c('CTRL', 'PE'),
    ylim = c(min(0, min(
      beats.means - beats.sds
    )), max(beats.means + beats.sds) + 0.05),
    main = 'Heart Rate'
  )
arrows(
  x0 = beats.barplot,
  y0 = beats.means + beats.sds,
  y1 = beats.means - beats.sds,
  angle = 90,
  code = 3,
  length = 0.1
)

# Perform Shapiro-Wilk tests
shapiro_p <- list()
sp1 <- shapiro.test(ctrl.heights)
sp2 <- shapiro.test(pe.heights)
sp3 <- shapiro.test(ctrl.beats)
sp4 <- shapiro.test(pe.beats)
shapiro_p <- append(shapiro_p, sp1$p.value)
shapiro_p <- append(shapiro_p, sp2$p.value)
shapiro_p <- append(shapiro_p, sp3$p.value)
shapiro_p <- append(shapiro_p, sp4$p.value)

# Check p-values and perform appropriate tests
if (any(shapiro_p > 0.5)) {
  var_p <- list()
  var1 <- var.test(ctrl.heights, pe.heights)
  var2 <- var.test(ctrl.beats, pe.beats)
  var_p <- append(var_p, var1$p.value)
  var_p <- append(var_p, var2$p.value)
  if (any(var_p > 0.5)) {
    t1 <- t.test(ctrl.heights, pe.heights, var.equal = FALSE)
    t2 <- t.test(ctrl.beats, pe.beats, var.equal = FALSE)
  } else {
    t1 <- t.test(ctrl.heights, pe.heights, var.equal = TRUE)
    t2 <- t.test(ctrl.beats, pe.beats, var.equal = TRUE)
  }
} else {
  t1 <- wilcox.test(ctrl.heights, pe.heights)
  t2 <- wilcox.test(ctrl.beats, pe.beats)
}

# Print mean_heights_df, beats_df, and test results
print(mean_heights_df)
print(beats_df)

print('TEST RESULTS')
print(sp1)
print(sp2)
print(sp3)
print(sp4)
if (exists('var1') & exists('var2')) {
  print(var1)
  print(var2)
}
print(t1)
print(t2)
