#########################
#########################
# Bell Bottoms Analysis #
#########################
#########################

# load libraries 
library(lme4)
library(nlme)
library(boot)
library(car) 
library(reshape2)
library(ggplot2)
library(ez)
library(plyr)
library(ggsignif)
library(lsr)
library(sjmisc)
library(sjstats)
library(BayesFactor)
library(foreign)
library(dplyr)
library(lattice)
library(Hmisc)

# 1750

# load data
D.1 = read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)
D.2 = read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)
D.3 = read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)
D.4 = read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)
D.5 = read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)

D.1.noise = read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)
D.2.noise = read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)
D.3.noise = read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)
D.4.noise = read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)
D.5.noise = read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)

D.1.lessNoise = read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)
D.2.lessNoise = read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)
D.3.lessNoise = read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)
D.4.lessNoise = read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)
D.5.lessNoise = read.table(file.choose(), header = FALSE, stringsAsFactors = FALSE)

D = rbind(D.1, D.2, D.3, D.4, D.5,
                D.1.noise, D.2.noise, D.3.noise, D.4.noise, D.5.noise,
          D.1.lessNoise, D.2.lessNoise, D.3.lessNoise, D.4.lessNoise, D.5.lessNoise)

# get dimensionality of D
dim(D)

# create an ID column
D$ID = rep(1:300, each = 72)

# factors
itemVec = rep(c("Named Match", "Named Mismatch", "Neither"), each = 6, times = 4)
D$itemType = rep(itemVec, times = 300)
D$itemType = as.factor(D$itemType)

D$modificationType = rep(c("Modified", "Unmodified"), each = 36, times = 300)
D$modificationType = as.factor(D$modificationType)

D$epochs = rep(c("1500 epochs", "1750 epochs", "2000 epochs",
                 "2250 epochs", "2500 epochs"), each = 1440, times = 3)
D$epochs = as.factor(D$epochs)

D$noise = rep(c("No Noise", "Noise", "Less Noise"), each = 7200)
D$noise = as.factor(D$noise)

# DV
D$score = D$V3


# remove columns
D = D[,-c(1:3)]


# figure
D.noNoise = subset(D, ! noise %in% c("Noise", "Less Noise"))
D.noise = subset(D, ! noise %in% c("No Noise", "Less Noise"))
D.lessNoise = subset(D, ! noise %in% c("No Noise", "Noise"))

# No noise
p.noNoise = ggplot(D.noNoise, aes(itemType, score, fill=itemType)) + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Score") + # change the label of the y-axis
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  coord_cartesian(ylim=c(0, 12)) +
  scale_fill_manual(values = c("black", "azure3", "navy")) +
  labs(fill='Test Trial')  +
  facet_wrap(epochs~modificationType, ncol=5, scales = "free") +
  theme(axis.text.x = element_blank())

p.noNoise

# noise
p.noise = ggplot(D.noise, aes(itemType, score, fill=itemType)) + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Score") + # change the label of the y-axis
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  coord_cartesian(ylim=c(0, 15)) +
  scale_fill_manual(values = c("black", "azure3", "navy")) +
  labs(fill='Test Trial')  +
  facet_wrap(epochs~modificationType, ncol=5, scales = "free") +
  theme(axis.text.x = element_blank())

p.noise


# Less noise
p.lessNoise = ggplot(D.lessNoise, aes(itemType, score, fill=itemType)) + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Score") + # change the label of the y-axis
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  coord_cartesian(ylim=c(0, 15)) +
  scale_fill_manual(values = c("black", "azure3", "navy")) +
  labs(fill='Test Trial')  +
  facet_wrap(epochs~modificationType, ncol=5, scales = "free") +
  theme(axis.text.x = element_blank())

p.lessNoise
  
  
  
