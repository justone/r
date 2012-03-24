#!/usr/bin/env Rscript

# for melt
library("reshape")
# for pretty plotting
library("ggplot2")
# for help with date labels and scaling
library("scales")

# Read in raw times
times.raw <- read.csv('decimal_times.csv')

# make a copy to clean up
times.clean <- times.raw

# make rownames first column
rownames(times.clean) = times.clean[,1]
# remove first column
times.clean = times.clean[,-1]

# transpose and melt
times.melted <- melt(t(times.clean))

# set column names
colnames(times.melted) <- c('Date', 'Team', 'Time')
# parse date into date type
times.melted$Date <- strptime(times.melted$Date, "X%m.%d.%Y")
times.melted$Date <- as.POSIXct(times.melted$Date)

# subset for testing
#times.test <- times.melted[times.melted$Team == 'Lulu',]
# or could do it this way
# times.test <- subset(times.melted, Team == 'Lulu')

# just the teams
times.teams <- times.melted[times.melted$Team != 'Total',]
# order the factor
times.teams$Team <- factor(times.teams$Team, levels=c('Intro','Lulu','Blinky','Cheezo','Rosco','Fizbo','Skiddle','Checker','Twinkle','Bonker','Finish'))
# filter out the NAs
times.teams.clean <- times.teams[!is.na(times.teams$Time),]

# just the total
times.total <- times.melted[times.melted$Team == 'Total',]

# plot all teams
png(filename = "team_times.png", width=1200, height=750)
ggplot(times.teams.clean, aes(Date, Time)) + scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("1 week")) + geom_line(aes(colour=Team)) + geom_point(aes(colour=Team)) + facet_wrap(~ Team) + geom_hline(aes(yintercept=0.75)) + labs(x = "Presentation Date", y = "Length (Minutes)") + opts(title = 'Team Presentation Time')

# plot total time
png(filename = "total_time.png", width=750, height=425)
ggplot(times.total, aes(Date, Time)) + geom_smooth() + geom_point() + geom_hline(aes(yintercept=5)) + labs(x = "Presentation Date", y = "Length (Minutes)") + opts(title = 'Total Presentation Time')

dev.off()
