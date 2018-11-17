# Assignment in MAB2111
# by Marlon Vincent Abella
# ------------------------------

# 1.
# Read in the data file

WHO = read.csv("WHO.csv")

# d. country with the lowest literacy

WHO$Country[which.min(WHO$LiteracyRate)]
# Answer: Mali

# e. Richest country in Europe based on GNI

WHO.Europe = subset(WHO, Region == "Europe")
WHO.Europe$Country[which.max(WHO.Europe$GNI)]
# Answer:Luxembourg

# f. Mean Life expectancy of countries in Africa

WHO.Africa = subset(WHO, Region == "Africa")
mean(WHO.Africa$LifeExpectancy)
# Answer: 57.95652 

# g. Number of countries with population greater than 10,000 (This assumes in K)

sum(WHO$Population>10000)
# Answer: 86

# h. Top 5 countries in the Americas with the highest child mortality

WHO.Americas = subset(WHO, Region == "Americas")
head(WHO.Americas[order(WHO.Americas$ChildMortality,decreasing = TRUE),],5)
# Answer: Haiti, Bolivia, Guyana, Guatemala, Dominican Republic

# 2.
# Read in the data file that's already converted to csv

NBA = read.csv("Historical NBA Performance.csv")

# a. The year Bulls has the highest winning percentage

NBA.Bulls = subset(NBA, Team == "Bulls")
NBA.Bulls$Year[which.max(NBA.Bulls$Winning.Percentage)]
# Answer: 1995-96

# b. Teams with an even win-loss record in a year

NBA.Even = subset(NBA, Winning.Percentage == 0.5)

# 3.
# Read in the data

Seasons = read.csv("Seasons_Stats.csv")

# a. Player with the highest 3-pt attempt rate in a season

Seasons$Player[which(Seasons$X3PAr == max(Seasons$X3PAr, na.rm = TRUE))]

# b. Player with the highest free throw rate in a season

Seasons$Player[which(Seasons$FTr == max(Seasons$FTr, na.rm = TRUE))]

# c. What year/season does Lebron James scored the highest

Lebron = subset(Seasons, Player=="LeBron James")
Lebron$Year[which(Lebron$PTS == max(Lebron$PTS, na.rm = TRUE))]
# Answer: 2006

# d. What year/season does Michael Jordan scored the highest

Jordan = subset(Seasons, Player=="Michael Jordan*")
Jordan$Year[which(Jordan$PTS == max(Jordan$PTS, na.rm = TRUE))]
# Answer: 1987

# e. Player efficiency rating of Kobe Bryant in the year where his MP is the lowest

Kobe = subset(Seasons, Player=="Kobe Bryant")
Kobe$PER[which(Kobe$MP == min(Kobe$MP, na.rm = TRUE))]
# Answer: 10.7

# 4.
# Read in the data file

NUR = read.csv("National Universities Rankings.csv")

#a. University with the most number of undergrads

NUR$Name[which.max(NUR$Undergrad.Enrollment)]
# Answer: Tennessee Technological University

#b. Average Tuition in the Top 10 University 

mean(as.numeric(gsub(",", "", sub('.', '', head(NUR[order(NUR[['Rank']], decreasing = FALSE),], 10)[['Tuition.and.fees']]))))
# Answer: 49895.2

