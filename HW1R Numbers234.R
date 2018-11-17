NBA <-read.csv("Historical NBA Performance.csv")

#a.	The year Bulls has the highest winning percentage
Bulls<-subset(NBA,Team =="Bulls")
Bulls$Year[which.max(Bulls$Winning.Percentage)]
#[1] 1995-96
#72 Levels:  1946-47 ... 2016-17

#b.	Teams with an even win-loss record in a year
EvenRecord <- subset(NBA, Winning.Percentage ==0.5)
unique(EvenRecord$Team)

#Number 3
Season <-read.csv("Seasons_Stats.csv")
colnames(Season)

#a.	Player with the highest 3-point attempt rate in a season
#Bonus according to Sir's email ????

#b.	Player with the highest free throw rate in a season.
#Bonus according to Sir's email ????

#c.	What year/season does Lebron James scored the highest?
  
LeBron <-subset(Season, Player=="LeBron James")
LeBron$Year[which.max(LeBron$PTS)]

Michael <-subset(Season, Player=="Michael Jordan*")
Michael$Year[which.max(Michael$PTS)]

Kobe <-subset(Season, Player=="Kobe Bryant")
Kobe$PER[which.min(Kobe$MP)]

#Number 4
NUR <- read.csv("National Universities Rankings.csv")

#' Replace Commas Function
#'
#' This function converts a character representation of a number that contains a comma separator with a numeric value.
#' @keywords read data
#' @export
replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

#a.	University with the most number of undergrads
NUR$NewUEnrollment <- replaceCommas(NUR$Undergrad.Enrollment)
NUR$Name[which.max(NUR$NewUEnrollment)]

#b.	Average tuition in the Top 10 University

NUR$NodollarTuition<-gsub("\\$","", NUR$Tuition.and.fees)
NUR$NewNodollarTuition <-replaceCommas(NUR$NodollarTuition)

mean(NUR[order(NUR$Rank, decreasing = FALSE)[1:10],]$NewNodollarTuition)

replaceCommas<-function(x){
 
x<-as.numeric(gsub("\\,", "", x))
  + 