library(ggplot2)
library(plyr)

# two data frames: one with colleges + pops, one with the full Eights finish bump chart
df.pop <- read.csv(file.choose())
df.bumps <- read.csv(file.choose())


# first examine bumps for two measures of success

  # total number of crews entered
df.match <- as.data.frame(table(df.bumps$College))
names(df.match)[1] <- "College"
names(df.match)[2] <- "Crews"

  # combined position of M1 and W1 subtracted by total number of crews?
df.MW1 <- df.bumps[which(df.bumps$Crew=='M1' | df.bumps$Crew=='W1'), ] # new data frame keeping only first crews (only Benet's no repeat)
df.match <- merge (df.match, aggregate(df.MW1$Position, list(College = df.MW1$College), mean), by="College") # collapse by college
names(df.match)[3] <- "Position"
df.match$Position = ( nrow(df.bumps) / 2 ) - df.match$Position #adjust for number of crews and invert to higher = better



# second examine college pops for total size 
  # add together colleges with shared clubs as per Rules of Racing A1.1.2.i
levels(df.pop$College)[levels(df.pop$College)=="StCross"] <- "Wolfson"
levels(df.pop$College)[levels(df.pop$College)=="Blackfriars"] <- "StBenet'sHall"
levels(df.pop$College)[levels(df.pop$College)=="Nuffield"] <- "Linacre"
levels(df.pop$College)[levels(df.pop$College)=="WycliffeHall"] <- "TheQueen'sCollege"
levels(df.pop$College)[levels(df.pop$College)=="HarrisManchester"] <- "Wadham"
levels(df.pop$College)[levels(df.pop$College)=="Kellogg"] <- "ChristChurch"
levels(df.pop$College)[levels(df.pop$College)=="AllSouls"] <- "Magdalen"

  df.pop <- ddply(df.pop, .(College), summarize, UG = sum(Undergraduates), GS = sum(Graduates), Total = sum(Total))

  # modify names to match bumps records
levels(df.pop$College)[levels(df.pop$College)=="JesusCollege"] <- "Jesus"
levels(df.pop$College)[levels(df.pop$College)=="LadyMargaretHall"] <- "L.M.H."
levels(df.pop$College)[levels(df.pop$College)=="StEdmundHall"] <- "S.E.H."
levels(df.pop$College)[levels(df.pop$College)=="UniversityCollege"] <- "University"
levels(df.pop$College)[levels(df.pop$College)=="TheQueen'sCollege"] <- "Queen’s"
levels(df.pop$College) <- gsub("'", "’", levels(df.pop$College)) #since different apostrophe types

  # ratio of undergrad : grad (ignore other?)
df.pop$Ratio = df.pop$UG / ( df.pop$UG + df.pop$GS )

  # merge onto df.match
df.match <- merge (df.match, subset(df.pop, select = -c(UG,GS)), by="College") # collapse by college

df.match$Position[25] <- df.match$Position[25]*2 #correct Benet's for only having one first boat

# graphs & analysis

# total college size determines first boat positions
ggplot(data=df.match,aes(x=Total,y=Position)) +
  geom_point(size=5,colour="red") + 
  geom_text(aes(label=College),hjust=0,vjust=0,size=5,fontface="italic") +
  geom_smooth(method=lm) +
  theme_minimal() + 
  theme(plot.title = element_text(face="bold")) +
  ggtitle("Association between student population and bumps chart position (data from end 2015)") +
  xlab("Total student population")  + ylab("Men's and women's first boat positions from bottom")

cor.test(df.match$Total, df.match$Position)

# total college size determines number of crews
ggplot(data=df.match,aes(x=Total,y=Crews)) +
  geom_point() + geom_text(aes(label=College),hjust=0,vjust=0)

# ratio of grads determines first boat positions
ggplot(data=df.match,aes(x=Ratio,y=Position)) +
  geom_point() + geom_text(aes(label=College),hjust=0,vjust=0)

# ratio of grads determines number of crews
ggplot(data=df.match,aes(x=Ratio,y=Crews)) +
  geom_point() + geom_text(aes(label=College),hjust=0,vjust=0)