# create a plot of associated studies for math majors
library(ggplot2)
library(plyr)

# create a dataframe object from csv file and rename the columns
assoc_majors <- read.csv(file = 'math_associated_studies.csv', header = FALSE, sep = ',')
colnames(assoc_majors) <- c('Area', 'Type', 'Number')

# count the number of majors/minors with in each subject
sumofareas <- ddply(assoc_majors, .(Area), summarise, sum = sum(Number))
sumofareas <- sumofareas[order(-sumofareas$sum),] 
assoc_majors_withsum <- merge(assoc_majors, sumofareas)
assoc_majors_withsum <- assoc_majors_withsum[order(-assoc_majors_withsum$sum),] 

# generate a stacked bar plot of associated studies for math majors with decreasing number of students
ggplot(assoc_majors_withsum, aes(x=reorder(Area, sum), y=Number, fill = Type)) + geom_col() + coord_flip() + xlab("Area of study") + ylab("Count") + ggtitle("Associated Studies for Math Majors")

###same steps as above but for students who have a math minor
math_minors = read.csv("math_minor_associated_studies.csv", header = T)

names(math_minors) = c("Area", "Type", "Number")

sumofareasMinors <- ddply(math_minors, .(Area), summarise, sum = sum(Number))
sumofareasMinors <- sumofareasMinors[order(-sumofareasMinors$sum),] 
math_minors_withsum <- merge(math_minors, sumofareasMinors)
math_minors_withsum <- math_minors_withsum[order(-math_minors_withsum$sum),] 

ggplot(math_minors_withsum, aes(x=reorder(Area,sum), y=Number, fill = Type)) + geom_col() + coord_flip() + xlab("Area of Study") + ylab("Count") + ggtitle("Associated Studies for Math Minors")

