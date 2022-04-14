#Load library
library(DT)
#Load dataset
data<- read.csv("C:/Users/veniniyan/Downloads/cwurData.csv")

#data <- subset(data,!duplicated(data$institution))
data <- subset(data, data$year == 2015)
str(data)

#Visualizing the Top Countries in Higher Learning
height<- sort(table(data$country), decreasing = TRUE)

barplot(height[1:10], las = 3, main = "Top Countries in University Rankings 2015")

#Exploring US Schools
usa <- subset(data, data$country == "USA")

#Use the search bar to see if your school made the list!
datatable(usa)

#Analysis
#Quality Of Faculty
plot (usa$quality_of_faculty, usa$national_rank, xlab ="Quality of Faculty", ylab = "National Rank", main = "Quality of Faculty vs National Rank")
c <- lm(national_rank ~ quality_of_faculty, data = usa)
summary(c) #regression model

#Influence
plot (usa$influence, usa$national_rank, xlab ="Influence", ylab="National Rank", main = "Influnce vs National Rank")
c <- lm(national_rank ~ influence, data = usa)
summary(c) #regression model

#Citations
plot (usa$citations, usa$national_rank, xlab = "Citations", ylab = "National Rank", main = "Citations vs National Rank")
c <- lm(national_rank ~ citations, data = usa)
summary(c) #regression model

#Interpretation
regline <- lm(national_rank ~ quality_of_faculty + influence + citations, data = usa)

summary(regline)

regline <- lm(national_rank ~quality_of_education + alumni_employment + quality_of_faculty + publications + influence + citations +broad_impact + patents + score, data = usa)

summary(regline)

#Employment Rank
usaEmployment <- usa[order(usa$alumni_employment),]
usaEmploymentRank<-c(1:229) #229 because that is the # of US schools on the list
usaEmployment<-cbind(usaEmployment,usaEmploymentRank)

uTable <- data.frame(usaEmployment$institution, usaEmployment$usaEmploymentRank,usaEmployment$national_rank)
datatable(uTable, colnames = c("Institution", "National Employment Rank", "National Rank"))

#National Rank
string= "Employment Rank"

plot(usaEmployment$national_rank,usaEmployment$usaEmploymentRank, xlab = "National Rank", ylab = string ,main = "National Rank vs Employment Rank")
c<-lm(usaEmploymentRank ~ national_rank, data = usaEmployment)
summary(c)

#Quality Of Education
plot(usaEmployment$quality_of_education,usaEmployment$usaEmploymentRank, xlab = "Quality of Education ", ylab = string, main = "Quality of Education vs Employment Rank")
c<-lm(usaEmploymentRank ~ quality_of_education, data = usaEmployment)
summary(c)

#Quality Of faculty
plot(usaEmployment$quality_of_faculty,usaEmployment$usaEmploymentRank, xlab = "Quality of Faculty", ylab = string, main = "Quality of Faculty vs Employment Rank")
c<-lm(usaEmploymentRank ~ quality_of_faculty, data = usaEmployment)
summary(c)

linReg<- lm(usaEmploymentRank~national_rank+quality_of_education+quality_of_faculty, data = usaEmployment)
summary(linReg)

linReg<- lm(usaEmploymentRank~national_rank+quality_of_education, data = usaEmployment)
summary(linReg)






