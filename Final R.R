data(mpg)
data("midwest")
library(ggplot2)
library(tidyverse)
view(mpg)
view(midwest)
#options(scipen=999)
as.integer()

summary(midwest)
count<-table( midwest$county, midwest$popblack)
count
barplot(count, main = "popwhite")
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts))

hist(midwest$popwhite)
ggplot(mpg, aes(class,cty))+geom_violin()
glimpse(midwest)

ggplot(midwest, aes(x=popwhite, y=popblack)) + geom_bar(stat = "identity")

plot1 <- ggplot(midwest, aes(x=popblack)) + geom_bar() + labs(title="Frequency bar chart")  # Y axis derived from counts of X item
print(plot1)
