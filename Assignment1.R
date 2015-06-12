


require(knitr)
require(dplyr)
require(lattice)
setwd("C:/Users/Lukas.Snyder/Documents/GitHub/RepData_PeerAssessment1")



data.load<-read.csv("activity.csv")
no.nulls<-na.omit(data.load)



steps.per.day<-summarise(group_by(no.nulls,date),sum.steps=sum(steps))
steps.per.day

hist(steps.per.day$sum.steps)

mean(steps.per.day$sum.steps)
median(steps.per.day$sum.steps)



steps.per.interval<-summarise(group_by(no.nulls,interval),ave.steps=mean(steps))
plot(steps.per.interval$interval,steps.per.interval$ave.steps,type="l")

steps.per.interval[which(steps.per.interval$ave.steps==max(steps.per.interval$ave.steps)),1]

filled.in<-merge(data.load,steps.per.interval)

nrow(data.load[is.na(data.load$steps),])

imputed.values<-mutate(filled.in, new.steps = ifelse(is.na(steps)==TRUE,ave.steps,steps))


steps.per.day.imput<-summarise(group_by(inputed.values,date),sum.steps=sum(new.steps))

hist(steps.per.day.imput$sum.steps)

mean(steps.per.day.imput$sum.steps)
median(steps.per.day.imput$sum.steps)

dayoweek<-ifelse(weekdays(as.Date(imputed.values$date,"%Y-%m-%d")) %in% c("Saturday", "Sunday"),"Weekend","Weekday")
impute.with.days<-mutate(imputed.values,dayofweek=
                         ifelse(
                           weekdays(
                             as.Date(imputed.values$date,"%Y-%m-%d")) 
                           %in% c("Saturday", "Sunday")
                           ,"Weekend"
                           ,"Weekday"
                           )
                         )

Week.compare<-summarise(group_by(impute.with.days,dayofweek,interval),ave.steps=mean(new.steps))


xyplot(ave.steps~interval|dayofweek,
       data=Week.compare,
       type="l",
       layout=c(1,2))




