


ggplot(studentdata, aes(Drink)) + geom_bar()

studentdata$HoursSlept <- studentdata$WakeUp - studentdata$ToSleep
ggplot(studentdata, aes(HoursSlept)) + geom_histogram(binwidth=1)

ggplot(studentdata, aes(Gender, HoursSlept)) + geom_boxplot()

ggplot(studentdata, aes(ToSleep, HoursSlept)) + geom_point(alpha=0.2)

ggplot(studentdata, aes(ToSleep, HoursSlept)) + geom_point(alpha=0.2) + geom_smooth(method="lm")