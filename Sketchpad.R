


ggplot(studentdata, aes(Drink)) + geom_bar()

studentdata$HoursSlept <- studentdata$WakeUp - studentdata$ToSleep
gplot(studentdata, aes(HoursSlept)) + geom_histogram(binwidth=1)