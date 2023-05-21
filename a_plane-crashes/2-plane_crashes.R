library(dplyr)
library(ggplot2)
library(usmap)

# Jaka tu powinna byc sciezka xd? taka czy "../2001.csv.bz2" ?
# Odpalane z poziomu projektu powinno byc ./2001.csv.bz2, ale nie wrzucamy na gh pliku projektu
df <- read.csv("./2001.csv.bz2")
df <- filter(df, Month==9) %>%
  select(DayofMonth, Origin, Dest, Cancelled)


airports <- read.csv("./airports.csv")
airports <- select(airports, iata, state)

originCancelledByStates <- left_join(df, airports, by=join_by(Origin==iata)) %>%
  select(-c("Dest", "Origin")) %>%
  group_by(DayofMonth, state) %>%
  summarise(PlannedFlights=n(), Cancelled=sum(Cancelled)) %>%
  ungroup() %>%
  mutate(CancelledPercentage=(Cancelled/PlannedFlights)*100)

plot_usmap(data=originCancelledByStates[originCancelledByStates$DayofMonth==11,], values="CancelledPercentage") +
  scale_fill_continuous(name="% of cancelled flights", low="#ffd9d9", high="#ff0000", limits=c(0,100))


