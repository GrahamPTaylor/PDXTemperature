library(ggplot2)
library(dplyr)

temps <- data.frame(read.csv('pdx_temps.csv'))


temps$avgtemp <- (temps$tmin + temps$tmax) / 2
#temps$tempdiff <- temps$tmax - temps$tmin
temps$date <- as.Date(temps$date)


smalltemps <- temps[temps$date > '1985-01-01',]
head(smalltemps)
# recent data


t <- ggplot() +
  #geom_line(data = smalltemps, aes(x = date, y = avgtemp), color = 'black') +
  geom_line(data = temps, aes(x = date, y = tmax), color = '#ffa07a') +
  geom_line(data = temps, aes(x = date, y = tmin), color = 'light blue') +
  #geom_line(data = smalltemps, aes(x = date, y = tempdiff), color = 'blue') +
  geom_hline(yintercept = 32, color = 'black') +
  scale_y_continuous(limits = c(0,125), breaks = (seq(0,125,5))) +
  ylab(c('Daily Max/Min Temp'))
t



temps2 <- data.frame(Month = as.numeric(format(smalltemps$date, format = "%m")),
                     Day = as.numeric(format(smalltemps$date, format = "%d")),
                     Year = as.numeric(format(smalltemps$date, format = "%Y")),
                     smalltemps$tmax,
                     smalltemps$tmin,
                     smalltemps$avgtemp)
names(temps2) <- c("Month", "Day", "Year", "Tmax", "Tmin", "avgtemp")



Past <- temps2 %>%
  group_by(Year, Month) %>%
  #arrange(Day) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(newDay = seq(1, length(Day))) %>%   # label days as 1:365 (will represent x-axis)         
  ungroup() %>%
  filter(Year != 2017) %>%     # filter out missing data (identified with '-99' value) & current year data
  group_by(newDay) %>%
  mutate(upper = max(Tmax), # identify max value for each day
         lower = min(Tmin), # identify min value for each day
         avghigh = mean(Tmax),  # calculate mean max value for each day
         sehigh = sd(Tmax)/sqrt(length(Tmax)), # calculate standard error of mean max
         avglow = mean(Tmin), # calculate mean min value for each day
         selow = sd(Tmin)/sqrt(length(Tmin))) %>%   #calculate standard error of mean min
  mutate(avg_upper_high = avghigh+(2.101*sehigh),  # calculate 95% CI for mean
         avg_lower_high = avghigh-(2.101*sehigh),
         avg_upper_low = avglow+(2.101*selow),  # calculate 95% CI for mean
         avg_lower_low = avglow-(2.101*selow)) %>%  # calculate 95% CI for mean
  ungroup() %>%
  group_by(newDay)



Present <- temps2 %>%
  group_by(Year, Month) %>%
  #arrange(Day) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(newDay = seq(1, length(Day))) %>%  # create matching x-axis as historical data
  ungroup() %>%
  filter(Year == 2018)  # filter out missing data & select current year data


# create dataframe that represents the lowest temp for each day for the historical data
PastLows <- Past %>%
  group_by(newDay) %>%
  summarise(Pastlow = min(Tmin)) # identify lowest temp for each day from 1995-2013



# create dataframe that identifies the days in 2014 in which the temps were lower than all previous 19 years
PresentLows <- Present %>%
  left_join(PastLows) %>%  # merge historical lows to current year low data
  mutate(record = ifelse(Tmin<Pastlow, "Y", "N")) %>% # identifies if current year was record low
  filter(record == "Y")  # filter for days that represent current year record lows

# create dataframe that represents the highest temp for each day for the historical data
PastHighs <- Past %>%
  group_by(newDay) %>%
  summarise(Pasthigh = max(Tmax))  # identify highest temp for each day from 1995-2013

# create dataframe that identifies the days in 2014 in which the temps were higher than all previous 19 years
PresentHighs <- Present %>%
  left_join(PastHighs) %>%  # merge historical highs to current year low data
  mutate(record = ifelse(Tmax>Pasthigh, "Y", "N")) %>% # identifies if current year was record high
  filter(record == "Y")  # filter for days that represent current year record highs



# function to turn y-axis labels into degree formatted values
dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

# create y-axis variable
a <- dgr_fmt(seq(-20,100, by=10))

# create a small dataframe to represent legend symbol for 2014 Temperature
legend_data <- data.frame(x=seq(175,182),y=rnorm(8,15,2))

head(Past)

# absolute high/low range

p <- ggplot(Past) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),  
        axis.title = element_blank()) +
  geom_linerange(Past, mapping=aes(x=newDay, ymin=lower, ymax=upper), colour = "wheat2", alpha=.1)

# average high/low range

p <- p +
  geom_linerange(Past, mapping = aes(x = newDay, ymin = avg_lower_high, ymax = avg_upper_high), color = 'wheat4') +
  geom_linerange(Past, mapping = aes(x = newDay, ymin = avg_lower_low, ymax = avg_upper_low), color = 'wheat4') 
  

# 2018 data - max,min temperatures

p <- p +
  #geom_line(Present, mapping = aes(x = newDay, y = avgtemp, group = 1)) +
  geom_line(Present, mapping = aes(x = newDay, y = Tmax, group = 1), color = 'firebrick3') +
  geom_line(Present, mapping = aes(x = newDay, y = Tmin, group = 1), color = 'blue3') +
  geom_vline(xintercept = 0, color = 'wheat4', linetype = 1, size = 1)

p

# add grids

p <- p + 
  geom_hline(yintercept = -20, colour = "white", linetype=1) +
  geom_hline(yintercept = -10, colour = "white", linetype=1) +
  geom_hline(yintercept = 0, colour = "white", linetype=1) +
  geom_hline(yintercept = 10, colour = "white", linetype=1) +
  geom_hline(yintercept = 20, colour = "white", linetype=1) +
  geom_hline(yintercept = 30, colour = "white", linetype=1) +
  geom_hline(yintercept = 40, colour = "white", linetype=1) +
  geom_hline(yintercept = 50, colour = "white", linetype=1) +
  geom_hline(yintercept = 60, colour = "white", linetype=1) +
  geom_hline(yintercept = 70, colour = "white", linetype=1) +
  geom_hline(yintercept = 80, colour = "white", linetype=1) +
  geom_hline(yintercept = 90, colour = "white", linetype=1) +
  geom_hline(yintercept = 100, colour = "white", linetype=1)

p <- p + 
  geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.5) 


p <- p +
  coord_cartesian(ylim = c(-20,100)) +
  scale_y_continuous(breaks = seq(-20,100, by=10), labels = a) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("January", "February", "March", "April",
                                "May", "June", "July", "August", "September",
                                "October", "November", "December"))

# add labels and legends


p <- p +
  ggtitle("Portland's Weather in 2018") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20)) +
  annotate("text", x = 19, y = 98, label = "Temperature", size=4, fontface="bold")



p <- p +
  annotate("text", x = 66, y = 93, 
           label = "Data represents max and min daily temperatures. Accessible data dates back to", size=3, colour="gray30") +
  annotate("text", x = 62, y = 89, 
           label = "January 1, 1985. Data for 2018 is only available through December 16.", size=3, colour="gray30") 


p
# explain data layers


p <- p +
  annotate("segment", x = 181, xend = 181, y = 5, yend = 25, colour = "wheat2", size=3) +
  annotate("segment", x = 181, xend = 181, y = 12, yend = 18, colour = "wheat4", size=3) +
  #geom_line(data=legend_data, aes(x=x,y=y)) +
  annotate("segment", x = 183, xend = 185, y = 17.7, yend = 17.7, colour = "wheat4", size=.5) +
  annotate("segment", x = 183, xend = 185, y = 12.2, yend = 12.2, colour = "wheat4", size=.5) +
  annotate("segment", x = 185, xend = 185, y = 12.2, yend = 17.7, colour = "wheat4", size=.5) +
  annotate("text", x = 196, y = 14.75, label = "NORMAL RANGE", size=2, colour="gray30") +
  #annotate("text", x = 162, y = 14.75, label = "2018 TEMPERATURE", size=2, colour="gray30") +
  annotate("text", x = 193, y = 25, label = "RECORD HIGH", size=2, colour="gray30") +
  annotate("text", x = 193, y = 5, label = "RECORD LOW", size=2, colour="gray30")

p
