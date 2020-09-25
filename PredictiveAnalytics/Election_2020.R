setwd("C:/Users/johna/Dropbox/Programming/PredictiveAnalytics")

library(ggplot2)

x <- read.csv(url("https://projects.fivethirtyeight.com/2020-general-data/presidential_national_toplines_2020.csv"))
election <- as.data.frame(x)
election2 <- subset(election, select = c(modeldate, ev_inc, ev_chal,
                                         national_voteshare_inc,
                                         national_voteshare_chal))
colnames(election2) <- c("Date","ElectoralTrump","ElectoralBiden",
                         "PopularTrump","PopularBiden")
election2$Date <- as.Date(election2$Date, "%m/%d/%Y")
head(election2)

ggplot(data = election2) +
  geom_line(aes(x = Date, y = ElectoralBiden, group=1,
                color = "ElectoralBiden"), size = 2) +
  geom_text(aes(x = max(Date), y = max(ElectoralBiden),
                label = round(max(ElectoralBiden), 0))) +
  geom_line(aes(x = Date, y = ElectoralTrump, group=1,
                color = "ElectoralTrump"), size = 2) +
  geom_text(aes(x = max(Date), y = max(ElectoralTrump),
                label = round(max(ElectoralTrump), 0))) +
  labs(x="Date", y="Electoral Votes",
       title="Election 2020 - Electoral Vote",
       caption = "R language script: @j363j  Data source: FiveThirtyEight.com") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_colour_manual(name = " ",
                      values=c("ElectoralBiden"="blue",
                               "ElectoralTrump"="red"),
                      labels=c("Biden", "Trump"))

ggplot(data = election2) +
  geom_line(aes(x = Date, y = PopularBiden, group=1,
                color = "PopularBiden"), size = 2) +
  geom_text(aes(x = max(Date), y = max(PopularBiden),
                label = round(max(PopularBiden), 0))) +
  geom_line(aes(x = Date, y = PopularTrump, group=1,
                color = "PopularTrump"), size = 2) +
  geom_text(aes(x = max(Date), y = max(PopularTrump),
                label = round(max(PopularTrump), 0))) +
  labs(x="Date", y="Popular Votes",
       title="Election 2020 - Popular Vote",
       caption = "R language script: @j363j  Data source: FiveThirtyEight.com") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_colour_manual(name = " ",
                      values=c("PopularBiden"="blue",
                               "PopularTrump"="red"),
                      labels=c("Biden", "Trump"))
