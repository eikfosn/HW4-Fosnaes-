library(tidyverse)
library(readr)
library(dplyr)

murders <- read_csv("raw_data/murders.csv")

#only using year 1995
murders_95 <- filter(murders, year == 1995)

summary(murders_95)

hist(murders_95$murders)

cor(murders_95[c('murders', 'arrests', 'popul', 'murdrate', 'arrestrate', 'percblack')])

#part 1
part1 <- lm(murders ~ perc1019 + perc2029 + percblack + percmale + rpcpersinc, data = murders_95)
summary(part1)

#q5 using murders_95 for standardized residuals and constant variance
murders_95 <- murders_95 %>% 
  mutate(
    stand_res = rstandard(part1)
  )
murders_95 %>% ggplot(aes(x = stand_res)) +
  geom_histogram(bins = 400) + xlab("Standardized residuals")


murders_95$fitted <- part1$fitted.values
murders_95$residuals <- part1$residuals
murders_95 %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point()

#part2
part2 <- lm(murdrate ~ perc1019 + perc2029 + percblack + percmale + rpcpersinc, data = murders_95)
summary(part2)

#q6 using murders_95 and murders per 10,000 people for standardized residuals and constant variance
murders_95 <- murders_95 %>% 
  mutate(
    stand_res = rstandard(part2)
  )
murders_95 %>% ggplot(aes(x = stand_res)) +
  geom_histogram(bins = 300) + xlab("Standardized residuals")


murders_95$fitted <- part2$fitted.values
murders_95$residuals <- part2$residuals
murders_95 %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point()

