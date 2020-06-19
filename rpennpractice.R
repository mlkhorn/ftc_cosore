# rpennpractice
# 06/11/2020
# Mercedes Horn



### Packages

library(gapminder)
library(dplyr)
library(ggplot2)
library(tidyr)



### Question 1

# work
range(gapminder[3])

# answer
#### 1952 to 2007, ie 56 years
 


### Question 2

# answer
print(gapminder,n=20)
summary(gapminder)



### Question 3  

# answer
gapminder %>%
     filter(year == 2007) %>%
     group_by(continent) %>%
     summarise(lecMean=weighted.mean(lifeExp,pop)) %>% 
     print()



### Question 4

# answer
gapminder %>%
  filter(country == "China") %>% 
  select(year, lifeExp) %>%
  ggplot(aes(x=year,y=lifeExp)) +
  geom_point(color="blue")+
  xlab("Time (years)")+
  ylab("Mean Life Expectancy (years)")+
  ggtitle("Change in Chinese Life Expectancy Over Time")+
  theme_minimal()



### Question 5

# answer
gapminder %>%    
  group_by(country) %>%
  summarise(maxpopyear=year[which(pop == max(pop))])



### Question 6

# answer
gapminder %>%
  filter(year == 1967) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, colour=continent)) +
  geom_point( aes(size=pop)) +
  xlab("GDP per capita") +
  ylab("life expectancy") +
  ggtitle("Year 1967") +
  labs(subtitle="Gapminder Dataset")+
  scale_x_log10()

 

### Question 7 

# work
library(babynames)

# answer 1
babynames %>%
filter(name == "Alex") %>% 
ggplot(aes(year, prop)) +
  geom_point()+
  xlab("Time") +
  ylab("Proportion") +
  ggtitle("Use Of The Name Alex Over Time")+
  facet_wrap(~ sex)

# answer 2
gapminder %>%
  filter(continent == "Europe") %>%
  ggplot(aes(year,pop)) +
  geom_point() +
  facet_wrap( ~ country )


  
























