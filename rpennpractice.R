# rPractice
###### 06/11/2020
###### Mercedes Horn



### Packages

library(gapminder)
library(dplyr)
library(ggplot2)
library(tidyr)



### Question 1

###### checking out the dataset
gapminder 

###### work
range(gapminder[3])

####### answer
#### 1952 to 2007, ie 55 years
 


### Question 2

### checking out what this does
gapminder %>%  
  print(n = 20) %>%  
  summary()

###### work
gapminder[1:20,] 

###### answer
print(gapminder[1:20,])
summary(gapminder)



### Question 3  

###### work
n_distinct(gapminder$continent)

###### answer
gapminder %>%
     filter(year == 2007) %>%
     group_by(continent) %>%
     summarise(lecMean=mean(lifeExp)) %>% 
     print()

```
# A tibble: 5 x 2
continent lecMean
<fct>       <dbl>
  1 Africa       54.8
2 Americas     73.6
3 Asia         70.7
4 Europe       77.6
5 Oceania      80.7

```



### Question 4

###### answer
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

###### answer
gapminder %>%    # looking wrong
  group_by(country) %>%
  summarise(maxpop=max(pop), witchy=which())
gapminder %>%    # looking wrong
  group_by(country) %>%
  summarise(maxpopyear=year[which(pop == max(pop))])



### Question 6

###### answer
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

###### work
library(babynames)

###### answer 1
babynames %>%
filter(name == "Alex") %>% 
ggplot(aes(year, prop)) +
  geom_point()+
  xlab("Time") +
  ylab("Proportion") +
  ggtitle("Use Of The Name Alex Over Time")+
  facet_wrap(~ sex)

###### answer 2
gapminder %>%
  filter(continent == "Europe") %>%
  ggplot(aes(year,pop)) +
  geom_point() +
  facet_wrap( ~ country )


  
























