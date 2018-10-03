Homework 03: Use dplyr/ggplot2 to manipulate and explore data
================
Aidan Hughes
October 1, 2018

We were given a list of tasks to choose three from and complete. Here are my choices:

-   *"How is life expectancy changing over time on different continents?"*

-   *"Get the maximum and minimum of GDP per capita for all continents."*

-   *"Task 3"*

Don't forget to include our packages:

``` r
suppressPackageStartupMessages(library("gapminder"))
suppressPackageStartupMessages(library("tidyverse"))
```

Task 1: "How is life expectancy changing over time on different continents?"
----------------------------------------------------------------------------

To see how life expectancy over time, let's find the mean life expectancy of each continent for each year we have data for. We'll print a sample of the data in a table since printing all the data would be a little overwhelming.

``` r
lifeExpOverTime <- gapminder %>%
  select(year, continent, lifeExp) %>%
  group_by(continent, year) %>%
  mutate(meanLifeExp = mean(lifeExp), sdLifeExp = sd(lifeExp)) %>%
  select(year, continent, meanLifeExp, sdLifeExp)

# Sample a single row for each of the continents
lifeExpOverTime %>%
  ungroup() %>%
  distinct(continent, .keep_all = TRUE) %>%
  knitr::kable()
```

|  year| continent |  meanLifeExp|  sdLifeExp|
|-----:|:----------|------------:|----------:|
|  1952| Asia      |     46.31439|  9.2917507|
|  1952| Europe    |     64.40850|  6.3610883|
|  1952| Africa    |     39.13550|  5.1515814|
|  1952| Americas  |     53.27984|  9.3260819|
|  1952| Oceania   |     69.25500|  0.1909188|

A simple line plot of mean life expectancy vs. year will give us an idea of what's happening over time.

``` r
lifeExpPlot <- ggplot(lifeExpOverTime, aes(y = meanLifeExp, x = year, color = continent, group = continent)) +
  ggtitle("Life Expectancy Vs. Year") +
  
  #Center the title, left-aligned by default
  theme(plot.title = element_text(hjust = 0.5))

lifeExpPlot +
  geom_line()
```

![](hw03-aidanh14_files/figure-markdown_github/line%20plot-1.png)

Plotting the mean only tells us part of the story. What about the standard devation of the life expectancy? We can try representing this as error bars.

``` r
lifeExpPlot +
  geom_line() +
  geom_errorbar(aes(ymin = meanLifeExp - sdLifeExp,
                    ymax = meanLifeExp + sdLifeExp))
```

![](hw03-aidanh14_files/figure-markdown_github/line%20with%20error%20bars-1.png)

The plot is getting a little messy now. A ribbon plot can tell us the same information with less clutter.

``` r
lifeExpPlot +
  geom_ribbon(aes(ymin = meanLifeExp - sdLifeExp,
                  ymax = meanLifeExp + sdLifeExp,
                  fill=continent), alpha=0.2)
```

![](hw03-aidanh14_files/figure-markdown_github/ribbon%20plot-1.png)

Task 2: "Get the maximum and minimum of GDP per capita for all continents."
---------------------------------------------------------------------------

Start by searching through all the data to find the minimum and maximum GDP per capita of each continent.

``` r
gdpPerCapMaxMin <- gapminder %>%
  select(continent, gdpPercap) %>%
  group_by(continent) %>%
  mutate(mingdpPerCap = min(gdpPercap), maxgdpPerCap = max(gdpPercap)) %>%
  distinct(continent, mingdpPerCap, maxgdpPerCap)

knitr::kable(gdpPerCapMaxMin)
```

| continent |  mingdpPerCap|  maxgdpPerCap|
|:----------|-------------:|-------------:|
| Asia      |      331.0000|     113523.13|
| Europe    |      973.5332|      49357.19|
| Africa    |      241.1659|      21951.21|
| Americas  |     1201.6372|      42951.65|
| Oceania   |    10039.5956|      34435.37|

We can go further and find which country in each continent had the minimum and maximum GDP per capita, along with which year.

``` r
countries <- gapminder %>%
  select(country, year, continent, gdpPercap) %>%
  filter(gdpPercap %in% gdpPerCapMaxMin$mingdpPerCap |
         gdpPercap %in% gdpPerCapMaxMin$maxgdpPerCap) %>%
  mutate(MinOrMax = ifelse(gdpPercap %in% gdpPerCapMaxMin$mingdpPerCap, "min", "max")) %>%
  arrange(continent, desc(MinOrMax))

knitr::kable(countries)
```

| country                |  year| continent |    gdpPercap| MinOrMax |
|:-----------------------|-----:|:----------|------------:|:---------|
| Congo, Dem. Rep.       |  2002| Africa    |     241.1659| min      |
| Libya                  |  1977| Africa    |   21951.2118| max      |
| Haiti                  |  2007| Americas  |    1201.6372| min      |
| United States          |  2007| Americas  |   42951.6531| max      |
| Myanmar                |  1952| Asia      |     331.0000| min      |
| Kuwait                 |  1957| Asia      |  113523.1329| max      |
| Bosnia and Herzegovina |  1952| Europe    |     973.5332| min      |
| Norway                 |  2007| Europe    |   49357.1902| max      |
| Australia              |  1952| Oceania   |   10039.5956| min      |
| Australia              |  2007| Oceania   |   34435.3674| max      |

We can plot the max and min GDP per capita of each continent conveniently in a bar graph.

``` r
ggplot(countries, aes(x = continent, y = gdpPercap, fill = MinOrMax)) +
  geom_col(position = "dodge") +
  
  # Labelling
  geom_text(aes(label = country, vjust = -0.5), size = 2, fontface="bold") +
  labs(y = "GDP Per Capita", x = "Continent", fill = "min. or max. \n GDP") +
  
  # ggplot automatically adds a legend for size and alpha, need to remove them
  guides(size=FALSE, alpha=FALSE) +
  
  ggtitle("Maximum and Minimum \nGDP Per Capita") +
  
  # Center the title, left-aligned by default
  theme(plot.title = element_text(hjust = 0.5))
```

![](hw03-aidanh14_files/figure-markdown_github/bar%20plot-1.png)

Task 3:
-------
