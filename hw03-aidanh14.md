hw03-aidanh14
================
Aidan Hughes
October 1, 2018

Homework 03: Use dplyr/ggplot2 to manipulate and explore data
=============================================================

We were given a list of tasks to choose three from and complete. Here are my choices:

-   *"How is life expectancy changing over time on different continents?"*

-   *"Task 2"*

-   *"Task 3"*

Don't forget to include our packages:

``` r
suppressPackageStartupMessages(library("gapminder"))
suppressPackageStartupMessages(library("tidyverse"))
```

"How is life expectancy changing over time on different continents?"
--------------------------------------------------------------------

To see how life expectancy over time, let's find the mean life expectancy of each continent for each year we have data for.

``` r
lifeExpOverTime <- gapminder %>%
  select(year, continent, lifeExp) %>%
  group_by(continent, year) %>%
  mutate(meanLifeExp = mean(lifeExp), sdLifeExp = sd(lifeExp)) %>%
  select(year, continent, meanLifeExp, sdLifeExp)

lifeExpOverTime
```

    ## # A tibble: 1,704 x 4
    ## # Groups:   continent, year [60]
    ##     year continent meanLifeExp sdLifeExp
    ##    <int> <fct>           <dbl>     <dbl>
    ##  1  1952 Asia             46.3      9.29
    ##  2  1957 Asia             49.3      9.64
    ##  3  1962 Asia             51.6      9.82
    ##  4  1967 Asia             54.7      9.65
    ##  5  1972 Asia             57.3      9.72
    ##  6  1977 Asia             59.6     10.0 
    ##  7  1982 Asia             62.6      8.54
    ##  8  1987 Asia             64.9      8.20
    ##  9  1992 Asia             66.5      8.08
    ## 10  1997 Asia             68.0      8.09
    ## # ... with 1,694 more rows

A simple line plot of mean life expectancy vs. year will give us an idea of what's happening over time.

``` r
lifeExpPlot <- ggplot(lifeExpOverTime, aes(y = meanLifeExp, x = year, color=continent, group=continent)) 

lifeExpPlot +
  geom_line()
```

![](hw03-aidanh14_files/figure-markdown_github/line%20plot-1.png)

Plotting the mean only tells us part of the story. What about the standard devation of the life expectancy? We can try representing this as error bars.

``` r
lifeExpPlot +
  geom_line() +
  geom_errorbar(aes(ymin = meanLifeExp - sdLifeExp, ymax = meanLifeExp + sdLifeExp))
```

![](hw03-aidanh14_files/figure-markdown_github/line%20with%20error%20bars-1.png)

The plot is getting a little messy now. A ribbon plot can tell us the same information with less clutter.

``` r
lifeExpPlot +
  geom_ribbon(aes(ymin = meanLifeExp - sdLifeExp, ymax = meanLifeExp + sdLifeExp, fill=continent), alpha=0.2)
```

![](hw03-aidanh14_files/figure-markdown_github/ribbon%20plot-1.png)

Now *that's* a clean plot that gives us all the information we need.
