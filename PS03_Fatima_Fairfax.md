---
title: "PS03"
author: "Fatima Fairfax"
date: "9/4/21"
output:
  html_document:
    keep_md: true
---

##PS03: Data Wrangling

To set up, I'm running the relevant packages:

```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

Next I will load and take a look at the data we're using:

```r
data(txhousing)
glimpse(txhousing)
```

```
## Rows: 8,602
## Columns: 9
## $ city      <chr> "Abilene", "Abilene", "Abilene", "Abilene", "Abilene", "Abil…
## $ year      <int> 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, …
## $ month     <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, …
## $ sales     <dbl> 72, 98, 130, 98, 141, 156, 152, 131, 104, 101, 100, 92, 75, …
## $ volume    <dbl> 5380000, 6505000, 9285000, 9730000, 10590000, 13910000, 1263…
## $ median    <dbl> 71400, 58700, 58100, 68600, 67300, 66900, 73500, 75000, 6450…
## $ listings  <dbl> 701, 746, 784, 785, 794, 780, 742, 765, 771, 764, 721, 658, …
## $ inventory <dbl> 6.3, 6.6, 6.8, 6.9, 6.8, 6.6, 6.2, 6.4, 6.5, 6.6, 6.2, 5.7, …
## $ date      <dbl> 2000.000, 2000.083, 2000.167, 2000.250, 2000.333, 2000.417, …
```


**Question 1**

The last city listed in the data set is Wichita Falls


**Question 2**

The variable 'listings' correspond to the total active listings


Now I'm extracting the variables sales and volume:

```r
txhousing |> 
  select(sales,volume)
```

```
## # A tibble: 8,602 × 2
##    sales   volume
##    <dbl>    <dbl>
##  1    72  5380000
##  2    98  6505000
##  3   130  9285000
##  4    98  9730000
##  5   141 10590000
##  6   156 13910000
##  7   152 12635000
##  8   131 10710000
##  9   104  7615000
## 10   101  7040000
## # … with 8,592 more rows
```

Now I'm looking at the data set with all variables *except* the date:

```r
txhousing <- txhousing |> 
  select(-date)
```

Filter to pull out specific rows based on criteria in the columns. First looking at all houses in the year 2012 in Austin:

```r
austin_12 <- txhousing |> 
  filter(year == 2012, city == "Austin")
```


Now all houses in Austin before the year 2004:

```r
txhousing |> 
  filter(year < 2004, city == "Austin")
```

```
## # A tibble: 48 × 8
##    city    year month sales    volume median listings inventory
##    <chr>  <int> <int> <dbl>     <dbl>  <dbl>    <dbl>     <dbl>
##  1 Austin  2000     1  1025 173053635 133700     3084       2  
##  2 Austin  2000     2  1277 226038438 134000     2989       2  
##  3 Austin  2000     3  1603 298557656 136700     3042       2  
##  4 Austin  2000     4  1556 289197960 136900     3192       2.1
##  5 Austin  2000     5  1980 393073774 144700     3617       2.3
##  6 Austin  2000     6  1885 368290072 148800     3799       2.4
##  7 Austin  2000     7  1818 351539312 149300     3944       2.6
##  8 Austin  2000     8  1880 360255090 146300     3948       2.6
##  9 Austin  2000     9  1498 292799874 148700     4058       2.6
## 10 Austin  2000    10  1524 300952544 150100     4100       2.6
## # … with 38 more rows
```

Now houses in Austin or Abilene:

```r
aust_ab <- txhousing |> 
  filter(city == "Austin" | city == "Abilene")
```


Now I'll mutate to add a new variable that indicates volume in units of 100k:

```r
txhousing <- txhousing |> 
  mutate(vol_100k = volume/100000)
```

Now I'll provide summary stats for the Austin 2012 data to see the mean of sales:

```r
austin_12 |> summarize(x_bar_sales = mean(sales))
```

```
## # A tibble: 1 × 1
##   x_bar_sales
##         <dbl>
## 1       2127.
```

Now to look at a bunch of summaries at once:

```r
austin_12 |> summarize(x_bar_sales = mean(sales),
                       sd_sales = sd(sales), 
                        min_vol = min(volume), 
                        max_vol = max(volume), 
                        mdn_list = median(listings), 
                        iqr_list = IQR(listings),
                        sample_size = n())
```

```
## # A tibble: 1 × 7
##   x_bar_sales sd_sales   min_vol   max_vol mdn_list iqr_list sample_size
##         <dbl>    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>       <int>
## 1       2127.     501. 265821275 791281075     7925     949.          12
```

Now I can arrange the data by the volume column:

```r
austin_12 |> 
  arrange(desc(volume))
```

```
## # A tibble: 12 × 8
##    city    year month sales    volume median listings inventory
##    <chr>  <int> <int> <dbl>     <dbl>  <dbl>    <dbl>     <dbl>
##  1 Austin  2012     6  2837 791281075 216000     8641       4.5
##  2 Austin  2012     7  2604 718755768 211000     8519       4.3
##  3 Austin  2012     8  2647 708540314 205100     8112       4  
##  4 Austin  2012     5  2611 705383898 210200     8465       4.5
##  5 Austin  2012    10  2173 585049910 197100     7091       3.4
##  6 Austin  2012     4  2128 563288160 207400     8239       4.5
##  7 Austin  2012    12  2014 562680422 211500     5737       2.7
##  8 Austin  2012     3  2083 533800484 198600     8186       4.5
##  9 Austin  2012     9  2000 516045542 196800     7641       3.8
## 10 Austin  2012    11  1827 485197329 198200     6425       3.1
## 11 Austin  2012     2  1415 353527608 191600     7738       4.3
## 12 Austin  2012     1  1182 265821275 177400     7432       4.2
```

Finally, I can use group_by to look at data across levels of a variable. In this case, looking at the average number of monthly sales in Austin and Abilene across all years:

```r
aust_ab |> group_by(city) |> 
  summarize(x_bar_sales = mean(sales))
```

```
## # A tibble: 2 × 2
##   city    x_bar_sales
##   <chr>         <dbl>
## 1 Abilene        150.
## 2 Austin        1997.
```

I can also group_by multiple variables. This will show the average sales for each city for each month:

```r
aust_ab |> group_by(city, month) |> 
  summarize(x_bar_sales = mean(sales))
```

```
## `summarise()` has grouped output by 'city'. You can override using the `.groups` argument.
```

```
## # A tibble: 24 × 3
## # Groups:   city [2]
##    city    month x_bar_sales
##    <chr>   <int>       <dbl>
##  1 Abilene     1        96.3
##  2 Abilene     2       121  
##  3 Abilene     3       151. 
##  4 Abilene     4       160. 
##  5 Abilene     5       178. 
##  6 Abilene     6       190. 
##  7 Abilene     7       184. 
##  8 Abilene     8       181. 
##  9 Abilene     9       144. 
## 10 Abilene    10       144. 
## # … with 14 more rows
```


**Question 3**

I'll remove the inventory variable and overwrite the txhousing dataframe:

```r
txhousing <- txhousing |> select(-inventory)
```


**Question 4**

Now I'll make a subset of the data for Dallas in 2012 and 2013:

```r
dallas_sub <- txhousing |> filter(city == "Dallas", year == 2012 | year == 2013)
dallas_sub
```

```
## # A tibble: 24 × 8
##    city    year month sales     volume median listings vol_100k
##    <chr>  <int> <int> <dbl>      <dbl>  <dbl>    <dbl>    <dbl>
##  1 Dallas  2012     1  2555  509458081 150800    16721    5095.
##  2 Dallas  2012     2  3085  634067291 157100    17173    6341.
##  3 Dallas  2012     3  4068  898320563 167300    17433    8983.
##  4 Dallas  2012     4  4291  983333297 168700    17632    9833.
##  5 Dallas  2012     5  5004 1175419749 175100    17726   11754.
##  6 Dallas  2012     6  5196 1209024869 177900    17587   12090.
##  7 Dallas  2012     7  4859 1117428758 176000    17314   11174.
##  8 Dallas  2012     8  5264 1168296112 174200    16611   11683.
##  9 Dallas  2012     9  4151  921678400 173500    15994    9217.
## 10 Dallas  2012    10  4214  885330428 166700    15008    8853.
## # … with 14 more rows
```


**Question 5**

Now adding a new column to dallas_sub that calculated the percentage of listing sold:

```r
dallas_sub <- dallas_sub |>
  mutate(prct_sold = (sales/listings * 100))
dallas_sub
```

```
## # A tibble: 24 × 9
##    city    year month sales     volume median listings vol_100k prct_sold
##    <chr>  <int> <int> <dbl>      <dbl>  <dbl>    <dbl>    <dbl>     <dbl>
##  1 Dallas  2012     1  2555  509458081 150800    16721    5095.      15.3
##  2 Dallas  2012     2  3085  634067291 157100    17173    6341.      18.0
##  3 Dallas  2012     3  4068  898320563 167300    17433    8983.      23.3
##  4 Dallas  2012     4  4291  983333297 168700    17632    9833.      24.3
##  5 Dallas  2012     5  5004 1175419749 175100    17726   11754.      28.2
##  6 Dallas  2012     6  5196 1209024869 177900    17587   12090.      29.5
##  7 Dallas  2012     7  4859 1117428758 176000    17314   11174.      28.1
##  8 Dallas  2012     8  5264 1168296112 174200    16611   11683.      31.7
##  9 Dallas  2012     9  4151  921678400 173500    15994    9217.      26.0
## 10 Dallas  2012    10  4214  885330428 166700    15008    8853.      28.1
## # … with 14 more rows
```

**Question 6**

Now calculating the average percentage of listing sold in each month of the year:

```r
dallas_summary <- dallas_sub |>
  group_by(month) |> 
  summarize(avg_prct_sold = mean(prct_sold))
dallas_summary
```

```
## # A tibble: 12 × 2
##    month avg_prct_sold
##    <int>         <dbl>
##  1     1          20.5
##  2     2          23.5
##  3     3          32.2
##  4     4          34.5
##  5     5          38.2
##  6     6          37.2
##  7     7          37.1
##  8     8          38.5
##  9     9          31.8
## 10    10          32.1
## 11    11          30.6
## 12    12          35.5
```


**Question 7**

Arranging the dallas_summary in desc order based on avg percent to show which had the largest. August (8) had the highest.

```r
dallas_summary |> arrange(desc(avg_prct_sold))
```

```
## # A tibble: 12 × 2
##    month avg_prct_sold
##    <int>         <dbl>
##  1     8          38.5
##  2     5          38.2
##  3     6          37.2
##  4     7          37.1
##  5    12          35.5
##  6     4          34.5
##  7     3          32.2
##  8    10          32.1
##  9     9          31.8
## 10    11          30.6
## 11     2          23.5
## 12     1          20.5
```

**Question 8**


```r
txhousing |>
  filter(year == 2012 | year == 2013, city == "Dallas") |> 
  mutate(prct_sold = sales/listings*100) |> 
  group_by(month) |> 
  summarize(mean_prct_sold = mean(prct_sold)) |> 
  arrange(desc(mean_prct_sold))
```

```
## # A tibble: 12 × 2
##    month mean_prct_sold
##    <int>          <dbl>
##  1     8           38.5
##  2     5           38.2
##  3     6           37.2
##  4     7           37.1
##  5    12           35.5
##  6     4           34.5
##  7     3           32.2
##  8    10           32.1
##  9     9           31.8
## 10    11           30.6
## 11     2           23.5
## 12     1           20.5
```

The above code did all of the operations I just ran separately above. Specifically, it took the original data frame and (1) filtered it to only include listings in Dallas in 2012 or 2013, (2) added a new column that was percentage of listing sold, (3) grouped the data by month, (4) created a summary value of the average percent sold and, (5) arranged the resulting data in descending order of percent sold.


**Question 9**

In Jan 2015, the city with the fewest houses listed for sale was: San Marcos

```r
txhousing |>
  filter(year == 2015, month == 1) |>
  arrange(listings)
```

```
## # A tibble: 46 × 8
##    city             year month sales   volume median listings vol_100k
##    <chr>           <int> <int> <dbl>    <dbl>  <dbl>    <dbl>    <dbl>
##  1 San Marcos       2015     1    18  3376703 150000       85     33.8
##  2 Garland          2015     1   114 17591250 135800      198    176. 
##  3 Irving           2015     1    82 20208733 157800      278    202. 
##  4 Victoria         2015     1    54 10362907 172500      280    104. 
##  5 Nacogdoches      2015     1    20  3222150 140000      284     32.2
##  6 Paris            2015     1    25  3611892 123300      299     36.1
##  7 Brazoria County  2015     1    69 10382931 146000      301    104. 
##  8 Odessa           2015     1    63 10040640 156200      308    100. 
##  9 Texarkana        2015     1    75  9330521 101400      317     93.3
## 10 Lufkin           2015     1    37  6865950 134000      404     68.7
## # … with 36 more rows
```

**Question 10**

In 2012, the month where the most houses were sold was: August (8)

```r
txhousing |>
  filter(year == 2012) |> 
  group_by(month) |> 
  summarize(month_sales = sum(sales)) |> 
  arrange(desc(month_sales))
```

```
## # A tibble: 12 × 2
##    month month_sales
##    <int>       <dbl>
##  1     8       29995
##  2     6       29574
##  3     5       28348
##  4     7       28269
##  5    10       24259
##  6     4       23954
##  7     9       23256
##  8     3       22934
##  9    12       22457
## 10    11       22018
## 11     2       17689
## 12     1       14299
```

**Question 11**

Now generate a single table showing number of houses sold in Austin in 2000 and 2001 and the total of houses sold in Dallas in 2000 and 2001.

To do this, first I will select the relevant variables and filter for the cities and years I need to analyze: 

```r
aus_dall_0001 <- txhousing |>
  select(city, year,sales) |> 
  filter(city == "Austin" | city == "Dallas", year == 2000 | year == 2001)
aus_dall_0001
```

```
## # A tibble: 48 × 3
##    city    year sales
##    <chr>  <int> <dbl>
##  1 Austin  2000  1025
##  2 Austin  2000  1277
##  3 Austin  2000  1603
##  4 Austin  2000  1556
##  5 Austin  2000  1980
##  6 Austin  2000  1885
##  7 Austin  2000  1818
##  8 Austin  2000  1880
##  9 Austin  2000  1498
## 10 Austin  2000  1524
## # … with 38 more rows
```

Then I will group_by city and sum up the sales:

```r
aus_dall_0001 <- aus_dall_0001 |> 
  group_by(city, year) |> 
  summarize(years_summed = sum(sales))
```

```
## `summarise()` has grouped output by 'city'. You can override using the `.groups` argument.
```

```r
aus_dall_0001
```

```
## # A tibble: 4 × 3
## # Groups:   city [2]
##   city    year years_summed
##   <chr>  <int>        <dbl>
## 1 Austin  2000        18621
## 2 Austin  2001        18392
## 3 Dallas  2000        45446
## 4 Dallas  2001        46992
```

Now I'll try grouping by city to combine the years:

```r
aus_dall_0001 |> 
  group_by(city) |> 
  summarize(total_homes_sold = sum(years_summed))
```

```
## # A tibble: 2 × 2
##   city   total_homes_sold
##   <chr>             <dbl>
## 1 Austin            37013
## 2 Dallas            92438
```

This makes me think I could have just grouped by city above to get to this table. since I already selected out for relevant cities and years. I'll try it that way below to see if that's right:


```r
aus_dall_v2 <- txhousing |>
  select(city, year,sales) |> 
  filter(city == "Austin" | city == "Dallas", year == 2000 | year == 2001) |> 
  group_by(city) |> 
  summarize(total_homes_sold = sum(sales))
aus_dall_v2
```

```
## # A tibble: 2 × 2
##   city   total_homes_sold
##   <chr>             <dbl>
## 1 Austin            37013
## 2 Dallas            92438
```

Yay! That worked. (Assuming these values are correct)


**Ignore Me: changing slightly to see if I can do a new save**
