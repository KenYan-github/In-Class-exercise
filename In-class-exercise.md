in-class exercise
================
Xingjian Yan
2/26/2021

\#\#\#\#\#1.Make a table that describes each plane. It should have a
column for tailnum, another column for average arrival delay, and
another for the year the plane was manufactured.

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.6.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(nycflights13)
flights %>% 
  group_by(tailnum) %>%
  summarize(missing_delay = mean(arr_delay, na.rm = T)) %>%
  left_join(planes) %>%
  select(tailnum:year) %>%
  head(10)
```

    ## Joining, by = "tailnum"

    ## # A tibble: 10 x 3
    ##    tailnum missing_delay  year
    ##    <chr>           <dbl> <int>
    ##  1 D942DN         31.5      NA
    ##  2 N0EGMQ          9.98     NA
    ##  3 N10156         12.7    2004
    ##  4 N102UW          2.94   1998
    ##  5 N103US         -6.93   1999
    ##  6 N104UW          1.80   1999
    ##  7 N10575         20.7    2002
    ##  8 N105UW         -0.267  1999
    ##  9 N107US         -5.73   1999
    ## 10 N108UW         -1.25   1999

\#\#\#\#\#2.Make a table where each row is a day of the year. The first
column is the date. The 2:4 columns give the number of (scheduled)
departures from EWR, LGA, and JFK.

``` r
library(tidyr)
flights %>%
  mutate(date = as.Date(time_hour)) %>%
  group_by(origin, date) %>%
  summarize(deps = n()) %>%
  pivot_wider(names_from = origin, values_from = deps) %>%
  head(10)
```

    ## `summarise()` has grouped output by 'origin'. You can override using the `.groups` argument.

    ## # A tibble: 10 x 4
    ##    date         EWR   JFK   LGA
    ##    <date>     <int> <int> <int>
    ##  1 2013-01-01   255   236   218
    ##  2 2013-01-02   351   319   260
    ##  3 2013-01-03   336   320   261
    ##  4 2013-01-04   340   319   258
    ##  5 2013-01-05   262   303   203
    ##  6 2013-01-06   272   309   203
    ##  7 2013-01-07   348   307   277
    ##  8 2013-01-08   336   291   276
    ##  9 2013-01-09   336   289   279
    ## 10 2013-01-10   341   302   282

\#\#\#\#\#3.Make a table where each row is a day of the year. Each
destination airport is a column. The elements (day x destination) give
the number of flights to that destination. What should NA’s be?

``` r
flights %>%
  mutate(date = as.Date(time_hour)) %>%
  group_by(date,dest) %>%
  summarize(deps = n()) %>%
  pivot_wider(names_from = dest, values_from = deps) %>% head(10)
```

    ## `summarise()` has grouped output by 'date'. You can override using the `.groups` argument.

    ## # A tibble: 10 x 106
    ## # Groups:   date [10]
    ##    date         ALB   ATL   AUS   AVL   BDL   BNA   BOS   BQN   BTV   BUF   BUR
    ##    <date>     <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ##  1 2013-01-01     2    38     4     1     1     8    20     1     5    12     2
    ##  2 2013-01-02     3    48     7     1     2    11    28     3     9    15     2
    ##  3 2013-01-03     2    50     7    NA     2    11    27     3     6    14     2
    ##  4 2013-01-04     2    48     6    NA     1    11    30     3     7    13     2
    ##  5 2013-01-05     2    36     4    NA     2    10    23     3     8    15     2
    ##  6 2013-01-06     2    40     5    NA     1    11    26     3     7    12     2
    ##  7 2013-01-07     2    49     6    NA     1    14    44     3     7    14     1
    ##  8 2013-01-08     2    47     5    NA     1    14    48     3     7    14     1
    ##  9 2013-01-09     2    47     6    NA     1    14    49     3     7    13     1
    ## 10 2013-01-10     2    48     6    NA     1    14    48     3     8    13     1
    ## # … with 94 more variables: BWI <int>, CAK <int>, CHS <int>, CLE <int>,
    ## #   CLT <int>, CMH <int>, CRW <int>, CVG <int>, DAY <int>, DCA <int>,
    ## #   DEN <int>, DFW <int>, DTW <int>, EGE <int>, FLL <int>, GRR <int>,
    ## #   GSO <int>, GSP <int>, HNL <int>, HOU <int>, IAD <int>, IAH <int>,
    ## #   IND <int>, JAC <int>, JAX <int>, LAS <int>, LAX <int>, LGB <int>,
    ## #   MCI <int>, MCO <int>, MDW <int>, MEM <int>, MHT <int>, MIA <int>,
    ## #   MKE <int>, MSN <int>, MSP <int>, MSY <int>, MYR <int>, OAK <int>,
    ## #   OMA <int>, ORD <int>, ORF <int>, PBI <int>, PDX <int>, PHL <int>,
    ## #   PHX <int>, PIT <int>, PWM <int>, RDU <int>, RIC <int>, ROC <int>,
    ## #   RSW <int>, SAN <int>, SAT <int>, SAV <int>, SDF <int>, SEA <int>,
    ## #   SFO <int>, SJC <int>, SJU <int>, SLC <int>, SMF <int>, SNA <int>,
    ## #   SRQ <int>, STL <int>, STT <int>, SYR <int>, TPA <int>, XNA <int>,
    ## #   DSM <int>, OKC <int>, PSE <int>, PVD <int>, TUL <int>, TYS <int>,
    ## #   BHM <int>, CAE <int>, BZN <int>, EYW <int>, HDN <int>, MTJ <int>,
    ## #   PSP <int>, BGR <int>, CHO <int>, ABQ <int>, ACK <int>, MVY <int>,
    ## #   TVC <int>, ANC <int>, LGA <int>, SBN <int>, ILM <int>, LEX <int>

*NAs should be zeros, it means that no flights at that day flied to the
destination.*
