Data Manipulation using dplyr
================

# Data Manipulation using **`dplyr`**

> Learning Objectives
> 
>   - Select columns in a data frame with the **`dplyr`** function
>     `select`.
> 
>   - Select rows in a data frame according to filtering conditions with
>     the **`dplyr`** function `filter`.
> 
>   - Direct the output of one **`dplyr`** function to the input of
>     another function with the ‘pipe’ operator `%>%`.
> 
>   - Add new columns to a data frame that are functions of existing
>     columns with `mutate`.
> 
>   - Understand the split-apply-combine concept for data analysis.
> 
>   - ## Use `summarize`, `group_by`, and `tally` to split a data frame into groups of observations, apply a summary statistics for each group, and then combine the results.

We will be working a small subset of the data from the [Stanford Open
Policing Project](https://openpolicing.stanford.edu). It contains
information about traffic stops for blacks and whites in the state of
Mississippi during January 2013 to mid-July of 2016.

Let’s begin with loading our sample data into a data frame.

``` r
trafficstops <- read.csv("ms_trafficstop_data.csv")
head(trafficstops)
```

Manipulation of dataframes is a common task when you start exploring
your data. We might select certain observations (rows) or variables
(columns), group the data by a certain variable(s), or calculate summary
statistics.

``` r
library(dplyr)
```

    #> 
    #> Attaching package: 'dplyr'

    #> The following objects are masked from 'package:lubridate':
    #> 
    #>     intersect, setdiff, union

    #> The following objects are masked from 'package:stats':
    #> 
    #>     filter, lag

    #> The following objects are masked from 'package:base':
    #> 
    #>     intersect, setdiff, setequal, union

``` r
levels(trafficstops$county_name)
```

    #>  [1] "Adams County"           "Alcorn County"         
    #>  [3] "Amite County"           "Attala County"         
    #>  [5] "Benton County"          "Bolivar County"        
    #>  [7] "Calhoun County"         "Carroll County"        
    #>  [9] "Chickasaw County"       "Choctaw County"        
    #> [11] "Claiborne County"       "Clarke County"         
    #> [13] "Clay County"            "Coahoma County"        
    #> [15] "Copiah County"          "Covington County"      
    #> [17] "DeSoto County"          "Forrest County"        
    #> [19] "Franklin County"        "George County"         
    #> [21] "Greene County"          "Grenada County"        
    #> [23] "Hancock County"         "Harrison County"       
    #> [25] "Hinds County"           "Holmes County"         
    #> [27] "Humphreys County"       "Issaquena County"      
    #> [29] "Itawamba County"        "Jackson County"        
    #> [31] "Jasper County"          "Jefferson County"      
    #> [33] "Jefferson Davis County" "Jones County"          
    #> [35] "Kemper County"          "Lafayette County"      
    #> [37] "Lamar County"           "Lauderdale County"     
    #> [39] "Lawrence County"        "Leake County"          
    #> [41] "Lee County"             "Leflore County"        
    #> [43] "Lincoln County"         "Lowndes County"        
    #> [45] "Madison County"         "Marion County"         
    #> [47] "Marshall County"        "Monroe County"         
    #> [49] "Montgomery County"      "Neshoba County"        
    #> [51] "Newton County"          "Noxubee County"        
    #> [53] "Oktibbeha County"       "Panola County"         
    #> [55] "Pearl River County"     "Perry County"          
    #> [57] "Pike County"            "Pontotoc County"       
    #> [59] "Prentiss County"        "Quitman County"        
    #> [61] "Rankin County"          "Scott County"          
    #> [63] "Sharkey County"         "Simpson County"        
    #> [65] "Smith County"           "Stone County"          
    #> [67] "Sunflower County"       "Tallahatchie County"   
    #> [69] "Tate County"            "Tippah County"         
    #> [71] "Tishomingo County"      "Tunica County"         
    #> [73] "Union County"           "Walthall County"       
    #> [75] "Warren County"          "Washington County"     
    #> [77] "Wayne County"           "Webster County"        
    #> [79] "Wilkinson County"       "Winston County"        
    #> [81] "Yalobusha County"       "Yazoo County"

``` r
sum(is.na(trafficstops$subject_age)) # there are 387 values are missing in age column
```

    #> [1] 387

``` r
str(trafficstops)   
```

    #> 'data.frame':    758412 obs. of  13 variables:
    #>  $ raw_row_number : int  1 2 3 4 5 6 7 8 9 10 ...
    #>  $ date           : Factor w/ 1297 levels "2013-01-01","2013-01-02",..: 317 771 318 845 197 850 322 931 938 812 ...
    #>  $ county_name    : Factor w/ 82 levels "Adams County",..: 61 18 61 53 75 45 61 24 34 26 ...
    #>  $ subject_age    : int  43 42 47 19 29 48 28 45 19 33 ...
    #>  $ subject_race   : Factor w/ 4 levels "asian/pacific islander",..: 2 4 2 4 4 2 4 4 2 2 ...
    #>  $ subject_sex    : Factor w/ 2 levels "female","male": 2 1 1 1 2 1 1 1 2 2 ...
    #>  $ department_id  : int  6103 1801 6103 5302 7501 4504 6103 2401 3401 26 ...
    #>  $ department_name: Factor w/ 365 levels "Abbeville Police Dept",..: 102 125 102 308 340 276 102 26 90 135 ...
    #>  $ type           : Factor w/ 1 level "vehicular": 1 1 1 1 1 1 1 1 1 1 ...
    #>  $ violation      : Factor w/ 68 levels "A35","B07","B08",..: 22 22 9 5 30 30 5 59 30 5 ...
    #>  $ speed          : int  NA NA NA NA NA NA NA 46 NA NA ...
    #>  $ posted_speed   : int  NA NA NA NA NA NA NA 35 NA NA ...
    #>  $ raw_race       : Factor w/ 5 levels "B","I","O","W",..: 1 4 1 4 4 1 4 4 1 1 ...

Change the column names

``` r
trafficstops <- trafficstops %>% rename(drivers_age = subject_age) 
trafficstops <- trafficstops %>% rename(drivers_race = subject_race)
trafficstops <- trafficstops %>% rename(drivers_sex = subject_sex)
trafficstops <- trafficstops %>% rename(Police_department = department_name)
trafficstops <- trafficstops %>% rename(Officer_id = department_id)

head(trafficstops)
```

    #>   raw_row_number       date      county_name drivers_age drivers_race
    #> 1              1 2013-11-13    Rankin County          43        black
    #> 2              2 2015-02-10   Forrest County          42        white
    #> 3              3 2013-11-14    Rankin County          47        black
    #> 4              4 2015-04-25 Oktibbeha County          19        white
    #> 5              5 2013-07-16    Warren County          29        white
    #> 6              6 2015-04-30   Madison County          48        black
    #>   drivers_sex Officer_id       Police_department      type violation speed
    #> 1        male       6103     Flowood Police Dept vehicular       D36    NA
    #> 2      female       1801 Hattiesburg Police Dept vehicular       D36    NA
    #> 3      female       6103     Flowood Police Dept vehicular       B53    NA
    #> 4      female       5302  Starkville Police Dept vehicular       B26    NA
    #> 5        male       7501    Vickburg Police Dept vehicular       F04    NA
    #> 6      female       4504   Ridgeland Police Dept vehicular       F04    NA
    #>   posted_speed raw_race
    #> 1           NA        B
    #> 2           NA        W
    #> 3           NA        B
    #> 4           NA        W
    #> 5           NA        W
    #> 6           NA        B

If we were interested in the mean age of the driver in different
counties we can do this using the normal base R operations:

``` r
result <- trafficstops %>%
          filter(trafficstops$county_name == "Grenada County")
mean(result$drivers_age, na.rm = TRUE)
```

    #> [1] 33.9421

``` r
# na. rm in r refers to the logical parameter that tells the function whether or not to remove NA values from the calculation. It literally means NA remove. ... rm is TRUE, the function skips over any NA values.
```

Bracket subsetting is handy, but it can be cumbersome and difficult to
read, especially for complicated operations. Furthermore, there is a
fair amount of repetition. Repeating yourself will cost you time, both
now and later, and potentially introduce some nasty bugs.

**`dplyr`** is a package for making tabular data manipulation easier.

> Brief recap: Packages in R are sets of additional functions that let
> you do more stuff. Functions like `str()` or `data.frame()`, come
> built into R; packages give you access to more of them. Before you use
> a package for the first time you need to install it on your machine,
> and then you should import it in every subsequent R session when you
> need it. If you haven’t, please installe the **`tidyverse`** package.

``` r
install.packages("tidyverse")    
```

**`tidyverse`** is an “umbrella-package” that installs a series of
packages useful for data analysis which work together well. Some of them
are considered **core** packages (among them **`tidyr`**, **`dplyr`**,
**`ggplot2`**), because you are likely to use them in almost every
analysis. Other packages, like `lubridate` (to work wiht dates) or
`haven` (for SPSS, Stata, and SAS data) that you are likely to use not
for every analysis are also installed.

If you type the following command, it will load the **core** `tidyverse`
packages.

``` r
#library("tidyverse")    ## load the core tidyverse packages, incl. dplyr
```

If you need to use functions from `tidyverse` packages other than the
core packages, you will need to load them separately.

## What is **`dplyr`**?

**`dplyr`** is one part of a larger **`tidyverse`** that enables you to
work with data in tidy data formats. “Tidy datasets are easy to
manipulate, model and visualise, and have a specific structure: each
variable is a column, each observation is a row, and each type of
observational unit is a table.” (From Wickham, H. (2014): Tidy Data
<https://www.jstatsoft.org/article/view/v059i10>)

The package **`dplyr`** provides convenient tools for the most common
data manipulation tasks. It is built to work directly with data frames,
with many common tasks optimized by being written in a compiled language
(C++). An additional feature is the ability to work directly with data
stored in an external database. The benefits of doing this are that the
data can be managed natively in a relational database, queries can be
conducted on that database, and only the results of the query are
returned.

This addresses a common problem with R in that all operations are
conducted in-memory and thus the amount of data you can work with is
limited by available memory. The database connections essentially remove
that limitation in that you can have a database of many 100s GB, conduct
queries on it directly, and pull back into R only what you need for
analysis.

To learn more about **`dplyr`** after the workshop, you may want to
check out the [handy data transformation with **`dplyr`**
cheatsheet](https://github.com/rstudio/cheatsheets/raw/master/data-transformation.pdf).

## Subsetting columns and rows

To select columns of a data frame with `dplyr`, use `select()`. The
first argument to this function is the data frame (`trafficstops`), and
the subsequent arguments are the columns to keep.

``` r
select(trafficstops, Police_department, Officer_id, drivers_race)
```

    #>         Police_department Officer_id drivers_race
    #> 1     Flowood Police Dept       6103        black
    #> 2 Hattiesburg Police Dept       1801        white
    #> 3     Flowood Police Dept       6103        black
    #> 4  Starkville Police Dept       5302        white
    #> 5    Vickburg Police Dept       7501        white
    #> 6   Ridgeland Police Dept       4504        black

It is worth knowing that `dplyr` comes with a number of [“select
helpers”](https://www.rdocumentation.org/packages/dplyr/versions/0.7.2/topics/select_helpers),
which are functions that allow you to select columns based on their
names. For example:

iris \<- tbl\_df(iris) \# so it prints a little nicer select(iris,
starts\_with(“Petal”)) select(iris, ends\_with(“Width”)) select(iris,
contains(“etal”)) select(iris, matches(“.t.”)) select(iris,
Petal.Length, Petal.Width) select(iris, everything()) vars \<-
c(“Petal.Length”, “Petal.Width”) select(iris, one\_of(vars))

``` r
select(trafficstops, starts_with("driver"))
```

    #>   drivers_age drivers_race drivers_sex
    #> 1          43        black        male
    #> 2          42        white      female
    #> 3          47        black      female
    #> 4          19        white      female
    #> 5          29        white        male
    #> 6          48        black      female

To choose rows based on specific criteria, use `filter()`:

``` r
filter(trafficstops, county_name == "Yazoo County")
```

    #>   raw_row_number       date  county_name drivers_age drivers_race
    #> 1           2845 2013-01-11 Yazoo County          32        black
    #> 2           2859 2013-05-15 Yazoo County          58        white
    #> 3           2863 2013-07-02 Yazoo County          26        black
    #> 4           3020 2013-01-12 Yazoo County          42        black
    #> 5           3185 2013-01-12 Yazoo County          42        black
    #> 6           3200 2013-01-12 Yazoo County          28        black
    #>   drivers_sex Officer_id           Police_department      type violation
    #> 1        male         82 Yazoo County Sheriff Office vehicular       B51
    #> 2        male       8204      Yazoo City Police Dept vehicular       M40
    #> 3        male       8204      Yazoo City Police Dept vehicular       M14
    #> 4        male         82 Yazoo County Sheriff Office vehicular       B53
    #> 5        male         82 Yazoo County Sheriff Office vehicular       B51
    #> 6      female         82 Yazoo County Sheriff Office vehicular       B51
    #>   speed posted_speed raw_race
    #> 1    NA           NA        B
    #> 2    NA           NA        W
    #> 3    NA           NA        B
    #> 4    NA           NA        B
    #> 5    NA           NA        B
    #> 6    NA           NA        B

Here are some other ways to select rows:

  - select certain rows by row number: `slice(trafficstops, 1:3) #
    rows 1-3`
  - select random rows:
      - `sample_n(trafficstops, 5) # number of rows to select`
      - `sample_frac(trafficstops, .01) # fraction of rows to select`

To sort rows by variables use the `arrange` function:
`arrange(trafficstops, county_name, stop_date)`

    #>   raw_row_number       date    county_name drivers_age drivers_race
    #> 1              1 2013-11-13  Rankin County          43        black
    #> 2              2 2015-02-10 Forrest County          42        white
    #> 3              3 2013-11-14  Rankin County          47        black
    #>   drivers_sex Officer_id       Police_department      type violation speed
    #> 1        male       6103     Flowood Police Dept vehicular       D36    NA
    #> 2      female       1801 Hattiesburg Police Dept vehicular       D36    NA
    #> 3      female       6103     Flowood Police Dept vehicular       B53    NA
    #>   posted_speed raw_race
    #> 1           NA        B
    #> 2           NA        W
    #> 3           NA        B

    #>   raw_row_number       date  county_name drivers_age drivers_race
    #> 1          44431 2013-01-01 Adams County          35        black
    #> 2           7924 2013-01-02 Adams County          44        white
    #> 3           7925 2013-01-02 Adams County          61        white
    #> 4          37011 2013-01-02 Adams County          35        black
    #> 5          22880 2013-01-03 Adams County          20        black
    #> 6          44111 2013-01-05 Adams County          25        black
    #>   drivers_sex Officer_id   Police_department      type violation speed
    #> 1      female        102 Natchez Police Dept vehicular       S92    49
    #> 2      female        102 Natchez Police Dept vehicular       S92    55
    #> 3        male        102 Natchez Police Dept vehicular       S92    59
    #> 4      female        102 Natchez Police Dept vehicular       M14    NA
    #> 5      female        102 Natchez Police Dept vehicular       D36    NA
    #> 6      female        102 Natchez Police Dept vehicular       D36    NA
    #>   posted_speed raw_race
    #> 1           35        B
    #> 2           45        W
    #> 3           45        W
    #> 4           NA        B
    #> 5           NA        B
    #> 6           NA        B

## Pipes

What if you wanted to filter **and** select on the same data? For
example, lets find drivers over 85 years and only keep the violation and
gender columns. There are three ways to do this: use intermediate steps,
nested functions, or pipes.

  - Intermediate steps:

With intermediate steps, you essentially create a temporary data frame
and use that as input to the next function. This can clutter up your
workspace with lots of objects.

``` r
tmp_df <- filter(trafficstops, drivers_age > 85)
select(tmp_df, violation, drivers_sex)
```

  - Nested functions

You can also nest functions (i.e. one function inside of another). This
is handy, but can be difficult to read if too many functions are nested
as things are evaluated from the inside out.

``` r
select(filter(trafficstops, drivers_age > 85), violation, drivers_sex)
```

  - Pipes\!

The last option, pipes, are a fairly recent addition to R. Pipes let you
take the output of one function and send it directly to the next, which
is useful when you need to do many things to the same dataset. Pipes in
R look like `%>%` and are made available via the `magrittr` package,
installed automatically with **`dplyr`**. If you use RStudio, you can
type the pipe with **’ Ctrl + Shift + M ’** if you have a PC.

``` r
trafficstops %>%
  filter(drivers_age > 85) %>%
  select(violation, drivers_sex)
```

In the above, we use the pipe to send the `trafficstops` dataset first
through `filter()` to keep rows where `drivers_sex` is Black, then
through `select()` to keep only the `Officer_id` and `date` columns.
Since `%>%` takes the object on its left and passes it as the first
argument to the function on its right, we don’t need to explicitly
include it as an argument to the `filter()` and `select()` functions
anymore.

\#contains() function choses the variables including the written text

``` r
select(trafficstops, contains("driver"))
```

If we wanted to create a new object with this smaller version of the
data, we could do so by assigning it a new name:

``` r
senior_drivers <- trafficstops %>%
  filter(drivers_age > 85) %>%
  select(violation, drivers_sex, drivers_race)
senior_drivers
```

    #>   violation drivers_sex drivers_race
    #> 1       F04        male        black

Note that the final data frame is the leftmost part of this expression.

> 
> 
> <h3>
> 
> Challenge
> 
> </h3>
> 
> Using pipes, subset the `trafficstops` data to include stops in Tunica
> County only and retain the columns `stop_date`, `driver_age`, and
> `violation_raw`. Bonus: sort the table by driver age.

``` r
## Answer
trafficstops %>% 
  filter(county_name == "Tunica County") %>% 
  select(date, drivers_age, violation) %>% 
  arrange(desc(drivers_age))
```

## Add new columns

Frequently you’ll want to create new columns based on the values in
existing columns. For this we’ll use `mutate()`.

To create a new column with the year the driver was born we can extract
the first 4 elements of the string that represents the
`driver_birthdate` and add it to the data frame like this:

``` r
trafficstops %>% 
  mutate(birth_year = substring(1984, 1, 4)) -> trafficstops -> df1
df1
```

The new and edited columns will not permanently be added to the existing
data frame – unless we explicitly save the output.

> 
> 
> <h3>
> 
> Challenge
> 
> </h3>
> 
> Create a new data frame from the `trafficstops` data that meets the
> following criteria: contains only the `violation` column for female
> drivers of age 50 that were stopped on a Sunday. For this add a new
> column to your data frame called `weekday_of_stop` containing the
> number of the weekday when the stop occurred. Use the `wday()`
> function from `lubridate` (Sunday = 1).
> 
> Think about how the commands should be ordered to produce this data
> frame\!

``` r
## Answer
trafficstops %>% 
  filter(drivers_age == 50 & drivers_sex == "female") %>% 
  mutate(wds = wday(ymd(date))) %>% 
  select(violation, wds) %>% 
  filter(wds == 1)
```

—\>

## What is split-apply-combine?

Many data analysis tasks can be approached using the
*split-apply-combine* paradigm: split the data into groups, apply some
analysis to each group, and then combine the results.

**`dplyr`** makes this very easy through the use of the `group_by()`
function.

`group_by()` is often used together with `summarize()`, which collapses
each group into a single-row summary of that group. `group_by()` takes
as arguments the column names that contain the **categorical** variables
for which you want to calculate the summary statistics. So to view the
mean age for black and white drivers:

``` r
trafficstops %>%
  group_by(drivers_race) %>%
  summarize(mean_age = mean(drivers_age, na.rm=TRUE))
```

    #> Warning: Factor `drivers_race` contains implicit NA, consider using
    #> `forcats::fct_explicit_na`

    #> # A tibble: 5 x 2
    #>   drivers_race           mean_age
    #>   <fct>                     <dbl>
    #> 1 asian/pacific islander     36.0
    #> 2 black                      32.7
    #> 3 other                      32.4
    #> 4 white                      34.9
    #> 5 <NA>                       32.0

So, back to the beginning of the chapter, where we tried to calculate
the mean age of the driver for different counties, how would we do this?
Like this:

``` r
trafficstops %>%
  group_by(county_name) %>%
  summarize(mean_age = mean(drivers_age, na.rm=TRUE))
```

You can also group by multiple columns:

``` r
trafficstops %>% 
  group_by(county_name, drivers_race) %>%
  summarize(mean_age = mean(drivers_age, na.rm=TRUE))
```

    #> Warning: Factor `county_name` contains implicit NA, consider using
    #> `forcats::fct_explicit_na`

    #> Warning: Factor `drivers_race` contains implicit NA, consider using
    #> `forcats::fct_explicit_na`

    #> # A tibble: 361 x 3
    #> # Groups:   county_name [83]
    #>    county_name   drivers_race           mean_age
    #>    <fct>         <fct>                     <dbl>
    #>  1 Adams County  asian/pacific islander     55  
    #>  2 Adams County  black                      34.3
    #>  3 Adams County  other                      38.4
    #>  4 Adams County  white                      37.2
    #>  5 Adams County  <NA>                       19.8
    #>  6 Alcorn County asian/pacific islander     64  
    #>  7 Alcorn County black                      32.8
    #>  8 Alcorn County other                      29.6
    #>  9 Alcorn County white                      32.7
    #> 10 Alcorn County <NA>                       40.3
    #> # ... with 351 more rows

If we wanted to remove the line with `NA` we could insert a `filter()`
in the chain:

``` r
trafficstops %>%
  filter(!is.na(drivers_race)) %>% 
  group_by(county_name, drivers_race) %>%
  summarize(mean_age = mean(drivers_age, na.rm=TRUE))
```

    #> Warning: Factor `county_name` contains implicit NA, consider using
    #> `forcats::fct_explicit_na`

    #> # A tibble: 308 x 3
    #> # Groups:   county_name [83]
    #>    county_name   drivers_race           mean_age
    #>    <fct>         <fct>                     <dbl>
    #>  1 Adams County  asian/pacific islander     55  
    #>  2 Adams County  black                      34.3
    #>  3 Adams County  other                      38.4
    #>  4 Adams County  white                      37.2
    #>  5 Alcorn County asian/pacific islander     64  
    #>  6 Alcorn County black                      32.8
    #>  7 Alcorn County other                      29.6
    #>  8 Alcorn County white                      32.7
    #>  9 Amite County  black                      36.4
    #> 10 Amite County  other                      34.6
    #> # ... with 298 more rows

Recall that `is.na()` is a function that determines whether something is
an `NA`. The `!` symbol negates the result, so we’re asking for
everything that is *not* an `NA`.

You may have noticed that the output from these calls looks a little
different. That’s because **`dplyr`** has changed our `data.frame`
object to an object of class `tbl_df`, also known as a “tibble”.
Tibble’s data structure is very similar to a data frame. For our
purposes the only differences are that (1) columns of class `character`
are never converted into factors, and (2) in addition to displaying the
data type of each column under its name, it only prints the first few
rows of data and only as many columns as fit on one screen. If we wanted
to print all columns we can use the print command, and set the `width`
parameter to `Inf`. To print the first 6 rows for example we would do
this: `print(my_tibble, n=6, width=Inf)`.

Once the data are grouped, you can also summarize multiple variables at
the same time (and not necessarily on the same variable). For instance,
we could add a column indicating the minimum age in each group
(i.e. county):

``` r
trafficstops %>%
  filter(!is.na(drivers_race)) %>% 
  group_by(county_name, drivers_race) %>%
  summarize(mean_age = mean(drivers_age, na.rm=TRUE),
            min_age = min(drivers_age, na.rm=TRUE))
```

    #> Warning: Factor `county_name` contains implicit NA, consider using
    #> `forcats::fct_explicit_na`

    #> # A tibble: 308 x 4
    #> # Groups:   county_name [83]
    #>    county_name   drivers_race           mean_age min_age
    #>    <fct>         <fct>                     <dbl>   <int>
    #>  1 Adams County  asian/pacific islander     55        55
    #>  2 Adams County  black                      34.3      16
    #>  3 Adams County  other                      38.4      19
    #>  4 Adams County  white                      37.2      16
    #>  5 Alcorn County asian/pacific islander     64        57
    #>  6 Alcorn County black                      32.8      16
    #>  7 Alcorn County other                      29.6      16
    #>  8 Alcorn County white                      32.7      15
    #>  9 Amite County  black                      36.4      16
    #> 10 Amite County  other                      34.6      19
    #> # ... with 298 more rows

## Tallying

When working with data, it is also common to want to know the number of
observations found for each factor or combination of factors. For this,
**`dplyr`** provides `tally()`. For example, if we wanted to see how
many traffic stops each officer recorded we would do:

``` r
trafficstops %>%
  group_by(Officer_id) %>%
  tally()
```

We can optionally sort the results in descending order by adding
`sort=TRUE`:

``` r
trafficstops %>%
  group_by(Officer_id) %>%
  tally(sort=TRUE)
```

Here, `tally()` is the action applied to the groups created by
`group_by()` and counts the total number of records for each category.

Alternatives:

``` r
trafficstops %>%
  group_by(Officer_id) %>%
  summarize(n = n()) # n() is useful when count is needed for a calculation

trafficstops %>%
  count(Officer_id) # count() calls group_by automatically, then tallies
```

> Challenge
> 
> Which 3 counties were the ones with the most stops in 2013? Hint: use
> the year() function from lubridate.

## Joining two tables

\<\<\<\<\<\<\< HEAD It is not uncommon that we have our data spread out
in different tables and need to bring those together for analysis. In
this example we will combine the numbers of trafficstops for black and
white drivers per county together with the numbers of the black and
white total population for these counties. The population data are the
estimated values of the 5 year average from the 2011-2015 American
Community Survey (ACS):

``` r
population <- read.csv("MS_acs2015_bw.csv")
head(population)
```

    #>              County  FIPS black_pop white_pop bw_pop
    #> 1      Jones County 28067     19711     47154  66865
    #> 2 Lauderdale County 28075     33893     43482  77375
    #> 3       Pike County 28113     21028     18282  39310
    #> 4    Hancock County 28045      4172     39686  43858
    #> 5     Holmes County 28051     15498      3105  18603
    #> 6    Jackson County 28059     30704    101686 132390

In a first step we will use a previous `dplyr` command to count all the
trafficstops per county.

``` r
trafficstops %>% 
  group_by(county_name) %>% 
  summarise(n_stops = n())
```

    #> Warning: Factor `county_name` contains implicit NA, consider using
    #> `forcats::fct_explicit_na`

    #> # A tibble: 83 x 2
    #>    county_name      n_stops
    #>    <fct>              <int>
    #>  1 Adams County        3306
    #>  2 Alcorn County       7267
    #>  3 Amite County        4198
    #>  4 Attala County       6682
    #>  5 Benton County        337
    #>  6 Bolivar County      8270
    #>  7 Calhoun County      3377
    #>  8 Carroll County      2479
    #>  9 Chickasaw County    4500
    #> 10 Choctaw County      1439
    #> # ... with 73 more rows

We will then pipe this into our next operation where we bring the two
tables together. We will use `left_join`, which returns all rows from
the left table, and all columns from the left and the right table. As
unique ID, which uniquely identifies the corresponding records in each
table we use the County Names.

``` r
trafficstops %>% 
  group_by(county_name) %>% 
  summarise(n_stops = n()) %>% 
  left_join(population, by = c("county_name" = "County")) %>% 
  head()
```

    #> Warning: Factor `county_name` contains implicit NA, consider using
    #> `forcats::fct_explicit_na`

    #> # A tibble: 6 x 6
    #>   county_name    n_stops  FIPS black_pop white_pop bw_pop
    #>   <fct>            <int> <int>     <int>     <int>  <int>
    #> 1 Adams County      3306 28001     17757     12856  30613
    #> 2 Alcorn County     7267 28003      4281     31563  35844
    #> 3 Amite County      4198 28005      5416      7395  12811
    #> 4 Attala County     6682 28007      8194     10649  18843
    #> 5 Benton County      337 28009      3078      5166   8244
    #> 6 Bolivar County    8270 28011     21648     11197  32845
