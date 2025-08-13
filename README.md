
# Tinystatr, an automated process of selecting statistical methods <a href="https://maze-icicle-277.notion.site/Tinystatr-an-automated-process-of-selecting-statistical-methods-fcae55ded6f34e9f99cd25c661851f44"><img src="man/figure/logo.png" align="right" height="139" alt="tinystatr website" /></a>



[https://github.com/yunzhu0304/tinystatr](https://github.com/yunzhu0304/tinystatr)

# 🦖About tinystatr

The *tinystatr* is an R package for statistical analysis. It automatically assesses the normality and homogeneity of variance of input data, selects appropriate statistical methods based on the characteristics of the data, and returns the statistical analysis results.

# 🦒Introduction

Our statistical analysis process is based on the following two workflows: two groups, and more than two groups.

🍒For two groups: 

- Normality test and homogeneity of variance test.
1. Both datasets are normally distributed and variance equal, will apply the t-test.
2. Both datasets are normally distributed but the variance is unequal, will apply the Welch’s t-test.
3. One of the datasets is non-normally distributed, and the sample size in a single group is **more than** 30, the choice of statistical method will depend on their variances. Equal to the Welch’s t-test, unequal to the Mann-Whitney test(Wilcoxon test).
4. One of the datasets is non-normally distributed but the sample size in a single group is **less than** 30 or both datasets are non-normally distributed, will apply the Wilcoxon test.

🍇For more than two groups: 

- Normality test and homogeneity of variance test.
1. One of the datasets is non-normally distributed, will apply the Kruskal-Wallis test and Dunn’s test (Bonferroni test) as the post-hoc test.
2. All of the datasets are normally distributed, the choice of statistical method will depend on their variances. Equal to the ANOVA test (post-hoc: Tukey HSD), unequal to the Kruskal-Wallis test (post-hoc: Dunn’s test).

<img width="412" alt="Snipaste_2024-04-27_22-29-21" src="https://github.com/yunzhu0304/tinystatr/assets/146269920/c21a7aa5-62ad-4ffa-9713-073d507fb427">
<img width="409" alt="Snipaste_2024-04-27_22-29-01" src="https://github.com/yunzhu0304/tinystatr/assets/146269920/5c132215-a572-4bc9-9a68-7d68445c1781">


# 🏗️Installation

```r
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")
    
devtools::install_github("yunzhu0304/tinystatr")
```

# 📚Load the package

```r
library(tinystatr)
```

# 📏Load data and explore the package

## 📘Load the data

```r
data("ToothGrowth")
data("HairEyeColor")
```

## 📈Statistical analysis for **two groups(***stat2()***)**

### 📌More than just grouping and value information

```r
df <- ToothGrowth%>%
filter(dose %in% c("0.5","1"))

> df
    len supp dose
1   4.2   VC  0.5
2  11.5   VC  0.5
3   7.3   VC  0.5
4   5.8   VC  0.5
5   6.4   VC  0.5
6  10.0   VC  0.5

```

After executing the function(*stat2()*), the results will include assessments of normality, homogeneity of variance, and the selected statistical method.

```r
# Dataframe with multiple columns, need to filter variable
> result <- stat2(data = df,variable = "supp",id="VC",group = "dose",value = "len",
formula = len ~ dose)

Normally distributed  
Variance equal  
 t-test

# A tibble: 1 × 7
  group1 group2    n1    n2           p variable method
  <chr>  <chr>  <int> <int>       <dbl> <chr>    <chr> 
1 0.5    1         10    10 0.000000681 VC       t test
```

We also obtain an S4 object of class statresult, which includes:
# stat: A data frame containing the statistical test results.
# normal: A data frame with normality test results for each group.
# p_position: A data frame indicating the position of p-values for visualization purposes.

```r
> result@stat
# A tibble: 1 × 7
  group1 group2    n1    n2           p variable method
  <chr>  <chr>  <int> <int>       <dbl> <chr>    <chr> 
1 0.5    1         10    10 0.000000681 VC       t test

> stat2result[["normal"]]
  group variable normal meanvalue       sd
1   0.5       VC   TRUE      7.98 2.746634
2   1.0       VC   TRUE     16.77 2.515309

p = 6.81e-07
```

We also obtain a list named *stat2result*, which contains two data frames. One is stat, used to store the statistical result. The other is normal, used to store the results of normality, mean and sd value.

We also obtain an S4 object of class statresult, which includes:

- stat: A data frame containing the statistical test results.
- normal: A data frame with normality test results for each group.
- p_position: A data frame indicating the position of p-values for visualization purposes.

```r
> result@stat
  group1 group2 n1 n2        p variable method
1    0.5      1 10 10 6.81e-07       VC t test

> result@normal
  group variable normal meanvalue       sd
1   0.5       VC   TRUE      7.98 2.746634
2   1.0       VC   TRUE     16.77 2.515309

> result@p_position
  group1 group2 y.position
1    0.5      1     30.576

```

### 📌Statistical analysis based solely on grouping and value data

If the datasets only contain information on groups and values, we will ignore the *variable* and *id*.

```r
# Dataframe with only two columns (group,value)
data("HairEyeColor")
df <- as.data.frame(HairEyeColor)[,c(3,4)]


> stat2(data = df,group = "Sex",value = "Freq", formula = Freq ~ Sex) # Ignoring variable and id

Non-normally distributed  
 wilcoxon test
# A tibble: 1 × 7
  group1 group2    n1    n2     p variable method       
  <chr>  <chr>  <int> <int> <dbl> <chr>    <chr>        
1 Male   Female    16    16  0.88 id       Wilcoxon test

> stat2result[["stat"]]
# A tibble: 1 × 7
  group1 group2    n1    n2     p variable method       
  <chr>  <chr>  <int> <int> <dbl> <chr>    <chr>        
1 Male   Female    16    16  0.88 id       Wilcoxon test

> stat2result[["normal"]]
   group variable normal meanvalue       sd
1   Male       id  FALSE   17.4375 16.00820
2 Female       id  FALSE   19.5625 20.71382

> result <- stat2(data = df,group = "Sex",value = "Freq", formula = Freq ~ Sex) # Ignoring variable and id

Non-normally distributed  
 wilcoxon test
p = 0.88

> result@stat
  group1 group2 n1 n2    p variable        method
1   Male Female 16 16 0.88       id Wilcoxon test

> result@normal
   group variable normal meanvalue       sd
1   Male       id  FALSE   17.4375 16.00820
2 Female       id  FALSE   19.5625 20.71382

> result@p_position
  group1 group2 y.position
1   Male Female      73.92


```

## 📊Statistical analysis for more than **two groups(*stat3()*)**

### 📍More than just grouping and value information

```r
# Dataframe with multiple columns, need to filter variable
data("ToothGrowth")
df <- ToothGrowth

> df
    len supp dose
1   4.2   VC  0.5
2  11.5   VC  0.5
3   7.3   VC  0.5
...
11 16.5   VC  1.0
12 16.5   VC  1.0
13 15.2   VC  1.0
...
21 23.6   VC  2.0
22 18.5   VC  2.0
23 33.9   VC  2.0
...
31 15.2   OJ  0.5
32 21.5   OJ  0.5
33 17.6   OJ  0.5
...
```


After executing the function(*stat3()*), we will obtain the statistical result and a list named *stat3result*.

```r
> stat3(data = df, group = "dose", value = "len", variable = "supp", id = "OJ", formula = len ~ dose)


After executing the function(*stat3()*), we will obtain the statistical result and an S4 object.

```r
> result <- stat3(data = df, group = "dose", value = "len", variable = "supp", id = "OJ", formula = len ~ dose)

All groups have 3 or more samples. 
Normally distributed  
Variance equal  
 Anova



> result@stat

  group2 group1        p.adj posthoc variable           p1 P1method p.adj.signif
1      1    0.5 1.584138e-05     hsd       OJ 8.887164e-08    ANOVA         ****
2      2    0.5 9.386773e-08     hsd       OJ 8.887164e-08    ANOVA         ****
3      2      1 1.309258e-01     hsd       OJ 8.887164e-08    ANOVA           ns


> stat3result[["stat"]]
  group2 group1        p.adj posthoc variable           p1 P1method p.adj.signif
1      1    0.5 1.584138e-05     hsd       OJ 8.887164e-08    ANOVA         ****
2      2    0.5 9.386773e-08     hsd       OJ 8.887164e-08    ANOVA         ****
3      2      1 1.309258e-01     hsd       OJ 8.887164e-08    ANOVA           ns

> stat3result[["normal"]]

> result@normal
  group variable normal meanvalue       sd
1   0.5       OJ   TRUE     13.23 4.459709
2   1.0       OJ   TRUE     22.70 3.910953
3   2.0       OJ   TRUE     26.06 2.655058

> result@normal

  group variable normal meanvalue       sd
1   0.5       OJ   TRUE     13.23 4.459709
2   1.0       OJ   TRUE     22.70 3.910953
3   2.0       OJ   TRUE     26.06 2.655058
```

### 📍Statistical analysis based solely on grouping and value data

If the datasets only contain information on groups and values, we will ignore the *variable* and *id*.

```r
# Dataframe with only two columns (group,value)
data("HairEyeColor")
df <- as.data.frame(HairEyeColor)[,c(2,4)]


> stat3(data = df,group = "Eye",value = "Freq", formula = Freq ~ Eye) # Ignoring variable and id

All groups have 3 or more samples. 
Non-normally distributed  
Variance unequal  
Kruskal-Wallis 
# A tibble: 6 × 8
  group1 group2 p.adj posthoc    variable     p1 P1method p.adj.signif
  <chr>  <chr>  <dbl> <chr>      <chr>     <dbl> <chr>    <chr>       
1 Brown  Blue   1     bonferroni id       0.0637 K_W      ns          
2 Brown  Hazel  0.671 bonferroni id       0.0637 K_W      ns          
3 Brown  Green  0.368 bonferroni id       0.0637 K_W      ns          
4 Blue   Hazel  0.347 bonferroni id       0.0637 K_W      ns          
5 Blue   Green  0.176 bonferroni id       0.0637 K_W      ns          
6 Hazel  Green  1     bonferroni id       0.0637 K_W      ns  

> stat3result[["stat"]]
# A tibble: 6 × 8
  group1 group2 p.adj posthoc    variable     p1 P1method p.adj.signif
  <chr>  <chr>  <dbl> <chr>      <chr>     <dbl> <chr>    <chr>       
1 Brown  Blue   1     bonferroni id       0.0637 K_W      ns          
2 Brown  Hazel  0.671 bonferroni id       0.0637 K_W      ns          
3 Brown  Green  0.368 bonferroni id       0.0637 K_W      ns          
4 Blue   Hazel  0.347 bonferroni id       0.0637 K_W      ns          
5 Blue   Green  0.176 bonferroni id       0.0637 K_W      ns          
6 Hazel  Green  1     bonferroni id       0.0637 K_W      ns      
    
> stat3result[["normal"]]

>  result <-  stat3(data = df,group = "Eye",value = "Freq", formula = Freq ~ Eye) # Ignoring variable and id
All groups have 3 or more samples. 
Non-normally distributed  
Variance unequal  
Kruskal-Wallis

> result@stat
  group1 group2     p.adj    posthoc variable     p1 P1method p.adj.signif
1  Brown   Blue 1.0000000 bonferroni       id 0.0637      K_W           ns
2  Brown  Hazel 0.6709306 bonferroni       id 0.0637      K_W           ns
3  Brown  Green 0.3683564 bonferroni       id 0.0637      K_W           ns
4   Blue  Hazel 0.3466669 bonferroni       id 0.0637      K_W           ns
5   Blue  Green 0.1764459 bonferroni       id 0.0637      K_W           ns
6  Hazel  Green 1.0000000 bonferroni       id 0.0637      K_W           ns

> result@normal

  group variable normal meanvalue        sd
1 Brown       id   TRUE    27.500 23.348295
2  Blue       id   TRUE    26.875 21.463840
3 Hazel       id  FALSE    11.625  9.694439
4 Green       id   TRUE     8.000  4.598136



> result@p_position
  group1 group2 y.position
1  Brown   Blue   73.92000
2  Brown  Hazel   79.83360
3  Brown  Green   86.22029
4   Blue  Hazel   93.11791
5   Blue  Green  100.56734
6  Hazel  Green  108.61273

```

# 📖References

1. **[Comparing Means in R](http://www.sthda.com/english/wiki/comparing-means-in-r)**
2. **[Learning Statistics with R](https://learningstatisticswithr.com/)**
3. **[HOW CAN I DO POST-HOC PAIRWISE COMPARISONS IN R? | R FAQ](https://stats.oarc.ucla.edu/r/faq/how-can-i-do-post-hoc-pairwise-comparisons-in-r/)**


