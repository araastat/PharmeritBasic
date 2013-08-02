---
title       : Introduction to R
subtitle    : Getting up and running
author      : Abhijit Dasgupta, PhD 
job         : 
framework   : io2012   # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : mathjax           # {mathjax, quiz, bootstrap}
mode        : standalone # {standalone, draft}
---

## Introduction to R

Abhijit Dasgupta, PhD<br>
ARAASTAT<br>
@webbedfeet

<div style="position:fixed; bottom:50px; left:50px; ">
    <img src="assets/img/DC2_logo_wide_720wide.png" style="height:100px" />
</div>

---

## Why R?

+ R is free (GPL3) statistical software
+ R is a powerful development platform for statistical algorithms
+ R is an ecosystem of over 4500 user-contributed packages
+ R is flexible, powerful and often cutting-edge
+ R can be accessed from SAS, SPSS, Python, Java
+ R can be accessed from Oracle, SAP, Netezza, Teradata, Greenplum

<div style="position:fixed; bottom:0; left:800px; ">
    <img src="assets/img/DC2_logo_wide_720wide.png" style="height:40px" />
</div>

---

## Why R?

+ Standard and cutting-edge statistical modeling
+ Flexible data munging
+ High-quality visualizations
+ Reproducible and automated reporting
+ Increasingly, web-based content-rich graphics


--- .quote .nobackground .segue .dark
<q>
 R is the *lingua statistica* </q> <br>    -- Anthony Damico

--- .quote .nobackground .segue .dark

<q> The question is not if R can do it, by how.</q>
       <p align="right"> -- Douglas Bates </p>

--- .nobackground .segue .dark

## Let's get started

---

## Reading data into R

You can read data in from 

+ Text files
+ Excel files
+ Files from other statistical software
+ Databases (MySQL, Postgres, MonetDB, SQLite)

---

## Reading data into R

Reading from text files

```r
pheno <- read.csv('data/pheno.csv')
geno <- read.table('data/geno.csv', sep=',', header=T)
```

---

## Reading data into R

You can read directly from the web

```r
pheno2 <- read.csv('http://faculty.washington.edu/kenrice/sisg/example-pheno.csv')
```

---

## Reading data into R

Reading from a database

```r
library(RSQLite)
sqlite <- dbDriver('SQLite')
exampledb <- dbConnect(sqlite,'data/mydb.sqlite')
dbListTables(exampledb)

library(sqldf)
sqldf('select * from phenotype limit 5', dbname='data/mydb.sqlite')
```

You can also use `sqldf` to read from data already in R

```r
sqldf('select * from pheno limit 5') # use the data.frame pheno
```

--- .segue .dark

## What does our data look like?

---

## Data structure

```
str(pheno)
```

---

## Peeking at data

```r
head(pheno)
tail(pheno)
summary(pheno)
```

---
 
## Data types

### Aggregate

+ data.frame
+ list
+ matrix

### Atomic

+ numeric
+ integer
+ character
+ factor
+ Date

---
 
## data.frame
 
The `data.frame` is the basic data unit for storing dat in R
<p>
It can store different types of data together
<p>
A `data.frame` looks like a matrix, but it is really a `list`<br>
However, some matrix operations do work on a `data.frame`

```r
pheno[1,]
pheno[,2:4]
pheno[1,2]
```

You can extract columns of a `data.frame` by name
```r
pheno$sex
pheno[,'sex']
```

---

## Factors

Factors look like character vectors <p>
However their internal representation is integer <p>
Need to be careful trying to manipulate factors like characters

```r
x.character <- as.character(15:20)
x.factor <- as.factor(x.character)

as.numeric(x.character)
as.numeric(x.factor)
```

--- .segue .dark

## Transformations

---

## transform

The `transform` function works on data.frames and allows you to quickly generate transformations of your data

```r
data(mtcars)
mtcars <- transform(mtcars, kmpg = mpg*1.6)
```

---

## Subsetting

The `subset` function can be used to easily subset data frames by rows and columns

```r
cars2 <- subset(mtcars, subset=(cyl==4 & mpg > 25) )
```

This is similar to using `sqldf`

```r
sqldf('select * from mtcars where cyl=4 and mpg > 25')
```

--- .segue .dark

## "Apply"ing yourself

---

## apply

`apply` works with matrices and applies a function along rows or columns

```r
X = matrix(rnorm(100), ncol=5)
apply(X, 1, mean) # mean by rows
apply(X, 2, max) # maximum within columns
```

---

## lapply

`lapply` applies the same function over elements of a list

```r
x <- list()
x[[1]] <- rnorm(10)
x[[2]] <- rnorm(25)
x[[3]] <- c('a','c','x','g')

lapply(x, sort)
```

Recall that a `data.frame` is really a `list`, with the columns as elements of the list
```r
lapply(mtcars, sort)
```

--- .segue .dark

## Wickham's duo: reshaping and split-apply-combine

---

## reshape2

`reshape2` has functions to allow you to take a data frame from wide to long

```r
R.df2 <- melt(R.df, id='ID')
```

---

## Split-apply-combine

The basic idea of split-apply-combine is to

+ split the data by some criterion
+ apply a function to the split data
+ combine the results of the function

---

## plyr

plyr provides functions of the form (x)(y)ply, where

+ (x) is the data type of the input
+ (y) is the data type of the output

For example, ddply, dlply, ldply, etc.

```r
ddply(mtcars, ~cyl, summarise, mean.mpg=mean(mpg))
dlply(mtcars,~cyl)
```

--- .segue .dark

## Now for something problematic

--- .segue .dark

## Visualization

---

## Basic plots

---

## Small multiples

---

## ggplot2

---

## rCharts

```r
r1 <- rPlot(mpg~wt|am+vs, data=mtcars, type='point',color='gear')
r1
```
```r
hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, type = "multiBarChart")
```

--- .segue .dark

## Modeling

--- .segue .dark

## Reporting

---

## Markdown

Markdown is an easy way to write a report that can be printed in a variety of formats<p>
I prefer `pandoc` flavor of markdown for it's flexibility<p>

---

## knitr

`knitr` is a package that can take markdown interspersed with R code, then

+ Extract and run the R code
+ Replace the code with the results

---

## pander

`pander` is a package that makes writing and running pandoc from R very easy

--- .segue .dark

## Presentations

---

## Slidify
