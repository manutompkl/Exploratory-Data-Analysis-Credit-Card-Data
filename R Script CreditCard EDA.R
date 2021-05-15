

```{r}
library(tufte)
```

# Exploratory Data Analysis on Credit card dataset



## Credit Card:
The credit card dataset gives us the details of 1319 people of a particular area applied for a credit card. It comprises of 1319 observations and 17 variables.


## Package Loading
These are the pacakages used in the credit dataset to do the analysis.

```{r message=FALSE, warning=FALSE}
library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(rsq)
library(ggpubr)
library(plotly)
```


## "Dataset Loading"


```{r warning=FALSE}
setwd("/Users/manu/Downloads")
credit_card <- as.data.frame(read.csv("CreditCard.csv"))
```


## "Data Preprocessing"

Using the structure function we could find the structure of the given dataset.
Here we have numerical, integer and charecter data types.
```{r warning=FALSE}
str(credit_card)
```



The summary of the dataset is found using the summary function. This explains the length of the characters min, max, median, mean etc of the given dataset.
```{r warning=FALSE}
summary(credit_card)
```


checking if there is any null values
```{r warning=FALSE}
sum(is.na(credit_card))
sum(is.null(credit_card))
```



The categorical values are changed to numerical values using the impute function.
```{r warning=FALSE}
credit_card <- mutate(credit_card,card1 = ifelse(card == 'yes',1,0))
credit_card <- mutate(credit_card,owner1 = ifelse(owner == 'yes',1,0))
credit_card <- mutate(credit_card,selfemp1 = ifelse(selfemp == 'yes',1,0))
credit_card <- mutate(credit_card,income1 = income*10000)
#credit_card <- mutate(credit_card,card2 = ifelse())
```


```{r}
str(credit_card)
```


title: "Data Analysis"




The card owns give an idea about the number of people owns a credit card. Here 1023 peoples owns a credit card out of 1319 observations.
```{r warning=FALSE}
p1 <- plot_ly(credit_card,x=~card,type = "histogram")
p1 <- p1 %>% layout(title="Card Owns")
p1
```




But there is a derogatory report given in the dataset which explains about the report count of each individuals. But we can see that even if a person has 1 derogatory report, they are also given credit cards. 
```{r warning=FALSE}
p2 <- plot_ly(credit_card,x=~reports,color=~card,type="histogram")
p2 <- p2 %>% layout(title="Derogatory Report v/s Card ")
p2
```



Age is another parameter on considering the chance to get a credit card. Here we can see that people of age between 20-30 has a channce of getting credit card compared to others.
```{r warning=FALSE}
p3 <- plot_ly(credit_card,x=~age,color =~card,type = "histogram")
p3 <- p3 %>% layout(title="Age v/s Card")
p3
```


Now we move on to see hoe people spend with their credit card.
Here we check the relation between income and expenditure on the basis of age.ie., if there is an increase in expenditure for a persons who are under low age.
By looking into the graph we can see that, the age increases, the expenditure is less compared to people with less age.
```{r warning=FALSE}
p4 <- plot_ly(credit_card,x=~expenditure,y=~income1,type="histogram",color =~age)
p4 <- p4 %>% layout(title = "Income v/s Expenditure",
                    ylab = "Income",
                    xlab = "Expenditure")
p4
```



Share is the ratio of monthly credit card expenditure to annual income.
By plotting a graph on share v/s expenditure, we can find that there is steep positive increase. i.e, as share increases, expenditure also increases.
```{r warning=FALSE}
p5 <- plot_ly(credit_card, x = ~expenditure, y = ~share,color = 'red')
p5 <- p5 %>% layout(title="Share v/s Expenditure")
p5
```


Self employment is another factor that governs the income and expenditure rate. Here the graph explains that, expenditure is more for non-self employed people than self employed people.
```{r warning=FALSE}
p6 <- plot_ly(credit_card,x=~expenditure,y=~income1,color = ~selfemp)
p6 <- p6 %>% layout(title="Income V/s Expenditure")
p6
```

Correlation test is used to evaluate the association between two or more variables. Here R is the correlation coefficient and p-value is the significance level.
Now we plot a correlation graph on share to expenditure to see is they are dependent or not. Here the correlation is positive and correlation coefficient is 0.84 and p-value 2.2^e-16
```{r warning=FALSE}
ggscatter(credit_card,x = "expenditure",y = "share",title = "Share-Expenditure Correlation Graph",add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
```


This correlation graph gives us a clear picture that as age increases, share decreases. ie, the correlation coefficient of this graph is -0.12.
```{r warning=FALSE}
ggscatter(credit_card,x = "age",y = "share",fill = 'grey',title = "Age-Share Correlation Graph",palette = c('blue','red'),add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
```



Here we have a zero correlated graph. ie. the correlation coefficient is approximately equal to zero, which means that there is no correlation between these to variables, ie there is correlation between expenditure and age.
```{r warning=FALSE}
ggscatter(credit_card,x = "age",y = "expenditure",title = "Expenditure v/s Age",add = "reg.line", conf.int = TRUE,cor.coef = TRUE, cor.method = "pearson")
```

Here we check the relation between expenditure and income on the basis of dependency.ie., if there is an increase in dependency, expenditure decreases.
and for peopel with high income has less dependency.
```{r}
p7 <- plot_ly(credit_card,x=~income1,y=~expenditure,color = ~dependents)
p7 <- p7 %>% layout(title="Expenditure and Dependency v/s Income")
p7
```

This graph give a picture on people who are self employed to if they have acard or not. Here we can conclude that majority of the people are not self employed but have a credit card.
```{r warning=FALSE}
p8 <- plot_ly(credit_card,x=~card,type = "histogram",color =~selfemp)
p8 <- p8 %>% layout(title="Self employed and have card")
p8
```


This graph explains that if the self employed people having a credit card as a house or not. we can see that majority of the dont have a house but still have credit card.
```{r warning=FALSE}
p9 <- plot_ly(credit_card,x=~selfemp,type = "histogram",color=~owner)
p9 <- p9 %>% layout(title="Self Employed v/s Having House")
p9
```
THis graph draws a picture that people with less active credit card will get a new credit card.
```{r warning=FALSE}
p10 <- plot_ly(credit_card,x=~active,type = "histogram",color =~card)
p10 <- p10 %>% layout(title="Active CArd v/s Owning Credit card")
p10
```


This is a correlation matrix showing the correatiom plots of age, income, share and expenditure.
```{r warning=FALSE}
credit_card %>% filter(card1==1) %>% extract(,4:7) %T>% plot() %>% summary()
```

This is a linear regression done to find is there is any general trend in between income and expenditure.
```{r}
lr1 <- lm(credit_card$income1~credit_card$expenditure)
lr1
r1 <- rsq(lm(credit_card$income1~credit_card$expenditure))
r1
```

## Result
* People between age of 20-30 uses widely uses credit card.
* The expenditure is more for the people with less dependency.
* The income of majority of the people lies in between 20k and 40k.
* There is a negative relationship between share and expenditure.
* Weather a person has a house or weather they are self employed are not considered as a parameter for issuing credit card.
* People with 1 derogatory report can also get a credit card. 
* People with less amount of credit will get a new credit card easily.

## Conclusion

The exploratory data analysis done on the credit card data set helped us to understand that which category of people are more prone to get a credit card. It was found that people comes under age group of 20-30 and less active card have chance to get a new credit card. This analysis helps us to understand that if a person will get a credit card or not. It was also found that people with less dependence spends more with credit card. 
