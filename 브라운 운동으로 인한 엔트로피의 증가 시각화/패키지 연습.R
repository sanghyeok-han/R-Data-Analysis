install.packages("topicmodels")


library(collapsibleTree)

df<- data.frame(a= rep(c("a","b"), each= 8), aa= rep(c("aa","ab","ba","bb"), each= 4), aaa= c(28,23,12,11,42,12,2,33,11,22,31,4,5,6,5,4))

df

collapsibleTree(df, hierarchy= c("a", "aa", "aaa"), collapsed= F)

collapsibleTreeSummary(df, hierarchy= c("a", "aa", "aaa"), root= "bababa", attribute= "aaa", width= 400)


library(normtest)

x<- runif(100, min=1, max=4)

ajb.norm.test(x)

hist(x)


library(doBy)

summaryBy(Sepal.Width + Sepal.Length ~ Species, iris)

df<- data.frame(a= rep(c("aa","bb"), each= 4), b= c(1,2,3,4,1,2,5,2))
df

orderBy(~ -a, df)

sampleBy(~a, frac= 0.5, data=df, replace= T)

library(ggvis)

first_plot<- ggvis(iris, x= ~Sepal.Length, y= ~Sepal.Width, shape= ~Species, size= ~Petal.Length,fill= ~Species)
layer_points(first_plot) %>% add_axis("x", title= "asdf")

iris %>% ggvis(x= ~Sepal.Length, y= ~Petal.Length) %>% layer_lines()
iris %>% ggvis(x= ~Sepal.Length, y= ~Petal.Length) %>% layer_ribbons()

iris %>% ggvis( ~factor(Species), ~ Sepal.Length) %>% layer_boxplots()


library(tidyverse)
library(caret)
install.packages("mlbench")

set.seed(1234)

Sonar

data(Sonar, package= "mlbench")

Sonar<- Sonar %>% as_tibble


indexTrain<- createDataPartition(Sonar$Class, p= 0.7, list= F)

training<- Sonar[indexTrain, ]
testing<- Sonar[-indexTrain, ]

training
testing
