### Necessary packages
library(mirt)
library(tidyverse)
library(ggplot2)
### Loading responses to two different test forms
mydata_1 <- readRDS('G6_NP5_Y2018_Ready.rds')[3:47]
mydata_2 <- readRDS('G6_LT_Y2018_Ready.rds')[3:49]
### Merging data
mydata <- bind_rows(mydata_1, mydata_2)
### Making group variable
group <- c(rep("Group 1", nrow(mydata_1)),
           rep("Group 2", nrow(mydata_2)))
### Inspecting data
apply(mydata, 2, table, exclude = NULL)
### Fitting a unidimensional single group model 
mymodel <- mirt(mydata,
                1,
                itemtype = 'Rasch')
### Calculating IRT score
student_IRT_score <- fscores(mymodel)
### Calculating sum score on tests
student_sum_score <- apply(mydata, 1, sum, na.rm =T)
### Merging datapoints
test_comp <- data.frame(group, student_IRT_score, student_sum_score)
### Plotting latent trait score on sum score for each test
ggplot(test_comp, aes(x = student_sum_score,
                      y = F1,
                      color = group))+
  geom_point()
