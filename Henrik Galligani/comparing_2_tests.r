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
                itemtype = '2PL') ### Show with 2PL as well
### Calculating IRT score
student_IRT_score <- fscores(mymodel)
### Calculating sum score on tests
student_sum_score <- apply(mydata, 1, sum, na.rm =T)
### Merging datapoints
test_comp <- data.frame(group, student_IRT_score, student_sum_score)
### Plotting latent trait score on sum score for each test
ggplot(test_comp, aes(x = F1,
                      y = student_sum_score,
                      color = group))+
  geom_point()
### national test scale
test_comp$F1_NT <- test_comp$F1*10+50
ggplot(test_comp, aes(x = student_sum_score,
                      y = F1_NT,
                      color = group))+
  geom_point()

for(i in 1:length(mydata)){
  ItemPlot <- itemfit(mymodel, 
                      group.bins=15,
                      empirical.plot = i,
                      empirical.CI = .95,
                      method = 'ML') 
  print(ItemPlot)
}
### IF TIME ###

### Dimensionality assessment 

### Bi-factor script to estimate the proportion variance explained by 
### different groups of items as defined by belonging to different 
### test-forms. 

### Necessary package(s)
#install.packages('mirt')
library(mirt)

### Tests to analyse:
# test_1 <- readRDS('G5_NP5_Y2018_Ready.rds')[3:47]
# test_1 <- readRDS('G6_NP5_Y2018_Ready.rds')[3:47]
# test_1 <- readRDS('G6_LT_Y2018_Ready.rds')[3:50]
# test_1 <- readRDS('G7_LT_Y2018_Ready.rds')[3:50]
# test_1 <- readRDS('G7_NP8_Y2018_Ready.rds')[3:52]
# test_1 <- readRDS('G8_NP8_Y2018_Ready.rds')[3:52]

### Replace test_1 input with relevant test to be analysed
### Replace test_2 with a test sharing some of the items with test_1
test_2 <- readRDS('G6_NP5_Y2018_Ready.rds')
test_1 <- readRDS('G7_LT_Y2018_Ready.rds')

### Trimming data to only have item information. Modify as necessary 
test_1 <- test_1[3:length(test_1)]
test_2 <- test_2[3:length(test_2)]

### Items of test_1 and test_2
items_gr1 <- colnames(test_1)
items_gr2 <- colnames(test_2)
### Spesifying model for bi-factor analysis in 'mirt' for test_1
### subfactor 1 is items unique to test_1
### Subfactor 2 is items shared with test_2
find    <- items_gr2
replace <- c(rep(2, length(items_gr2)))
found   <- match(items_gr1, find)
test_1_bfactor_model <- ifelse(is.na(found), 1, replace[found])
### Estimating bifactor model for test_1
mymodel <- bfactor(test_1,
                   test_1_bfactor_model)
### Storing model. 
### NB: Store model under a name that reflects the data analysed!
### If not, you will override the stored model, and recovering models 
### will be imposible. 
saveRDS(mymodel, file = "G7_LT_bfactor_item_group.rds")

### Previously estimated models
G6_LT_bfactor_item_group <- readRDS('G6_LT_bfactor_item_group.rds')
summary(G6_LT_bfactor_item_group)
G7_LT_bfactor_item_group <- readRDS('G7_LT_bfactor_item_group.rds')
summary(G7_LT_bfactor_item_group)
### Printing summary for bi-factor model for test_1 using item 
### groups as specified above. Variance explained can be found in 
### the console output, as "Proportion Var:"
### Reminder: 
### subfactor 1 is items unique to test_1
### Subfactor 2 is items shared with test_2
summary(mymodel)


