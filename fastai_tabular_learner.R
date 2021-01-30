
# Initialize Libraries

library(reticulate)
use_condaenv("pipenv")
library(tidyverse)
library(fastai)

# Read in src
df = data.table::fread('rawset.csv')
df %>% dplyr::mutate(Y = case_when(Outcome.x == 1 ~ "Win", TRUE ~ "Loss")) -> df

# URLs_ADULT_SAMPLE()
# df = data.table::fread('adult_sample/adult.csv')



# Define Dependent Var
dep_var = 'Y'

# Define  Categorical Vars
cat_names = c('FinishCount.x','FinishCount.y',
              'FinishedCount.x','FinishedCount.y',
              'CountFights.x','CountFights.y',
              'StreakLength.x','StreakLength.y',
              'PreviousOutcome.x','PreviousOutcome.y',
              'FinishPrevious.x','FinishPrevious.x')

# Define  Continuous Vars
cont_names = c('WinRatio.x','WinRatio.y',
               'FinishRatio.x','FinishRatio.y',
               'FinishedRatio.x','FinishedRatio.y',
               'DamageDiff.x','DamageDiff.y')


# Define Pre-processing
procs = list(FillMissing(),Categorify(),Normalize())

# Define Data Loader
dls = TabularDataTable(df, procs, cat_names, cont_names,
                       y_names = dep_var,splits = list(c(1:2000),c(2001:2786))) %>%
  dataloaders(bs=64)

# Learn Model
model = dls %>% tabular_learner(layers=c(200,100), metrics=accuracy)

model %>% summary()

model %>% lr_find()
model %>% plot_lr_find(dpi = 200)
model %>% fit(15,lr = 10^-2)

model %>% plot_loss(dpi = 200)


# View Perf
model %>% get_confusion_matrix()
interp = ClassificationInterpretation_from_learner(model)
interp %>% plot_confusion_matrix(dpi = 90,figsize = c(6,6))

# Prediction 
model %>% predict(df[255:265,])

model %>% predict(df[0:2786,]) %>% mutate(RC=row_number()) -> outcomes

outcomes %>% filter(., Win >0.9 & class == 1 | Loss > 0.9 & class == 0) ->outcomes_we

df %>% left_join(outcomes_we,by=c("V1"="RC")) -> df_fit

df_fit %>% na.omit() -> df_fit

df_fit %>% dplyr::mutate(Y = case_when(Outcome.x == 1 ~ "Win", TRUE ~ "Loss")) -> df_fit

####

write.csv(df_fit,"model_validation.csv")


# caret

library(tidyverse)
library(caret)

setwd("C:/Users/eoedd/Desktop/locstore/projects/_octagon")



df_caret = data.table::fread('rawset.csv',drop = 'V1')
df_caret %>% dplyr::mutate(Y = case_when(Outcome.x == 1 ~ "Win", TRUE ~ "Loss")) -> df_caret
df_caret %>% select(-Outcome.x) -> df_caret

inTraining <- createDataPartition(df_caret$Y, p = .70, list = FALSE)
training <- df_caret[ inTraining,]
testing  <- df_caret[-inTraining,]

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)


set.seed(825)

# fit gbm
gbmFit1 <- train(Y ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
gbmFit1


earthFit1 <- train(Y ~ ., data = training, 
                 method = "rpart", 
                 trControl = fitControl,
                 verbose = FALSE)
earthFit1

adaFit1 <- train(Y ~ ., data = training, 
                 method = "adaboost", 
                 trControl = fitControl,
                 verbose = FALSE)



gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 6), 
                        n.trees = (1:3)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

nrow(gbmGrid)

set.seed(825)
gbmFit2 <- train(Y ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2

trellis.par.set(caretTheme())
plot(gbmFit2)  


fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

set.seed(825)
gbmFit3 <- train(Y ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid,
                 ## Specify which metric to optimize
                 metric = "ROC")
gbmFit3



pp_hpc <- preProcess(df_caret[, -21])
pp_hpc


transformed <- predict(pp_hpc, newdata = df_caret[, -21])
head(transformed)

pp_no_nzv <- preProcess(df_caret[, -21], 
                        method = c("center", "scale", "YeoJohnson", "nzv"))

pp_no_nzv

predict(pp_no_nzv, newdata = df_caret[1:20, -21])
