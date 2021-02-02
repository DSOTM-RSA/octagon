# TREE MBASED MODELS

library(tidyverse)
library(caret)

setwd("C:/Users/eoedd/Desktop/locstore/projects/_octagon")
df_caret = data.table::fread('2021-01-31_modelset_selected.csv',drop = c('V1','Method.x'))


# make a binary problem and remove zero variance predictors
df_caret %>% filter(Result.x=="win" | Result.x =="loss") %>% 
  select(-Fighter.x, -Opponent.x) -> df_caret_bin


# split into training and test data
inTraining <- createDataPartition(df_caret_bin$Result.x, p = .70, list = FALSE)
training <- df_caret_bin[ inTraining,]
testing  <- df_caret_bin[-inTraining,]


# basic summary
str(df_caret_bin)
summary(df_caret_bin)


# pre-process for this model
preProcessSet <- preProcess(df_caret_bin,
                            method = c("center","scale","YeoJohnson","nzv"))

preProcessSet

# apply to data
trainTransformed <- predict(preProcessSet,training)
testTransformed <- predict(preProcessSet,testing)


# define training regime
plsCtrl <- trainControl(
  method = "repeatedcv", 
  repeats = 5,
  number=5,
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)


# train model
set.seed(111)
plsFit <- train(Result.x~.,
                 data = trainTransformed,
                 method="pls",
                 tuneLength=5,
                 trControl = plsCtrl,
                 metric="ROC")

plsFit

# predict on training
plsClasses <- predict(plsFit, newdata = testTransformed)

plsProbs <- predict(plsFit, newdata = testTransformed, type = "prob")
head(plsProbs)

# check performance
confusionMatrix(data=plsClasses, as.factor(testTransformed$Result.x),positive = "win")



# gbm native

library(gbm)
set.seed(111)

training %>% mutate(FinishPrevious.x = as.factor(FinishPrevious.x),
                    FinishPrevious.y = as.factor(FinishPrevious.y),
                    Result = case_when(Result.x == "win" ~1, TRUE~0)) ->training

gbmFit<- gbm(Result~.,
             data=training[,-1],
             n.trees = 2000,
             interaction.depth = 3,
             shrinkage = 0.001,
             cv.folds = 5,
             n.cores = 3)

gbmFit

gbm.perf(gbmFit,method = "cv")

pretty.gbm.tree(gbmFit)

predict.gbm(gbmFit, newdata = testing[,-1], type = "response")

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







