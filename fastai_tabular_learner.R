
# Initilize Libraries

library(reticulate)
use_condaenv("pipenv")
library(magrittr)
library(fastai)

# Read in src

df = data.table::fread('adult_sample/adult.csv')

# Define Dependent Var
dep_var = 'salary'

# Define  Categorical Vars
cat_names = c('workclass', 'education', 'marital-status', 'occupation', 'relationship', 'race')

# Define  Contin Vars
cont_names = c('age', 'fnlwgt', 'education-num')


# Define Pre-processing
procs = list(FillMissing(),Categorify(),Normalize())

# Define Data Loader
dls = TabularDataTable(df, procs, cat_names, cont_names,
                       y_names = dep_var, 
                       splits = list(c(1:32000),c(32001:32561))) %>%
  dataloaders(bs = 64)

# Learn Model
model = dls %>% tabular_learner(layers=c(200,100), metrics=accuracy)

model %>% summary()

model %>% lr_find()
model %>% plot_lr_find(dpi = 200)
model %>% fit(5, lr = 10^-1)

# View Perf
model %>% get_confusion_matrix()
interp = ClassificationInterpretation_from_learner(model)
interp %>% plot_confusion_matrix(dpi = 90,figsize = c(6,6))

# Prediction 
model %>% predict(df[10:15,])
