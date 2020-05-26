library(tidyverse)
library(caret)

cocktails <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv'
  )
# boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

skimr::skim(cocktails)

dat <- cocktails %>% 
  filter(!is.na(alcoholic)) %>% 
  mutate_if(is.character, str_to_lower) %>% 
  mutate_at(c("glass", "category", "alcoholic", "ingredient"), as.factor) %>%
  group_by(drink) %>% 
  mutate(ingred_count = n()) %>% 
  ungroup() %>% 
  select(glass, alcoholic, category, ingredient, ingred_count)

dat %>% ggplot(aes(glass, category)) + geom_count()

dat %>% ggplot(aes(glass, alcoholic)) + geom_count()

dat %>% ggplot(aes(glass, ingredient)) + geom_count()

dat %>% ggplot(aes(glass, ingred_count)) + geom_boxplot()

set.seed(12345)
inTrain <- createDataPartition(dat$glass, p = 0.8, list=F)

train <- dat[inTrain, ]
test <- dat[-inTrain, ]

ctrl <- trainControl(
  method = "cv",
  number = 5,
  preProcOptions = c("center", "scale")
)

set.seed(12345)
mod <- train(
  glass ~ category + alcoholic + ingredient + ingred_count,
  data = train,
  method = "knn",
  trControl = ctrl,
  tuneGrid = expand.grid(k = c(3,5,7,10))
)

pred <- predict(mod, test)

confusionMatrix(pred, test$glass)
