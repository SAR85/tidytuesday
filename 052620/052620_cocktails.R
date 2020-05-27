library(tidyverse)
library(caret)
library(xgboost)

cocktails <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv"
  )
# boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

skimr::skim(cocktails)

dat <- cocktails %>%
  filter(!is.na(alcoholic)) %>%
  mutate_if(is.character, str_to_lower) %>%
  mutate_at(c("category", "alcoholic", "ingredient"), as.factor) %>%
  group_by(drink) %>%
  mutate(ingred_count = n()) %>%
  ungroup() %>%
  select(glass, alcoholic, category, ingredient, ingred_count)

dat %>% ggplot(aes(glass, category)) +
  geom_count()

dat %>% ggplot(aes(glass, alcoholic)) +
  geom_count()

dat %>% ggplot(aes(glass, ingredient)) +
  geom_count()

dat %>% ggplot(aes(glass, ingred_count)) +
  geom_boxplot()

top_10_glasses <- dat %>% count(glass) %>% 
  arrange(desc(n)) %>% 
  top_n(10, wt = n) %>% 
  select(glass) %>% 
  as_vector() %>% 
  as.character()

dat$glass[!(dat$glass %in% top_10_glasses)] <- "other"
unique(dat$glass)
dat <- dat %>% mutate(glass = as.factor(glass))

set.seed(12345)
inTrain <- createDataPartition(dat$glass, p = 0.8, list = F)

train <- dat[inTrain, ]
test <- dat[-inTrain, ]

# ctrl <- trainControl(
#   method = "cv",
#   number = 5,
#   preProcOptions = c("center", "scale")
# )
#
# set.seed(12345)
# mod <- train(
#   glass ~ category + alcoholic + ingredient + ingred_count,
#   data = train,
#   method = "knn",
#   trControl = ctrl,
#   tuneGrid = expand.grid(k = c(3,5,7,10))
# )
#
# pred <- predict(mod, test)
#
# confusionMatrix(pred, test$glass)

class_labels <- tibble(
  glass = unique(levels(dat$glass)),
  glass_code = seq_along(glass) - 1
)

xgb_train <- train %>% 
  left_join(class_labels, by = "glass") %>% 
  select(-glass)
xgb_test <- test %>% 
  left_join(class_labels, by = "glass") %>% 
  select(-glass)

mm_train <- model.matrix(glass_code ~ alcoholic + category + ingredient + ingred_count, xgb_train)[, -1]
mm_test <- model.matrix(glass_code ~ alcoholic + category + ingredient + ingred_count, xgb_test)[, -1]

dtrain <- xgb.DMatrix(mm_train, label = xgb_train$glass_code)
dtest <- xgb.DMatrix(mm_test, label = xgb_test$glass_code)

mod2 <- xgb.train(
  data = dtrain,
  nrounds = 5000,
  params = list(
    max_depth = 3,
    gamma = 5,
    min_child_weight = 5,
    eta = 0.05,
    subsample = 0.8,
    colsample_bytree = 0.6,
    num_class = 11
  ),
  watchlist = list(train = dtrain, test = dtest),
  objective = "multi:softprob",
  eval.metric = "merror"
)

pred2 <- predict(mod2, dtest)

predictions2 <- matrix(pred2, nrow = nrow(class_labels), length(pred2)/nrow(class_labels)) %>% 
  t() %>% 
  as_tibble() %>% 
  mutate(
    label = xgb_test$glass_code,
    max_prob = max.col(., "last") - 1
  ) %>% 
  left_join(class_labels, by = c(max_prob = "glass_code"))

confusionMatrix(factor(predictions2$glass), test$glass)

xgb.importance(model = mod2)
                