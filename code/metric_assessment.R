########## Metric Assessment ##########

## Methodology:
## 1. We randomly removed 5 ratings from Eric, Lawrence, and Nadia's Overall ratings
## 2. We then used our recommender model to predict these five
## 3. Baseline: We created a random rating for each of the 5 from 0 to 10
## 4. We then calculated the Sum of the Squared Deviation from the actual rating, for our model and the baseline
## 5. Smaller values indicate more accurate recommendation
## 6. We repeated this procedure 100 times and averaged, so randomness wasn't the reason for the improvement

## Libraries
library(readr)
library(dplyr)

## Merge all the data
bar_ratings <- read_csv("food_bars.csv") %>%
    select(Bar = `Food Bar`, Taster, ActualRating = Overall)
bar_recs <- read_csv("food_recommendations.csv")

## Repeat a random rating 100 times
ratios <- replicate(n = 100, expr = {
    bar_all <- bar_recs %>%
        left_join(bar_ratings) %>%
        mutate(RandomRating = runif(nrow(.), 0, 10))
    
    ## Our model prediction
    our_model <- sum((bar_all$PredictedRating - bar_all$ActualRating)^2, na.rm = TRUE)
    
    ## Baseline prediction
    baseline <- sum((bar_all$RandomRating - bar_all$ActualRating)^2, na.rm = TRUE)
    
    ## Ratio
    baseline / our_model
})

summary(ratios)

