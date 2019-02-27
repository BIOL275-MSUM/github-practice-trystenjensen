
# load packages -----------------------------------------------------------
library(readxl)     # load readxl, for reading Excel files
library(tidyverse)  # load tidyverse, for working with datasets

# read data ---------------------------------------------------------------

# read the finches data
finches <- read_excel("finches_data.xlsx")

# print the finches tibble in the console
finches

# take a quick look at all the variables in the dataset
glimpse(finches)

# histogram ---------------------------------------------------------------

# histogram of beak length, grouped by survival, with labels
ggplot(
  data = finches,                     # use the finches dataset
  mapping = aes(x = beak_length,      # put beak length on the x axis
                fill = outcome)       # fill sets the color of the boxes
) +
  geom_histogram(bins = 14) +         # add the histogram, use 14 bins
  facet_wrap(~ outcome, ncol = 1) +   # outcome is the grouping variable
  guides(fill = FALSE) +              # don't show a legend for fll color
  labs(
    title = "Figure 1.",              # title
    x = "Beak Length (mm)",           # x-axis label
    y = "Number of Birds"             # y-axis label
  )

# summarize ---------------------------------------------------------------

# summarize the dataset by outcome (survived vs. died)
beak_length_grouped_summary <- 
  finches %>% 
  group_by(outcome) %>% 
  summarize(mean = mean(beak_length),
            sd = sd(beak_length),
            n = n()) %>% 
  mutate(sem = sd / sqrt(n),
         upper = mean + 1.96 * sem,
         lower = mean - 1.96 * sem)
# print the results in the console
beak_length_grouped_summary

# bar chart ---------------------------------------------------------------

# bar chart of mean beak lengths
ggplot(
  data = beak_length_grouped_summary,   # dont use the original finches dataset
  mapping = aes(x = outcome,            # survival on the x axis
                y = mean,               # mean beak length on the y axis
                fill = outcome)         # make died/survived different colors
) +
  geom_col() +                          # add columns
  geom_errorbar(                        # add error bars
    mapping = aes(ymin = lower,         #   lower 95% confidence limit
                  ymax = upper),        #   upper 95% confidence limit
    width = .3                          #   width of horizontal part of bars
  ) +
  guides(fill = FALSE) +                # don't show a legend for fll color
  labs(
    title = "Figure 2.",                # title
    x = "Survival Outcome",             # x-axis label
    y = "Beak Length (mm)"              # y-axis label
  )

# t-test ------------------------------------------------------------------

# get a vector of beak lengths for birds that died
beak_length_died <-
  finches %>%                     # start with finches dataset
  filter(outcome == "died") %>%   # only include rows w/ outcome=died
  pull(beak_length)     

# print the new object in the console... it is a vector
beak_length_died

# get a vector of beak lengths for birds that survived
beak_length_survived <-
  finches %>% 
  filter(outcome == "survived") %>% 
  pull(beak_length)

# print the results in the console
beak_length_survived

# perform a two-sample t-test assuming unequal variances
t.test(beak_length_died, beak_length_survived)
