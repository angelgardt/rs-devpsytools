library(tidyverse)
theme_set(theme_bw())
theme_update(legend.position = "bottom")

## DATA IMPORT
taia <- read_csv("https://github.com/angelgardt/taia/raw/master/data/taia.csv")
str(taia)

## GET ITEM NAMES VECTORS
taia %>% select(starts_with(c("pr", "co", "de", "ut", "fa", "de", "un"))) %>% colnames() -> taia_items
taia %>% select(starts_with("pr")) %>% colnames() -> pr_items
taia %>% select(starts_with("co")) %>% colnames() -> co_items
taia %>% select(starts_with("ut")) %>% colnames() -> ut_items
taia %>% select(starts_with("fa")) %>% colnames() -> fa_items
taia %>% select(starts_with("de")) %>% colnames() -> de_items
taia %>% select(starts_with("un")) %>% colnames() -> un_items

taia %>% select(starts_with("gt")) %>% colnames() -> gt_items


## DESCRIPTIVES
### predictability
taia %>% 
  select(all_of(pr_items)) %>% 
  psych::describe()

taia %>% 
  select(all_of(pr_items)) %>% 
  pivot_longer(cols = all_of(pr_items),
               names_to = "item",
               values_to = "score") %>% 
  ggplot(aes(score)) +
  geom_bar() +
  facet_wrap(~ item, scales = "free_x") +
  scale_x_continuous(breaks = 0:5) +
  labs(x = "Score", y = "Frequency")

taia %>% 
  select(all_of(co_items)) %>% 
  pivot_longer(cols = all_of(co_items),
               names_to = "item",
               values_to = "score") %>% 
  ggplot(aes(score)) +
  geom_bar() +
  facet_wrap(~ item, scales = "free_x") +
  scale_x_continuous(breaks = 0:5) +
  labs(x = "Score", y = "Frequency")


## PSYCHOMETRIC ANALYSIS
### predictability
pr_alpha <- psych::alpha(
  taia %>% select(all_of(pr_items)),
  cumulative = TRUE,
  check.keys = FALSE
)

pr_alpha$total
pr_alpha$alpha.drop
pr_alpha$item.stats %>% mutate(difficulty = mean / 5) -> pr_alpha$item.stats

pr_alpha$item.stats %>% 
  rownames_to_column("item") %>% 
  select(item, discrimination = raw.r, difficulty) %>% 
  pivot_longer(cols = -item,
               names_to = "stat") %>% 
  ggplot(aes(x = item,
             y = value,
             shape = stat)) +
  geom_point(size = 2) +
  geom_hline(yintercept = c(.05, .95), linetype = "dotted") +
  geom_hline(yintercept = .2, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  scale_shape_manual(values = c(1, 16),
                     labels = c("Difficulty", "Discrimination")) +
  scale_y_continuous(breaks = seq(-1, 1, .1)) +
  labs(x = "Item",
       y = "Value",
       shape = "Characteristics",
       title = "Predictability scale",
       subtitle = "Item characteristics")


taia %>% 
  select(all_of(pr_items)) %>% 
  psych::describe()

taia %>% 
  select(all_of(pr_items)) %>% 
  pivot_longer(cols = all_of(pr_items),
               names_to = "item",
               values_to = "score") %>% 
  ggplot(aes(score)) +
  geom_bar() +
  facet_wrap(~ item, scales = "free_x") +
  scale_x_continuous(breaks = 0:5) +
  labs(x = "Score", y = "Frequency")

taia %>% 
  select(all_of(co_items)) %>% 
  pivot_longer(cols = all_of(co_items),
               names_to = "item",
               values_to = "score") %>% 
  ggplot(aes(score)) +
  geom_bar() +
  facet_wrap(~ item, scales = "free_x") +
  scale_x_continuous(breaks = 0:5) +
  labs(x = "Score", y = "Frequency")


## consistency
co_alpha <- psych::alpha(
  taia %>% select(all_of(co_items)),
  cumulative = TRUE,
  check.keys = FALSE
)

co_alpha$total
co_alpha$alpha.drop
co_alpha$item.stats %>% mutate(difficulty = mean / 5) -> co_alpha$item.stats

co_alpha$item.stats %>% 
  rownames_to_column("item") %>% 
  select(item, discrimination = raw.r, difficulty) %>% 
  pivot_longer(cols = -item,
               names_to = "stat") %>% 
  ggplot(aes(x = item,
             y = value,
             shape = stat)) +
  geom_point(size = 2) +
  geom_hline(yintercept = c(.05, .95), linetype = "dotted") +
  geom_hline(yintercept = .2, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  scale_shape_manual(values = c(1, 16),
                     labels = c("Difficulty", "Discrimination")) +
  scale_y_continuous(breaks = seq(-1, 1, .1)) +
  labs(x = "Item",
       y = "Value",
       shape = "Characteristics",
       title = "Consistency scale",
       subtitle = "Item characteristics")

taia %>% 
  select(all_of(pr_items)) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(
    type = "lower", lab = TRUE, lab_size = 3,
    colors = c("indianred1", "white", "royalblue1"),
    title = "Predictability. Inter-item correlations",
    show.legend = FALSE
  )


taia %>% 
  select(all_of(co_items)) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(
    type = "lower", lab = TRUE, lab_size = 3,
    colors = c("indianred1", "white", "royalblue1"),
    title = "Consistency. Inter-item correlations",
    show.legend = FALSE
  )
