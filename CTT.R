pkgs <- c("tidyverse", "qgraph", "ggcorrplot", "psych", "GPArotation", "EFAtools", "lavaan", "semPlot")
install.packages(pkgs[!pkgs %in% installed.packages()])

library(tidyverse)
theme_set(theme_bw())
theme_update(legend.position = "bottom")
library(ggcorrplot)
library(psych)
library(GPArotation)
library(lavaan)
library(semPlot)

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
  describe()

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
  select(all_of(pr_items)) %>% 
  cor() %>% 
  ggcorrplot(
    type = "lower", lab = TRUE, lab_size = 3,
    colors = c("indianred1", "white", "royalblue1"),
    title = "Predictability. Inter-item correlations",
    show.legend = FALSE
  )


### consistency
taia %>% 
  select(all_of(co_items)) %>% 
  describe()

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

taia %>% 
  select(all_of(co_items)) %>% 
  cor() %>% 
  ggcorrplot(
    type = "lower", lab = TRUE, lab_size = 3,
    colors = c("indianred1", "white", "royalblue1"),
    title = "Consistency. Inter-item correlations",
    show.legend = FALSE
  )


## ALL CORRELATIONS AT ONCE
ggcorrplot(
  cor(taia %>% select(all_of(taia_items))),
  type = "lower",
  colors = c("indianred1", "white", "royalblue1"),
  title = "TAIA. Interitems correlations", tl.cex = 5, tl.srt = 90,
  legend.title = "Value"
)

## ANOTHER VIS FOR COLERATIONS
qgraph::qgraph(
  cor(taia %>% select(all_of(taia_items))),
  layout = "spring",
  posCol = "royalblue",
  negCol = "indianred"
)



## PSYCHOMETRIC ANALYSIS
### predictability
pr_alpha <- psych::alpha(
  taia %>% select(all_of(pr_items)),
  cumulative = TRUE,
  check.keys = FALSE
)

pr_alpha
pr_alpha$total
pr_alpha$alpha.drop
pr_alpha$item.stats %>% mutate(difficulty = mean / 5) -> pr_alpha$item.stats
pr_alpha$item.stats

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


### consistency
co_alpha <- psych::alpha(
  taia %>% select(all_of(co_items)),
  cumulative = TRUE,
  check.keys = FALSE
)

co_alpha$total
co_alpha$alpha.drop
co_alpha$item.stats %>% mutate(difficulty = mean / 5) -> co_alpha$item.stats
co_alpha$item.stats

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


## SPLITHALF RELIABILITY
### predictability
splitHalf(taia %>% select(all_of(pr_items)),
          raw=F, brute=F, n.sample=100, covar=F,
          check.keys=F, key=NULL, use="pairwise")

### consistency
splitHalf(taia %>% select(all_of(co_items)),
          raw=F, brute=F, n.sample=100, covar=F,
          check.keys=F, key=NULL, use="pairwise")

### taia
splitHalf(taia %>% select(all_of(taia_items)),
          raw=F, brute=F, n.sample=100, covar=F,
          check.keys=F, key=NULL, use="pairwise")


## ITEMS EXCLUSION
pr_items_1 <- pr_items
co_items_1 <- co_items[-7]
ut_items_1 <- ut_items[-10]
fa_items_1 <- fa_items
de_items_1 <- de_items[-4]
un_items_1 <- un_items
taia_items_1 <- c(pr_items_1,
                  co_items_1,
                  ut_items_1,
                  fa_items_1,
                  de_items_1,
                  un_items_1)



## EXPLORATORY FACTOR ANALYSIS
### check suitability
EFAtools::BARTLETT(taia %>% select(all_of(taia_items_1)))
EFAtools::KMO(taia %>% select(all_of(taia_items_1)))

## parallel analysis
psych::fa.parallel(
  taia %>% select(all_of(taia_items_1)), 
  fa = "fa",
  fm = "ml")


## try different models
efa_6f_vm <- factanal(
  taia %>% select(all_of(taia_items_1)),
  factors = 6,
  scores = "regression",
  rotation = "varimax"
)
efa_6f_vm

loadings(efa_6f_vm)
efa_6f_vm$uniquenesses %>% 
  sort(decreasing = TRUE) %>% 
  round(2)

efa_6f_pm <- factanal(
  taia %>% select(all_of(taia_items_1)),
  factors = 6,
  scores = "regression",
  rotation = "promax"
)
efa_6f_pm

loadings(efa_6f_pm)
efa_6f_pm$uniquenesses %>% 
  sort(decreasing = TRUE) %>% 
  round(2)


efa_5f_vm <- factanal(
  taia %>% select(all_of(taia_items_1)),
  factors = 5,
  scores = "regression",
  rotation = "varimax"
)
efa_5f_vm

loadings(efa_6f_vm)
efa_6f_vm$uniquenesses %>% 
  sort(decreasing = TRUE) %>% 
  round(2)

efa_5f_om <- factanal(
  taia %>% select(all_of(taia_items_1)),
  factors = 5,
  scores = "regression",
  rotation = "oblimin"
)
efa_5f_om

loadings(efa_5f_om)
efa_5f_om$uniquenesses %>% 
  sort(decreasing = TRUE) %>% 
  round(2)



## CONFIRMATORY FACTOR ANALYSIS
### basic model
mdl1 <- "
PR =~ pr01 + pr02 + pr03 + pr04 + pr05 + pr06 + pr07 + pr08 + pr09 + pr10
CO =~ co01 + co02 + co03 + co04 + co05 + co06 + co08 + co09 + co10
UT =~ ut01 + ut02 + ut03 + ut04 + ut05 + ut06 + ut07 + ut08 + ut09 + ut11 + ut12
FA =~ fa01 + fa02 + fa03 + fa04 + fa05 + fa06 + fa07 + fa08 + fa09 + fa10
DE =~ de01 + de02 + de03 + de05 + de06 + de07 + de08 + de09 + de10 + de11
UN =~ un01 + un02 + un03 + un04 + un05 + un06 + un07 + un08 + un09 + un10 + un11 + un12
"

model1 <- cfa(mdl1, taia %>% select(all_of(taia_items_1)))
summary(model1)

fitmeasures(
  model1,
  c("chisq", "df", "pvalue", "gfi", "agfi", "cfi", "tli", "srmr","rmsea")
)

smodel1 <- standardizedsolution(model1)

smodel1 %>% filter(op == "=~")
smodel1 %>% filter(op == "~~" & lhs != rhs)
smodel1 %>% filter(op == "~~" & lhs == rhs)

semPaths(model1,
         what = "std",
         whatLabels = "est",
         style = "lisrel",
         theme = "colorblind",
         rotation = 1,
         layout = "tree",
         cardinal = "lat cov",
         curvePivot = TRUE,
         sizeMan = 5,
         sizeLat = 7)

smodel1 %>%
  filter(op == "=~" & est.std < 0.4)


### model with general factor
mdl2 <- "
PR =~ pr01 + pr02 + pr03 + pr04 + pr05 + pr06 + pr07 + pr08 + pr09 + pr10
CO =~ co01 + co02 + co03 + co04 + co05 + co06 + co08 + co09 + co10
UT =~ ut01 + ut02 + ut03 + ut04 + ut05 + ut06 + ut07 + ut08 + ut09 + ut11 + ut12
FA =~ fa01 + fa02 + fa03 + fa04 + fa05 + fa06 + fa07 + fa08 + fa09 + fa10
DE =~ de01 + de02 + de03 + de05 + de06 + de07 + de08 + de09 + de10 + de11
UN =~ un01 + un02 + un03 + un04 + un05 + un06 + un07 + un08 + un09 + un10 + un11 + un12
TAIA =~ PR + CO + UT + FA + DE + UN
"

model2 <- cfa(mdl2, taia %>% select(all_of(taia_items_1)))
summary(model2)

fitmeasures(
  model2,
  c("chisq", "df", "pvalue", "gfi", "agfi", "cfi", "tli", "srmr","rmsea")
)

smodel2 <- standardizedsolution(model2)

smodel2 %>% filter(op == "=~")
smodel2 %>% filter(op == "~~" & lhs != rhs)
smodel2 %>% filter(op == "~~" & lhs == rhs)

semPaths(model2,
         what = "std",
         whatLabels = "est",
         style = "lisrel",
         theme = "colorblind",
         rotation = 1,
         layout = "tree",
         cardinal = "lat cov",
         curvePivot = TRUE,
         sizeMan = 5,
         sizeLat = 7)

smodel2 %>%
  filter(op == "=~" & est.std < 0.4)

modificationindices(model2) %>% arrange(desc(mi)) -> modif2



## CONVERGENT VALIDITY
taia %>% 
  select(id, all_of(taia_items_1)) %>% 
  pivot_longer(all_of(taia_items_1),
               names_to = "subscale",
               values_to = "score") %>% 
  mutate(subscale = str_remove_all(subscale, "[:digit:]{2}") %>% toupper()) %>% 
  summarise(total_score = sum(score),
            .by = c(id, subscale)) %>% 
  pivot_wider(id_cols = id,
              names_from = subscale,
              values_from = total_score) %>% 
  mutate(DT = PR + CO + UT + FA + DE + UN) %>% 
  full_join(taia) -> taia

taia %>% 
  pivot_longer(cols = c("PR", "CO", "UT", "FA", "DE", "UN"),
               names_to = "subscale",
               values_to = "score") %>%
  mutate(subscale = factor(subscale, 
                           ordered = TRUE, 
                           levels = c("PR", "CO", "UT", "FA", "DE", "UN"))) %>% 
  ggplot(aes(score, gt_score, color = subscale)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm") +
  facet_wrap(~ subscale, scales = "free") +
  guides(color = "none") +
  labs(x = "TAIA subscale total score",
       y = "General Trust Scale total score",
       title = "Corelations between General Trust and TAIA subscales") +
  theme(plot.title = element_text(hjust = .5))


cor.test(taia$PR, taia$gt_score)
cor.test(taia$CO, taia$gt_score)


taia %>% 
  ggplot(aes(DT, gt_score)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "TAIA score", y = "General Trust Score",
       title = "Correlation between General Trust and TAIA") +
  theme(plot.title = element_text(hjust = .5))

cor.test(taia$DT, taia$gt_score)



### CRITERION VALIDITY

specs <- c(
  "программист",
  "информатик-экономист",
  "информатика и вычислительная техника",
  "оператор пк",
  "программирование",
  "бизнес-информатика",
  "информатик- экономист",
  "информатика и икт",
  "информационная безопасность",
  "информационные системы",
  "информационные системы в экономике",
  "информационные системы и технологии",
  "информационные технологии и системы",
  "кибернетика",
  "компьютерная инженерия",
  "компьютерные системы управления",
  "прикладная информатика в экономике",
  "программист в компьютерных сетях",
  "программное обеспечение вычислительных систем",
  "сис админ",
  "системный администратор",
  "техник по информационным системам",
  "техник программист"
)

taia %>% 
  mutate(spec1 = tolower(spec1),
         spec2 = tolower(spec2),
         dig_spec = ifelse(spec1 %in% specs | spec2 %in% specs, TRUE, FALSE)) -> taia

t.test(taia$DT ~ taia$dig_spec)
