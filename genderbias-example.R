# 1. Loading necessary libraries ------

# uncomment line below if the gender package is not installed yet
# install.packages("gender")
library(gender)
library(plyr)
library(tidyverse)


# 2. Read in data and obtain gender based on first name ----------------

# Same as original data set, but first names replaced by top baby names in Ontario,
# Quebec, USA, and Taiwan
gsc = read_csv("2017-03-16 example gender bias ranking - Sheet1.csv")
gsc$minyear = 1980 # necessary for the gender function to work
gsc$maxyear = 2012

# create gender association with first name
gsc.gender = gender_df(gsc, method = "ssa", year_col = c("minyear", "maxyear"))
summary(gsc.gender) # gender_df drops names w/o information!
# TODO: how can I get the genders for all the names (also foreign)?

#combine gender w/ gsc ranking data
gsc.combined = left_join(gsc, gsc.gender, by = "name")

# cleaning up
gsc.combined = gsc.combined %>% 
  mutate(yearaward = paste(Date, Award)) %>% #unique grouping
  na.omit #remove all ambiguous first names to make the rest easier
gsc.combined 

# 3. Exploratory data analysis

gsc.combined %>% ggplot(aes(x = gender, y = ranking)) + geom_boxplot() + 
  facet_wrap(~ yearaward, scales = "free", as.table = TRUE, dir = "v") + 
  theme_light() + ggtitle("Ranked according to date") 

# 4. Mann-Whitney gender.test function --------
# based on the STATA article

gender.test = function(x){
  # use data provided by Wilcox test to compute effect size: 
  # probability that a female grad student will have a higher ranking than 
  # a male grad student
  
  groups = tapply(x$ranking, x$gender, length) # compute numbers in each group
  if (length(groups) == 1) {
    # this is necessary to capture comparisons w/ only male or female grad students
    c(p.value = NA, effect.size = NA, 
      samplesize = groups, prop.female = ifelse(names(groups) == "female", 100, 0))
  }
  else {
    # wilcox.test provides the p-value and nominator of the effect size calculation
    # The denominator is the number of female-male comparison pairs
    # The output also has number of students and the proportion of females
    # See http://www.stata-journal.com/article.html?article=st0253
    # TODO: is this the correct implementation of the STATA article?
    result = wilcox.test(ranking ~ gender, alternative = "greater", exact = T, data = x)
    c(p.value = result$p.value, effect.size = unname(result$statistic/(groups[1]*groups[2])),
      sample.size = unname(groups[1] + groups[2]), prop.female = unname(groups[1]/(groups[1]+groups[2])))
    # TODO: why are the variable names changed? Solved w/ unname
  }
}

# another example to check results w/ stata
prob2 = data.frame(gender = rep(c(0,1), each = 10), ranking = c(2,4,3,1,2,3,3,2,3,1,3,5,4,2,4,3,5,5,3,2))
gender.test(prob2) # effect size corresponds! => function works correctly

# 5.GSC gender bias calculations -------------

# Apply gender.test function to each award
# summary.gender.gsc = as.data.frame(t(sapply(split(gsc.combined, gsc.combined$yearaward), gender.test)))
# TODO: Can I pipe these type of functions? yes, see code below

summary.gender.gsc = gsc.combined %>% split(.$yearaward) %>%
  map(gender.test) %>% ldply()


# 6. Analysis --------

# Significance per ranking exercise?
boxplot(summary.gender.gsc$p.value, ylim = c(0,1))
abline(h = 0.05) # distribution of the p-values
title("P-values of effect sizes per ranking exercise")

# What is the gender bias effect size?
boxplot(summary.gender.gsc$effect.size, ylim = c(0,1))
abline(h = 0.5) # distribution of the p-values
title("Effect size: probability that a female grad student \n has a higher ranking than a male graduate student")

# Is there evidence across all ranking exercises combined (i.e. a meta-analysis)?
t.test(summary.gender.gsc$effect.size, alternative = "greater", mu = 0.5)
# p-value = 0.49, perfect, because names were randomly distributed
