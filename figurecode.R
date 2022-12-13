# libraries
library(ggplot2) 
library(viridis)
library(RColorBrewer)
library(tm)
library(dplyr)
library(ggpubr)
library(rstanarm)
library(remotes)
library(easystats)
library(tidyr)
library(ggridges)
library(glue)
library(bayesplot)

# load data
survey <- read.csv('dataclean_Nov2.csv', header = TRUE)


# replace empty cells with NA
# transform all columns
survey <- survey %>% mutate_each(funs(empty_as_na)) 

# convert columns from character to factor
str(survey)
survey$condition <- as.factor(survey$condition)
survey$BIPOC <- as.factor(survey$BIPOC)
survey$first_gen <- as.factor(survey$first_gen)
survey$first_language_english <- as.factor(survey$first_language_english)
survey$gender_identity <- as.factor(survey$gender_identity)
survey$writing_word <- as.factor(survey$writing_word)
survey$review_word <- as.factor(survey$review_word)

# recode gender identity to eliminate non-binary/third
survey$female <- ifelse(test = survey$gender_identity == "Female", 1, 0)
survey$ESL <- ifelse(test = survey$first_language_english == "Yes", 0, 1)
survey$firstgen <- ifelse(test = survey$first_gen == "Yes", 1, 0)
survey$BIPOC <- ifelse(test = survey$BIPOC == "Yes", 1, 0)
survey$condition <- ifelse(test = survey$condition == "Yes", 1, 0)

# create new columns for total training and pubs
survey$trainingtot <- rowSums(survey[,c("graduate_yrs", "postdoc_yrs")], na.rm=TRUE)
survey$pubtotal <- rowSums(survey[,c("firstauthor_pubs", "coauthor_pubs")], na.rm=TRUE)

# create a new column for career stage
survey$stage <- ifelse(is.na(survey$postdoc_yrs), "grad", "postdoc")

# make two dataframes for grads and postdocs ----
grads <- subset(survey, is.na(survey$postdoc_yrs))
postdocs <- subset(survey, !is.na(survey$postdoc_yrs))

# career interests ----
# make a career database
grads2 <- gather(grads[,c(11:17)], factor_key=TRUE)
postdocs2 <- gather(postdocs[,c(11:17)], factor_key = TRUE)
survey2 <- gather(survey[,c(11:17)], factor_key = TRUE)
grads2 %>% dplyr::group_by(key)%>%
  dplyr::summarise(mean= mean(value, na.rm = TRUE), 
            sd= sd(value,na.rm = TRUE), 
            max = max(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE))

postdocs2 %>% dplyr::group_by(key)%>%
  dplyr::summarise(mean= mean(value, na.rm = TRUE), 
                   sd= sd(value,na.rm = TRUE), 
                   max = max(value, na.rm = TRUE),
                   min = min(value, na.rm = TRUE))




# colored by density function
all_career <- ggplot(aes(x = value, y = key, fill = 0.5-abs(0.5-stat(ecdf))), data = survey2) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_gradientn(name = "Tail probability",
                         colours = c("#405364","#585b74","#6c5b7b","#966480","#c6798f", "#df858e", "#eda09c"),
                         values = c(1, 0.83, 0.66, 0.49, 0.32, 0.15, 0)) +
  theme_classic(base_size = 16) +
  xlim(0,10) +
  theme(panel.border = element_rect(fill = NA, size = 1),
        axis.line = element_blank()) +
  #scale_fill_viridis_c(name = "Tail probability", direction = -1) +
  ylab("Career path") +
  xlab("Interest level") +
  scale_y_discrete(labels = c('Teaching','R2 or R3',
                              'R1', 'Government',
                              'Industry or Data Science',
                              'Communication',
                              'NGO'))

# ggsave(all_career, filename = glue("figures/career_interest_figure_{Sys.Date()}.png"), width = 7, height = 5, dpi=300)


grad_career <- ggplot(aes(x = value, y = key, fill = 0.5-abs(0.5-stat(ecdf))), data = grads2) +
  #geom_density_ridges(scale = 1.5, alpha = 0.7, jittered_points = FALSE) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  theme_classic(base_size = 14) +
  xlim(0,10) +
  theme(panel.border = element_rect(fill = NA, size = 1),
        axis.line = element_blank()) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1) +
  ylab("Career path") +
  xlab("Interest level") +
  theme(legend.position = "none")

postdoc_career <- ggplot(aes(x = value, y = key, fill = key), data = postdocs2) +
  geom_density_ridges(scale = 1.5, stat = "identity", alpha = 0.7, jittered_points = FALSE) +
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(fill = NA, size = 1),
        axis.line = element_blank()) +
  scale_fill_viridis_d() +
  ylab("Career path") +
  xlab("Interest level") +
  theme(legend.position = "none")

ggarrange(grad_career +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.y = element_blank() ),
          postdoc_career +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.y = element_blank() ),
          ncol = 2,
          align = "hv",
          legend = "none")





# identity ----
survey %>%
  group_by(stage) %>%
  summarize(avgfirst = mean(firstauthor_pubs, na.rm = TRUE),
            avgco = mean(coauthor_pubs, na.rm = TRUE),
            sdfirst = sd(firstauthor_pubs, na.rm = TRUE),
            sdco = sd(coauthor_pubs, na.rm = TRUE))

# First_gen n = 272, grad = 194 respondents, postdoc = 78
survey %>%
  group_by(first_gen) %>%
  summarize(n())

# 193 grads, 140 female, 47 male, 6 non-binary-thirdgender/other
# 76 postdocs, 46 female, 28 male, 2 other
survey %>%
  group_by(gender_identity) %>%
  summarize(n())

# disability or health issue
# n = 193 grads, n = 77 postdocs
survey %>%
  group_by(condition) %>%
  summarize(n())

# first language English
# n = 199 grads, n = 78 postdocs
survey %>%
  group_by(first_language_english) %>%
  summarize(n())

# BIPOC
# n = 360, 53 = yes, 307 = no
survey %>%
  group_by(BIPOC) %>%
  summarize(n())


# publishing history
first_author <- ggplot(aes(x = firstauthor_pubs, fill = stage), data = survey) +
  geom_density(alpha = 0.2) + 
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(fill = NA, size = 1),
        axis.line = element_blank()) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  xlab("Density") +
  xlab("First-authored publications") +
  xlim(c(0,20))


co_author <- ggplot(aes(x = coauthor_pubs, fill = stage), data = survey) +
  geom_density(alpha = 0.2) + theme_bw(base_size = 14) +
  xlab("Density") +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(fill = NA, size = 1),
        axis.line = element_blank()) +
  xlab("Co-authored publications") +
  xlim(c(0,20))

ggarrange(labels = "AUTO",
          align = "hv",
          first_author, 
          co_author, 
          ncol=1, 
          nrow=2, 
          common.legend = TRUE, 
          legend="bottom")
ggsave(filename = "figures/firstandcopubs.png", dpi = 300, height = 8, width = 8)



# linear models comparing pubs vs. total training ----
summary(lm(pubtotal ~ trainingtot, data = survey))
summary(lm(firstauthor_pubs ~ trainingtot, data = survey))
summary(lm(coauthor_pubs ~ trainingtot, data = survey))

# linear models comparing pubs vs. grad + postdoc ----
# maybe analyze postdocs and grads separately?
# for those with postdoc experience, grad yrs didn't matter
summary(lm(pubtotal ~ graduate_yrs + postdoc_yrs, data = survey))
summary(lm(firstauthor_pubs ~ graduate_yrs + postdoc_yrs, data = survey))
summary(lm(coauthor_pubs ~ graduate_yrs + postdoc_yrs, data = survey))

# linear models comparing pubs vs. grad training ----
summary(lm(pubtotal ~ graduate_yrs, data = survey))
summary(lm(firstauthor_pubs ~ graduate_yrs, data = survey))
summary(lm(coauthor_pubs ~ graduate_yrs, data = survey))

# linear models comparing pubs vs. postdoc training ----
summary(lm(pubtotal ~ postdoc_yrs, data = survey))
summary(lm(firstauthor_pubs ~ postdoc_yrs, data = survey))
summary(lm(coauthor_pubs ~ postdoc_yrs, data = survey))

# # for grads, only yrs in graduate school matters
# # same for first and co-author pubs separately
# summary(lm(pubtotal ~
#              graduate_yrs +
#              first_gen +
#              gender_identity +
#              BIPOC +
#              condition +
#              first_language_english +
#              hrs_wk_writing, 
#            data = grads), na.rm = TRUE)

# grads and postdocs vs. identity in publishing ----
# Bayesian model for graduate students only
model_bayes2 <- stan_glm(
  pubtotal ~
    graduate_yrs +
    firstgen +
    female +
    BIPOC +
    condition +
    ESL,
  iter = 10000,
  cores = 3,
  chains = 4,
  warmup = 5000,
  data = grads,
  seed = 111
)

summary(model_bayes2)

# Bayesian model for postdocs only
model_bayes <- stan_glm(
  pubtotal ~
    graduate_yrs +
    postdoc_yrs +
    firstgen +
    female +
    BIPOC +
    condition +
    ESL,
  iter = 10000,
  cores = 3,
  chains = 4,
  warmup = 5000,
  data = postdocs,
  seed = 111
)

# model summaries
summary(model_bayes, digits = 3)
posterior_interval(
  model_bayes,
  prob = 0.9)


# # check models for influential points using 'loo'
# loo(model_bayes)



#### posterior plots for multiple regressions ----
color_scheme_set("darkgray")

## postdoc multiple regression
posteriors <- describe_posterior(model_bayes)
# for a nicer table
print_md(posteriors, digits = 3)

multreg_plot <- mcmc_intervals(
  posterior,
  pars = c(
    "ESL",
    "condition",
    "BIPOC",
    "female",
    "firstgen",
    "postdoc_yrs",
    "graduate_yrs"
),
    prob_outer = 0.95) +
    #plot_title +
    theme_bw(base_size = 16) +
    geom_vline(
      xintercept = 0,
      linetype = "dotted",
      colour = "black",
      size = 1
    ) +
    scale_y_discrete(
      labels = c(
        'ESL',
        'Chronic condition',
        'BIPOC',
        'Female-identifying',
        'First-generation',
        'Postdoc training (yrs)',
        'Graduate training (yrs)'
      )
    ) +
    xlab("Effect on total publications") +
    ylab("Parameter") +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
print(multreg_plot)
ggsave(multreg_plot, filename = "figures/postdocmulti.png", dpi = 300, width = 5, height = 5)

## grad multiple regression
# postdoc multiple regression

posterior2 <- as.matrix(model_bayes2)

label_wrap <- function(variable, value) {
  lapply(strwrap(as.character(value), width=10, simplify=FALSE), 
         paste, collapse="\n")
}

multreggrad_plot <- bayesplot::mcmc_intervals(
  posterior2,
  pars = c(
    "ESL",
    "condition",
    "BIPOC",
    "female",
    "firstgen",
    "graduate_yrs"
  ),
  prob_outer = 0.95) +
  #plot_title +
  theme_bw(base_size = 16) +
  geom_vline(
    xintercept = 0,
    linetype = "dotted",
    colour = "black",
    size = 1
  ) +
  scale_y_discrete(
    labels = c(
      'ESL',
      'Chronic condition',
      'BIPOC',
      'Female-identifying',
      'First-generation',
      'Graduate training (yrs)'
    )) +
  xlab("Effect on total publications") +
  ylab("Parameter") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(multreggrad_plot)
ggsave(filename = "figures/gradmulti.png", dpi = 300, width = 5, height = 5)


## make side-by-side


identitymulti <- ggarrange(
  multreggrad_plot,
  multreg_plot,
  ncol = 2,
  nrow = 1,
  align = "hv",
  labels = "AUTO",
  font.label = list(color = "black", size = 14)
)

print(identitymulti) 
ggsave(filename = "figures/identitymulti.png", dpi = 300, height = 8, width = 12)

summary(model_bayes, digits = 2)
posteriors <- describe_posterior(model_bayes)
# for a nicer table
print_md(posteriors, digits = 2)

posterior <- as.matrix(model_bayes)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("graduate_yrs", 
                    "postdoc_yrs",
                    "first_genYes", 
                    "gender_identityMale", 
                    "gender_identityNon-binary/third gender/other",
                    "BIPOCYes", 
                    "conditionYes", 
                    "first_language_englishYes"),
           prob = 0.8) + plot_title

mcmc_intervals(posterior, pars = c("graduate_yrs", 
                                   "postdoc_yrs", 
                                   "first_genYes", 
                                   "male", 
                                   "BIPOCYes", 
                                   "conditionYes", 
                                   "ESL"))


# relationship between COVID and identity ----

# grad students ----
# recode yes and no
grads$COVIDimpact <- ifelse(grads$COVID_impact_writing == "Yes", 1, 0)
grads$female <- ifelse(grads$male == 1, 0, 1)

model_bayes <- stan_glm(COVIDimpact ~ 
                          graduate_yrs +
                          firstgen +
                          female +
                          BIPOC +
                          condition +
                          ESL,
                          family = binomial,
                        iter = 10000,
                        warmup = 5000,
                        data = grads, 
                        seed = 111)

loo(model_bayes)

summary(model_bayes, digits = 2)
posterior_interval(
  model_bayes,
  prob = 0.5)

posteriors <- describe_posterior(model_bayes)
# for a nicer table
print_md(posteriors, digits = 3)

# convert to probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# grad school
logit2prob(0.033)
logit2prob(0.033*2)
logit2prob(0.033*3)
logit2prob(0.033*4)
logit2prob(0.033*5)

# first gen
logit2prob(0.972)
# female
logit2prob(0.431)
# BIPOC
logit2prob(2.103)
# condition
logit2prob(0.355)
# ESL
logit2prob(-1.574)

posterior <- as.matrix(model_bayes)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("graduate_yrs", 
                    "firstgen", 
                    "female",
                    "BIPOC", 
                    "condition", 
                    "ESL"),
           prob = 0.5) + plot_title

COVIDgrads <- mcmc_intervals(posterior, pars = c("graduate_yrs", 
                                   "firstgen",
                                   "female",
                                   "BIPOC",
                                   "condition",
                                   "ESL"))

# postdocs ----
# recode yes and no
postdocs$COVIDimpact <- ifelse(postdocs$COVID_impact_writing == "Yes", 1, 0)
postdocs$female <- ifelse(postdocs$male == 1, 0, 1)

model_bayes <- stan_glm(COVIDimpact ~ 
                         postdoc_yrs +
                          firstgen +
                          female +
                          BIPOC +
                          condition +
                          ESL,
                        family = binomial,
                        iter = 10000,
                        warmup = 5000,
                        chains = 4, 
                        data = postdocs, 
                        seed = 111)

summary(model_bayes, digits = 2)
posterior_interval(
  model_bayes,
  prob = 0.5)

posteriors <- describe_posterior(model_bayes)
# for a nicer table
print_md(posteriors, digits = 3)

# convert to probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# postdoc years
logit2prob(0.269)
logit2prob(0.269*2)
logit2prob(0.269*3)
logit2prob(0.269*4)
# first gen
logit2prob(0.39)
# female
logit2prob(0.693)
# BIPOC
logit2prob(0.528)
# condition
logit2prob(-1.222)
# ESL
logit2prob(-1.369)

posterior <- as.matrix(model_bayes)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("postdoc_yrs", 
                    "firstgen", 
                    "female",
                    "BIPOC", 
                    "condition", 
                    "ESL"),
           prob = 0.5) + plot_title

COVIDpostdocs <- mcmc_intervals(posterior, pars = c("postdoc_yrs",
                                                    "firstgen",
                                                 "female",
                                                 "BIPOC",
                                                 "condition",
                                                 "ESL"))

# graph of relationship total pubs and total training ----
linpubs <- ggplot(aes(x = trainingtot, y = pubtotal), data = survey) +
  geom_point(aes(size = postdoc_yrs), alpha = 0.5) +
  scale_fill_viridis() +
  theme_bw(base_size = 14) +
  xlab("Total yrs as trainee (grad + postdoc)") +
  ylab("Total publications")

summary(lm(pubtotal ~ trainingtot, data = survey))
summary(lm(pubtotal ~ trainingtot, data = grads))
summary(lm(pubtotal ~ trainingtot, data = postdocs))
cor(survey$pubtotal, survey$trainingtot, method = "pearson")
cor(survey$pubtotal, survey$trainingtot, method = "spearman")

# relationship first author pubs and total training ----
linpubs <- ggplot(aes(x = trainingtot, y = firstauthor_pubs), data = survey) +
  geom_point(pch = 21, aes(size = postdoc_yrs, fill = postdoc_yrs), alpha = 0.5) +
  scale_fill_viridis() +
  theme_bw(base_size = 14) +
  xlab("Total yrs as trainee (grad + postdoc)") +
  ylab("First-author publications")

summary(lm(firstauthor_pubs ~ trainingtot, data = survey))

# does writing more mean more papers? YES
writepubs <- ggplot(aes(x = hrs_wk_writing, y = pubtotal), data = survey) +
  geom_point(pch = 21, aes(size = graduate_yrs, fill = postdoc_yrs), alpha = 0.5) +
  scale_fill_viridis() +
  theme_bw(base_size = 14) +
  xlab("Hrs per week devoted to writing") +
  ylab("All publications")

writetrain <- ggplot(aes(y = hrs_wk_writing, x = trainingtot), data = survey) +
  geom_point(pch = 21, aes(size = graduate_yrs, fill = postdoc_yrs), alpha = 0.5) +
  scale_fill_viridis() +
  theme_bw(base_size = 14) +
  ylab("Hrs per week devoted to writing") +
  xlab("Yrs as trainee")

# hrs writing per week and training
summary(lm(hrs_wk_writing ~ trainingtot, data = survey)) #yes - more training = more time
summary(lm(hrs_wk_writing ~ trainingtot, data = grads)) #yes - more training = more time
summary(lm(hrs_wk_writing ~ trainingtot, data = postdocs)) #no - not increasing writing time with training

# total writing time and first author pubs
summary(lm(firstauthor_pubs ~ hrs_wk_writing, data = survey)) #yes
summary(lm(firstauthor_pubs ~ hrs_wk_writing, data = grads)) #no
summary(lm(firstauthor_pubs ~ hrs_wk_writing, data = postdocs)) #marginal

# hrs per week and coauthor pubs
summary(lm(coauthor_pubs ~ hrs_wk_writing, data = survey)) #no
summary(lm(coauthor_pubs ~ hrs_wk_writing, data = grads)) #yes
summary(lm(coauthor_pubs ~ hrs_wk_writing, data = postdocs)) #no

# hrs per week and all pubs
summary(lm(pubtotal ~ hrs_wk_writing, data = survey)) #yes
summary(lm(pubtotal ~ hrs_wk_writing, data = grads)) #yes
summary(lm(pubtotal ~ hrs_wk_writing, data = postdocs)) #no

# made a word cloud for writing attitudes ----

# attitudes toward science writing and review
summary(survey$writing_word)
summary(survey$review_word)

# boxplots attitudes about writing and review ----
# writing word connotation vs. time spent writing
library(car)
survey$writing_word <- as.factor(survey$writing_word)
summary((lm(hrs_wk_writing ~ writing_word, data = survey, na.rm = TRUE))
boxplot(hrs_wk_writing ~ writing_word, data = survey)

# writing tracking ----
boxplot(hrs_wk_writing ~ plan_writing, data = survey)
summary(lm(hrs_wk_writing ~ plan_writing, data = survey, na.rm = TRUE))
# plan writing model
plan_model <- stan_glm(hrs_wk_writing ~ plan_writing, data = survey)
summary(plan_model)
posteriors <- describe_posterior(plan_model)
# for a nicer table
print_md(posteriors, digits = 3)

# time per week spent writing and attitude toward writing
# plan writing model
attitude_model <- stan_glm(hrs_wk_writing ~ writing_word, data = survey)
summary(attitude_model)
posteriors <- describe_posterior(attitude_model)
# for a nicer table
print_md(posteriors, digits = 3)

attitude_model <- lm(hrs_wk_writing ~ writing_word, data = survey)
summary(attitude_model)
# writing attitude vs. plan writing ----

survey$plan_writing <- as.numeric(survey$plan_writing)
attitude_model <- stan_glm(plan_writing ~ 1 + writing_word, 
                           data = survey, 
                           family = binomial)

library(bayestestR)
library(logspline)
describe_posterior(attitude_model, test = c("p_direction", "rope", "bayesfactor"))
summary(attitude_model, digits = 3)
posteriors <- posterior(attitude_model)
boxplot(hrs_wk_writing ~ writing_word, data = survey)

# convert to probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# positive probability of tracking writing
logit2prob(0.365)
logit2prob(0.064)
logit2prob(-0.824)


# writing success (i.e., pubs) linked to feelings about writing ----
writing_box <- ggplot(aes(x = writing_word, y = pubtotal), data = survey) +
  geom_boxplot() + theme_bw(base_size = 14) +
  xlab("Feelings about writing process") +
  ylab("Total publications")

ggplot(aes(y = writing_word, x = stage, fill = stage), data = survey) +
  geom_boxplot() + 
  theme_bw(base_size = 14)

ggplot(survey, aes(fill = writing_word, x = stage)) + 
  geom_bar(position="stack", stat="identity")

writing_box <- ggplot(aes(y = pubtotal, x = writing_word, fill = stage),
                      data = survey) +
  geom_boxplot() + theme_bw(base_size = 14) +
  xlab("Feelings about writing process") +
  ylab("Total publications")

ggplot(aes(x = review_word, y = pubtotal, fill = stage), 
       data = survey) +
  geom_boxplot() + theme_bw(base_size = 14) +
  xlab("Feelings about peer review") +
  ylab("Total publications")

# all data and grads yes, postdocs alone = no
summary(aov(lm(pubtotal ~ writing_word, data = survey)))
summary(aov(lm(pubtotal ~ writing_word, data = grads)))
grad.aov <- aov(lm(pubtotal ~ writing_word, data = grads))
TukeyHSD(grad.aov)
summary(aov(lm(pubtotal ~ writing_word, data = postdocs))) # not for postdocs

pub_attitude <- stan_glm(pubtotal ~ writing_word, data = survey,
                         chains = 4)
summary(pub_attitude, digits = 3)

# writing attitude IS linked to first-author pubs for grads
summary(lm(firstauthor_pubs ~ writing_word, data = survey)) # yes
summary(lm(firstauthor_pubs ~ writing_word, data = grads)) # yes
summary(lm(firstauthor_pubs ~ writing_word, data = postdocs)) #nope

# writing attitude not linked to co-author pubs
summary(lm(coauthor_pubs ~ writing_word, data = survey)) # no
summary(lm(coauthor_pubs ~ writing_word, data = grads)) # yes
summary(lm(coauthor_pubs ~ writing_word, data = postdocs)) # no


# writing success (i.e., pubs) NOT linked to peer-review attitude
review_box <- ggplot(aes(x = review_word, y = pubtotal, fill = stage), 
                     data = na.omit(survey[,c("pubtotal", "review_word", "stage")])) +
  geom_boxplot() + theme_bw(base_size = 14) +
  xlab("Feelings about peer review") +
  ylab("Total publications")

ggarrange(common.legend = TRUE,
          writing_box, 
         review_box, 
         align = "hv", 
         nrow = 2,
         labels = "AUTO"
        )

# peer review attitude not linked to total pubs
summary(lm(pubtotal ~ review_word, data = survey))
summary(lm(pubtotal ~ review_word, data = grads))
summary(lm(pubtotal ~ review_word, data = postdocs))

# peer review attitude not linked to first-author pubs
summary(lm(firstauthor_pubs ~ review_word, data = survey))
summary(lm(firstauthor_pubs ~ review_word, data = grads))
summary(lm(firstauthor_pubs ~ review_word, data = postdocs))

# peer review attitude not linked to co-author pubs
summary(lm(coauthor_pubs ~ review_word, data = survey))
summary(lm(coauthor_pubs ~ review_word, data = grads))
summary(lm(coauthor_pubs ~ review_word, data = postdocs))






# make writing groups figure
# if else hell
survey$GoalSetting <- ifelse(survey[,39]=="Improved",1, ifelse(survey[,39]=="No Change", 0, -1))
survey$Reviews <- ifelse(survey[,40]=="Improved",1, ifelse(survey[,40]=="No Change", 0, -1))
survey$Collaboration <- ifelse(survey[,41]=="Improved",1, ifelse(survey[,41]=="No Change", 0, -1))
survey$Camaraderie <- ifelse(survey[,42]=="Improved",1, ifelse(survey[,42]=="No Change", 0, -1))
survey$Skills <- ifelse(survey[,43]=="Improved",1, ifelse(survey[,43]=="No Change", 0, -1))
survey$Starting <- ifelse(survey[,44]=="Improved",1, ifelse(survey[,44]=="No Change", 0, -1))
survey$TimeManagement <- ifelse(survey[,45]=="Improved",1, ifelse(survey[,45]=="No Change", 0, -1))
survey$Perfectionism <- ifelse(survey[,46]=="Improved",1, ifelse(survey[,46]=="No Change", 0, -1))
survey$Anxiety <- ifelse(survey[,47]=="Improved",1, ifelse(survey[,47]=="No Change", 0, -1))
survey$Imposter <- ifelse(survey[,48]=="Improved",1, ifelse(survey[,48]=="No Change", 0, -1))
survey$Quality <- ifelse(survey[,49]=="Improved",1, ifelse(survey[,49]=="No Change", 0, -1))
survey$Output <- ifelse(survey[,50]=="Improved",1, ifelse(survey[,50]=="No Change", 0, -1))

# reshape the data
library(tidyr)

groups_wide <- survey[,c(7, 76:87)]
groups_wide2 <- survey[,c(7, 39:50)]

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
groups_long <- gather(groups_wide2, key = factor, value = rating, Goal.setting:Writing.output, factor_key=TRUE)
groups_long

require(dplyr)

g <- groups_long %>%
  group_by(factor, rating) %>%
  summarise(cnt = n(), na.rm = TRUE) %>%
  mutate(freq = round(cnt / sum(cnt), 3), na.rm = TRUE) %>% 
  arrange(desc(freq))

# remove NAs
g2 <- as.data.frame(g[c(13:42),])

# Build plot
# use function likert() to plot likert data
g2 <- ggplot()+
  geom_bar(data = g2, aes(x = reorder(factor, cnt), y=cnt, fill=as.factor(rating)), position="stack", stat="identity")+
  coord_flip() + 
  ylab("Rating")+
  xlab("Factor")+
  scale_fill_brewer(palette="PRGn",
                    name  ="Response",
                    labels=c("Improved", "No Change", "Worsened"))+
  theme(legend.position="bottom") +
  theme_bw(base_size = 14) +
  xlab("Accountability group effect") +
  ylab("Number of respondents")
#scale_x_discrete(limits=c("StronglyAgree", "Agree", "DontKnow","Disagree","StronglyDisagree"))
g2

View()
