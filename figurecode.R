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
        'Female',
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
      'Female',
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



# relationship between COVID and identity ----

# grad students ----
# recode yes and no
grads$COVIDimpact <- ifelse(grads$COVID_impact_writing == "Yes", 1, 0)
grads$female <- ifelse(grads$male == 1, 0, 1)

model_bayes4 <- stan_glm(COVIDimpact ~ 
                          graduate_yrs +
                          firstgen +
                          female +
                          BIPOC +
                          condition +
                          ESL,
                          family = binomial(link = "logit"),
                        iter = 10000,
                        warmup = 5000,
                        data = grads, 
                        seed = 111)

# # check model for influential points
# loo(model_bayes4)

summary(model_bayes4, digits = 2)
posterior_interval(
  model_bayes4,
  prob = 0.5)

posteriors <- describe_posterior(model_bayes4)
# for a nicer table
print_md(posteriors, digits = 3)

# convert to probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# grad school
logit2prob(0.022)
logit2prob(0.022*2)
logit2prob(0.022*3)
logit2prob(0.022*4)
logit2prob(0.022*5)

# first gen
logit2prob(0.902)
# female
logit2prob(1.213)
# BIPOC
logit2prob(1.971)
# condition
logit2prob(0.483)
# ESL
logit2prob(-1.115)

posterior <- as.matrix(model_bayes4)


covidgrad <- bayesplot::mcmc_intervals(
  posterior,
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
      'Female',
      'First-generation',
      'Graduate training (yrs)'
    )) +
  xlab("COVID and writing habits") +
  ylab("Parameter") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(covidgrad)
ggsave(covidgrad, filename = "figures/covidgrad.png", dpi = 300, width = 5, height = 5)



# postdocs ----
# recode yes and no
postdocs$COVIDimpact <- ifelse(postdocs$COVID_impact_writing == "Yes", 1, 0)
postdocs$female <- ifelse(postdocs$male == 1, 0, 1)

model_bayes5 <- stan_glm(COVIDimpact ~ 
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

summary(model_bayes5, digits = 2)
posterior_interval(
  model_bayes5,
  prob = 0.5)

posteriors <- describe_posterior(model_bayes5)
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

## make figure
posterior <- as.matrix(model_bayes5)


covidpostdoc <- bayesplot::mcmc_intervals(
  posterior,
  pars = c(
    "ESL",
    "condition",
    "BIPOC",
    "female",
    "firstgen",
    "postdoc_yrs"
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
      'Female',
      'First-generation',
      'Postdoctoral training (yrs)'
    )) +
  xlab("COVID and writing habits") +
  ylab("Parameter") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(covidpostdoc)
ggsave(covidpostdoc, filename = "figures/covidpostdoc.png", dpi = 300, width = 5, height = 5)

# graph of relationship total pubs and total training ----
# ## this is why we need to account for this. 
# linpubs <- ggplot(aes(x = trainingtot, y = pubtotal), data = survey) +
#   geom_point(aes(size = postdoc_yrs), alpha = 0.5) +
#   scale_fill_viridis() +
#   theme_bw(base_size = 14) +
#   xlab("Total yrs as trainee (grad + postdoc)") +
#   ylab("Total publications")

# summary(lm(pubtotal ~ trainingtot, data = survey))
# summary(lm(pubtotal ~ trainingtot, data = grads))
# summary(lm(pubtotal ~ trainingtot, data = postdocs))
# cor(survey$pubtotal, survey$trainingtot, method = "pearson")
# cor(survey$pubtotal, survey$trainingtot, method = "spearman")

# # relationship first author pubs and total training ----
# linpubs <- ggplot(aes(x = trainingtot, y = firstauthor_pubs), data = survey) +
#   geom_point(pch = 21, aes(size = postdoc_yrs, fill = postdoc_yrs), alpha = 0.5) +
#   scale_fill_viridis() +
#   theme_bw(base_size = 14) +
#   xlab("Total yrs as trainee (grad + postdoc)") +
#   ylab("First-author publications")
# 
# summary(lm(firstauthor_pubs ~ trainingtot, data = survey))