#####################################################################################################################
# Code used for analyses reported in:
# The Role of Linguistic Alignment on Question Development in FTF and SCMC Contexts: A Partial Replication Study
# Authors:  YouJin Kim, Stephen Skalicky, and YeonJoo Jung
# To appear in: Language Learning

# Please send all questions or comments regarding code to Stephen Skalicky - scskalicky@gmail.com
#####################################################################################################################

# clear memory
rm(list = ls())

library(rstudioapi)
# put the data in the same folder as the code then use this to set relative dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# load required packages
library(lme4)
library(lmerTest)
library(MuMIn)
library(ggplot2)
library(effects)
library(tidyverse)
library(emmeans)
options(scipen=999)

# Priming data analysis ####
# Load the data
priming <- read.csv('priming_data.csv')

# LMEs for alignment sessions ####

# Z-score the predictors and set inversion/direct prime as the prime type reference level
priming <- priming %>%
  mutate_at(vars(WMC, indirect_production_pre, inversion_production_pre), scale, center = T, scale = T) %>%
  mutate(prime_type = relevel(prime_type, ref="dir"))

# Check for NAs
sapply(priming, function(x) sum(is.na(x)))

# Indirect Questions Alignment Session
# Main effects model for predicting indirect questions during alignment sessions. 
# Will not converge with random slope of trial order
indirect_main_effects_model <- glmer(indirect_dv ~ prime_type + WMC + modality + indirect_production_pre + (1 + prime_type |subject) + (1 | verb), priming, family="binomial"(link="logit"), glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(indirect_main_effects_model) 

# Calculate the odds ratio.
me.CI <-confint(indirect_main_effects_model,parm="beta_",level=0.90,method="Wald")
me.tab <-cbind(est=fixef(indirect_main_effects_model),me.CI)
me.tab <- exp(me.tab)
me.tab <- as.data.frame(me.tab)
me.tab <- rownames_to_column(me.tab)
me.tab

# Full indirect questions model. Slope of trial order removed because it will not converge.
# No significant interactions
indirect_full_model <- glmer(indirect_dv ~ prime_type*modality + prime_type*WMC +  prime_type*indirect_production_pre + (1 + prime_type  | subject) + (1|verb), priming, family="binomial"(link="logit"), glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(indirect_full_model)

# compare the models
anova(indirect_main_effects_model,indirect_full_model)

# Inversion Questions (direct questions) Alignment Sessions
# Change the baseline level to indirect question primes.
priming$prime_type<-relevel(priming$prime_type, ref="ind")

# Main effects model for predicting inversion questions during alignment sessions. 
inversion_main_effects_model<-glmer(inversion_dv ~ prime_type + WMC + modality +  inversion_production_pre + (1 + prime_type + trial_order | subject) + (1|verb), priming, family="binomial"(link="logit"),glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(inversion_main_effects_model)

# Odds ratios for main effects model
inversion.me.CI<-confint(inversion_main_effects_model, parm="beta_", level=0.90, method="Wald")
inversion.me.tab <- cbind(est = fixef(inversion_main_effects_model),inversion.me.CI)
inversion.me.tab <- exp(inversion.me.tab)
inversion.me.tab <- as.data.frame(inversion.me.tab)
inversion.me.tab <- rownames_to_column(inversion.me.tab)
inversion.me.tab

# Full inversion questions model.
# Significant interactions for prime_type*modality
inversion_full_model <- glmer(inversion_dv ~ prime_type*modality + prime_type*WMC + prime_type*inversion_production_pre + (1 + prime_type + trial_order | subject) + (1|verb), priming, family="binomial"(link="logit"), glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(inversion_full_model)

# Model with only the significant interaction.
inversion_full_model_sig_int_only <- glmer(inversion_dv ~ WMC +  inversion_production_pre + prime_type*modality + (1 + prime_type + trial_order | subject) + (1|verb), priming, family="binomial"(link="logit"), glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(inversion_full_model_sig_int_only)
r.squaredGLMM(inversion_full_model_sig_int_only)

# model comparisons 
# main and full
anova(inversion_main_effects_model, inversion_full_model)

# full and sig int only
anova(inversion_full_model, inversion_full_model_sig_int_only)

# main and sig interaction
anova(inversion_main_effects_model, inversion_full_model_sig_int_only)

# Odds ratios for final models.
inversion.final.CI<-confint(inversion_full_model_sig_int_only, parm="beta_", level=0.90, method="Wald")
inversion.final.tab <- cbind(est = fixef(inversion_full_model_sig_int_only),inversion.final.CI)
inversion.final.tab <- exp(inversion.final.tab)
inversion.final.tab <- as.data.frame(inversion.final.tab)
inversion.final.tab <- rownames_to_column(inversion.final.tab)
inversion.final.tab


# Visualize the interaction between prime type and modality.####

# save the specific effect to a variable
effect2 <-effect("prime_type*modality",inversion_full_model_sig_int_only)
summary(effect2)
plot(effect2)

# convert it into a data frame
effect2 <-as.data.frame(effect2)
effect2$modality <- relevel(effect2$modality, ref = 'scmc')
effect2$prime_type <- factor(effect2$prime_type, levels = c('dir', 'ind'), labels = c("Direct", "Indirect"))

# plot it nicely
ggplot(effect2, aes(x = prime_type, y = fit, linetype = modality))+
  geom_line(aes(group = modality)) +
  geom_point(aes(group = modality))+
  theme_base(base_size=12) +
  scale_linetype_manual(values=c(1,2),name="Modality",labels=c("SCMC","FTF")) +
  labs(title="Direct Question Alignment")+
  labs(x="Prime Type",y="Priming Amount (log odds)")+
  theme(legend.title=element_text(color="black",size=11,face="bold"))+
  coord_cartesian(ylim = c(-0,1))+
  theme(plot.title=element_text(size=12,face="bold",margin=margin(10,0,10,0),hjust=0.5))+
  theme(axis.title.x=element_text(face="bold",size=12))+
  theme(axis.title.y=element_text(face="bold",size=12))
  # theme(legend.justification=c(-.8,2.85),legend.position=c(.5,.9))


#### Production data analysis ####
# clear memory 
rm(list = ls())
production <- read.csv('production_data.csv')

#### Predict production of indirect questions on post tests ####

# z-score variables and change the baseline to pre test
production <- production %>%
  mutate_at(vars(WMC, indirect_priming, inv_priming, indirect_production_pre, inversion_production_pre), scale, center = T, scale = T) %>%
  mutate(test_order = relevel(test_order, ref="pre"))

# Check for NAs
sapply(production, function(x) sum(is.na(x)))

# Main effects for indirect questions during production tests.
indirect_production_me <- glmer(indirect_dv ~ WMC + modality + test_order + indirect_priming + (1 + test_order | subject) + (1 | verb), production, family = "binomial"(link = 'logit'), glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 100000)))

summary(indirect_production_me)
r.squaredGLMM(indirect_production_me)

# main effects model
ind_prod_me_CI <- confint(indirect_production_me,parm="beta_",level=0.90,method="Wald")
ind_prod_me_tab <- cbind(est = fixef(indirect_production_me), ind_prod_me_CI)
ind_prod_me_tab <- exp(ind_prod_me_tab)
ind_prod_me_tab <- as.data.frame(ind_prod_me_tab)
ind_prod_me_tab <- rownames_to_column(ind_prod_me_tab)
ind_prod_me_tab

# Model with interactions fit on modality.
indirect_production_int <- glmer(indirect_dv ~  WMC*test_order + modality*test_order +   indirect_priming*test_order + (1 + test_order | subject) + (1 | verb), production, family = "binomial"(link = 'logit'), glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 100000)))

summary(indirect_production_int) # no significant interactions.

# compare the models
anova(indirect_production_me, indirect_production_int)

# post hoc analyses to compare all levels of test order
# between groups indirect production main effect only
ind_prod_me_between_groups <- emmeans(indirect_production_me,c("test_order"), type = "response")
pairs(ind_prod_me_between_groups, reverse = F, type = 'response', adjust = 'none')
plot(ind_prod_me_between_groups)

#### direct question production on production tests ####
# main effects for indirect questions during production tests
direct_production_me <- glmer(inversion_dv ~  WMC + modality + test_order +  inv_priming + (1 + test_order | subject) + (1 | verb), production, family = "binomial"(link = 'logit'), glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 100000)))

summary(direct_production_me)
r.squaredGLMM(direct_production_me)

# calculate odds ratios
# main effects model
dir_prod_me_CI <- confint(direct_production_me,parm="beta_",level=0.90,method="Wald")
dir_prod_me_tab <- cbind(est = fixef(direct_production_me), dir_prod_me_CI)
dir_prod_me_tab <- exp(dir_prod_me_tab)
dir_prod_me_tab <- as.data.frame(dir_prod_me_tab)
dir_prod_me_tab <- rownames_to_column(dir_prod_me_tab)
dir_prod_me_tab

# between groups direct production main effect only
direct_prod_me_between_groups <- emmeans(direct_production_me,c("test_order"), type = "response")
pairs(direct_prod_me_between_groups, reverse = F, type = 'response', adjust = 'none')
pairs(direct_prod_me_between_groups, reverse = F, adjust = 'none')

# Model with interactions between test order.
direct_production_int <- glmer(inversion_dv ~  WMC*test_order + modality*test_order  + inv_priming*test_order + (1 + test_order | subject) + (1 | verb), production, family = "binomial"(link = 'logit'), glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 100000)))

summary(direct_production_int)

# interaction model
dir_prod_int_CI <- confint(direct_production_int, parm = "beta_", level = .90, method = 'Wald')
dir_prod_int_tab <- cbind(est = fixef(direct_production_int), dir_prod_int_CI)
dir_prod_int_tab <- exp(dir_prod_int_tab)
dir_prod_int_tab <- as.data.frame(dir_prod_int_tab)
dir_prod_int_tab <- rownames_to_column(dir_prod_int_tab)
dir_prod_int_tab

# Model with sig interactions only
direct_production_int_sig_only <- glmer(inversion_dv ~  WMC + modality + inv_priming*test_order + (1 + test_order | subject) + (1 | verb), production, family = "binomial"(link = 'logit'), glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 100000)))

summary(direct_production_int_sig_only)
r.squaredGLMM(direct_production_int_sig_only)

# model comparisons

# main and full
anova(direct_production_me, direct_production_int)

# full int and sig int
anova(direct_production_int, direct_production_int_sig_only)

# sig int versus full
anova(direct_production_int_sig_only, direct_production_me)

# interaction model odds ratios
dir_prod_sig_int_CI <- confint(direct_production_int_sig_only, parm = "beta_", level = .90, method = 'Wald')
dir_prod_sig_int_tab <- cbind(est = fixef(direct_production_int_sig_only), dir_prod_sig_int_CI)
dir_prod_sig_int_tab <- exp(dir_prod_sig_int_tab)
dir_prod_sig_int_tab <- as.data.frame(dir_prod_sig_int_tab)
dir_prod_sig_int_tab <- rownames_to_column(dir_prod_sig_int_tab)
dir_prod_sig_int_tab

# post hocs between groups direct production sig int effect only
direct_prod_sigInt_between_groups <- emmeans(direct_production_int_sig_only,c("test_order","inv_priming"), type = "response")
pairs(direct_prod_sigInt_between_groups, reverse = T, type = 'response', adjust = 'none')

