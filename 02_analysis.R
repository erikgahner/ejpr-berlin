###
## Article:   Do Terrorist Attacks Feed Populist Eurosceptics?
##            Evidence from Two Comparative Quasi-Experiments
## 
##            European Journal of Political Research
##
##            Erik Gahner Larsen          David Cutts             Matthew J. Goodwin
##            E.G.Larsen@kent.ac.uk       D.Cutts@bham.ac.uk      M.J.Goodwin@kent.ac.uk 
##
##        
## Data:      Chatham House Survey: -
##            European Social Survey: http://www.europeansocialsurvey.org/
##            Eurobarometer: http://ec.europa.eu/commfrontoffice/publicopinion/
##            German Longitudinal Election Study: http://gles.eu/wordpress/english/
##
###

# Load packages
library("tidyverse")
library("geofacet")
library("rio")
library("countrycode")
library("scales")
library("grid")
library("gridExtra")
library("dataMaid")
library("stargazer")
library("xtable")
library("interplot")
library("MatchIt")
library("TOSTER")

# See 01_create-data.R on how to create the specific .csv files
ess <- import("data_ess.csv")
ch <- import("data_ch.csv")
eb <- import("data_eb.csv")
gles <- import("data_gles.csv")

# No scientific notation in print output
options(scipen=999)

# Set select() to dplyr::select as default
select <- dplyr::select

# Set theme options
theme_set(
  theme_grey(base_size = 11.5) %+replace% 
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"), plot.background = element_blank(), panel.background = element_blank(),
      panel.border = element_blank(), legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA), legend.title = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.3), panel.grid.minor = element_blank(),
      axis.ticks = element_blank(), axis.line = element_line(color = "#FFFFFF", size = 0.3),
      plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
      plot.caption = element_text(size = 10, colour = "#212121", margin = margin(t = 15)),
      axis.title = element_text(size = 11, face = "plain"), axis.text = element_text(size = 10, face = "plain"),
      legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain")
    )
)

# Create panel A in Figure 1
fig_best <- ch %>%
  filter(Q2a == 1 | Q2a == 3 | Q2a == 4 | Q2a == 5 | Q2a == 7) %>%
  group_by(eu_best) %>%
  summarise(number=n()) %>%
  ggplot(aes(x = reorder(eu_best, -number), y = number / sum(number))) +
  geom_bar(stat="identity") +
  xlab("") +
  ylab("") +
  ggtitle("(A) Greatest achievement of the EU") +
  scale_y_continuous(labels=percent) +
  coord_flip()

# Create panel B in Figure 1
fig_worst <- ch %>%
  filter(Q3a == 1 | Q3a == 3 | Q3a == 4 | Q3a == 6 | Q3a == 8) %>%
  group_by(eu_worst) %>%
  summarise(number=n()) %>%
  ggplot(aes(x = reorder(eu_worst, -number), y = number / sum(number))) +
  geom_bar(stat="identity") +
  xlab("") +
  ylab("") +
  ggtitle("(B) Greatest failure of the EU") +
  scale_y_continuous(labels=percent) +
  coord_flip()

pdf("Figure_1.pdf", width=10, height=4)
grid.arrange(fig_best, fig_worst, ncol=2)
dev.off()

# Regression models, bivariate
direct_reg_ch_biv <- function(x, y){
  lm(paste(x," ~ tr"), data = ch[ch$name == y,])
}

direct_reg_ess_biv <- function(x, y){
  lm(paste(x," ~ tr"), data = ess[ess$name == y,])
}

ch_reg_eu1_biv <- direct_reg_ch_biv("eu1", "Germany")
ch_reg_eu2_biv <- direct_reg_ch_biv("eu2", "Germany")
ch_reg_eu3_biv <- direct_reg_ch_biv("eu3", "Germany")
ch_reg_eu4_biv <- direct_reg_ch_biv("eu4", "Germany")
ess_reg_eu5_biv <- direct_reg_ess_biv("eu5", "Germany")


ch_reg_imm1_biv <- direct_reg_ch_biv("imm1", "Germany")
ch_reg_imm2_biv <- direct_reg_ch_biv("imm2", "Germany")
ch_reg_imm3_biv <- direct_reg_ch_biv("imm3", "Germany")
ch_reg_imm4_biv <- direct_reg_ch_biv("imm4", "Germany")
ch_reg_imm5_biv <- direct_reg_ch_biv("imm5", "Germany")
ch_reg_imm6_biv <- direct_reg_ch_biv("imm6", "Germany")
ess_reg_imm7_biv <- direct_reg_ess_biv("imm7", "Germany")

ch_reg_ref1_biv <- direct_reg_ch_biv("ref1", "Germany")
ess_reg_ref2_biv <- direct_reg_ess_biv("ref2", "Germany")
ess_reg_ref3_biv <- direct_reg_ess_biv("ref3", "Germany")
ess_reg_ref4_biv <- direct_reg_ess_biv("ref4", "Germany")

direct_reg_ch <- function(x, y){
  lm(paste(x," ~ tr + male + age + edu + income + unemployed + lrscale"), data = ch[ch$name == y,])
}
direct_reg_ess <- function(x, y){
  lm(paste(x," ~ tr + male + age + edu + income + unemployed + lrscale"), data = ess[ess$name == y,])
}

ch_reg_eu1 <- direct_reg_ch("eu1", "Germany")
ch_reg_eu2 <- direct_reg_ch("eu2", "Germany")
ch_reg_eu3 <- direct_reg_ch("eu3", "Germany")
ch_reg_eu4 <- direct_reg_ch("eu4", "Germany")
ess_reg_eu5 <- direct_reg_ess("eu5", "Germany")

ch_reg_imm1 <- direct_reg_ch("imm1", "Germany")
ch_reg_imm2 <- direct_reg_ch("imm2", "Germany")
ch_reg_imm3 <- direct_reg_ch("imm3", "Germany")
ch_reg_imm4 <- direct_reg_ch("imm4", "Germany")
ch_reg_imm5 <- direct_reg_ch("imm5", "Germany")
ch_reg_imm6 <- direct_reg_ch("imm6", "Germany")
ess_reg_imm7 <- direct_reg_ess("imm7", "Germany")

ch_reg_ref1 <- direct_reg_ch("ref1", "Germany")
ess_reg_ref2 <- direct_reg_ess("ref2", "Germany")
ess_reg_ref3 <- direct_reg_ess("ref3", "Germany")
ess_reg_ref4 <- direct_reg_ess("ref4", "Germany")

# Balance tests
cov_number <- 6
ch_country <- NROW(unique(ch$name[ch$country == 1]))
ess_country <- NROW(unique(ess$name[ess$country == 1]))

covariate_df <- 
  data.frame(
    country = c(rep("Austria", cov_number),
                rep("Hungary", cov_number),
                rep("Belgium", cov_number),
                rep("Greece", cov_number),
                rep("Germany", cov_number),
                rep("France", cov_number),
                rep("Spain", cov_number),
                rep("Poland", cov_number),
                
                rep("Germany", cov_number),
                rep("Estonia", cov_number),
                rep("France", cov_number),
                rep("Israel", cov_number),
                rep("Netherlands", cov_number),
                rep("Poland", cov_number)),
    dataset = c(rep("CHS", cov_number*ch_country), rep("ESS", cov_number*ess_country)),
    variable = c(rep(c("Male", "Age", "Education", "Income", "Unemployed", "Ideology"), ch_country+ess_country)),
    pval = NA
  )

for (i in unique(ch$name[ch$country == 1])) {
  covariate_df$pval[covariate_df$dataset == "CHS" & covariate_df$country == i & covariate_df$variable == "Male"] <- coef(summary(lm(male ~ tr, data = ch[ch$name == i,])))["tr","Pr(>|t|)"]
  covariate_df$pval[covariate_df$dataset == "CHS" & covariate_df$country == i & covariate_df$variable == "Age"] <- coef(summary(lm(age ~ tr, data = ch[ch$name == i,])))["tr","Pr(>|t|)"]
  covariate_df$pval[covariate_df$dataset == "CHS" & covariate_df$country == i & covariate_df$variable == "Education"] <- coef(summary(lm(edu ~ tr, data = ch[ch$name == i,])))["tr","Pr(>|t|)"]
  covariate_df$pval[covariate_df$dataset == "CHS" & covariate_df$country == i & covariate_df$variable == "Income"] <- coef(summary(lm(income ~ tr, data = ch[ch$name == i,])))["tr","Pr(>|t|)"]
  covariate_df$pval[covariate_df$dataset == "CHS" & covariate_df$country == i & covariate_df$variable == "Unemployed"] <- coef(summary(lm(unemployed ~ tr, data = ch[ch$name == i,])))["tr","Pr(>|t|)"]
  covariate_df$pval[covariate_df$dataset == "CHS" & covariate_df$country == i & covariate_df$variable == "Ideology"] <- coef(summary(lm(lrscale ~ tr, data = ch[ch$name == i,])))["tr","Pr(>|t|)"]
}

for (i in unique(ess$name[ess$country == 1])) {
  covariate_df$pval[covariate_df$dataset == "ESS" & covariate_df$country == i & covariate_df$variable == "Male"] <- coef(summary(lm(male ~ tr, data = ess[ess$name == i,])))["tr","Pr(>|t|)"]
  covariate_df$pval[covariate_df$dataset == "ESS" & covariate_df$country == i & covariate_df$variable == "Age"] <- coef(summary(lm(age ~ tr, data = ess[ess$name == i,])))["tr","Pr(>|t|)"]
  covariate_df$pval[covariate_df$dataset == "ESS" & covariate_df$country == i & covariate_df$variable == "Education"] <- coef(summary(lm(edu ~ tr, data = ess[ess$name == i,])))["tr","Pr(>|t|)"]
  covariate_df$pval[covariate_df$dataset == "ESS" & covariate_df$country == i & covariate_df$variable == "Income"] <- coef(summary(lm(income ~ tr, data = ess[ess$name == i,])))["tr","Pr(>|t|)"]
  covariate_df$pval[covariate_df$dataset == "ESS" & covariate_df$country == i & covariate_df$variable == "Unemployed"] <- coef(summary(lm(unemployed ~ tr, data = ess[ess$name == i,])))["tr","Pr(>|t|)"]
  covariate_df$pval[covariate_df$dataset == "ESS" & covariate_df$country == i & covariate_df$variable == "Ideology"] <- coef(summary(lm(lrscale ~ tr, data = ess[ess$name == i,])))["tr","Pr(>|t|)"]
}

ggplot(covariate_df, aes(x=country, y=pval, group=variable, colour=variable, shape=dataset)) +
  geom_point(size=3, alpha=0.5) + 
  scale_x_discrete("") +
  scale_y_continuous("P-value", lim=c(0,1)) +
  geom_hline(yintercept = 0.05, linetype="dashed", colour="gray80") +
  coord_flip()

ggsave("Figure_2.pdf", width=6, height=4)
#ggsave("Figure_2.png", width=6, height=4)

# Estimate effects
effect_number <- 16
effect_df <- 
  data.frame(
    country = c(rep("Austria", effect_number),
                rep("Hungary", effect_number),
                rep("Belgium", effect_number),
                rep("Greece", effect_number),
                rep("Germany", effect_number),
                rep("France", effect_number),
                rep("Spain", effect_number),
                rep("Poland", effect_number),
                
                rep("Germany", effect_number),
                rep("Estonia", effect_number),
                rep("France", effect_number),
                rep("Israel", effect_number),
                rep("Netherlands", effect_number),
                rep("Poland", effect_number)),
    dataset = c(rep("CHS", effect_number*ch_country), rep("ESS", effect_number*ess_country)),
    group = c(rep( c(rep(c("EU"), 5), rep(c("Immigration"), 7), rep(c("Refugees"), 4)), ch_country+ess_country)),
    variable = c(rep(c("EU1", "EU2", "EU3", "EU4", "EU5", "IMM1", "IMM2", "IMM3", "IMM4", "IMM5", "IMM6", "IMM7", "REF1", "REF2", "REF3", "REF4"), ch_country+ess_country)),
    est = NA,
    se = NA,
    cov = NA
  )

effect_df <- rbind(effect_df, effect_df)
effect_df[1:(NROW(effect_df)/2) , ]$cov <- "With covariates"
effect_df$cov[is.na(effect_df$cov)] <- "Bivariate"

for (i in unique(ch$name[ch$country == 1])) {
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU1" & effect_df$cov == "With covariates"] <- coef(summary(lm(eu1 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU1" & effect_df$cov == "With covariates"] <- coef(summary(lm(eu1 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU2" & effect_df$cov == "With covariates"] <- coef(summary(lm(eu2 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU2" & effect_df$cov == "With covariates"] <- coef(summary(lm(eu2 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU3" & effect_df$cov == "With covariates"] <- coef(summary(lm(eu3 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU3" & effect_df$cov == "With covariates"] <- coef(summary(lm(eu3 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU4" & effect_df$cov == "With covariates"] <- coef(summary(lm(eu4 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU4" & effect_df$cov == "With covariates"] <- coef(summary(lm(eu4 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM1" & effect_df$cov == "With covariates"] <- coef(summary(lm(imm1 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM1" & effect_df$cov == "With covariates"] <- coef(summary(lm(imm1 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM2" & effect_df$cov == "With covariates"] <- coef(summary(lm(imm2 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM2" & effect_df$cov == "With covariates"] <- coef(summary(lm(imm2 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM3" & effect_df$cov == "With covariates"] <- coef(summary(lm(imm3 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM3" & effect_df$cov == "With covariates"] <- coef(summary(lm(imm3 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM4" & effect_df$cov == "With covariates"] <- coef(summary(lm(imm4 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM4" & effect_df$cov == "With covariates"] <- coef(summary(lm(imm4 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM5" & effect_df$cov == "With covariates"] <- coef(summary(lm(imm5 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM5" & effect_df$cov == "With covariates"] <- coef(summary(lm(imm5 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM6" & effect_df$cov == "With covariates"] <- coef(summary(lm(imm6 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM6" & effect_df$cov == "With covariates"] <- coef(summary(lm(imm6 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "REF1" & effect_df$cov == "With covariates"] <- coef(summary(lm(ref1 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "REF1" & effect_df$cov == "With covariates"] <- coef(summary(lm(ref1 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr","Std. Error"]
}

for (i in unique(ess$name[ess$country == 1])) {
  effect_df$est[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "EU5" & effect_df$cov == "With covariates"] <- coef(summary(lm(eu5 ~ tr + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "EU5" & effect_df$cov == "With covariates"] <- coef(summary(lm(eu5 ~ tr + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "IMM7" & effect_df$cov == "With covariates"] <- coef(summary(lm(imm7 ~ tr + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "IMM7" & effect_df$cov == "With covariates"] <- coef(summary(lm(imm7 ~ tr + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "REF2" & effect_df$cov == "With covariates"] <- coef(summary(lm(ref2 ~ tr + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "REF2" & effect_df$cov == "With covariates"] <- coef(summary(lm(ref2 ~ tr + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "REF3" & effect_df$cov == "With covariates"] <- coef(summary(lm(ref3 ~ tr + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "REF3" & effect_df$cov == "With covariates"] <- coef(summary(lm(ref3 ~ tr + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "REF4" & effect_df$cov == "With covariates"] <- coef(summary(lm(ref4 ~ tr + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "REF4" & effect_df$cov == "With covariates"] <- coef(summary(lm(ref4 ~ tr + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr","Std. Error"]
}

for (i in unique(ch$name[ch$country == 1])) {
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU1" & effect_df$cov == "Bivariate"] <- coef(summary(lm(eu1 ~ tr, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU1" & effect_df$cov == "Bivariate"] <- coef(summary(lm(eu1 ~ tr, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU2" & effect_df$cov == "Bivariate"] <- coef(summary(lm(eu2 ~ tr, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU2" & effect_df$cov == "Bivariate"] <- coef(summary(lm(eu2 ~ tr, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU3" & effect_df$cov == "Bivariate"] <- coef(summary(lm(eu3 ~ tr, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU3" & effect_df$cov == "Bivariate"] <- coef(summary(lm(eu3 ~ tr, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU4" & effect_df$cov == "Bivariate"] <- coef(summary(lm(eu4 ~ tr, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "EU4" & effect_df$cov == "Bivariate"] <- coef(summary(lm(eu4 ~ tr, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM1" & effect_df$cov == "Bivariate"] <- coef(summary(lm(imm1 ~ tr, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM1" & effect_df$cov == "Bivariate"] <- coef(summary(lm(imm1 ~ tr, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM2" & effect_df$cov == "Bivariate"] <- coef(summary(lm(imm2 ~ tr, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM2" & effect_df$cov == "Bivariate"] <- coef(summary(lm(imm2 ~ tr, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM3" & effect_df$cov == "Bivariate"] <- coef(summary(lm(imm3 ~ tr, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM3" & effect_df$cov == "Bivariate"] <- coef(summary(lm(imm3 ~ tr, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM4" & effect_df$cov == "Bivariate"] <- coef(summary(lm(imm4 ~ tr, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM4" & effect_df$cov == "Bivariate"] <- coef(summary(lm(imm4 ~ tr, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM5" & effect_df$cov == "Bivariate"] <- coef(summary(lm(imm5 ~ tr, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM5" & effect_df$cov == "Bivariate"] <- coef(summary(lm(imm5 ~ tr, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM6" & effect_df$cov == "Bivariate"] <- coef(summary(lm(imm6 ~ tr, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "IMM6" & effect_df$cov == "Bivariate"] <- coef(summary(lm(imm6 ~ tr, data = ch[ch$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "REF1" & effect_df$cov == "Bivariate"] <- coef(summary(lm(ref1 ~ tr, data = ch[ch$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "CHS" & effect_df$country == i & effect_df$variable == "REF1" & effect_df$cov == "Bivariate"] <- coef(summary(lm(ref1 ~ tr, data = ch[ch$name == i,])))["tr","Std. Error"]
}

for (i in unique(ess$name[ess$country == 1])) {
  effect_df$est[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "EU5" & effect_df$cov == "Bivariate"] <- coef(summary(lm(eu5 ~ tr, data = ess[ess$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "EU5" & effect_df$cov == "Bivariate"] <- coef(summary(lm(eu5 ~ tr, data = ess[ess$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "IMM7" & effect_df$cov == "Bivariate"] <- coef(summary(lm(imm7 ~ tr, data = ess[ess$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "IMM7" & effect_df$cov == "Bivariate"] <- coef(summary(lm(imm7 ~ tr, data = ess[ess$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "REF2" & effect_df$cov == "Bivariate"] <- coef(summary(lm(ref2 ~ tr, data = ess[ess$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "REF2" & effect_df$cov == "Bivariate"] <- coef(summary(lm(ref2 ~ tr, data = ess[ess$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "REF3" & effect_df$cov == "Bivariate"] <- coef(summary(lm(ref3 ~ tr, data = ess[ess$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "REF3" & effect_df$cov == "Bivariate"] <- coef(summary(lm(ref3 ~ tr, data = ess[ess$name == i,])))["tr","Std. Error"]
  effect_df$est[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "REF4" & effect_df$cov == "Bivariate"] <- coef(summary(lm(ref4 ~ tr, data = ess[ess$name == i,])))["tr","Estimate"]
  effect_df$se[effect_df$dataset == "ESS" & effect_df$country == i & effect_df$variable == "REF4" & effect_df$cov == "Bivariate"] <- coef(summary(lm(ref4 ~ tr, data = ess[ess$name == i,])))["tr","Std. Error"]
}


effect_df <- effect_df %>%
  mutate(
    variable_name = case_when(
      variable == "EU1" ~ "EU, power",
      variable == "EU2" ~ "EU, federation",
      variable == "EU3" ~ "EU, enlargement",
      variable == "EU4" ~ "EU, new countries",
      variable == "EU5" ~ "European unification",
      variable == "IMM1" ~ "Immigration, good",
      variable == "IMM2" ~ "Immigration, jobs",
      variable == "IMM3" ~ "Immigration, cultural life",
      variable == "IMM4" ~ "Immigration, crime",
      variable == "IMM5" ~ "Immigration, Muslim culture",
      variable == "IMM6" ~ "Immigration, welfare",
      variable == "IMM7" ~ "Immigration, economy",
      variable == "REF1" ~ "Refugees, positive",
      variable == "REF2" ~ "Refugees, application",
      variable == "REF3" ~ "Refugees, risk",
      variable == "REF4" ~ "Refugees, family"
    )
  )

## Results: Germany
effect_df %>%
  filter(country == "Germany") %>%
  ggplot(aes(x = variable_name, y = est, shape = dataset, group=group, colour=group)) + 
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(aes(ymin=est-1.645*se, ymax=est+1.645*se), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se), colour="black", width=0) + 
  geom_point(size = 6, shape=19, colour = "white") + 
  geom_point(size = 3) +
  scale_y_continuous("Average effect") +
  scale_x_discrete("") +
  scale_shape_manual(values=c(1, 2)) +
  facet_wrap(~ cov) +
  theme(legend.title = element_blank(), legend.key = element_blank(),
        legend.position = 'top', legend.direction = 'horizontal') +
  coord_flip()

ggsave("Figure_3.pdf", width=7, height=6)

## Spillover effects

effect_df %>%
  filter(country != "Germany" & country != "Israel" & cov == "Bivariate") %>%
  ggplot(aes(x = variable_name, y = est, shape = dataset, group=group, colour=group)) + 
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(aes(ymin=est-1.645*se, ymax=est+1.645*se), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se), colour="black", width=0) + 
  geom_point(size = 6, shape=19, colour = "white") + 
  geom_point(size = 3) +
  scale_y_continuous("Average effect") +
  scale_x_discrete("") +
  facet_wrap(~ country) +
  scale_shape_manual(values=c(1, 2)) +
  theme(legend.title = element_blank(), legend.key = element_blank(),
        legend.position = 'top', legend.direction = 'horizontal') +
  coord_flip()


effect_df %>%
  filter(country != "Germany" & country != "Israel" & cov == "With covariates") %>%
  ggplot(aes(x = variable_name, y = est, shape = dataset, group=group, colour=group)) + 
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(aes(ymin=est-1.645*se, ymax=est+1.645*se), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se), colour="black", width=0) + 
  geom_point(size = 6, shape=19, colour = "white") + 
  geom_point(size = 3) +
  scale_y_continuous("Average effect") +
  scale_x_discrete("") +
  facet_wrap(~ country) +
  scale_shape_manual(values=c(1, 2)) +
  theme(legend.title = element_blank(), legend.key = element_blank(),
        legend.position = 'top', legend.direction = 'horizontal') +
  coord_flip()

ggsave("Figure_6.pdf", width=7, height=8.5)


## Heterogeneous effects

effect_number <- 16

effect_df_ideology <- 
  data.frame(
    country = c(rep("Austria", effect_number),
                rep("Hungary", effect_number),
                rep("Belgium", effect_number),
                rep("Greece", effect_number),
                rep("Germany", effect_number),
                rep("France", effect_number),
                rep("Spain", effect_number),
                rep("Poland", effect_number),
                
                rep("Germany", effect_number),
                rep("Estonia", effect_number),
                rep("France", effect_number),
                rep("Israel", effect_number),
                rep("Netherlands", effect_number),
                rep("Poland", effect_number)),
    dataset = c(rep("CHS", effect_number*ch_country), rep("ESS", effect_number*ess_country)),
    group = c(rep( c(rep(c("EU"), 5), rep(c("Immigration"), 7), rep(c("Refugees"), 4)), ch_country+ess_country)),
    variable = c(rep(c("EU1", "EU2", "EU3", "EU4", "EU5", "IMM1", "IMM2", "IMM3", "IMM4", "IMM5", "IMM6", "IMM7", "REF1", "REF2", "REF3", "REF4"), ch_country+ess_country)),
    est = NA,
    se = NA
  )

for (i in unique(ch$name[ch$country == 1])) {
  effect_df_ideology$est[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "EU1"] <- coef(summary(lm(eu1 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "EU1"] <- coef(summary(lm(eu1 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Std. Error"]
  effect_df_ideology$est[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "EU2"] <- coef(summary(lm(eu2 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "EU2"] <- coef(summary(lm(eu2 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Std. Error"]
  effect_df_ideology$est[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "EU3"] <- coef(summary(lm(eu3 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "EU3"] <- coef(summary(lm(eu3 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Std. Error"]
  effect_df_ideology$est[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "EU4"] <- coef(summary(lm(eu4 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "EU4"] <- coef(summary(lm(eu4 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Std. Error"]
  effect_df_ideology$est[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "IMM1"] <- coef(summary(lm(imm1 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "IMM1"] <- coef(summary(lm(imm1 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Std. Error"]
  effect_df_ideology$est[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "IMM2"] <- coef(summary(lm(imm2 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "IMM2"] <- coef(summary(lm(imm2 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Std. Error"]
  effect_df_ideology$est[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "IMM3"] <- coef(summary(lm(imm3 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "IMM3"] <- coef(summary(lm(imm3 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Std. Error"]
  effect_df_ideology$est[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "IMM4"] <- coef(summary(lm(imm4 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "IMM4"] <- coef(summary(lm(imm4 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Std. Error"]
  effect_df_ideology$est[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "IMM5"] <- coef(summary(lm(imm5 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "IMM5"] <- coef(summary(lm(imm5 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Std. Error"]
  effect_df_ideology$est[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "IMM6"] <- coef(summary(lm(imm6 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "IMM6"] <- coef(summary(lm(imm6 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Std. Error"]
  effect_df_ideology$est[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "REF1"] <- coef(summary(lm(ref1 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "CHS" & effect_df_ideology$country == i & effect_df_ideology$variable == "REF1"] <- coef(summary(lm(ref1 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == i,])))["tr:lrscale","Std. Error"]
}

for (i in unique(ess$name[ess$country == 1])) {
  effect_df_ideology$est[effect_df_ideology$dataset == "ESS" & effect_df_ideology$country == i & effect_df_ideology$variable == "EU5"] <- coef(summary(lm(eu5 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "ESS" & effect_df_ideology$country == i & effect_df_ideology$variable == "EU5"] <- coef(summary(lm(eu5 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr:lrscale","Std. Error"]
  effect_df_ideology$est[effect_df_ideology$dataset == "ESS" & effect_df_ideology$country == i & effect_df_ideology$variable == "IMM7"] <- coef(summary(lm(imm7 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "ESS" & effect_df_ideology$country == i & effect_df_ideology$variable == "IMM7"] <- coef(summary(lm(imm7 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr:lrscale","Std. Error"]
  effect_df_ideology$est[effect_df_ideology$dataset == "ESS" & effect_df_ideology$country == i & effect_df_ideology$variable == "REF2"] <- coef(summary(lm(ref2 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "ESS" & effect_df_ideology$country == i & effect_df_ideology$variable == "REF2"] <- coef(summary(lm(ref2 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr:lrscale","Std. Error"]
  effect_df_ideology$est[effect_df_ideology$dataset == "ESS" & effect_df_ideology$country == i & effect_df_ideology$variable == "REF3"] <- coef(summary(lm(ref3 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "ESS" & effect_df_ideology$country == i & effect_df_ideology$variable == "REF3"] <- coef(summary(lm(ref3 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr:lrscale","Std. Error"]
  effect_df_ideology$est[effect_df_ideology$dataset == "ESS" & effect_df_ideology$country == i & effect_df_ideology$variable == "REF4"] <- coef(summary(lm(ref4 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr:lrscale","Estimate"]
  effect_df_ideology$se[effect_df_ideology$dataset == "ESS" & effect_df_ideology$country == i & effect_df_ideology$variable == "REF4"] <- coef(summary(lm(ref4 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == i,])))["tr:lrscale","Std. Error"]
}

effect_df_ideology <- effect_df_ideology %>%
  mutate(
    variable_name = case_when(
      variable == "EU1" ~ "EU, power",
      variable == "EU2" ~ "EU, federation",
      variable == "EU3" ~ "EU, enlargement",
      variable == "EU4" ~ "EU, new countries",
      variable == "EU5" ~ "European unification",
      variable == "IMM1" ~ "Immigration, good",
      variable == "IMM2" ~ "Immigration, jobs",
      variable == "IMM3" ~ "Immigration, cultural life",
      variable == "IMM4" ~ "Immigration, crime",
      variable == "IMM5" ~ "Immigration, Muslim culture",
      variable == "IMM6" ~ "Immigration, welfare",
      variable == "IMM7" ~ "Immigration, economy",
      variable == "REF1" ~ "Refugees, positive",
      variable == "REF2" ~ "Refugees, application",
      variable == "REF3" ~ "Refugees, risk",
      variable == "REF4" ~ "Refugees, family"
    )
  )


int_reg_ch <- function(x, y){
  lm(paste(x," ~ tr*lrscale + male + age + edu + income + unemployed"), data = ch[ch$name == y,])
}
int_reg_ess <- function(x, y){
  lm(paste(x," ~ tr*lrscale + male + age + edu + income + unemployed"), data = ess[ess$name == y,])
}

ch_int_eu1 <- int_reg_ch("eu1", "Germany")
ch_int_eu2 <- int_reg_ch("eu2", "Germany")
ch_int_eu3 <- int_reg_ch("eu3", "Germany")
ch_int_eu4 <- int_reg_ch("eu4", "Germany")
ess_int_eu5 <- int_reg_ess("eu5", "Germany")

ch_int_imm1 <- int_reg_ch("imm1", "Germany")
ch_int_imm2 <- int_reg_ch("imm2", "Germany")
ch_int_imm3 <- int_reg_ch("imm3", "Germany")
ch_int_imm4 <- int_reg_ch("imm4", "Germany")
ch_int_imm5 <- int_reg_ch("imm5", "Germany")
ch_int_imm6 <- int_reg_ch("imm6", "Germany")
ess_int_imm7 <- int_reg_ess("imm7", "Germany")

ch_int_ref1 <- int_reg_ch("ref1", "Germany")
ess_int_ref2 <- int_reg_ess("ref2", "Germany")
ess_int_ref3 <- int_reg_ess("ref3", "Germany")
ess_int_ref4 <- int_reg_ess("ref4", "Germany")

## Results: Germany

effect_df_ideology %>%
  filter(country == "Germany") %>%
  ggplot(aes(x = variable_name, y = est, shape = dataset, group=group, colour=group)) + 
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(aes(ymin=est-1.645*se, ymax=est+1.645*se), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se), colour="black", width=0) + 
  geom_point(size = 6, shape=19, colour = "white") + 
  geom_point(size = 3) +
  scale_y_continuous("Terror × Ideology") +
  scale_x_discrete("") +
  scale_shape_manual(values=c(1, 2)) +
  theme(legend.title = element_blank(), legend.key = element_blank(),
        legend.position = 'top', legend.direction = 'vertical') +
  coord_flip()

ggsave("Figure_4.pdf", width=5, height=6)

reg_ref2 <- lm(ref2 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == "Germany",])
reg_ref2_data <- interplot(m = reg_ref2, var1 = "tr", var2 = "lrscale", plot=FALSE)

ggplot(reg_ref2_data, aes(x = lrscale)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), col="#649EFC", linetype=2) +
  geom_line(aes(y = lb), col="#649EFC", linetype=2) +
  scale_x_continuous(name="Ideology", breaks=0:10, labels=c("Left", rep("", 9), "Right")) +
  scale_y_continuous(name="Marginal effect of terrorist attack") 

ggsave("Figure_5.pdf", width=4, height=4)

effect_df_ideology %>%
  filter(country != "Germany" & country != "Israel") %>%
  ggplot(aes(x = variable_name, y = est, shape = dataset, group=group, colour=group)) + 
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(aes(ymin=est-1.645*se, ymax=est+1.645*se), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se), colour="black", width=0) + 
  geom_point(size = 6, shape=19, colour = "white") + 
  geom_point(size = 3) +
  scale_y_continuous("Terror × Ideology") +
  scale_x_discrete("") +
  facet_wrap(~ country) +
  scale_shape_manual(values=c(1, 2)) +
  theme(legend.title = element_blank(), legend.key = element_blank(),
        legend.position = 'top', legend.direction = 'horizontal') +
  coord_flip()

ggsave("Figure_7.pdf", width=7, height=8.5)




# APPENDIX

# Appendix A: The European context of the terrorist attack
eb_agg <- eb %>%
  group_by(isocntry, period) %>%
  summarise(imp_terror = mean(imp_terror, na.rm=TRUE))

eb_agg$name <- countrycode(eb_agg$isocntry, 'iso2c', 'country.name') 
eb_agg$name[eb_agg$isocntry == "DE-E" | eb_agg$isocntry == "DE-W"] <- "Germany"
eb_agg$name[eb_agg$isocntry == "GB-GBN" | eb_agg$isocntry == "GB-NIR"] <- "United Kingdom"
eb_agg$name[eb_agg$isocntry == "CY-TCC"] <- "Northern Cyprus"
eb_agg$name[eb_agg$name == "Czechia"] <- "Czech Republic"

eb_agg <- eb_agg %>%
  mutate(sample = ifelse(name == "Austria" |
                           name == "Belgium" |
                           name == "Estonia" |
                           name == "France" |
                           name == "Greece" |
                           name == "Hungary" |
                           name == "Netherlands" |
                           name == "Poland" |
                           name == "Spain" |
                           name == "Germany", 1, 0)) %>%
  mutate(sample = ifelse(name == "Albania" |
                           name == "Northern Cyprus" |
                           name == "Montenegro" |
                           name == "Macedonia" |
                           name == "Serbia" |
                           name == "Turkey", NA, sample)) %>%
  filter(!is.na(sample))


pdf("A-FigureA1.pdf", width=10, height=7)
ggplot(eb_agg, aes(as.Date(period), imp_terror)) +
  geom_vline(xintercept=as.numeric(as.Date("2016-12-19")), colour = "gray50", linetype = "dashed") +
  geom_line(aes(color = factor(sample))) +
  scale_color_manual(values=c('#999999','#E69F00')) +
  facet_geo(~ name, grid = "eu_grid1") + 
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(y = "Important issue facing country: Terrorism",
       x = "") +
  theme(legend.position="none")
dev.off()

# Appendix C: Descriptive statistics

ess %>% 
  filter(!is.na(tr) & country == 1) %>%
  group_by(name) %>%
  summarise(n = n(), 
            tr = mean(tr, na.rm=TRUE), 
            eu5 = paste0(round(mean(eu5, na.rm=TRUE),2), " (", round(sd(eu5, na.rm=TRUE), 2), ")"),
            imm7 = paste0(round(mean(imm7, na.rm=TRUE),2), " (", round(sd(imm7, na.rm=TRUE), 2), ")"),
            ref2 = paste0(round(mean(ref2, na.rm=TRUE),2), " (", round(sd(ref2, na.rm=TRUE), 2), ")"),
            ref3 = paste0(round(mean(ref3, na.rm=TRUE),2), " (", round(sd(ref3, na.rm=TRUE), 2), ")"),
            ref4 = paste0(round(mean(ref4, na.rm=TRUE),2), " (", round(sd(ref4, na.rm=TRUE), 2), ")")
  ) %>%
  xtable() %>%
  print(type="html", file="A-TableC1.htm")

ch %>% 
  filter(!is.na(tr) & country == 1) %>%
  group_by(name) %>%
  summarise(n = n(), 
            tr = mean(tr, na.rm=TRUE), 
            eu1 = paste0(round(mean(eu1, na.rm=TRUE),2), " (", round(sd(eu1, na.rm=TRUE), 2), ")"),
            eu2 = paste0(round(mean(eu2, na.rm=TRUE),2), " (", round(sd(eu2, na.rm=TRUE), 2), ")"),
            eu3 = paste0(round(mean(eu3, na.rm=TRUE),2), " (", round(sd(eu3, na.rm=TRUE), 2), ")"),
            eu4 = paste0(round(mean(eu4, na.rm=TRUE),2), " (", round(sd(eu4, na.rm=TRUE), 2), ")"),
            imm1 = paste0(round(mean(imm1, na.rm=TRUE),2), " (", round(sd(imm1, na.rm=TRUE), 2), ")"),
            imm2 = paste0(round(mean(imm2, na.rm=TRUE),2), " (", round(sd(imm2, na.rm=TRUE), 2), ")"),
            imm3 = paste0(round(mean(imm3, na.rm=TRUE),2), " (", round(sd(imm3, na.rm=TRUE), 2), ")"),
            imm4 = paste0(round(mean(imm4, na.rm=TRUE),2), " (", round(sd(imm4, na.rm=TRUE), 2), ")"),
            imm5 = paste0(round(mean(imm5, na.rm=TRUE),2), " (", round(sd(imm5, na.rm=TRUE), 2), ")"),
            imm6 = paste0(round(mean(imm6, na.rm=TRUE),2), " (", round(sd(imm6, na.rm=TRUE), 2), ")"),
            ref1 = paste0(round(mean(ref1, na.rm=TRUE),2), " (", round(sd(ref1, na.rm=TRUE), 2), ")")
            
  ) %>%
  xtable() %>%
  print(type="html", file="A-TableC2.htm")



# Appendix D: OLS estimates, country-level fixed effects models

reg_eu1_fe <- lm(eu1 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ch)
reg_eu2_fe <- lm(eu2 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ch)
reg_eu3_fe <- lm(eu3 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ch)
reg_eu4_fe <- lm(eu4 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ch)
reg_eu5_fe <- lm(eu5 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ess)

stargazer(reg_eu1_fe, reg_eu2_fe, reg_eu3_fe, reg_eu4_fe, reg_eu5_fe, type="text",
          no.space = TRUE,
          keep = c("tr", "male", "age", "edu", "income", "unemployed", "lrscale"),
          covariate.labels = c("Terror", "Male", "Age", "Education", "Income", "Unemployed", "Ideology"),
          column.labels = c("Power", "Federation", "Enlargement", "New countries", "Unification"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableD1.htm"
)

reg_imm1_fe <- lm(imm1 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ch)
reg_imm2_fe <- lm(imm2 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ch)
reg_imm3_fe <- lm(imm3 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ch)
reg_imm4_fe <- lm(imm4 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ch)
reg_imm5_fe <- lm(imm5 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ch)
reg_imm6_fe <- lm(imm6 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ch)
reg_imm7_fe <- lm(imm7 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ess)

stargazer(reg_imm1_fe, reg_imm2_fe, reg_imm3_fe, reg_imm4_fe, reg_imm5_fe, reg_imm6_fe, reg_imm7_fe, type="text",
          no.space = TRUE,
          keep = c("tr", "male", "age", "edu", "income", "unemployed", "lrscale"),
          covariate.labels = c("Terror", "Male", "Age", "Education", "Income", "Unemployed", "Ideology"),
          column.labels = c("Good", "Jobs", "Cultural life", "Crime", "Muslim culture", "Welfare", "Economy"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableD2.htm"
)

reg_ref1_fe <- lm(ref1 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ch)
reg_ref2_fe <- lm(ref2 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ess)
reg_ref3_fe <- lm(ref3 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ess)
reg_ref4_fe <- lm(ref4 ~ tr + male + age + edu + income + unemployed + lrscale + factor(name), data = ess)

stargazer(reg_ref1_fe, reg_ref2_fe, reg_ref3_fe, reg_ref4_fe, type="text",
          no.space = TRUE,
          keep = c("tr", "male", "age", "edu", "income", "unemployed", "lrscale"),
          covariate.labels = c("Terror", "Male", "Age", "Education", "Income", "Unemployed", "Ideology"),
          column.labels = c("Positive feelings", "Application", "Risk", "Family"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableD3.htm"
)

# Appendix E: OLS estimates, direct effects, Germany

stargazer(ch_reg_eu1_biv, ch_reg_eu2_biv, ch_reg_eu3_biv, ch_reg_eu4_biv, ess_reg_eu5_biv, type="text",
          no.space = TRUE,
          covariate.labels = c("Terror"),
          column.labels = c("Power", "Federation", "Enlargement", "New countries", "Unification"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableE1.htm"
)

stargazer(ch_reg_imm1_biv, ch_reg_imm2_biv, ch_reg_imm3_biv, ch_reg_imm4_biv, ch_reg_imm5_biv, ch_reg_imm6_biv, ess_reg_imm7_biv, type="text",
          no.space = TRUE,
          covariate.labels = c("Terror"),
          column.labels = c("Good", "Jobs", "Cultural life", "Crime", "Muslim culture", "Welfare", "Economy"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableE3.htm"
)


stargazer(ch_reg_ref1_biv, ess_reg_ref2_biv, ess_reg_ref3_biv, ess_reg_ref4_biv, type="text",
          no.space = TRUE,
          covariate.labels = c("Terror"),
          column.labels = c("Positive feelings", "Application", "Risk", "Family"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableE5.htm"
)


stargazer(ch_reg_eu1, ch_reg_eu2, ch_reg_eu3, ch_reg_eu4, ess_reg_eu5 , type="text",
          no.space = TRUE,
          keep = c("tr", "male", "age", "edu", "income", "unemployed", "lrscale"),
          covariate.labels = c("Terror", "Male", "Age", "Education", "Income", "Unemployed", "Ideology"),
          column.labels = c("Power", "Federation", "Enlargement", "New countries", "Unification"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableE2.htm"
)

stargazer(ch_reg_imm1, ch_reg_imm2, ch_reg_imm3, ch_reg_imm4, ch_reg_imm5, ch_reg_imm6, ess_reg_imm7 , type="text",
          no.space = TRUE,
          keep = c("tr", "male", "age", "edu", "income", "unemployed", "lrscale"),
          covariate.labels = c("Terror", "Male", "Age", "Education", "Income", "Unemployed", "Ideology"),
          column.labels = c("Good", "Jobs", "Cultural life", "Crime", "Muslim culture", "Welfare", "Economy"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableE4.htm"
)

stargazer(ch_reg_ref1, ess_reg_ref2, ess_reg_ref3, ess_reg_ref4, type="text",
          no.space = TRUE,
          keep = c("tr", "male", "age", "edu", "income", "unemployed", "lrscale"),
          covariate.labels = c("Terror", "Male", "Age", "Education", "Income", "Unemployed", "Ideology"),
          column.labels = c("Positive feelings", "Application", "Risk", "Family"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableE6.htm"
)


# Appendix F: OLS estimates, matched sample, Germany

ch_germany <- ch %>%
  filter(name == "Germany") %>%
  select(tr, eu1, eu2, eu3, eu4, imm1, imm2, imm3, imm4, imm5, imm6, ref1, male, age, edu, income, unemployed, lrscale) %>%
  na.omit()

ess_germany <- ess %>%
  filter(name == "Germany") %>%
  select(tr, eu5, imm7, ref2, ref3, ref4, male, age, edu, income, unemployed, lrscale) %>%
  na.omit()

tr_response <- tr ~ male + age + edu + income + unemployed + lrscale

ch_matched <- matchit(tr_response, method = "full", data = ch_germany)
ch_matched_data <- match.data(ch_matched)
ess_matched <- matchit(tr_response, method = "full", data = ess_germany)
ess_matched_data <- match.data(ess_matched)

direct_mat_ch_biv <- function(x){
  lm(paste(x," ~ tr"), data = ch_matched_data)
}
direct_mat_ch <- function(x){
  lm(paste(x," ~ tr + male + age + edu + income + unemployed + lrscale"), data = ch_matched_data)
}
direct_mat_ess_biv <- function(x){
  lm(paste(x," ~ tr"), data = ess_matched_data)
}
direct_mat_ess <- function(x){
  lm(paste(x," ~ tr + male + age + edu + income + unemployed + lrscale"), data = ess_matched_data)
}

ch_mat_eu1_biv <- direct_mat_ch_biv("eu1")
ch_mat_eu2_biv <- direct_mat_ch_biv("eu2")
ch_mat_eu3_biv <- direct_mat_ch_biv("eu3")
ch_mat_eu4_biv <- direct_mat_ch_biv("eu4")
ess_mat_eu5_biv <- direct_mat_ess_biv("eu5")

stargazer(ch_mat_eu1_biv, ch_mat_eu2_biv, ch_mat_eu3_biv, ch_mat_eu4_biv, ess_mat_eu5_biv, type="text",
          no.space = TRUE,
          covariate.labels = c("Terror"),
          column.labels = c("Power", "Federation", "Enlargement", "New countries", "Unification"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableF1.htm"
)

ch_mat_imm1_biv <- direct_mat_ch_biv("imm1")
ch_mat_imm2_biv <- direct_mat_ch_biv("imm2")
ch_mat_imm3_biv <- direct_mat_ch_biv("imm3")
ch_mat_imm4_biv <- direct_mat_ch_biv("imm4")
ch_mat_imm5_biv <- direct_mat_ch_biv("imm5")
ch_mat_imm6_biv <- direct_mat_ch_biv("imm6")
ess_mat_imm7_biv <- direct_mat_ess_biv("imm7")

stargazer(ch_mat_imm1_biv, ch_mat_imm2_biv, ch_mat_imm3_biv, ch_mat_imm4_biv, ch_mat_imm5_biv, ch_mat_imm6_biv, ess_mat_imm7_biv, type="text",
          no.space = TRUE,
          covariate.labels = c("Terror"),
          column.labels = c("Good", "Jobs", "Cultural life", "Crime", "Muslim culture", "Welfare", "Economy"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableF3.htm"
)

ch_mat_ref1_biv <- direct_mat_ch_biv("ref1")
ess_mat_ref2_biv <- direct_mat_ess_biv("ref2")
ess_mat_ref3_biv <- direct_mat_ess_biv("ref3")
ess_mat_ref4_biv <- direct_mat_ess_biv("ref4")

stargazer(ch_mat_ref1_biv, ess_mat_ref2_biv, ess_mat_ref3_biv, ess_mat_ref4_biv, type="text",
          no.space = TRUE,
          covariate.labels = c("Terror"),
          column.labels = c("Positive feelings", "Application", "Risk", "Family"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableF5.htm"
)

ch_mat_eu1 <- direct_mat_ch("eu1")
ch_mat_eu2 <- direct_mat_ch("eu2")
ch_mat_eu3 <- direct_mat_ch("eu3")
ch_mat_eu4 <- direct_mat_ch("eu4")
ess_mat_eu5 <- direct_mat_ess("eu5")

stargazer(ch_mat_eu1, ch_mat_eu2, ch_mat_eu3, ch_mat_eu4, ess_mat_eu5 , type="text",
          no.space = TRUE,
          keep = c("tr", "male", "age", "edu", "income", "unemployed", "lrscale"),
          covariate.labels = c("Terror", "Male", "Age", "Education", "Income", "Unemployed", "Ideology"),
          column.labels = c("Power", "Federation", "Enlargement", "New countries", "Unification"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableF2.htm"
)

ch_mat_imm1 <- direct_mat_ch("imm1")
ch_mat_imm2 <- direct_mat_ch("imm2")
ch_mat_imm3 <- direct_mat_ch("imm3")
ch_mat_imm4 <- direct_mat_ch("imm4")
ch_mat_imm5 <- direct_mat_ch("imm5")
ch_mat_imm6 <- direct_mat_ch("imm6")
ess_mat_imm7 <- direct_mat_ess("imm7")

stargazer(ch_mat_imm1, ch_mat_imm2, ch_mat_imm3, ch_mat_imm4, ch_mat_imm5, ch_mat_imm6, ess_mat_imm7 , type="text",
          no.space = TRUE,
          keep = c("tr", "male", "age", "edu", "income", "unemployed", "lrscale"),
          covariate.labels = c("Terror", "Male", "Age", "Education", "Income", "Unemployed", "Ideology"),
          column.labels = c("Good", "Jobs", "Cultural life", "Crime", "Muslim culture", "Welfare", "Economy"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableF4.htm"
)

ch_mat_ref1 <- direct_mat_ch("ref1")
ess_mat_ref2 <- direct_mat_ess("ref2")
ess_mat_ref3 <- direct_mat_ess("ref3")
ess_mat_ref4 <- direct_mat_ess("ref4")

stargazer(ch_mat_ref1, ess_mat_ref2, ess_mat_ref3, ess_mat_ref4, type="text",
          no.space = TRUE,
          keep = c("tr", "male", "age", "edu", "income", "unemployed", "lrscale"),
          covariate.labels = c("Terror", "Male", "Age", "Education", "Income", "Unemployed", "Ideology"),
          column.labels = c("Positive feelings", "Application", "Risk", "Family"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableF6.htm"
)

# Appendix G: OLS estimates, interaction results, Germany

stargazer(ch_int_eu1, ch_int_eu2, ch_int_eu3, ch_int_eu4, ess_int_eu5 , type="text",
          no.space = TRUE,
          covariate.labels = c("Terror", "Ideology", "Male", "Age", "Education", "Income", "Unemployed", "Terror × Ideology"),
          column.labels = c("Power", "Federation", "Enlargement", "New countries", "Unification"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableG1.htm"
)

stargazer(ch_int_imm1, ch_int_imm2, ch_int_imm3, ch_int_imm4, ch_int_imm5, ch_int_imm6, ess_int_imm7 , type="text",
          no.space = TRUE,
          covariate.labels = c("Terror", "Ideology", "Male", "Age", "Education", "Income", "Unemployed", "Terror × Ideology"),
          column.labels = c("Good", "Jobs", "Cultural life", "Crime", "Muslim culture", "Welfare", "Economy"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableG2.htm"
)

stargazer(ch_int_ref1, ess_int_ref2, ess_int_ref3, ess_int_ref4, type="text",
          no.space = TRUE,
          covariate.labels = c("Terror", "Ideology", "Male", "Age", "Education", "Income", "Unemployed", "Terror × Ideology"),
          column.labels = c("Positive feelings", "Application", "Risk", "Family"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableG2.htm"
)

# Appendix H: OLS estimates, spillover effects, bivariate models

effect_df %>%
  filter(country != "Germany" & country != "Israel" & cov == "Bivariate") %>%
  ggplot(aes(x = variable_name, y = est, shape = dataset, group=group, colour=group)) + 
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(aes(ymin=est-1.645*se, ymax=est+1.645*se), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se), colour="black", width=0) + 
  geom_point(size = 6, shape=19, colour = "white") + 
  geom_point(size = 3) +
  scale_y_continuous("Average effect") +
  scale_x_discrete("") +
  facet_wrap(~ country) +
  scale_shape_manual(values=c(1, 2)) +
  theme(legend.title = element_blank(), legend.key = element_blank(),
        legend.position = 'top', legend.direction = 'horizontal') +
  coord_flip()

ggsave("A-FigureH1.pdf", width=7, height=8.5)


# Appendix I: Test for differences, Germany and other countries

ch_germany_eu1 <- lm(eu1 ~ tr*germany, data=ch)
ch_germany_eu2 <- lm(eu2 ~ tr*germany, data=ch)
ch_germany_eu3 <- lm(eu3 ~ tr*germany, data=ch)
ch_germany_eu4 <- lm(eu4 ~ tr*germany, data=ch)
ess_germany_eu5 <- lm(eu5 ~ tr*germany, data=ess)
ch_germany_imm1 <- lm(imm1 ~ tr*germany, data=ch)
ch_germany_imm2 <- lm(imm2 ~ tr*germany, data=ch)
ch_germany_imm3 <- lm(imm3 ~ tr*germany, data=ch)
ch_germany_imm4 <- lm(imm4 ~ tr*germany, data=ch)
ch_germany_imm5 <- lm(imm5 ~ tr*germany, data=ch)
ch_germany_imm6 <- lm(imm6 ~ tr*germany, data=ch)
ess_germany_imm7 <- lm(imm7 ~ tr*germany, data=ess)
ch_germany_ref1 <- lm(ref1 ~ tr*germany, data=ch)
ess_germany_ref2 <- lm(ref2 ~ tr*germany, data=ess)
ess_germany_ref3 <- lm(ref3 ~ tr*germany, data=ess)
ess_germany_ref4 <- lm(ref4 ~ tr*germany, data=ess)


stargazer(ch_germany_eu1, ch_germany_eu2, ch_germany_eu3, ch_germany_eu4, ess_germany_eu5 , type="text",
          no.space = TRUE,
          covariate.labels = c("Terror", "Germany", "Terror × Germany"),
          column.labels = c("Power", "Federation", "Enlargement", "New countries", "Unification"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableI1.htm"
)


stargazer(ch_germany_imm1, ch_germany_imm2, ch_germany_imm3, ch_germany_imm4, ch_germany_imm5, ch_germany_imm6, ess_germany_imm7 , type="text",
          no.space = TRUE,
          covariate.labels = c("Terror", "Germany", "Terror × Germany"),
          column.labels = c("Good", "Jobs", "Cultural life", "Crime", "Muslim culture", "Welfare", "Economy"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableI2.htm"
)

stargazer(ch_germany_ref1, ess_germany_ref2, ess_germany_ref3, ess_germany_ref4, type="text",
          no.space = TRUE,
          covariate.labels = c("Terror", "Germany", "Terror × Germany"),
          column.labels = c("Positive feelings", "Application", "Risk", "Family"),
          model.numbers = FALSE,
          digits = 2,
          out="A-TableI3.htm"
)


## Table I.4
eq <- function(a, b, c, d, e, f, g) {
  TOSTtwo.raw(m1 = a, m2 = b, sd1 = c, sd2 = d, n1 = e, n2 = f, low_eqbound = -g, high_eqbound = g, alpha = 0.05, var.equal=TRUE, plot = FALSE)
}

ch_eu1_meandif <- mean(ch$eu1[ch$tr == 1 & ch$germany == 1], na.rm=TRUE) - mean(ch$eu1[ch$tr == 0 & ch$germany == 1], na.rm=TRUE)
ch_eu2_meandif <- mean(ch$eu2[ch$tr == 1 & ch$germany == 1], na.rm=TRUE) - mean(ch$eu2[ch$tr == 0 & ch$germany == 1], na.rm=TRUE)
ch_eu3_meandif <- mean(ch$eu3[ch$tr == 1 & ch$germany == 1], na.rm=TRUE) - mean(ch$eu3[ch$tr == 0 & ch$germany == 1], na.rm=TRUE)
ch_eu4_meandif <- mean(ch$eu4[ch$tr == 1 & ch$germany == 1], na.rm=TRUE) - mean(ch$eu4[ch$tr == 0 & ch$germany == 1], na.rm=TRUE)
ess_eu5_meandif <- mean(ess$eu5[ess$tr == 1 & ess$germany == 1], na.rm=TRUE) - mean(ess$eu5[ess$tr == 0 & ess$germany == 1], na.rm=TRUE)

eu_meandif <- mean(c(ch_eu1_meandif, ch_eu2_meandif, ch_eu3_meandif, ch_eu4_meandif, ess_eu5_meandif))

eq(mean(ch$eu1[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   mean(ch$eu1[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$eu1[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$eu1[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   length(ch$eu1[ch$tr == 1 & ch$germany == 0]), 
   length(ch$eu1[ch$tr == 0 & ch$germany == 0]),
   eu_meandif
)

eq(mean(ch$eu2[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   mean(ch$eu2[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$eu2[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$eu2[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   length(ch$eu2[ch$tr == 1 & ch$germany == 0]), 
   length(ch$eu2[ch$tr == 0 & ch$germany == 0]),
   eu_meandif
)

eq(mean(ch$eu3[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   mean(ch$eu3[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$eu3[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$eu3[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   length(ch$eu3[ch$tr == 1 & ch$germany == 0]), 
   length(ch$eu3[ch$tr == 0 & ch$germany == 0]),
   eu_meandif
)

eq(mean(ch$eu4[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   mean(ch$eu4[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$eu4[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$eu4[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   length(ch$eu4[ch$tr == 1 & ch$germany == 0]), 
   length(ch$eu4[ch$tr == 0 & ch$germany == 0]),
   eu_meandif
)

eq(mean(ess$eu5[ess$tr == 1 & ess$germany == 0], na.rm=TRUE), 
   mean(ess$eu5[ess$tr == 0 & ess$germany == 0], na.rm=TRUE), 
   sd(ess$eu5[ess$tr == 1 & ess$germany == 0], na.rm=TRUE), 
   sd(ess$eu5[ess$tr == 0 & ess$germany == 0], na.rm=TRUE), 
   length(ess$eu5[ess$tr == 1 & ess$germany == 0]), 
   length(ess$eu5[ess$tr == 0 & ess$germany == 0]),
   eu_meandif
)

# Immigration

eq(mean(ch$imm1[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   mean(ch$imm1[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$imm1[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$imm1[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   length(ch$imm1[ch$tr == 1 & ch$germany == 0]), 
   length(ch$imm1[ch$tr == 0 & ch$germany == 0]),
   eu_meandif
)

eq(mean(ch$imm2[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   mean(ch$imm2[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$imm2[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$imm2[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   length(ch$imm2[ch$tr == 1 & ch$germany == 0]), 
   length(ch$imm2[ch$tr == 0 & ch$germany == 0]),
   eu_meandif
)

eq(mean(ch$imm3[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   mean(ch$imm3[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$imm3[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$imm3[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   length(ch$imm3[ch$tr == 1 & ch$germany == 0]), 
   length(ch$imm3[ch$tr == 0 & ch$germany == 0]),
   eu_meandif
)

eq(mean(ch$imm4[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   mean(ch$imm4[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$imm4[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$imm4[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   length(ch$imm4[ch$tr == 1 & ch$germany == 0]), 
   length(ch$imm4[ch$tr == 0 & ch$germany == 0]),
   eu_meandif
)

eq(mean(ch$imm5[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   mean(ch$imm5[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$imm5[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$imm5[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   length(ch$imm5[ch$tr == 1 & ch$germany == 0]), 
   length(ch$imm5[ch$tr == 0 & ch$germany == 0]),
   eu_meandif
)

eq(mean(ch$imm6[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   mean(ch$imm6[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$imm6[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$imm6[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   length(ch$imm6[ch$tr == 1 & ch$germany == 0]), 
   length(ch$imm6[ch$tr == 0 & ch$germany == 0]),
   eu_meandif
)

eq(mean(ess$imm7[ess$tr == 1 & ess$germany == 0], na.rm=TRUE), 
   mean(ess$imm7[ess$tr == 0 & ess$germany == 0], na.rm=TRUE), 
   sd(ess$imm7[ess$tr == 1 & ess$germany == 0], na.rm=TRUE), 
   sd(ess$imm7[ess$tr == 0 & ess$germany == 0], na.rm=TRUE), 
   length(ess$imm7[ess$tr == 1 & ess$germany == 0]), 
   length(ess$imm7[ess$tr == 0 & ess$germany == 0]),
   eu_meandif
)

eq(mean(ch$ref1[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   mean(ch$ref1[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$ref1[ch$tr == 1 & ch$germany == 0], na.rm=TRUE), 
   sd(ch$ref1[ch$tr == 0 & ch$germany == 0], na.rm=TRUE), 
   length(ch$ref1[ch$tr == 1 & ch$germany == 0]), 
   length(ch$ref1[ch$tr == 0 & ch$germany == 0]),
   eu_meandif
)

eq(mean(ess$ref2[ess$tr == 1 & ess$germany == 0], na.rm=TRUE), 
   mean(ess$ref2[ess$tr == 0 & ess$germany == 0], na.rm=TRUE), 
   sd(ess$ref2[ess$tr == 1 & ess$germany == 0], na.rm=TRUE), 
   sd(ess$ref2[ess$tr == 0 & ess$germany == 0], na.rm=TRUE), 
   length(ess$ref2[ess$tr == 1 & ess$germany == 0]), 
   length(ess$ref2[ess$tr == 0 & ess$germany == 0]),
   eu_meandif
)

eq(mean(ess$ref3[ess$tr == 1 & ess$germany == 0], na.rm=TRUE), 
   mean(ess$ref3[ess$tr == 0 & ess$germany == 0], na.rm=TRUE), 
   sd(ess$ref3[ess$tr == 1 & ess$germany == 0], na.rm=TRUE), 
   sd(ess$ref3[ess$tr == 0 & ess$germany == 0], na.rm=TRUE), 
   length(ess$ref3[ess$tr == 1 & ess$germany == 0]), 
   length(ess$ref3[ess$tr == 0 & ess$germany == 0]),
   eu_meandif
)

eq(mean(ess$ref4[ess$tr == 1 & ess$germany == 0], na.rm=TRUE), 
   mean(ess$ref4[ess$tr == 0 & ess$germany == 0], na.rm=TRUE), 
   sd(ess$ref4[ess$tr == 1 & ess$germany == 0], na.rm=TRUE), 
   sd(ess$ref4[ess$tr == 0 & ess$germany == 0], na.rm=TRUE), 
   length(ess$ref4[ess$tr == 1 & ess$germany == 0]), 
   length(ess$ref4[ess$tr == 0 & ess$germany == 0]),
   eu_meandif
)

# Appendix J: Marginal effects from interaction models

reg_ref1_de <- lm(ref1 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Germany",])
reg_ref2_de <- lm(ref2 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == "Germany",])
reg_ref3_de <- lm(ref3 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == "Germany",])
reg_ref4_de <- lm(ref4 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == "Germany",])
reg_eu1_de <- lm(eu1 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Germany",])
reg_eu2_de <- lm(eu2 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Germany",])
reg_eu3_de <- lm(eu3 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Germany",])
reg_eu4_de <- lm(eu4 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Germany",])
reg_eu5_de <- lm(eu5 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == "Germany",])
reg_imm1_de <- lm(imm1 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Germany",])
reg_imm2_de <- lm(imm2 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Germany",])
reg_imm3_de <- lm(imm3 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Germany",])
reg_imm4_de <- lm(imm4 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Germany",])
reg_imm5_de <- lm(imm5 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Germany",])
reg_imm6_de <- lm(imm6 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Germany",])
reg_imm7_de <- lm(imm7 ~ tr*lrscale + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == "Germany",])

reg_ref1_de_data <- interplot(m = reg_ref1_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_ref1_de_data$outcome <- "Refugee: Positive feelings"
reg_ref2_de_data <- interplot(m = reg_ref2_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_ref2_de_data$outcome <- "Refugee: Application"
reg_ref3_de_data <- interplot(m = reg_ref3_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_ref3_de_data$outcome <- "Refugee: Risk"
reg_ref4_de_data <- interplot(m = reg_ref4_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_ref4_de_data$outcome <- "Refugee: Family"
reg_eu1_de_data <- interplot(m = reg_eu1_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_eu1_de_data$outcome <- "EU: Power"
reg_eu2_de_data <- interplot(m = reg_eu2_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_eu2_de_data$outcome <- "EU: Federation"
reg_eu3_de_data <- interplot(m = reg_eu3_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_eu3_de_data$outcome <- "EU: Enlargement"
reg_eu4_de_data <- interplot(m = reg_eu4_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_eu4_de_data$outcome <- "EU: New countries"
reg_eu5_de_data <- interplot(m = reg_eu5_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_eu5_de_data$outcome <- "EU: Unification"
reg_imm1_de_data <- interplot(m = reg_imm1_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_imm1_de_data$outcome <- "Immigration: Good"
reg_imm2_de_data <- interplot(m = reg_imm2_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_imm2_de_data$outcome <- "Immigration: Jobs"
reg_imm3_de_data <- interplot(m = reg_imm3_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_imm3_de_data$outcome <- "Immigration: Cultural life"
reg_imm4_de_data <- interplot(m = reg_imm4_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_imm4_de_data$outcome <- "Immigration: Crime"
reg_imm5_de_data <- interplot(m = reg_imm5_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_imm5_de_data$outcome <- "Immigration: Muslim culture"
reg_imm6_de_data <- interplot(m = reg_imm6_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_imm6_de_data$outcome <- "Immigration: Welfare"
reg_imm7_de_data <- interplot(m = reg_imm7_de, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_imm7_de_data$outcome <- "Immigration: Economy"


marg_data_de <- rbind(reg_ref1_de_data, reg_ref2_de_data, reg_ref3_de_data, reg_ref4_de_data, 
                      reg_eu1_de_data, reg_eu2_de_data, reg_eu3_de_data, reg_eu4_de_data, reg_eu5_de_data,
                      reg_imm1_de_data, reg_imm2_de_data, reg_imm3_de_data, reg_imm4_de_data, reg_imm5_de_data, reg_imm6_de_data, reg_imm7_de_data)

ggplot(marg_data_de, aes(x = lrscale)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), linetype=2) +
  geom_line(aes(y = lb), linetype=2) +
  scale_x_continuous(name="Ideology", breaks=0:10, labels=c("Left", rep("", 9), "Right")) +
  facet_wrap(~ outcome) +
  scale_y_continuous(name="Marginal effect of terrorist attack")

ggsave("A-FigureJ1.pdf", width=12, height=10)


ch_reg_eu1_be <- lm(eu1 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Belgium",])
ch_reg_eu2_be <- lm(eu2 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Belgium",])
ch_reg_eu3_be <- lm(eu3 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Belgium",])
ch_reg_eu4_be <- lm(eu4 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Belgium",])
ch_reg_imm1_be <- lm(imm1 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Belgium",])
ch_reg_imm2_be <- lm(imm2 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Belgium",])
ch_reg_imm3_be <- lm(imm3 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Belgium",])
ch_reg_imm4_be <- lm(imm4 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Belgium",])
ch_reg_imm5_be <- lm(imm5 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Belgium",])
ch_reg_imm6_be <- lm(imm6 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Belgium",])
ch_reg_ref1_be <- lm(ref1 ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Belgium",])

effect_df_ideology %>% filter(country == "Belgium") %>%
  mutate(z = est/se)

reg_ref1_be <- lm(ref1 ~ tr*lrscale + male + age + edu + income + unemployed, data = ch[ch$name == "Belgium",])
reg_imm1_be <- lm(imm1 ~ tr*lrscale + male + age + edu + income + unemployed, data = ch[ch$name == "Belgium",])
reg_imm2_be <- lm(imm2 ~ tr*lrscale + male + age + edu + income + unemployed, data = ch[ch$name == "Belgium",])
reg_imm3_be <- lm(imm3 ~ tr*lrscale + male + age + edu + income + unemployed, data = ch[ch$name == "Belgium",])
reg_imm4_be <- lm(imm4 ~ tr*lrscale + male + age + edu + income + unemployed, data = ch[ch$name == "Belgium",])
reg_imm5_be <- lm(imm5 ~ tr*lrscale + male + age + edu + income + unemployed, data = ch[ch$name == "Belgium",])
reg_imm6_be <- lm(imm6 ~ tr*lrscale + male + age + edu + income + unemployed, data = ch[ch$name == "Belgium",])

reg_ref1_data <- interplot(m = reg_ref1_be, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_imm1_data <- interplot(m = reg_imm1_be, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_imm2_data <- interplot(m = reg_imm2_be, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_imm3_data <- interplot(m = reg_imm3_be, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_imm4_data <- interplot(m = reg_imm4_be, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_imm5_data <- interplot(m = reg_imm5_be, var1 = "tr", var2 = "lrscale", plot=FALSE)
reg_imm6_data <- interplot(m = reg_imm6_be, var1 = "tr", var2 = "lrscale", plot=FALSE)

fig_be_ref1 <- ggplot(reg_ref1_data, aes(x = lrscale)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), col="#649EFC", linetype=2) +
  geom_line(aes(y = lb), col="#649EFC", linetype=2) +
  ggtitle("Refugees, positive") +
  scale_x_continuous(name="Ideology", breaks=0:10, labels=c("Left", rep("", 9), "Right")) +
  scale_y_continuous(name="Marginal effect of terrorist attack") 

fig_be_imm1 <- ggplot(reg_imm1_data, aes(x = lrscale)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), col="#649EFC", linetype=2) +
  geom_line(aes(y = lb), col="#649EFC", linetype=2) +
  ggtitle("Immigration, good") +
  scale_x_continuous(name="Ideology", breaks=0:10, labels=c("Left", rep("", 9), "Right")) +
  scale_y_continuous(name="Marginal effect of terrorist attack") 


fig_be_imm2 <- ggplot(reg_imm2_data, aes(x = lrscale)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), col="#649EFC", linetype=2) +
  geom_line(aes(y = lb), col="#649EFC", linetype=2) +
  ggtitle("Immigration, jobs") +
  scale_x_continuous(name="Ideology", breaks=0:10, labels=c("Left", rep("", 9), "Right")) +
  scale_y_continuous(name="Marginal effect of terrorist attack")


fig_be_imm3 <- ggplot(reg_imm3_data, aes(x = lrscale)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), col="#649EFC", linetype=2) +
  geom_line(aes(y = lb), col="#649EFC", linetype=2) +
  ggtitle("Immigration, cultural life") +
  scale_x_continuous(name="Ideology", breaks=0:10, labels=c("Left", rep("", 9), "Right")) +
  scale_y_continuous(name="Marginal effect of terrorist attack") 


fig_be_imm4 <- ggplot(reg_imm4_data, aes(x = lrscale)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), col="#649EFC", linetype=2) +
  geom_line(aes(y = lb), col="#649EFC", linetype=2) +
  ggtitle("Immigration, crime") +
  scale_x_continuous(name="Ideology", breaks=0:10, labels=c("Left", rep("", 9), "Right")) +
  scale_y_continuous(name="Marginal effect of terrorist attack") 

fig_be_imm5 <- ggplot(reg_imm5_data, aes(x = lrscale)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), col="#649EFC", linetype=2) +
  geom_line(aes(y = lb), col="#649EFC", linetype=2) +
  ggtitle("Immigration, Muslim culture") +
  scale_x_continuous(name="Ideology", breaks=0:10, labels=c("Left", rep("", 9), "Right")) +
  scale_y_continuous(name="Marginal effect of terrorist attack") 

fig_be_imm6 <- ggplot(reg_imm6_data, aes(x = lrscale)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), col="#649EFC", linetype=2) +
  geom_line(aes(y = lb), col="#649EFC", linetype=2) +
  ggtitle("Immigration, welfare") +
  scale_x_continuous(name="Ideology", breaks=0:10, labels=c("Left", rep("", 9), "Right")) +
  scale_y_continuous(name="Marginal effect of terrorist attack") 


pdf("A-FigureJ2.pdf", width=9, height=10)
grid.arrange(fig_be_ref1, fig_be_imm1, 
             fig_be_imm2, fig_be_imm3, 
             fig_be_imm4, fig_be_imm5, 
             fig_be_imm6, ncol=3)
dev.off()

# Appendix K: OLS estimates, direct effects, Israel

effect_df %>%
  filter(country == "Israel" & variable %in% c("EU5", "IMM7", "REF2", "REF3", "REF4")) %>%
  ggplot(aes(x = variable_name, y = est, group=group, colour=group)) + 
  geom_hline(yintercept=0, colour="gray80") +
  geom_errorbar(aes(ymin=est-1.645*se, ymax=est+1.645*se), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se), colour="black", width=0) + 
  geom_point(size = 6, shape=19, colour = "white") + 
  geom_point(size = 3) +
  scale_y_continuous("Average effect") +
  scale_x_discrete("") +
  facet_wrap(~ cov) +
  scale_shape_manual(values=c(1, 2)) +
  theme(legend.title = element_blank(), legend.key = element_blank(),
        legend.position = 'top', legend.direction = 'horizontal') +
  coord_flip()

ggsave("A-FigureK1.pdf", width=6, height=4)


# Appendix L: Replication: Germany
eb_germany <- eb %>%
  filter(eb$isocntry == "DE-E" | eb$isocntry == "DE-W")

eb_data <- data.frame(
  period = rep(unique(eb$period), 3),
  outcome = c(rep("EU", 5), rep("Refugees", 5), rep("Immigration", 5)),
  est = NA,
  se = NA,
  pval = NA
)

for (i in eb_data$period) {
  eb_data$est[eb_data$period == i & eb_data$outcome == "EU"] <- mean(eb_germany$eu[eb_germany$period == i], na.rm=TRUE)
  eb_data$se[eb_data$period == i & eb_data$outcome == "EU"] <- sd(eb_germany$eu[eb_germany$period == i], na.rm=TRUE)/sqrt(length(eb_germany$eu[eb_germany$period == i & !is.na(eb_germany$eu)]))
  
  eb_data$est[eb_data$period == i & eb_data$outcome == "Refugees"] <- mean(eb_germany$ref_agree[eb_germany$period == i], na.rm=TRUE)
  eb_data$se[eb_data$period == i & eb_data$outcome == "Refugees"] <- sd(eb_germany$ref_agree[eb_germany$period == i], na.rm=TRUE)/sqrt(length(eb_germany$ref_agree[eb_germany$period == i & !is.na(eb_germany$ref_agree)]))
  
  eb_data$est[eb_data$period == i & eb_data$outcome == "Immigration"] <- mean(eb_germany$imm_agree[eb_germany$period == i], na.rm=TRUE)
  eb_data$se[eb_data$period == i & eb_data$outcome == "Immigration"] <- sd(eb_germany$imm_agree[eb_germany$period == i], na.rm=TRUE)/sqrt(length(eb_germany$imm_agree[eb_germany$period == i & !is.na(eb_germany$imm_agree)]))
}

eb_data$pval[eb_data$period == "2016-11-01" & eb_data$outcome == "EU"] <- 
  round(t.test(x = eb_germany$eu_trust[eb_germany$period == "2016-11-01"], 
               y = eb_germany$eu_trust[eb_germany$period == "2017-05-01"])$p.val, 10)

eb_data$pval[eb_data$period == "2016-11-01" & eb_data$outcome == "Refugees"] <- 
  round(t.test(x = eb_germany$ref_agree[eb_germany$period == "2016-11-01"], 
               y = eb_germany$ref_agree[eb_germany$period == "2017-05-01"])$p.val, 3)

eb_data$pval[eb_data$period == "2016-11-01" & eb_data$outcome == "Immigration"] <- 
  round(t.test(x = eb_germany$imm_agree[eb_germany$period == "2016-11-01"], 
               y = eb_germany$imm_agree[eb_germany$period == "2017-05-01"])$p.val, 3)

eb_data$pval <- ifelse(eb_data$pval < 0.01, 0.01, eb_data$pval)
eb_data$pval_text <- ifelse(!is.na(eb_data$pval), paste0("p = ", eb_data$pval), NA)
eb_data$pval_text[eb_data$pval == 0.01] <- "p < 0.01"

ggplot(eb_data, aes(x=as.Date(period), y=est, ymin=est-se*1.96, ymax=est+se*1.96)) +
  geom_line() +
  geom_ribbon(alpha=0.2) +
  scale_y_continuous(limits = c(0,1), labels=percent) +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  geom_vline(xintercept=as.numeric(as.Date("2016-12-19")), colour = "gray50", linetype = "dashed") +
  facet_wrap(. ~ outcome) +
  geom_text(
    data    = eb_data, mapping = aes(x = as.Date("2017-12-01"), y = 0.7, label = pval_text),
    hjust   = 1.05, vjust   = 1.5, size    = 3
  ) +
  labs(y = "Support",
       x = "") 
ggsave("A-FigureL1.pdf", width=8, height=3)

gles_df <- data.frame(
  year = c(2016, 2017),
  est = c(mean(gles$imm16, na.rm=TRUE), mean(gles$imm17, na.rm=TRUE)),
  se = c(sd(gles$imm16, na.rm=TRUE)/sqrt(length(gles$imm16)),
         sd(gles$imm17, na.rm=TRUE)/sqrt(length(gles$imm17))
  )
)

ggplot(gles_df, aes(x = year, y = est)) + 
  geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se), colour="black", width=0) + 
  geom_point(size = 5, shape=19, colour = "white") + 
  geom_point(size = 3) +
  scale_y_continuous("Support for immigration", limits = c(3.9, 4.1)) +
  scale_x_continuous("", limits = c(2015.5, 2017.5), breaks = c(2016, 2017)) +
  annotate("text", x = 2016.5, y = 4.02, 
           label = paste0("p = ", round(t.test(x = gles$imm16, y = gles$imm17)$p.val,3))
  )

ggsave("A-FigureL2.pdf", width=6, height=4)

mean(gles$imm16, na.rm=TRUE)
mean(gles$imm17, na.rm=TRUE)
mean(gles$imm16, na.rm=TRUE) - mean(gles$imm17, na.rm=TRUE)

# Not reported in manuscript or appendix
# Robustness: Ordered logit

ch_reg_eu1_ol <- polr(factor(eu1) ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Germany",], Hess=TRUE)
ch_reg_eu2_ol <- polr(factor(eu2) ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Germany",], Hess=TRUE)
ch_reg_eu3_ol <- polr(factor(eu3) ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Germany",], Hess=TRUE)
ch_reg_eu4_ol <- polr(factor(eu4) ~ tr + male + age + edu + income + unemployed + lrscale, data = ch[ch$name == "Germany",], Hess=TRUE)
ess_reg_eu5_ol <- polr(factor(eu5) ~ tr + male + age + edu + income + unemployed + lrscale, data = ess[ess$name == "Germany",], Hess=TRUE)

stargazer(ch_reg_eu1_ol, ch_reg_eu2_ol, ch_reg_eu3_ol, ch_reg_eu4_ol, ess_reg_eu5_ol, type="text")