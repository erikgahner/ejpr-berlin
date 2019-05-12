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
library("rio")
library("countrycode")

# Set select() to dplyr::select as default
select <- dplyr::select

# Load data sources
ch <- import("DC Chatham HouseOrig.sav")
ess <- import("ESS8e02_1.dta")
eb1511 <- import("ZA6643_v3-1-0.dta")
eb1605 <- import("ZA6694_v1-1-0.dta")
eb1611 <- import("ZA6788_v1-3-0.dta")
eb1705 <- import("ZA6863_v1-0-0.dta")
eb1711 <- import("ZA6928_v1-0-0.dta")
gles <- import("ZA5770_v1-0-0.dta")

# Recode variables in CH
ch <- ch %>%
  mutate(
    # Terror variable
    tr = case_when(
      ED > 19 & EM == 12 ~ 1,
      ED < 19 & EM == 12 ~ 0,
      TRUE  ~  NA_real_
    ),
    
    # Countries
    country = ifelse(B %in% c(1, 3, 4, 
                              5, 7, 12, 
                              35, 39), 1, 0),
    name = case_when(
      B == 1 ~ "Belgium",
      B == 3 ~ "Germany",
      B == 4 ~ "Greece",
      B == 5 ~ "Spain",
      B == 7 ~ "France",
      B == 9 ~ "Italy",
      B == 12 ~ "Austria",
      B == 22 ~ "United Kingdom",
      B == 35 ~ "Hungary",
      B == 39 ~ "Poland"
    ),
    
    ## Germany
    germany = case_when(
      B == 3 & country == 1 ~ 1,
      B != 3 & country == 1 ~ 0
    ),
    
    # Outcomes
    eu1 = ifelse(Q12 == 4, NA, (3-Q12)/2),
    eu2 = ifelse(Q13 == 6, NA, (5-Q13)/4),
    eu3 = ifelse(Q19_1 == 6, NA, (Q19_1-1)/4),
    eu4 = ifelse(Q19_2 == 6, NA, (5-Q19_2)/4),
    imm1 = ifelse(Q31_1 == 6, NA, (5-Q31_1)/4),
    imm2 = ifelse(Q31_2 == 6, NA, (Q31_2-1)/4),
    imm3 = ifelse(Q31_3 == 6, NA, (5-Q31_3)/4),
    imm4 = ifelse(Q31_4 == 6, NA, (Q31_4-1)/4),
    imm5 = ifelse(Q31_5 == 6, NA, (Q31_5-1)/4),
    imm6 = ifelse(Q31_6 == 6, NA, (Q31_6-1)/4),
    ref1 = ifelse(Q32_3 == 6, NA, (5-Q32_3)/4),
    
    peace = ifelse(Q2a == 1, 1, 0),
    
    # Covariates
    male = ifelse(QS1 == 1, 1, 0),
    age = QS2,
    edu = ifelse(D5R == 9, NA, D5R),
    income = ifelse(D10GP >= 9, NA, D10GP),
    unemployed = ifelse(D6 == 7, 1, 0),
    lrscale = ifelse(D7 == 12, NA, D7-1)
  )


ch <- ch %>%
  mutate(
    eu_best = case_when(Q2a == 1 ~ "Peace on the \n European continent", 
                        Q2a == 2 ~ "Protection of the environment",
                        Q2a == 3 ~ "The EU single market",
                        Q2a == 4 ~ "Freedom to live and \n work across the EU",
                        Q2a == 5 ~ "Removing borders \n between states",
                        Q2a == 6 ~ "Influence of Europe in the world",
                        Q2a == 7 ~ "The Euro currency",
                        Q2a == 8 ~ "A European identity",
                        Q2a == 9 ~ "Democratic values",
                        Q2a == 10 ~ "Support for deprived regions",
                        Q2a == 11 ~ "Protection of workers' rights",
                        Q2a == 12 ~ "Higher standards for consumers",
                        TRUE ~ NA_character_),
    eu_worst = case_when(
      Q3a == 1 ~ "Unemployment",
      Q3a == 2 ~ "Loss of national sovereignty",
      Q3a == 3 ~ "Mass immigration",
      Q3a == 4 ~ "Economic austerity",
      Q3a == 5 ~ "The Euro single currency",
      Q3a == 6 ~ "The refugee crisis",
      Q3a == 7 ~ "Removing borders between states",
      Q3a == 8 ~ "Bureaucracy and \n excessive regulation",
      Q3a == 9 ~ "Lack of democracy",
      Q3a == 10 ~ "Enlargement to other countries",
      Q3a == 11 ~ "Other",
      Q3a == 12 ~ "None",
      Q3a == 13 ~ "Don't know/ No answer",
      TRUE ~ NA_character_)
  )

# Recode variables in ESS
ess <- ess %>% 
  mutate(
    tr = case_when(
      inwmme == 12 & inwdde > 19 ~ 1,
      inwmme == 12 & inwdde < 19 ~ 0,
      TRUE  ~  NA_real_
    ),
    eu5 = ifelse(euftf > 11, NA, euftf/10),
    imm7 = ifelse(imbgeco > 11, NA, imbgeco/10),
    ref2 = ifelse(gvrfgap > 5, NA, (5-gvrfgap) / 4),
    ref3 = ifelse(rfgfrpc > 5, NA, (rfgfrpc - 1) / 4),
    ref4 = ifelse(rfgbfml > 5, NA, (5-rfgbfml) / 4),
    # Countries
    country = ifelse(cntry %in% c("DE", "EE", "FR", "IL", "PL", "NL"), 1, 0),
    
    ## Germany
    germany = case_when(
      cntry == "DE" & country == 1 ~ 1,
      cntry != "DE" & country == 1 ~ 0
    ),
    
    #Covariates 
    name = countrycode(cntry, 'iso2c', 'country.name'),
    male = case_when(
      gndr == 1 ~ 1,
      gndr == 2 ~ 0,
      TRUE  ~  NA_real_
    ),
    age = ifelse(agea > 97 & agea < 18, NA, agea),
    edu = ifelse(eisced > 10, NA, eisced),
    income = ifelse(hinctnta > 10, NA, hinctnta),
    unemployed = case_when(
      uempla == 1 ~ 1,
      uempli == 1 ~ 1,
      TRUE  ~  0
    ),
    lrscale = ifelse(lrscale > 10, NA, lrscale)
  )


eb1511 <- eb1511 %>% 
  mutate(period = as.Date("2015-11-01"),
         imp_terror = qa3a_6,
         ref = qd11_6,
         imm = qd11_3,
         eu_trust = qa8a_10)

eb1605 <- eb1605 %>% 
  mutate(period = as.Date("2016-05-01"),
         imp_terror = qa3a_6,
         ref = qd4_5,
         imm = qd4_2,
         eu_trust = qa8a_9)

eb1611 <- eb1611 %>% 
  mutate(period = as.Date("2016-11-01"),
         imp_terror = qa3a_6,
         ref = qd9_5,
         imm = qd9_2,
         eu_trust = qa8a_14,
         security = case_when(
           qa17_4 == 1 ~ 1,
           qa17_4 == 2 ~ 0,
           TRUE ~ NA_real_
         )
  )

eb1705 <- eb1705 %>% 
  mutate(period = as.Date("2017-05-01"),
         imp_terror = qa3a_6,
         ref = qd11_6,
         imm = qd11_3,
         eu_trust = qa8a_9,
         security = case_when(
           qa16_4 == 1 ~ 1,
           qa16_4 == 2 ~ 0,
           TRUE ~ NA_real_
         )
  )

eb1711 <- eb1711 %>% 
  mutate(period = as.Date("2017-11-01"),
         imp_terror = qa3a_6,
         ref = qd9_4,
         imm = qd9_1,
         eu_trust = qa8a_14)

eb1511_select <- eb1511 %>%
  select(isocntry, period, imp_terror, ref, imm, eu_trust)

eb1605_select <- eb1605 %>%
  select(isocntry, period, imp_terror, ref, imm, eu_trust)

eb1611_select <- eb1611 %>%
  select(isocntry, period, imp_terror, ref, imm, eu_trust)

eb1705_select <- eb1705 %>%
  select(isocntry, period, imp_terror, ref, imm, eu_trust)

eb1711_select <- eb1711 %>%
  select(isocntry, period, imp_terror, ref, imm, eu_trust)

eb <- rbind(eb1511_select, eb1605_select, eb1611_select, eb1705_select, eb1711_select)

eb <- eb %>%
  mutate(eu = case_when(
    eu_trust == 1 ~ 1,
    eu_trust == 2 ~ 0,
    TRUE  ~  NA_real_
  ),
  ref_agree = case_when(
    ref == 1 | ref == 2 ~ 1,
    ref == 3 | ref == 4 ~ 0,
    TRUE ~ NA_real_
  ),
  imm_agree = case_when(
    imm == 1 | imm == 2 ~ 1,
    imm == 3 | imm == 4 ~ 0,
    TRUE ~ NA_real_
  )
  )

## Long-term Panel 2013-2017 (GLES)

gles <- gles %>%
  mutate(imm16 = ifelse(m580a < 0, NA, m580a),
         imm17 = ifelse(n580a < 0, NA, n580a))

# Export data

eb %>%
  filter(!is.na(eu) & !is.na(ref_agree) & !is.na(imm_agree)) %>%
  export("data_eb.csv")

ess %>% 
  select(tr, eu5, imm7, ref2, ref3, ref4, country, germany, name, male, age, edu, income, unemployed, lrscale) %>%
  filter(!is.na(tr)) %>%
  export("data_ess.csv")

ch %>%
  select(tr, country, name, germany, Q2a, Q3a, eu1, eu2, eu3, eu4, imm1, imm2, imm3, imm4, imm5, imm6, ref1, male, age, edu, income, unemployed, lrscale, eu_best, eu_worst) %>%
  filter(!is.na(tr)) %>%
  export("data_ch.csv")

gles %>%
  select(imm16, imm17) %>%
  filter(!is.na(imm16) & !is.na(imm17)) %>%
  export("data_gles.csv")