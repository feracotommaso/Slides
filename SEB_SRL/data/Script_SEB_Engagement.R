#### Library ####
library(psych)
library(dplyr)
library(lavaan)
library(readxl)
library(semTools)
library(careless)

#### Upload the dataset #### 
# The dataset contains all questionnaires that were administered during data collection.
# The dataset contains only participants who have completed all questionnaires.
d <- read_excel("Data_SEB_Engagement.xlsx")

# We included in the study only participants who reported Male or Female gender.
d <- d%>% filter(d$Gender == 1 | d$Gender == 2) 

#### Identify and delete careless responders ####
# In this section, we identify and delete careless respondents using the "careless" package.

# Long-string analysis: Identify participants with long sequences of identical responses to the items
# Subset the dataset to include only the BESSI items
sub_BESSI <- subset(d, select = c(bessi_1:bessi_45))

# Calculate the longest longstring value for each participant
sub_BESSI$careless_longest <- longstring(sub_BESSI, avg = TRUE)[, 1]

# Calculate mean and standard deviation of longstring values
long_mean <- mean(sub_BESSI$careless_longest)
long_sd <- sd(sub_BESSI$careless_longest)

# Standardize the longstring values and identify participants with values greater than 2 SD
sub_BESSI$bessi_longstring <- (sub_BESSI$careless_longest - long_mean) / long_sd
d$BESSI_longstring <- sub_BESSI$bessi_longstring >= 2

# Repeat the process for Student Engagement Scale items
sub_ENGAGE <- subset(d, select = c(aff_1:cog_12))
sub_ENGAGE$careless_longest <- longstring(sub_ENGAGE, avg = TRUE)[, 1]
long_mean2 <- mean(sub_ENGAGE$careless_longest)
long_sd2 <- sd(sub_ENGAGE$careless_longest)
sub_ENGAGE$ENGAGE_longstring <- (sub_ENGAGE$careless_longest - long_mean2) / long_sd2
d$ENGAGE_longstring <- sub_ENGAGE$ENGAGE_longstring >= 2

# Filter out participants identified as careless based on long-string analysis
d <- d %>% filter(ENGAGE_longstring == FALSE & BESSI_longstring == FALSE)

#112 participants were excluded with this procedure

# Psychometric Antonym Score
# In this section, we identify participants with high psychometric antonyms values using the items from all questionnaires.
# Subset the dataset to include all relevant items
sub_ant <- subset(d, select = c(bessi_1:burn_9))

# Calculate the critical value for psychometric antonyms
psychsyn_critval(sub_ant, anto = TRUE) 

# Calculate psychometric antonyms for each participant
sub_ant$antonyms <- psychant(sub_ant, -.50, diag = TRUE)[, 2] 

# Calculate mean and standard deviation of psychometric antonyms
ant_mean <- mean(sub_ant$antonyms, na.rm = TRUE)
ant_sd <- sd(sub_ant$antonyms, na.rm = TRUE)

# Standardize the psychometric antonyms values and identify participants with values less than or equal to -2
sub_ant$ant_score <- (sub_ant$antonyms - ant_mean) / ant_sd
d$ant_score <- sub_ant$ant_score <= -2

# Filter out participants identified as having high psychometric antonyms values
d <- d %>% filter(d$ant_score == FALSE) 

#6 participants were excluded with this procedure

#### Scoring of all the questionnaires #### 
# In this section, we score all questionnaires
# School achievement
# Values of 11 (10 with laude) are transformed into 10
d$achievement<-rowMeans(d[,c("italian",  "math", "english", "science", "history")], na.rm=TRUE)

cor_achiev <- subset(d, select = c(italian, math, english, science, history))

plot_achiev <- cor(x = cor_achiev, 
                y = cor_achiev, 
                use = "pairwise.complete.obs", method = "pearson")

# Scoring of the questionnaires
# BESSI 
d$SMD<-rowMeans(d[,c("bessi_1",  "bessi_6", "bessi_11", "bessi_16",
                       "bessi_21", "bessi_26", "bessi_31", "bessi_36",
                       "bessi_41")], na.rm=TRUE) # Self-management skills domain

d$IND<-rowMeans(d[,c("bessi_5", "bessi_10", "bessi_15", "bessi_20",
                       "bessi_25", "bessi_30", "bessi_35", "bessi_40",
                       "bessi_45")], na.rm=TRUE) # Innovation skills domain

d$COD<-rowMeans(d[,c("bessi_3", "bessi_8", "bessi_13", "bessi_18",
                       "bessi_23", "bessi_28", "bessi_33", "bessi_38",
                       "bessi_43")], na.rm=TRUE) # Cooperation skills domain

d$SED<-rowMeans(d[,c("bessi_2", "bessi_7", "bessi_12", "bessi_17",
                       "bessi_22", "bessi_27", "bessi_32", "bessi_37",
                       "bessi_42")], na.rm=TRUE) # Social engagement skills domain

d$ERD<-rowMeans(d[,c("bessi_4", "bessi_9", "bessi_14", "bessi_19",
                       "bessi_24", "bessi_29", "bessi_34", "bessi_39",
                       "bessi_44")], na.rm=TRUE) # Emotional resilience skills domain

# Student Engagement 
# Reverse items
d$rev_aff_9 <- 6 - d$aff_9
d$rev_beha_2 <- 6 - d$beha_2
d$rev_beha_4 <- 6 - d$beha_4
d$rev_beha_9 <- 6 - d$beha_9

d$AFF_ENG<-rowMeans(d[,c("aff_1", "aff_2", "aff_3", "aff_4",
                     "aff_5", "aff_6", "aff_7", "aff_8", 
                     "rev_aff_9")], na.rm=TRUE) # Affective engagement

d$BEH_ENG<-rowMeans(d[,c("beha_1", "rev_beha_2", "beha_3", "rev_beha_4",
                     "beha_5", "beha_6", "beha_7", "beha_8", 
                     "rev_beha_9", "beha_10", "beha_11",
                     "beha_12")], na.rm=TRUE) # Behavioral engagement

d$COG_ENG<-rowMeans(d[,c("cog_1", "cog_2", "cog_3", "cog_4",
                         "cog_5", "cog_6", "cog_7", "cog_8", 
                         "cog_9", "cog_10", "cog_11", 
                         "cog_12")], na.rm=TRUE) # Cognitive engagement

d$AG_ENG<-rowMeans(d[,c("agen_1", "agen_2", "agen_3", "agen_4",
                     "agen_5")], na.rm=TRUE) # Agentic engagement

d$TOT_ENG<-rowMeans(d[,c("aff_1", "aff_2", "aff_3", "aff_4",
                         "aff_5", "aff_6", "aff_7", "aff_8", 
                         "rev_aff_9", 
                         "beha_1", "rev_beha_2", "beha_3", "rev_beha_4",
                         "beha_5", "beha_6", "beha_7", "beha_8", 
                         "rev_beha_9", "beha_10", "beha_11", "beha_12",
                         "cog_1", "cog_2", "cog_3", "cog_4",
                         "cog_5", "cog_6", "cog_7", "cog_8", 
                         "cog_9", "cog_10", "cog_11", "cog_12",
                         "agen_1", "agen_2", "agen_3", "agen_4",
                         "agen_5")], na.rm=TRUE) # Global student engagement


# School and Life Satisfaction, School Burnout
d$School_Sat <-rowMeans(d[,c("school_sat_1", "school_sat_2", "school_sat_3", 
                             "school_sat_4", "school_sat_5")], na.rm=TRUE) 
d$Life_Sat <-rowMeans(d[,c("life_1", "life_2", "life_3", 
                           "life_4", "life_5")], na.rm=TRUE) 
d$Burnout <-rowMeans(d[,c("burn_1", "burn_2", "burn_3", 
                          "burn_4", "burn_5", "burn_6",
                          "burn_7", "burn_8", "burn_9")], na.rm=TRUE) 

#### Descriptive statistics ####
# Subset the dataset to include selected variables
Des_sub <- subset(d, select = c(Grade, Gender, School, age, achievement, 
                                SMD:ERD, AFF_ENG:TOT_ENG, 
                                School_Sat:Burnout, Cattell_TOT))

# Calculate various descriptive statistics for the selected variables
# Number of non-NA values in each variable
N <- sapply(Des_sub[,4:19], function(x) sum(!is.na(x)))
# Mean of each variable
means <- sapply(Des_sub[, 4:19], mean, na.rm = TRUE)
# Standard deviation of each variable
sds <- sapply(Des_sub[, 4:19], sd, na.rm = TRUE)
# Skewness of each variable
skew <- sapply(Des_sub[, 4:19], moments::skewness, na.rm = TRUE)
# Kurtosis of each variable
kurt <- sapply(Des_sub[, 4:19], moments::kurtosis, na.rm = TRUE)
# Minimum value of each variable
min <- sapply(Des_sub[, 4:19], min, na.rm = TRUE)
# Maximum value of each variable
max <- sapply(Des_sub[, 4:19], max, na.rm = TRUE)

# Create a data frame to store the descriptive statistics
table_1 <- data.frame(N = N, mean = round(means, 2), sd = round(sds, 2), 
                      skewness = round(skew, 2), kurtosis = round(kurt, 2),
                      min = round(min, 0), max = round(max, 0))

#### Reliability Analysis (Alpha and Omega)####
# Define the measurement model specifying the relationships between latent variables and their indicators
mod <- "SelfManagement =~ bessi_1 + bessi_6 + bessi_11 + bessi_16 +
                     bessi_21 + bessi_26 + bessi_31 + bessi_36 + bessi_41
        Innovation =~ bessi_5 + bessi_10 + bessi_15 + bessi_20 + 
                      bessi_25 + bessi_30 + bessi_35 + bessi_40 + bessi_45
        Cooperation =~ bessi_3 + bessi_8 + bessi_13 + bessi_18 + bessi_23 + 
                       bessi_28 + bessi_33 + bessi_38 + bessi_43
        SocialEngagement =~ bessi_2 + bessi_7 + bessi_12 + bessi_17 +
                            bessi_22 + bessi_27 + bessi_32 + bessi_37 + bessi_42
        EmotionResilience =~ bessi_4 + bessi_9 + bessi_14 + bessi_19 + bessi_24 + 
                              bessi_29 + bessi_34 + bessi_39 + bessi_44"                     

# Fit the structural equation model (SEM) using lavaan
fit <- sem(mod, data = d, ordered = TRUE)

# Calculate reliability coefficients (Omega and Alpha) using semTools package
semTools::reliability(fit)

# Repeat the same procedure for all variables
# Engagement (reliability for the 4 dimensions scores)
mod <- "Affective =~ aff_1 + aff_2 + aff_3 + aff_4 + aff_5 + aff_6 +
                     aff_7 + aff_8 + aff_9
        Behavioral =~ beha_1 + rev_beha_2 + beha_3 + rev_beha_4 + beha_5 + 
                      beha_6 + beha_7 + beha_8 + rev_beha_9 + beha_10 + 
                      beha_11 + beha_12
        Cognitive =~ cog_1 + cog_2 + cog_3 + cog_4 + cog_5 + cog_6 + cog_7 + 
                     cog_8 + cog_9 + cog_10 + cog_11 + cog_12
        Agentic =~ agen_1 + agen_2 + agen_3 + agen_4 + agen_5"
fit <- sem(mod, data = d, ordered = T)
semTools::reliability(fit)

# Engagement (reliability for the global score)
mod <-"Engagement =~ aff_1 + aff_2 + aff_3 + aff_4 + aff_5 + aff_6 +
                     aff_7 + aff_8 + rev_aff_9 + beha_1 + rev_beha_2 + beha_3 + rev_beha_4 + beha_5 + 
                      beha_6 + beha_7 + beha_8 + rev_beha_9 + beha_10 + 
                      beha_11 + beha_12 + cog_1 + cog_2 + cog_3 + cog_4 + cog_5 + cog_6 + cog_7 + 
                     cog_8 + cog_9 + cog_10 + cog_11 + cog_12 + agen_1 + agen_2 + agen_3 + agen_4 + agen_5"
fit <- sem(mod, data = d, ordered = T)
semTools::reliability(fit)

# School Satisfaction
mod <- "SchoolSati =~ school_sat_1 + school_sat_2 + school_sat_3 + school_sat_4 + school_sat_5"
fit <- sem(mod, data = d, ordered = T)
semTools::reliability(fit)

# Life Satisfaction
mod <- "LifeSati =~ life_1 + life_2 + life_3 + life_4 + life_5"
fit <- sem(mod, data = d, ordered = T)
semTools::reliability(fit)

# School Burnout
mod <- "SchoolBurnout =~ burn_1 + burn_2 + burn_3 + burn_4 + burn_5 + 
                   burn_6 + burn_7 + burn_8 + burn_9"
fit <- sem(mod, data = d, ordered = T)
semTools::reliability(fit)
#### Standardize the variables ####
# Standardize the variables to ensure comparability and interpretability
# Standardization transforms variables to have a mean of 0 and a standard deviation of 1

# Standardize variables across the entire dataset
d <- d %>% 
  mutate_at(c("age","SMD", "IND", "SED", "COD", "ERD", 
              "AFF_ENG", "BEH_ENG", "COG_ENG", "AG_ENG", "TOT_ENG", 
              "School_Sat", "Life_Sat", "Burnout"), 
            ~(scale(.) %>% as.vector()))

# Standardize achievement and Cattell Test scores within each grade
d <- d %>%
  group_by(Grade) %>%
  mutate(
    # Standardize Cattell Series score
    Cattell_Series = scale(Cattell_Series) %>% as.vector(),
    # Standardize Cattell Classification score
    Cattell_Classification = scale(Cattell_Classification) %>% as.vector(),
    # Standardize total Cattell Test score
    Cattell_TOT = scale(Cattell_TOT) %>% as.vector(),
    # Standardize achievement score
    achievement = scale(achievement) %>% as.vector()
  ) %>%
  ungroup()

#### Path analysis ####
#Define dummy variables for the schools
d$School2 <- ifelse(d$School == "2", 1, 0)
d$School3 <- ifelse(d$School == "3", 1, 0)
d$School4 <- ifelse(d$School == "4", 1, 0)

# Define the structural equation model (SEM)
# Each line represents a regression equation where variables are regressed on other variables or covariates.
# For example, in the line "TOT_ENG ~ age + Gender + a*SMD + b*IND + c*SED + d*COD + e*ERD", TOT_ENG is regressed on age, Gender, SMD, IND, SED, COD, and ERD.
# The ":=" operator is used to identify the mediated effects.
mod <- "TOT_ENG ~ age + Gender + School2 + School3 + School4 + a*SMD + b*IND + c*SED + d*COD + e*ERD 
        School_Sat ~ age + Gender + School2 + School3 + School4 + sa*TOT_ENG + sb*SMD + sc*IND + sd*SED + se*COD + sf*ERD
        Life_Sat ~ age + Gender + School2 + School3 + School4 + la*TOT_ENG + lb*SMD + lc*IND + ld*SED + le*COD + lf*ERD
        Burnout ~ age + Gender + School2 + School3 + School4 + ba*TOT_ENG + bb*SMD + bc*IND + bd*SED + be*COD + bf*ERD
        achievement ~ age + Gender + School2 + School3 + School4 + aa*TOT_ENG + ab*SMD + ac*IND + ad*SED + ae*COD + af*ERD + Cattell_TOT
        SMD_ENG_SchoolSAT := a*sa
        IND_ENG_SchoolSAT := b*sa
        SED_ENG_SchoolSAT := c*sa
        COD_ENG_SchoolSAT := d*sa
        ERD_ENG_SchoolSAT := e*sa
        SMD_ENG_LifeSAT := a*la
        IND_ENG_LifeSAT := b*la
        SED_ENG_LifeSAT := c*la
        COD_ENG_LifeSAT := d*la
        ERD_ENG_LifeSAT := e*la
        SMD_ENG_BURN := a*ba
        IND_ENG_BURN := b*ba
        SED_ENG_BURN := c*ba
        COD_ENG_BURN := d*ba
        ERD_ENG_BURN := e*ba
        SMD_ENG_ACHI := a*aa
        IND_ENG_ACHI := b*aa
        SED_ENG_ACHI := c*aa
        COD_ENG_ACHI := d*aa
        ERD_ENG_ACHI := e*aa" 

# Fit the SEM model to the data using maximum likelihood estimation
fit <- sem(mod, data = d, estimator = "ML", se = "bootstrap",
           bootstrap = 1000, test = "bootstrap")

# Provide a summary of the SEM fit, including standardized coefficients and R-squared values
summary(fit, standardized = T, rsquare = T)

# Define the fit measures to be computed
fi <- c("chisq", "df", "pvalue", "rmsea", "srmr", "nnfi", "cfi", "tli", "aic", "bic")

# Compute various fit measures for evaluating the SEM model
fitMeasures(fit, fit.measures = fi)

# This function is used to obtain confidence intervals of the path coefficients
standardizedsolution(fit)
##### Multivariate analysis - SEB skills & Student Engagement dimensions ####
# Fit a linear regression model to predict multiple outcomes simultaneously using the cbind function
# - Predictors: age, gender, and five domains of SEB skills
# - Outcomes: four dimensions of student engagement 

mod <- lm(cbind(AFF_ENG, BEH_ENG, COG_ENG, AG_ENG) ~ age + Gender + School2 + School3 + School4 + SMD + IND + SED + COD + ERD, data = d) 

#Provide a summary of the multlivariate regression
summary(mod)
round(mod[["coefficients"]], 2)

# Extract the 95% confidence intervals
round(stats::confint(mod, level = 0.95), 2)

#### Correlation analysis ####
# We run bivariate correlations between all the variables considered in the study
# We will use the corstars function 
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  # Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-Hmisc::rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "*   ", "    ")
  
  ## Trunctuate the correlation matrix to two decimal
  R <- round(cbind(rep(-1.11, ncol(x)), R), 2)
  R1<-R
  
  for (i in 1:ncol(R)){
    R1[ ,i]<-sub('^(-)?0[.]', '\\1.', R[ ,i])
  }
  
  R<-format(R1)[,-1]
  
  ## Build a new matrix that includes the correlations with their appropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- paste(1:ncol(x), colnames(x) , sep=".")
  colnames(Rnew) <- paste(1:ncol(x), ".", sep = "")
  
  ## Remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## Remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## Remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable::xtable(Rnew), type="html")
    else print(xtable::xtable(Rnew), type="latex") 
  }
}

# We select all the variable that we want to correlate 
cor_quest <- subset(d, select = c(SMD, IND, SED, COD, ERD, 
                                  AFF_ENG, BEH_ENG, COG_ENG, AG_ENG, TOT_ENG, 
                                  School_Sat, Life_Sat, Burnout, achievement, 
                                  Cattell_TOT, age))

# We obtain the correlation matrix
# * = p < .001
corstars(cor_quest, method = "pearson")

# We ran the correlations with gender
cor_gend <- subset(d, select = c(Gender, SMD, IND, SED, COD, ERD, 
                                  AFF_ENG, BEH_ENG, COG_ENG, AG_ENG, TOT_ENG, 
                                  School_Sat, Life_Sat, Burnout, achievement, 
                                  Cattell_TOT, age))

# We obtain the correlation matrix
# * = p < .001
corstars(cor_gend, method = "spearman")

#### Supplementary analysis ####
# These analyses are reported as "Supplementary Information"
# We examine the correlation between affective engagement and burnout
# We subset all relevant items
cor_quest <- subset(d, select = c(aff_1, aff_2, aff_3, aff_4, aff_5, aff_6,
                                  aff_7, aff_8, aff_9, 
                                  burn_1, burn_2, burn_3, 
                                  burn_4, burn_5, burn_6,
                                  burn_7, burn_8, burn_9))
# We obtain the correlation matrix
# * = p < .001
corstars(cor_quest, method = "pearson")

# We examine the correlations between affective engagement and school satisfaction
# We subset all relevant items
cor_quest <- subset(d, select = c(aff_1, aff_2, aff_3, aff_4, aff_5, aff_6,
                                    aff_7, aff_8, aff_9, 
                                  school_sat_1, school_sat_2, school_sat_3, 
                                  school_sat_4, school_sat_5))


# We obtain the correlation matrix
# * = p < .001
corstars(cor_quest, method = "pearson")

# We ran the path analysis again without affective engagement to ensure robustness of previous results
# Calculate global engagement score without affective engagement items
d$TOT_ENG<-rowMeans(d[,c("beha_1", "rev_beha_2", "beha_3", "rev_beha_4",
                         "beha_5", "beha_6", "beha_7", "beha_8", 
                         "rev_beha_9", "beha_10", "beha_11", "beha_12",
                         "cog_1", "cog_2", "cog_3", "cog_4",
                         "cog_5", "cog_6", "cog_7", "cog_8", 
                         "cog_9", "cog_10", "cog_11", "cog_12",
                         "agen_1", "agen_2", "agen_3", "agen_4",
                         "agen_5")], na.rm=TRUE)

# Fit the model again 
mod <- "TOT_ENG ~ age + Gender + School2 + School3 + School4 + a*SMD + b*IND + c*SED + d*COD + e*ERD 
        School_Sat ~ age + Gender + School2 + School3 + School4 + sa*TOT_ENG + sb*SMD + sc*IND + sd*SED + se*COD + sf*ERD
        Life_Sat ~ age + Gender + School2 + School3 + School4 + la*TOT_ENG + lb*SMD + lc*IND + ld*SED + le*COD + lf*ERD
        Burnout ~ age + Gender + School2 + School3 + School4 + ba*TOT_ENG + bb*SMD + bc*IND + bd*SED + be*COD + bf*ERD
        achievement ~ age + Gender + School2 + School3 + School4 + aa*TOT_ENG + ab*SMD + ac*IND + ad*SED + ae*COD + af*ERD + Cattell_TOT
        
        SMD_ENG_SchoolSAT := a*sa
        IND_ENG_SchoolSAT := b*sa
        SED_ENG_SchoolSAT := c*sa
        COD_ENG_SchoolSAT := d*sa
        ERD_ENG_SchoolSAT := e*sa
        SMD_ENG_LifeSAT := a*la
        IND_ENG_LifeSAT := b*la
        SED_ENG_LifeSAT := c*la
        COD_ENG_LifeSAT := d*la
        ERD_ENG_LifeSAT := e*la
        SMD_ENG_BURN := a*ba
        IND_ENG_BURN := b*ba
        SED_ENG_BURN := c*ba
        COD_ENG_BURN := d*ba
        ERD_ENG_BURN := e*ba
        SMD_ENG_ACHI := a*aa
        IND_ENG_ACHI := b*aa
        SED_ENG_ACHI := c*aa
        COD_ENG_ACHI := d*aa
        ERD_ENG_ACHI := e*aa" 

# Fit the SEM model to the data using maximum likelihood estimation
fit <- sem(mod, data = d, estimator = "ML", se = "bootstrap",
           bootstrap = 1000, test = "bootstrap")

# Provide a summary of the SEM fit, including standardized coefficients and R-squared values
summary(fit, standardized = T, rsquare = T)

# Define the fit measures to be computed
fi <- c("chisq", "df", "pvalue", "rmsea", "srmr", "nnfi", "cfi", "tli", "aic", "bic")

# Compute various fit measures for evaluating the SEM model
fitMeasures(fit, fit.measures = fi)

# This function is used to obtain confidence intervals of the path coefficients
standardizedsolution(fit)

