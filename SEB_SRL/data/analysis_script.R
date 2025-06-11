# -----------------------------------------------------------------------------------------------------------------#
############################################## OPEN THE DATA #######################################################
# -----------------------------------------------------------------------------------------------------------------#

library(readxl)
library(dplyr)
library(lavaan)
library(psych)
library(broom)
library(ggplot2)  

d = read_excel("dataSEB_SRL.xlsx")

# -----------------------------------------------------------------------------------------------------------------#
############################################### DESCRIPTIVES #######################################################
# -----------------------------------------------------------------------------------------------------------------#
# --- Careless: This shows how many participants have missing data or are careless respondents for each inventory
# car -> careless
# qas -> self-regulated learning strategies
# qc -> motivation questionnaire
# qe -> emotion questionnaire
# sl -> satisfaction with life questionnaire
apply(subset(d, select = c(carBessi, carQas, carQc, carQe, carSl)),2,table)

# --- Age and gender: Calculate mean age and gender distribution
table(d$Age)
mean(d$Age); sd(d$Age)
table(d$Gender)

# -----------------------------------------------------------------------------------------------------------------#
############################################### FACTOR SCORES ######################################################
# -----------------------------------------------------------------------------------------------------------------#
# In this section, factor scores for each construct are extracted
# As this may take time, the final R data is already available and can be opened in the next CORRELATIONS section

# Create a new dataframe to store factor scores + useful info (id, sex, age)
dd <- data.frame(ID = d$ID, 
                 sex = as.numeric(ifelse(d$Gender == "Male", 0, 
                                         ifelse(d$Gender == "Female", 1, d$Gender))),
                 age = d$Age)

# --- SELF MANAGEMENT
SELFM<-"SELFM=~ bessi_1 + bessi_6 + bessi_11 + bessi_16 + bessi_21 + bessi_26 + bessi_31 + bessi_36 + bessi_41
# NO Correlated residuals
"
fit_SELFM <- sem(SELFM, ordered = T, data = d) 
dd$SMD[lavInspect(fit_SELFM, "case.idx")] <- lavPredict(fit_SELFM)

# --- INNOVATION
INN<-"INN=~ bessi_5 + bessi_10 + bessi_15 + bessi_20 + bessi_25 + bessi_30 + bessi_35 + bessi_40 + bessi_45
# Correlated residuals
bessi_5 ~~ bessi_30
bessi_35 ~~ bessi_10
bessi_40 ~~ bessi_15
bessi_20 ~~ bessi_45
"
fit_INN <- sem(INN, ordered = T, data = d) 
dd$IND[lavInspect(fit_INN, "case.idx")] <- lavPredict(fit_INN)

# --- INTERACTION
INTER<-"INTER=~ bessi_2 + bessi_7 + bessi_12 + bessi_17 + bessi_22 + bessi_27 + bessi_32 + bessi_37 + bessi_42
# Correlated residuals
bessi_2 ~~ bessi_27
bessi_17 ~~ bessi_42
bessi_7 ~~ bessi_32
bessi_37 ~~ bessi_12
"
fit_INTER <- sem(INTER, ordered = T, data = d) 
dd$SED[lavInspect(fit_INTER, "case.idx")] <- lavPredict(fit_INTER)

# --- COOPERATION
COOP<-"COOP=~ bessi_3 + bessi_8 + bessi_13 + bessi_18 + bessi_23 + bessi_28 + bessi_33 + bessi_38 + bessi_43
# Correlated residuals
bessi_28 ~~ bessi_3
bessi_8 ~~ bessi_33
bessi_18 ~~ bessi_43
bessi_38 ~~ bessi_13
"
fit_COOP <- sem(COOP, ordered = T, data = d) 
dd$COD[lavInspect(fit_COOP, "case.idx")] <- lavPredict(fit_COOP)

# --- EMOTIONAL REGULATION
EMR<-"EMR=~ bessi_4 + bessi_9 + bessi_14 + bessi_19 + bessi_24 + bessi_29 + bessi_34 + bessi_39 + bessi_44
# Correlated residuals
bessi_4 ~~ bessi_29
bessi_34 ~~ bessi_9
bessi_14 ~~ bessi_39
bessi_19 ~~ bessi_44"
fit_EMR<- sem(EMR, ordered = T, data = d) 
dd$ESD[lavInspect(fit_EMR, "case.idx")] <- lavPredict(fit_EMR)

# --- QAS (self-regulated learning strategies)
mqas <- "
srl =~ qas_1+qas_10+qas_12+qas_15+qas_2+qas_3+qas_7+qas_13+qas_4+qas_8+qas_9+qas_14+qas_5+qas_6+qas_11+qas_16
"
fitQas <- sem(mqas, data = d, ordered = T)
dd$qas[lavInspect(fitQas, "case.idx")] <- lavPredict(fitQas)#[,5]

# --- QC (motivation questionnaires)
# --- Mindset
mnd <- "
mnds =~ qc_1+qc_2+qc_3+qc_4 
"
fitMnd <- sem(mnd, data = d, ordered = T)
dd$mindset[lavInspect(fitMnd, "case.idx")] <- lavPredict(fitMnd)

# --- Learning goals
mgl <- "
gls =~ qc_5+qc_6+qc_7+qc_8 
"
fitGls <- sem(mgl, data = d, ordered = T)
dd$goals[lavInspect(fitGls, "case.idx")] <- lavPredict(fitGls)

# --- Self-efficacy
mslf <- "
slf =~ qc_9+qc_10+qc_11+qc_12 # GOOD
"
fitSlf <- sem(mslf, data = d, ordered = T)
dd$selfEff[lavInspect(fitSlf, "case.idx")] <- lavPredict(fitSlf)

# --- QE (achievement emotions)
# --- Positive
mpos <- "
pos =~ qe_2+qe_4+qe_6+qe_8+qe_10+qe_12+qe_14
"
fitPos <- sem(mpos, data = d, ordered = T)
dd$emoPos[lavInspect(fitPos, "case.idx")] <- lavPredict(fitPos)

# --- Negative
mneg <- "
neg =~ qe_1+qe_3+qe_5+qe_7+qe_9+qe_11+qe_13 # GOOD
"
fitNeg <- sem(mneg, data = d, ordered = T)
dd$emoNeg[lavInspect(fitNeg, "case.idx")] <- lavPredict(fitNeg)

# --- Academic achievement (average grade)
dd$media <- d$media

# --- SWLS
mswls <- "
swl =~ swls_1+swls_2+swls_3+swls_4+swls_5
"
fitSwls <- sem(mswls, data = d, ordered = T)
dd$swls[lavInspect(fitSwls, "case.idx")] <- lavPredict(fitSwls)

# --- Acceptance
macc <- "
accept =~ acc_1+acc_2+acc_3+acc_4+acc_5+
          acc_6+acc_7+acc_8+acc_9+acc_10+
          acc_11+acc_12
"
fitAcc <- sem(macc, data = d, ordered = T)
dd$acceptance[lavInspect(fitAcc, "case.idx")] <- lavPredict(fitAcc)

# --- Reliabilities
# Extract alpha and omega coefficients
(rl_SELFM <- semTools::reliability(fit_SELFM)[2:3])
(rl_INN<- semTools::reliability(fit_INN)[2:3] )
(rl_INTER<- semTools::reliability(fit_INTER)[2:3] )
(rl_COOP<- semTools::reliability(fit_COOP)[2:3] )
(rl_EMR<- semTools::reliability(fit_EMR)[2:3] )
(rl_QAS<- semTools::reliability(fitQas)[2:3] )
(rl_MND<- semTools::reliability(fitMnd)[2:3] )
(rl_GLS<- semTools::reliability(fitGls)[2:3] )
(rl_SLF<- semTools::reliability(fitSlf)[2:3] )
(rl_POS<- semTools::reliability(fitPos)[2:3] )
(rl_NEG<- semTools::reliability(fitNeg)[2:3] )
(rl_SWL <- semTools::reliability(fitSwls)[2:3] )
(rl_ACC<- semTools::reliability(fitAcc)[2:3] )

relInd <- rbind(rl_SELFM, rl_INN, rl_COOP, rl_INTER, rl_EMR,
                rl_QAS, rl_MND, rl_GLS, rl_SLF, rl_POS, rl_NEG,
                rl_SWL, rl_ACC)
colnames(relInd) = c("alpha", "omega")

# save.image("factorScores.Rdata") # If you want to save and store your own results

# -----------------------------------------------------------------------------------------------------------------#
############################################### AIM 1 CORRELATIONS #################################################
# -----------------------------------------------------------------------------------------------------------------#
# Load factor scores if you skipped the previous section
# load("factorScores.Rdata")

dd[,4:17] <- apply(dd[,4:17],2,scale) # Standardize all the questionnaires 

# Create Table 2
ctab <- round(cor(subset(dd, select = c(sex,age,SMD:acceptance)), use = "pairwise.complete"),2) # Estimate correlations
rel <- round(as.numeric(c("","",relInd[1:11,1],"",relInd[12:13,1])),2) # Extract reliabilities
missing <- as.numeric(colSums(apply(dd[,-1],2,is.na))) # Get missing data info
careless <- colSums(subset(d, select = c(carBessi, carQas, carQc, carQe, carSl)) == TRUE, na.rm = TRUE) # Calculate careless
careless <- c(0,0,rep(careless[1],5),careless[2],rep(careless[3],3),rep(careless[4],2),0,0,careless[5])
Table2 <- cbind(missing,careless,rel,ctab) # Merge info for Table 2
rownames(Table2) <- colnames(dd[,-1]) # Adjust rownames
(Table2)

corrplot::corrplot(ctab, method = "square", addCoef.col = "black", # Show the correlation table
                   number.cex=0.50)

# -----------------------------------------------------------------------------------------------------------------#
########################################### AIM 2a SEB REGRESSIONS ##################################################
# -----------------------------------------------------------------------------------------------------------------#
# Multivariate regression model with sex + age + SEB skills as predictors 
m0 <- "
acceptance+swls+media+qas+mindset+goals+selfEff+emoNeg+emoPos ~ age+sex + SMD+IND+COD+SED+ESD
"

# Manually specify hypothesized effects for figures
#                     SM   IN   CO   SE   ER  
hp2a_x1 <- c(00, 00,-.10,-.10, .10, .20,-.10, # ACC 
             00, 00, .10,-.10,-.10, .10, .20, # LS
             00, 00, .10,-.10,-.10,-.10,-.10, # AC
             00, 00, .20, .10,-.10,-.10,-.10, # SR
             00, 00, .10, .10,-.10,-.10,-.10, # MI
             00, 00, .10, .10,-.10,-.10,-.10, # LG
             00, 00, .10, .10, .10, .10, .10, # SLF
             00, 00,-.10,-.10,-.10,-.10,-.30, #NEG
             00, 00,-.10,-.10,-.10,-.10, .10) # POS
hp2a_x2 <- c(00, 00, .10, .10, .30, .40, .10, 
             00, 00, .30, .10, .10, .30, .40, 
             00, 00, .30, .10, .10, .10, .10, 
             00, 00, .40, .30, .10, .10, .10, 
             00, 00, .30, .30, .10, .10, .10, 
             00, 00, .30, .30, .10, .10, .10, 
             00, 00, .30, .30, .30, .30, .30, 
             00, 00, .10, .10, .10, .10,-.10, 
             00, 00, .10, .10, .10, .10, .30)

fit0 <- sem(m0, dd,missing="fiml") # Fit the model
summary(fit0,std=T) # Show the results
res0 <- standardizedSolution(fit0) # Store standardized results
res0.1 <- res0[res0$op == "~", c("lhs","rhs","est.std","ci.lower","ci.upper","se","z")] # Extract useful results

(TableS1 <- data.frame(Predictor = res0.1$rhs, # Make Table S1
                      x = rep("→", nrow(res0.1)),
                      Outcome = res0.1$lhs,
                      Effect = paste0(round(res0.1$est.std,3), " [", round(res0.1$ci.lower,3), "; ", round(res0.1$ci.upper,3),"]"),
                      se = round(res0.1$se,3), z = round(res0.1$z,3))
)

R2m0 <- data.frame(variable = res0[64:72, "lhs"],
                   R2 = round(1-(res0[64:72, "est.std"]),2))

# --- Plotting
custom_orderY <- c("ESD", "COD", "SED", "IND", "SMD", "sex", "age") 
res0.1$rhs <- factor(res0.1$rhs, levels = custom_orderY)
custom_orderX <- c("qas","mindset","goals","selfEff","emoPos","emoNeg",
                   "media","swls","acceptance") 
res0.1$lhs <- factor(res0.1$lhs, levels = custom_orderX)
srl_labels <- c("qas" = "SRL strategies","mindset"="Mindset","goals"="Learning goals",
                "selfEff"="Self-efficacy","emoPos"="Positive emotions","emoNeg"="Negative emotions",
                "media"="Academic achievement","swls"="Satisfaction with life",
                "acceptance"="Peer acceptance")
y_labels <- c("Emotional resilience", "Cooperation", "Social engagement", 
              "Innovation", "Self-management", "Females", "Age")

res0.1$xmin = hp2a_x1; res0.1$xmax = hp2a_x2; res0.1$hp = rep(c(7,6,5,4,2,3,1),9)

(Figure2 <- 
    ggplot(res0.1, aes(x = est.std, y = rhs)) +
    geom_point(size = 3) +
    geom_errorbar(aes(xmin = ci.lower, xmax = ci.upper), width = 0.4, linewidth = .8) +
    geom_vline(xintercept = c(-.10, .10), linetype = "dashed") +
    # Adding highlighted rectangles based on hypothesized ranges
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = hp - 0.4, ymax = hp + 0.4), 
              fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
    # Rest of the plot
    labs(x = "Standardized Estimate", y = "Predictor") +
    facet_wrap(~lhs, labeller = labeller(lhs = srl_labels)) +
    scale_x_continuous(breaks = seq(-.30,.30, by = .15)) +
    coord_cartesian(xlim = c(-.35,.35)) +
    scale_y_discrete(labels = y_labels) +
    theme_bw(base_size = 18) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1.2, hjust=1, size = 14))
)
ggsave(plot = Figure2, filename = "Figure2.tiff", width = 14, height = 7, device='tiff', dpi=300)

# -----------------------------------------------------------------------------------------------------------------#
########################################### AIM 2b OUT REGRESSIONS #################################################
# -----------------------------------------------------------------------------------------------------------------#
# Multivariate model with the addition of SRL factors as predictors
m1 <- "
acceptance+swls+media ~ age+sex + qas+mindset+goals+selfEff+emoNeg+emoPos + SMD+IND+COD+SED+ESD
"
# Specify hypothesize for plot
#                        SM   IN   CO   SE   ER  
hp2b_x1 <- c(rep(0, 8),-.10,-.10, .10, .20,-.10, # ACC 
             rep(0, 8), .10,-.10,-.10, .10, .20, # LS
             rep(0, 8), .10,-.10,-.10,-.10,-.10) # AC)
hp2b_x2 <- c(rep(0, 8), .10, .10, .30, .40, .10, 
             rep(0, 8), .30, .10, .10, .30, .40, 
             rep(0, 8), .30, .10, .10, .10, .10)

fit1 <- sem(m1, dd,missing="fiml")
res1 <- standardizedSolution(fit1)
res1.1 <- res1[res1$op == "~", c("lhs","rhs","est.std","ci.lower","ci.upper","se","z")]

(TableS2 <- data.frame(Predictor = res1.1$rhs,
                      x = rep("→", nrow(res1.1)),
                      Outcome = res1.1$lhs,
                      Effect = paste0(round(res1.1$est.std,3), " [", round(res1.1$ci.lower,3), "; ", round(res1.1$ci.upper,3),"]"),
                      se = round(res1.1$se,3), z = round(res1.1$z,3)))

# R squared
R2m1 <- data.frame(variable = res1[40:42, "lhs"],
                   R2 = round(1-(res1[40:42, "est.std"]),2))

# --- Plotting
custom_orderY1 <- c("ESD", "COD", "SED", "IND", "SMD",
                    "qas","mindset","goals","selfEff","emoPos","emoNeg",
                    "sex", "age")
res1.1$rhs <- factor(res1.1$rhs, levels = custom_orderY1)
custom_orderX1 <- c("media","swls","acceptance")
res1.1$lhs <- factor(res1.1$lhs, levels = custom_orderX1)
out_labels <- c("media"="Academic achievement","swls"="Satisfaction with life",
                "acceptance"="Peer acceptance")
y_labels2 <- c("Emotional resilience", "Cooperation", "Social engagement", 
               "Innovation", "Self-management", "SRL strategies", "Mindset",
               "Learning goals", "Self-efficacy", "Positive emotions",
               "Negative emotions", "Females", "Age")
res1.1$xmin = hp2b_x1; res1.1$xmax = hp2b_x2; res1.1$hp = rep(c(13:6,5,4,2,3,1),3)

(Figure3 <- ggplot(res1.1, aes(x = est.std, y = rhs)) +
    geom_point(size=4) +
    geom_errorbar(aes(xmin = ci.lower, xmax = ci.upper), width = 0.4, linewidth = .8) +
    geom_vline(xintercept = c(-.10, .10), linetype = "dashed") +
    # Adding highlighted rectangles based on hypothesized ranges
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = hp - 0.4, ymax = hp + 0.4), 
              fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
    # Continue  
    labs(x = "Standardized Estimate", y = "Predictor Variables") +
    facet_wrap(~lhs, labeller = labeller(lhs = out_labels)) +
    scale_x_continuous(breaks = seq(-.20,.40, by = .10)) +
    scale_y_discrete(labels = y_labels2) +
    theme_bw(base_size = 18) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1.2, hjust=1, size = 14))
)
ggsave(plot = Figure3, filename = "Figure3.tiff", width = 14, height = 7, device='tiff', dpi=300)

# -----------------------------------------------------------------------------------------------------------------#
########################################### SUPPLEMENTARY PATH #####################################################
# -----------------------------------------------------------------------------------------------------------------#
pm <- "
# MEDIATORS REGRESSIONS
qas ~ age+sex + qas01*SMD+qas02*IND+qas03*COD+qas04*SED+qas05*ESD
mindset ~ age+sex + mnd01*SMD+mnd02*IND+mnd03*COD+mnd04*SED+mnd05*ESD
goals ~ age+sex + gls01*SMD+gls02*IND+gls03*COD+gls04*SED+gls05*ESD
selfEff ~ age+sex + slf01*SMD+slf02*IND+slf03*COD+slf04*SED+slf05*ESD
emoNeg ~ age+sex + neg01*SMD+neg02*IND+neg03*COD+neg04*SED+neg05*ESD
emoPos ~ age+sex + pos01*SMD+pos02*IND+pos03*COD+pos04*SED+pos05*ESD

# OUTCOMES REGRESSIONS
media ~      age+sex + 
             qas11*qas+mnd11*mindset+gls11*goals+slf11*selfEff+neg11*emoNeg+pos11*emoPos+
             SMD+IND+COD+SED+ESD
swls ~       age+sex + 
             qas12*qas+mnd12*mindset+gls12*goals+slf12*selfEff+neg12*emoNeg+pos12*emoPos+
             SMD+IND+COD+SED+ESD
acceptance ~ age+sex + 
             qas13*qas+mnd13*mindset+gls13*goals+slf13*selfEff+neg13*emoNeg+pos13*emoPos+
             SMD+IND+COD+SED+ESD

# MEDIATORS COVARIANCE
qas ~~ mindset+goals+selfEff+emoNeg+emoPos
mindset ~~ goals+selfEff+emoNeg+emoPos
goals ~~ selfEff+emoNeg+emoPos
selfEff ~~ emoNeg+emoPos
emoNeg ~~ emoPos

# INDIRECT EFFECTS
# - QAS
smdqasmedia:=qas01*qas11 
smdqasswls:=qas01*qas12 
smdqasacc:=qas01*qas13
indqasmedia:=qas02*qas11 
indqasswls:=qas02*qas12 
indqasacc:=qas02*qas13
codqasmedia:=qas03*qas11 
codqasswls:=qas03*qas12 
codqasacc:=qas03*qas13
sedqasmedia:=qas04*qas11 
sedqasswls:=qas04*qas12 
sedqasacc:=qas04*qas13
esdqasmedia:=qas05*qas11 
esdqasswls:=qas05*qas12 
esdqasacc:=qas05*qas13
# - mnd
smdmndmedia:=mnd01*mnd11 
smdmndswls:=mnd01*mnd12 
smdmndacc:=mnd01*mnd13
indmndmedia:=mnd02*mnd11 
indmndswls:=mnd02*mnd12 
indmndacc:=mnd02*mnd13
codmndmedia:=mnd03*mnd11 
codmndswls:=mnd03*mnd12 
codmndacc:=mnd03*mnd13
sedmndmedia:=mnd04*mnd11 
sedmndswls:=mnd04*mnd12 
sedmndacc:=mnd04*mnd13
esdmndmedia:=mnd05*mnd11 
esdmndswls:=mnd05*mnd12 
esdmndacc:=mnd05*mnd13
# - gls
smdglsmedia:=gls01*gls11 
smdglsswls:=gls01*gls12 
smdglsacc:=gls01*gls13
indglsmedia:=gls02*gls11 
indglsswls:=gls02*gls12 
indglsacc:=gls02*gls13
codglsmedia:=gls03*gls11 
codglsswls:=gls03*gls12 
codglsacc:=gls03*gls13
sedglsmedia:=gls04*gls11 
sedglsswls:=gls04*gls12 
sedglsacc:=gls04*gls13
esdglsmedia:=gls05*gls11 
esdglsswls:=gls05*gls12 
esdglsacc:=gls05*gls13
# - slf
smdslfmedia:=slf01*slf11 
smdslfswls:=slf01*slf12 
smdslfacc:=slf01*slf13
indslfmedia:=slf02*slf11 
indslfswls:=slf02*slf12 
indslfacc:=slf02*slf13
codslfmedia:=slf03*slf11 
codslfswls:=slf03*slf12 
codslfacc:=slf03*slf13
sedslfmedia:=slf04*slf11 
sedslfswls:=slf04*slf12 
sedslfacc:=slf04*slf13
esdslfmedia:=slf05*slf11 
esdslfswls:=slf05*slf12 
esdslfacc:=slf05*slf13
# - neg
smdnegmedia:=neg01*neg11 
smdnegswls:=neg01*neg12 
smdnegacc:=neg01*neg13
indnegmedia:=neg02*neg11 
indnegswls:=neg02*neg12 
indnegacc:=neg02*neg13
codnegmedia:=neg03*neg11 
codnegswls:=neg03*neg12 
codnegacc:=neg03*neg13
sednegmedia:=neg04*neg11 
sednegswls:=neg04*neg12 
sednegacc:=neg04*neg13
esdnegmedia:=neg05*neg11 
esdnegswls:=neg05*neg12 
esdnegacc:=neg05*neg13
# - pos
smdposmedia:=pos01*pos11 
smdposswls:=pos01*pos12 
smdposacc:=pos01*pos13
indposmedia:=pos02*pos11 
indposswls:=pos02*pos12 
indposacc:=pos02*pos13
codposmedia:=pos03*pos11 
codposswls:=pos03*pos12 
codposacc:=pos03*pos13
sedposmedia:=pos04*pos11 
sedposswls:=pos04*pos12 
sedposacc:=pos04*pos13
esdposmedia:=pos05*pos11 
esdposswls:=pos05*pos12 
esdposacc:=pos05*pos13
"
fitpm <- sem(pm, dd, missing = "fiml", se = "bootstrap")
respm <- standardizedSolution(fitpm)
respm1 <- respm[respm$op %in% c("~"), c("lhs","rhs","est.std","ci.lower","ci.upper","se","z")]

# --- Plotting direct effects
custom_orderYpm1 <- c("ESD", "COD", "SED", "IND", "SMD", "sex", "age") 

custom_orderYpm <- c("SMD", "IND", "SED", "COD", "ESD",
                     "qas","mindset","goals","selfEff","emoPos","emoNeg",
                     "sex", "age")
respm1$rhs <- factor(respm1$rhs, levels = custom_orderYpm)
custom_orderXpm <- c("qas","mindset","goals","selfEff","emoPos","emoNeg",
                     "media","swls","acceptance")
respm1$lhs <- factor(respm1$lhs, levels = custom_orderXpm)

library(patchwork) # To adjust plot sizes
# Split data and plots for plots with different number of predictors
# - Top plot
top_two_rows <- respm1 %>% dplyr::filter(lhs %in% c("SMD", "IND", "SED", "COD", "ESD",
                                                    "qas","mindset","goals","selfEff","emoPos","emoNeg",
                                                    "sex", "age"))
custom_orderYtop <- c("ESD", "COD", "SED", "IND", "SMD", "sex", "age") 
top_two_rows$rhs <- factor(top_two_rows$rhs, levels = custom_orderYtop)
y_labels3 <- c("Emotional resilience", "Cooperation", "Social engagement", 
               "Innovation", "Self-management", "Females", "Age")
# Create the two plots
plot_top <- ggplot(top_two_rows, aes(x = est.std, y = rhs)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = ci.lower, xmax = ci.upper), width = 0.4, linewidth = .7) +
  geom_vline(xintercept = c(-.10, .10), linetype = "dashed") +
  labs(x = element_blank(), y = "Predictor Variables") +
  facet_wrap(~lhs, labeller = labeller(lhs = srl_labels), ncol = 3) +
  scale_x_continuous(breaks = seq(-.45, .45, by = .10)) +
  coord_cartesian(xlim = c(-.50,.50)) +
  scale_y_discrete(labels = y_labels3) +
  theme_bw(base_size = 17) +
  theme(axis.text.x = element_blank())
# - Bottom plot
bottom_row <- respm1 %>% dplyr::filter(lhs %in% c("media", "swls", "acceptance"))
custom_orderYbottom <- c("ESD", "COD", "SED", "IND", "SMD",
                         "qas","mindset","goals","selfEff","emoPos","emoNeg",
                         "sex", "age")
bottom_row$rhs <- factor(bottom_row$rhs, levels = custom_orderYbottom)

res1.1$lhs <- factor(res1.1$lhs, levels = custom_orderX1)
plot_bottom <- ggplot(bottom_row, aes(x = est.std, y = rhs)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = ci.lower, xmax = ci.upper), width = 0.4, linewidth = .7) +
  geom_vline(xintercept = c(-.10, .10), linetype = "dashed") +
  labs(x = "Standardized Estimate", y = "Predictor Variables") +
  facet_wrap(~lhs, labeller = labeller(lhs = srl_labels), ncol = 3) +
  scale_x_continuous(breaks = seq(-.45, .45, by = .10)) +
  coord_cartesian(xlim = c(-.50,.50)) +
  scale_y_discrete(labels = y_labels2) +
  theme_bw(base_size = 17) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1, size = 14))
# - Combine plots with patchwork and adjust row heights
(FigureS2 <- plot_top / plot_bottom + plot_layout(heights = c(2.5, 2.5)))
ggsave(plot = FigureS2, filename = "FigureS2.tiff", width = 14, height = 8, device='tiff', dpi=300)

# --- Plotting indirect effects
respm1.1 <- respm[respm$op == ":=", c("lhs","rhs","label","est.std","ci.lower","ci.upper","se","z")]
# Calculate bootstrap CI for SEM
fct <- function(fit) {
  lavaan::standardizedSolution(fit)[153:242, "est.std"]
}
fit0 <- update(fitpm, se = "none")
fit_boot <- bootstrapLavaan(fit0, R = 2000, FUN = fct, iseed = 8970)
CI <- apply(fit_boot,2,quantile, c(.025,.975))
respm1.1$ci.lower <- CI[1,]
respm1.1$ci.upper <- CI[2,]
# Add variables for plot
respm1.1$skill = rep(c("SMD", "IND", "COD", "SED", "ESD"),each=3, times = 6)
respm1.1$mediator = rep(c("SRL strategies","Mindset","Learning goals","Self-efficacy",
                          "Negative emotions","Positive emotions"),each=15)
respm1.1$outcome = rep(c("Academic achievement","Life satisfaction","Peer acceptance"),30)
respm1.1$indirect = paste0(respm1.1$outcome, " ~ ", respm1.1$mediator)
respm1.1$indirect2 = paste0(respm1.1$skill, " * ", respm1.1$mediator)
respm1.1$wrap = paste0(respm1.1$skill, " -> ", respm1.1$outcome)
skill_labels <- c("ESD"="Emotional resilience","COD"= "Cooperation", "SED"="Social engagement", 
                  "IND"="Innovation","SMD"= "Self-management")
y_labels2 <- c("Emotional resilience", "Cooperation", "Social engagement", 
               "Innovation", "Self-management", "SRL strategies", "Mindset",
               "Learning goals", "Self-efficacy", "Positive emotions",
               "Negative emotions", "Females", "Age")

(FigureS3 <- ggplot(respm1.1, aes(x = est.std, y = mediator, colour = skill)) +
    geom_point(size = 3, aes(shape = skill)) +
    geom_errorbar(aes(xmin = ci.lower, xmax = ci.upper), width = 0.4, linewidth = .8) +
    geom_hline(yintercept = seq(6.5,24.5,by=6)) +
    geom_vline(xintercept = c(-.01, .01), linetype = "dashed") +
    labs(x = "Standardized Estimate", y = "Predictor Variables") +
    facet_wrap(~wrap,ncol = 3) +
    theme_bw(base_size = 14) +
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 45, vjust = 1.2, hjust=1, size = 14))
)
ggsave(plot = FigureS3, filename = "FigureS3.tiff", width = 10, height = 12, device='tiff', dpi=300)

# Figure only selected effects
selectedInd <- c("codnegacc",
                 "esdposswls","esdnegacc",
                 "indslfswls","indslfmedia",
                 "sednegacc",
                 "smdslfswls","smdposswls","smdslfmedia")
dplotindirect <- respm1.1[respm1.1$label %in% selectedInd,]

(FigureS1 <- ggplot(dplotindirect,aes(x = est.std, y = indirect2)) +
    geom_point(size = 3, aes(shape = outcome)) +
    geom_errorbar(aes(xmin = ci.lower, xmax = ci.upper), width = 0.4, linewidth = .8) +
    geom_vline(xintercept = c(-.01, .01), linetype = "dashed") +
    labs(x = "Standardized Mediation Estimate", y = "Mediation path")+
    theme_bw(base_size = 18) +
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 45, vjust = 1.2, hjust=1, size = 14))
)  
ggsave(plot = FigureS1, filename = "FigureS1.tiff", width = 10.5, height = 5, device='tiff', dpi=300)
