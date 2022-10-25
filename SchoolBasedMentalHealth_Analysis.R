########################################################################################################
# School-based Mental Health Interventions
########################################################################################################
# Authors: Qiyang Zhang, Jun Wang & Amanda Neitzel
# Contact: qzhang74@jhu.edu
# Created: 2021/01/05

# This file analyzes the included studies in the School-based Mental Health Interventions systematic 
# review, including preparing the data for analysis and meta-regressions.

########################################################################################################
# Initial Set-up
########################################################################################################
# Clear workspace
rm(list=ls(all=TRUE))

# Load packages
test<-require(googledrive)   #all gs_XXX() functions for reading data from Google
if (test == FALSE) {
  install.packages("googledrive")
  require(googledrive)
}
test<-require(googlesheets4)   #all gs_XXX() functions for reading data from Google
if (test == FALSE) {
  install.packages("googlesheets4")
  require(googlesheets4)
}
test<-require(plyr)   #rename()
if (test == FALSE) {
  install.packages("plyr")
  require(plyr)
}
test<-require(metafor)   #escalc(); rma();
if (test == FALSE) {
  install.packages("metafor")
  require(metafor)
}
test<-require(robumeta)
if (test == FALSE) {
  install.packages("robumeta")
  require(robumeta)
}
test<-require(weightr) #selection modeling
if (test == FALSE) {
  install.packages("weightr")
  require(weightr)
}
test<-require(clubSandwich) #coeftest
if (test == FALSE) {
  install.packages("clubSandwich")
  require(clubSandwich)
}
test<-require(tableone)   #CreateTableOne()
if (test == FALSE) {
  install.packages("tableone")
  require(tableone)
}
test<-require(flextable)   
if (test == FALSE) {
  install.packages("flextable")
  require(flextable)
}
test<-require(officer)   
if (test == FALSE) {
  install.packages("officer")
  require(officer)
}
test<-require(tidyverse)   
if (test == FALSE) {
  install.packages("tidyverse")
  require(tidyverse)
}
test<-require(ggrepel)   
if (test == FALSE) {
  install.packages("ggrepel")
  require(ggrepel)
}
rm(test)

########################################################################################################
# Load data
########################################################################################################
# set up to load from Google
drive_auth(email = "zhangqiyang0329@gmail.com")
id <- drive_find(pattern = "School-based Mental Health Interventions", type = "spreadsheet")$id[1]

# load findings and studies
gs4_auth(email = "zhangqiyang0329@gmail.com")
# gs4_auth(email = "aneitzel@gmail.com")
findings <- read_sheet(id, sheet = "Findings", col_types = "c")
studies <- read_sheet(id, sheet = "Studies", col_types = "c")   # includes separate effect sizes for each finding from a study

rm(id)

########################################################################################################
# Clean data
########################################################################################################
# remove any empty rows & columns
studies <- subset(studies, is.na(studies$Study)==FALSE)
studies <- subset(studies, studies$Drop==0)
findings <- subset(findings, is.na(findings$Study)==FALSE)
findings <- subset(findings, findings$Drop==0)

# remove irrelevant columns
studies <- studies[c("Study", "Program name", "Age", "Grades", "Country", 
                     "Type", "N (T, C)", "Duration (weeks)", "Design", 
                     "Randomized", "Clustered", "Control", "Placebo", "Features","CBT", 
                     "Targeted Outcomes", "Program delivery", "Teacher","Notes", "WaitlistorBAU")]
findings <- findings[c("Drop", "Study", "Program name", "GradeLevel", 
                       "Universal", "Treatment.N", "Control.N", "Total.N",
                       "Treatment.Cluster", "Control.Cluster", "Unit", "Group", "Construct (D, A)", 
                       "Measure", "T_Mean_Pre", "T_SD_Pre", "C_Mean_Pre", 
                       "C_SD_Pre", "T_Mean_Post", "T_SD_Post", "C_Mean_Post", "C_SD_Post", 
                       "ReverseCoded (lower scores are better)", "Effect.Size", "Notes")]

# merge dataframes
full <- merge(studies, findings, by = c("Study", "Program name"), all = TRUE, suffixes = c(".s", ".f"))

# drop studies/findings determined to remove for now
#full <- subset(full, full$Drop==0)

# rename some cols to shorten
full <- plyr::rename(full, c("Construct (D, A)" = "Construct", "ReverseCoded (lower scores are better)" = "ReverseCoded", "Duration (weeks)" = "Duration.weeks"))

# recode outcomes (to eliminate internalizing)
full$Construct[which(full$Construct=="Internalizing Problems")]<-"D+A"

# remove ? from reverse-coding (go with assumed value for now while coding)
full$ReverseCoded <- gsub("\\?", "", full$ReverseCoded)

# format to correct variable types
nums <- c("Treatment.N", "Control.N", "Total.N", 
          "Treatment.Cluster", "Control.Cluster", 
          "T_Mean_Pre", "T_SD_Pre", "C_Mean_Pre", 
          "C_SD_Pre", "T_Mean_Post", "T_SD_Post", 
          "C_Mean_Post", "C_SD_Post", "ReverseCoded", 
          "Duration.weeks", "Teacher", "CBT", "Universal", 
          "Randomized", "Clustered", "WaitlistorBAU")
full[nums] <- lapply(full[nums], as.numeric)
rm(nums)

###############################################################
#Create unique identifiers (ES, study, program)
###############################################################
full$ESId <- as.numeric(rownames(full))
full$StudyID <- as.numeric(as.factor(full$Study))

########################################################################################################
# Prep data
########################################################################################################
##### Calculate ESs #####
# calculate pretest ES, SMD is standardized mean difference
full <- escalc(measure = "SMD", m1i = T_Mean_Pre, sd1i = T_SD_Pre, n1i = Treatment.N,
               m2i = C_Mean_Pre, sd2i = C_SD_Pre, n2i = Control.N, data = full)
full$vi <- NULL
full <- plyr::rename(full, c("yi" = "ES_Pre"))

# calculate posttest ES
full <- escalc(measure = "SMD", m1i = T_Mean_Post, sd1i = T_SD_Post, n1i = Treatment.N,
               m2i = C_Mean_Post, sd2i = C_SD_Post, n2i = Control.N, data = full)
full$vi <- NULL
full <- plyr::rename(full, c("yi" = "ES_Post"))

# calculate DID (post ES - pre ES)
full$ES_DID <- full$ES_Post - full$ES_Pre

# put various ES together.  Options:
## 1) used reported ES (so it should be in the Effect.Size column, nothing to do)
## 2) Effect.Size is NA, and DID not missing, replace with that
full$Effect.Size[which(is.na(full$Effect.Size)==TRUE & is.na(full$ES_DID)==FALSE)] <- full$ES_DID[which(is.na(full$Effect.Size)==TRUE & is.na(full$ES_DID)==FALSE)]
## 3) Effect.Size and DID is missing, used adjusted means (from posttest ES col), replace with that
full$Effect.Size[which(is.na(full$Effect.Size)==TRUE & is.na(full$ES_DID)==TRUE)] <- full$ES_Post[which(is.na(full$Effect.Size)==TRUE & is.na(full$ES_DID)==TRUE)]
full$Effect.Size <- as.numeric(full$Effect.Size)

###############################################################
#Calculate meta-analytic variables: Sample sizes
###############################################################
#create full sample/total clusters variables
full$Sample <- full$Treatment.N + full$Control.N
full$Cluster_Total <- full$Treatment.Cluster+full$Control.Cluster

###################################
#Create dummies
###################################
full$Smallsample <- 0
full$Smallsample[which(full$Sample<=250)] <- 1

full$Longduration <- 0
full$Longduration[which(full$Duration.weeks>=12)] <- 1

full$Longduration2 <- 0
full$Longduration2[which(full$Duration.weeks>12)] <- 1

full$Depression <- 0
full$Depression[which(full$Construct=="D")] <- 1

full$Anxiety <- 0
full$Anxiety[which(full$Construct=="A")] <- 1

full$DepressionAndAnxiety <- 0
full$DepressionAndAnxiety[which(full$Construct=="D+A"|full$Construct=="Internalizing Problems")]<-1

full$Elementary <- 0
full$Elementary[which(full$GradeLevel==1)]<-1

#####################
#Centering
#####################
full$CBT.c <- full$CBT - mean(full$CBT)
full$Smallsample.c <- full$Smallsample - mean(full$Smallsample)
full$Longduration.c <- full$Longduration - mean(full$Longduration)
full$Longduration2.c <- full$Longduration2 - mean(full$Longduration2)
full$Teacher.c <- full$Teacher - mean(full$Teacher)
full$Universal.c <- full$Universal - mean(full$Universal)
full$Depression.c <-full$Depression - mean(full$Depression)
full$Anxiety.c <- full$Anxiety - mean(full$Anxiety)
full$DepressionAndAnxiety.c <- full$DepressionAndAnxiety - mean(full$DepressionAndAnxiety)
full$Elementary.c <- full$Elementary - mean(full$Elementary)
full$WaitlistorBAU.c <- full$WaitlistorBAU - mean(full$WaitlistorBAU)

###############################################################
#Calculate meta-analytic variables: Correct ES for clustering (Hedges, 2007, Eq.19)
###############################################################
#first, create an assumed ICC
full$icc <- NA
full$icc[which(full$Clustered == 1)] <- 0.2

#find average students/cluster
full$Treatment.Cluster.n <- NA
full$Treatment.Cluster.n[which(full$Clustered == 1)] <- round(full$Treatment.N[which(full$Clustered == 1)]/full$Treatment.Cluster[which(full$Clustered == 1)], 0)
full$Control.Cluster.n <- NA
full$Control.Cluster.n[which(full$Clustered == 1)] <- round(full$Control.N[which(full$Clustered == 1)]/full$Control.Cluster[which(full$Clustered == 1)], 0)
# find other parts of equation
full$n.TU <- NA
full$n.TU <- ((full$Treatment.N * full$Treatment.N) - (full$Treatment.Cluster * full$Treatment.Cluster.n * full$Treatment.Cluster.n))/(full$Treatment.N * (full$Treatment.Cluster - 1))
full$n.CU <- NA
full$n.CU <- ((full$Control.N * full$Control.N) - (full$Control.Cluster * full$Control.Cluster.n * full$Control.Cluster.n))/(full$Control.N * (full$Control.Cluster - 1))

# next, calculate adjusted ES, save the originals, then replace only the clustered ones
full$Effect.Size.adj <- full$Effect.Size * (sqrt(1-full$icc*(((full$Total.N-full$n.TU*full$Treatment.Cluster - full$n.CU*full$Control.Cluster)+full$n.TU + full$n.CU - 2)/(full$Total.N-2))))

# save originals, replace for clustered
full$Effect.Size.orig <- full$Effect.Size
full$Effect.Size[which(full$Clustered==1)] <- full$Effect.Size.adj[which(full$Clustered==1)]

num <- c("Effect.Size")
full[num] <- lapply(full[num], as.numeric)
rm(num)
################################################################
# Calculate meta-analytic variables: Variances (Lipsey & Wilson, 2000, Eq. 3.23)
################################################################
#calculate standard errors
full$se<-sqrt(((full$Treatment.N+full$Control.N)/(full$Treatment.N*full$Control.N))+((full$Effect.Size*full$Effect.Size)/(2*(full$Treatment.N+full$Control.N))))

#calculate variance
full$var<-full$se*full$se

################################################################
# Calculate meta-analytic variables: Correct variance for clustering (Hedges, 2007, Eq. 20)
################################################################
# first, create an assumed ICC
full$icc <- NA
full$icc[which(full$Clustered == 1)] <- 0.2   # could look this up and specify

# find average students/cluster
full$Treatment.Cluster.n <- NA
full$Treatment.Cluster.n[which(full$Clustered == 1)] <- round(full$Treatment.N[which(full$Clustered == 1)]/full$Treatment.Cluster[which(full$Clustered == 1)], 0)
full$Control.Cluster.n <- NA
full$Control.Cluster.n[which(full$Clustered == 1)] <- round(full$Control.N[which(full$Clustered == 1)]/full$Control.Cluster[which(full$Clustered == 1)], 0)

# calculate the different parts of the formula (see Hedges 2007, equation)
full$ntilde <- NA
full$ntilde <- ((full$Control.N * full$Treatment.Cluster * full$Treatment.Cluster.n * full$Treatment.Cluster.n)/(full$Treatment.N * full$Sample)) +
  ((full$Treatment.N * full$Control.Cluster * full$Control.Cluster.n * full$Control.Cluster.n)/(full$Control.N * full$Sample))

full$n.TU <- NA
full$n.TU <- ((full$Treatment.N * full$Treatment.N) - (full$Treatment.Cluster * full$Treatment.Cluster.n * full$Treatment.Cluster.n))/(full$Treatment.N * (full$Treatment.Cluster - 1))
full$n.CU <- NA
full$n.CU <- ((full$Control.N * full$Control.N) - (full$Control.Cluster * full$Control.Cluster.n * full$Control.Cluster.n))/(full$Control.N * (full$Control.Cluster - 1))

full$AT <- NA
full$AT <-((full$Treatment.N^2 * full$Treatment.Cluster * full$Treatment.Cluster.n^2) + ((full$Treatment.Cluster * full$Treatment.Cluster.n^2)^2) - (2 * full$Treatment.N * full$Treatment.Cluster.n^3 * full$Treatment.Cluster))/(full$Treatment.N^2)

full$AC <- NA
full$AC <-((full$Control.N^2 * full$Control.Cluster * full$Control.Cluster.n^2) + ((full$Control.Cluster * full$Control.Cluster.n^2)^2) - (2 * full$Control.N * full$Control.Cluster.n^3 * full$Control.Cluster))/(full$Control.N^2)

full$A <- full$AT + full$AC

full$B <- NA
full$B <- full$n.TU * (full$Treatment.Cluster - 1) + full$n.CU * (full$Control.Cluster -1)

full$var.adj <- NA
full$var.adj <- ((full$Treatment.N + full$Control.N)/(full$Treatment.N * full$Control.N)) * (1 + (full$ntilde - 1) * full$icc) + ((((full$Sample - 2)*(1-full$icc)^2) + full$A * full$icc^2 + 2*full$icc*(1-full$icc))*full$Effect.Size^2)/(2*(full$Sample-2)*((full$Sample - 2)-full$icc*(full$Sample-2-full$B)))
full$var.old <- full$var
full$var[which(full$Clustered == 1)] <- full$var.adj[which(full$Clustered == 1)]

# swaps signs for reverse-coded outcomes (so positive is good and negative is bad)
full$Effect.Size[which(is.na(full$Effect.Size))] <- full$Effect.Size.orig #This is for Lewis study
full$Effect.Size[which(full$ReverseCoded==1)] <- full$Effect.Size[which(full$ReverseCoded==1)]*-1

###########################
#forest plot
###########################
#robumeta
# data_anxiety <- full[which(full$Construct == "A"),]
# data_depression <- full[which(full$Construct == "D"),]
# 
# MVnull2_anxiety <- robu(formula = Effect.Size ~ 1, studynum = StudyID, data = data_anxiety, var.eff.size = var)
# MVfull2_anxiety <- robu(formula = Effect.Size ~ Longduration.c + Teacher.c + CBT.c + Elementary.c, studynum = StudyID, data = data_anxiety, var.eff.size = var)
# 
# forest.robu(x = MVnull2_anxiety, es.lab = "Measure", study.lab = "Study")
# 
# MVnull2_depression <- robu(formula = Effect.Size ~ 1, studynum = StudyID, data = data_depression, var.eff.size = var)
# MVfull2_depression <- robu(formula = Effect.Size ~ Longduration.c + Teacher.c + CBT.c + Elementary.c, studynum = StudyID, data = data_depression, var.eff.size = var)
# 
# forest.robu(x = MVnull2_depression, es.lab = "Measure", study.lab = "Study")

#
# REnull <- rma(yi=Effect.Size, vi=var, data=full, method="REML", control=list(stepadj=0.5))
# summary(REnull)
# 
# REnull_anxiety <- rma(yi=Effect.Size, vi=var, data=data_anxiety, method="REML", control=list(stepadj=0.5))
# REnull_depression <- rma(yi=Effect.Size, vi=var, data=data_depression, method="REML", control=list(stepadj=0.5))

########################################
#meta-regression
########################################
#Null Model
V_list <- impute_covariance_matrix(vi=full$var, cluster=full$StudyID, r=0.8)

MVnull <- rma.mv(yi=Effect.Size,
                  V=V_list,
                  random=~1 | StudyID/ESId,
                  test="t",
                  data=full,
                  method="REML")
MVnull

# #t-test of each covariate#
MVnull.coef <- coef_test(MVnull, cluster=full$StudyID, vcov="CR2")
MVnull.coef

#meta-regression moderators using a fully centered model
terms <- c("Longduration.c","Smallsample.c","Universal.c",
            "Depression.c","Anxiety.c","DepressionAndAnxiety.c",
            "Teacher.c","CBT.c", "Elementary.c")

# #formate into formula
interact <- c("Teacher.c*CBT.c","Universal.c*Teacher.c","Teacher.c*Elementary.c","Teacher.c*Depression.c*Anxiety.c", "CBT.c*Depression.c*Anxiety.c")

formula <- reformulate(termlabels = c(terms, interact))
formula

MVfull <- rma.mv(yi=Effect.Size,
                 V=V_list,
                 mods=formula,
                 random=~1 | StudyID/ESId,
                 test="t",
                 data=full,
                 method="REML")
MVfull

#t-test of each covariate#
MVfull.coef <- coef_test(MVfull, cluster=full$StudyID, vcov="CR2")
MVfull.coef

#sensitivity analyses
full$Short_duration <- 0
full$Short_duration[which(full$Duration.weeks<=10)] <- 1

full$Medium_duration <- 0
full$Medium_duration[which(full$Duration.weeks>10 & full$Duration.weeks<=11)] <- 1

full$Small_sample <- 0
full$Small_sample[which(full$Sample<=177)] <- 1

full$Medium_sample <- 0
full$Medium_sample[which(full$Sample>177 & full$Sample<=428)] <- 1

full$WaitlistorBAU.c <- full$WaitlistorBAU - mean(full$WaitlistorBAU)
full$Short_duration.c <- full$Short_duration - mean(full$Short_duration)
full$Medium_duration.c <- full$Medium_duration - mean(full$Medium_duration)
full$Small_sample.c <- full$Small_sample - mean(full$Small_sample)
full$Medium_sample.c <- full$Medium_sample - mean(full$Medium_sample)

quantile(full$Sample, probs = c(.33,.66))
quantile(full$Duration.weeks, probs = c(.33,.66))

#meta-regression sensitivity analysis 1: replace duration and sample as continuous variables
terms_sensitivity_1 <- c("Universal.c",
                       "Depression.c","Anxiety.c","DepressionAndAnxiety.c",
                       "Teacher.c","CBT.c", "Elementary.c", "WaitlistorBAU.c",
                       "Duration.weeks", "Sample")

formula_s_1 <- reformulate(termlabels = c(terms_sensitivity_1, interact))
formula_s_1
#
MVfull_s_1 <- rma.mv(yi=Effect.Size,
                 V=V_list,
                 mods=formula_s_1,
                 random=~1 | StudyID/ESId,
                 test="t",
                 data=full,
                 method="REML")
MVfull_s_1

#t-test of each covariate#
MVfull.coef_s_1 <- coef_test(MVfull_s_1, cluster=full$StudyID, vcov="CR2")
MVfull.coef_s_1

#sensitivity analysis 2: replace duration and sample as categorical variables with three levels:
terms_sensitivity_2 <- c("Universal.c",
                         "Depression.c","Anxiety.c","DepressionAndAnxiety.c",
                         "Teacher.c","CBT.c", "Elementary.c", "WaitlistorBAU.c",
                         "Short_duration.c", "Medium_duration.c", "Small_sample.c", "Medium_sample.c")

formula_s_2 <- reformulate(termlabels = c(terms_sensitivity_2, interact))
formula_s_2
#
MVfull_s_2 <- rma.mv(yi=Effect.Size,
                   V=V_list,
                   mods=formula_s_2,
                   random=~1 | StudyID/ESId,
                   test="t",
                   data=full,
                   method="REML")
MVfull_s_2

#t-test of each covariate#
MVfull.coef_s_2 <- coef_test(MVfull_s_2, cluster=full$StudyID, vcov="CR2")
MVfull.coef_s_2

#################################################################################
# Marginal Means
#################################################################################
# re-run model for each moderator to get marginal means for each #
# set up table to store results
means <- data.frame(moderator = character(0), group = character(0), beta = numeric(0), SE = numeric(0), 
                    tstat = numeric(0), df = numeric(0), p_Satt = numeric(0))

# make factor moderators for means
full$Teacher.f <- as.factor(ifelse(full$Teacher==1, "Teacher", "Not Teacher"))
full$CBT.f <- as.factor(ifelse(full$CBT==1, "CBT", "Not CBT"))
full$Universal.f <- as.factor(ifelse(full$Universal==1, "Universal", "Targeted"))
full$Elementary.f <- as.factor(ifelse(full$Elementary==1, "Elementary", "Secondary"))
full$Depression.f <- as.factor(ifelse(full$Depression==1, "Depression", "Anxiety"))

full$TeacherXCBT <- as.factor(paste(full$Teacher.f, full$CBT.f, sep = " "))
full$UniversalXTeacher <- as.factor(paste(full$Universal.f, full$Teacher.f, sep = " "))
full$TeacherXElem <- as.factor(paste(full$Teacher.f, full$Elementary.f, sep = " "))
full$DepressionXTeacher <- as.factor(paste(full$Depression.f, full$Teacher.f, sep = " "))
full$DepressionXCBT <- as.factor(paste(full$Depression.f, full$CBT.f, sep = " "))

mods <- c("as.factor(Longduration)", "as.factor(Smallsample)","Teacher.f", "CBT.f", 
          "Universal.f", "Elementary.f", "Depression.f", "TeacherXCBT", 
          "UniversalXTeacher", "TeacherXElem", "DepressionXTeacher", "DepressionXCBT")

for(i in 1:length(mods)){
  # i <- 8
  formula <- reformulate(termlabels = c(mods[i], terms, interact, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=Effect.Size, #effect size
                      V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                      mods = formula, #ADD COVS HERE
                      random = ~1 | StudyID/ESId, #nesting structure
                      test= "t", #use t-tests
                      data=full, #define data
                      method="REML") #estimate variances using REML
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                            cluster=full$StudyID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- gsub(x = mods[i], pattern = "as.factor", replacement = "")
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  means <- dplyr::bind_rows(means, coef_mod_means)
}
means
#################################################################################
# Heterogeneity
#################################################################################
# 95% prediction intervals
print(PI_upper <- MVfull2$b[1] + (1.96*sqrt(MVfull2$sigma2[1] + MVfull2$sigma2[2])))
print(PI_lower <- MVfull2$b[1] - (1.96*sqrt(MVfull2$sigma2[1] + MVfull2$sigma2[2])))

#################################################################################
#Create Descriptives Table
#################################################################################
# identify variables for descriptive tables (study-level and outcome-level)
vars_study <- c("Country", "Universal","Elementary", "Longduration", 
                "Design", "CBT", "Teacher")
vars_outcome <- c("Construct")

# To make this work, you will need a df that is at the study-level for study-level 
# variables (such as research design) you may have already created this (see above, with study-level ESs), but if you didn't, here is an easy way:
# 1) make df with *only* the study-level variables of interest and studyIDs in it
study_level_full <- full[c("StudyID", "Country", "Universal", "Elementary","Longduration", 
                           "Design", "CBT", "Teacher")]
# 2) remove duplicated rows
study_level_full <- unique(study_level_full)
# 3) make sure it is the correct number of rows (should be same number of studies you have)
length(study_level_full$StudyID)==length(unique(study_level_full$StudyID))
# don't skip step 3 - depending on your data structure, some moderators can be
# study-level in one review, but outcome-level in another

# create the table "chunks"
table_study_df <- as.data.frame(print(CreateTableOne(vars = vars_study, data = study_level_full, 
                                                     includeNA = TRUE, 
                                                     factorVars = c("Universal", "Longduration",
                                                                    "CBT", "Teacher","Elementary")), 
                                      showAllLevels = TRUE))
table_outcome_df <- as.data.frame(print(CreateTableOne(vars = vars_outcome, data = full, includeNA = TRUE), 
                                        showAllLevels = TRUE))
rm(vars_study, vars_outcome)

################################
# Descriptives Table Formatting
################################
table_study_df$Category <- row.names(table_study_df)
rownames(table_study_df) <- c()
table_study_df <- table_study_df[c("Category", "level", "Overall")]
table_study_df$Category[which(substr(table_study_df$Category, 1, 1)=="X")] <- NA
table_study_df$Category <- gsub(pattern = "\\..mean..SD..", replacement = "", x = table_study_df$Category)
table_study_df$Overall <- gsub(pattern = "\\( ", replacement = "\\(", x = table_study_df$Overall)
table_study_df$Overall <- gsub(pattern = "\\) ", replacement = "\\)", x = table_study_df$Overall)
table_study_df$Category <- gsub(pattern = "\\.", replacement = "", x = table_study_df$Category)
table_study_df$Category[which(table_study_df$Category=="n")] <- "Total Studies"
# table_study_df$Category[which(table_study_df$Category=="Country")] <- "Country"
# table_study_df$Category[which(table_study_df$Category=="Universal")] <- "Universal"
# table_study_df$Category[which(table_study_df$Category=="Longduration")] <- "Longduration"
# table_study_df$Category[which(table_study_df$Category=="CBT")] <- "CBT"
# table_study_df$Category[which(table_study_df$Category=="Teacher")] <- "Teacher"
# table_study_df$Category[which(table_study_df$Category=="Construct")] <- "Construct"
# table_study_df$Category[which(table_study_df$Category=="Smallsample")] <- "Smallsample"
table_study_df$level[which(table_study_df$level=="1")] <- "Yes"
table_study_df$level[which(table_study_df$level=="0")] <- "No                                                                              "
# fill in blank columns (to improve merged cells later)
for(i in 1:length(table_study_df$Category)) {
  if(is.na(table_study_df$Category[i])) {
    table_study_df$Category[i] <- table_study_df$Category[i-1]
  }
}

table_outcome_df$Category <- row.names(table_outcome_df)
rownames(table_outcome_df) <- c()
table_outcome_df <- table_outcome_df[c("Category", "level", "Overall")]
table_outcome_df$Category[which(substr(table_outcome_df$Category, 1, 1)=="X")] <- NA
table_outcome_df$Overall <- gsub(pattern = "\\( ", replacement = "\\(", x = table_outcome_df$Overall)
table_outcome_df$Overall <- gsub(pattern = "\\) ", replacement = "\\)", x = table_outcome_df$Overall)
table_outcome_df$Category <- gsub(pattern = "\\.", replacement = "", x = table_outcome_df$Category)
table_outcome_df$Category[which(table_outcome_df$Category=="n")] <- "Total Effect Sizes"
# fill in blank columns (to improve merged cells later)
for(i in 1:length(table_outcome_df$Category)) {
  if(is.na(table_outcome_df$Category[i])) {
    table_outcome_df$Category[i] <- table_outcome_df$Category[i-1]
  }
}

########################
#Output officer
########################
myreport<-read_docx()
# Descriptive Table
myreport <- body_add_par(x = myreport, value = "Table 4: Descriptive Statistics", style = "Normal")
descriptives_study <- flextable(head(table_study_df, n=nrow(table_study_df)))
descriptives_study <- add_header_lines(descriptives_study, values = c("Study Level"), top = FALSE)
descriptives_study <- theme_vanilla(descriptives_study)
descriptives_study <- merge_v(descriptives_study, j = c("Category"))
myreport <- body_add_flextable(x = myreport, descriptives_study)

descriptives_outcome <- flextable(head(table_outcome_df, n=nrow(table_outcome_df)))
descriptives_outcome <- delete_part(descriptives_outcome, part = "header")
descriptives_outcome <- add_header_lines(descriptives_outcome, values = c("Outcome Level"))
descriptives_outcome <- theme_vanilla(descriptives_outcome)
descriptives_outcome <- merge_v(descriptives_outcome, j = c("Category"))
myreport <- body_add_flextable(x = myreport, descriptives_outcome)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

########################
# MetaRegression Table
########################
MVnull.coef
str(MVnull.coef)
MVnull.coef$coef <- row.names(as.data.frame(MVnull.coef))
row.names(MVnull.coef) <- c()
MVnull.coef <- MVnull.coef[c("coef", "beta", "SE", "tstat", "df", "p_Satt")]
MVnull.coef
str(MVnull.coef)

MVfull.coef$coef <- row.names(as.data.frame(MVfull.coef))
row.names(MVfull.coef) <- c()
MVfull.coef <- MVfull.coef[c("coef", "beta", "SE", "tstat", "df", "p_Satt")]

# MetaRegression Table
model_null <- flextable(head(MVnull.coef, n=nrow(MVnull.coef)))
colkeys <- c("beta", "SE", "tstat", "df")
model_null <- colformat_double(model_null,  j = colkeys, digits = 2)
model_null <- colformat_double(model_null,  j = c("p_Satt"), digits = 3)
#model_null <- autofit(model_null)
model_null <- add_header_lines(model_null, values = c("Null Model"), top = FALSE)
model_null <- theme_vanilla(model_null)

myreport <- body_add_par(x = myreport, value = "Table 5: Model Results", style = "Normal")
myreport <- body_add_flextable(x = myreport, model_null)
#myreport <- body_add_par(x = myreport, value = "", style = "Normal")

model_full <- flextable(head(MVfull.coef, n=nrow(MVfull.coef)))
model_full <- colformat_double(model_full,  j = c("beta"), digits = 2)
model_full <- colformat_double(model_full,  j = c("p_Satt"), digits = 3)
#model_full <- autofit(model_full)
model_full <- delete_part(model_full, part = "header")
model_full <- add_header_lines(model_full, values = c("Meta-Regression"))
model_full <- theme_vanilla(model_full)

myreport <- body_add_flextable(x = myreport, model_full)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Marginal Means Table
marginalmeans <- flextable(head(means, n=nrow(means)))
colkeys <- c("moderator", "group", "SE", "tstat", "df")
marginalmeans <- colformat_double(marginalmeans,  j = colkeys, digits = 2)
marginalmeans <- colformat_double(marginalmeans,  j = c("p_Satt"), digits = 3)
rm(colkeys)
marginalmeans <- theme_vanilla(marginalmeans)
marginalmeans <- merge_v(marginalmeans, j = c("moderator"))
myreport <- body_add_par(x = myreport, value = "Table: Marginal Means", style = "Normal")
myreport <- body_add_flextable(x = myreport, marginalmeans)

# Write to word doc
file = paste("TableResults.docx", sep = "")
print(myreport, file)

#FIND TOTAL N
sum(with(full, Sample[!duplicated(StudyID)]))
sum(with(full, Construct=="D"))
sum(with(full,GradeLevel[!duplicated(StudyID)]==1))
sum(with(full,GradeLevel[!duplicated(StudyID)]==2))
sum(with(full,GradeLevel[!duplicated(StudyID)]==3))
sum(with(full,Sample[GradeLevel[!duplicated(StudyID)]==1]))
sum(with(full,Sample[GradeLevel[!duplicated(StudyID)]==2]))
sum(with(full,Sample[GradeLevel[!duplicated(StudyID)]==3]))


#publication bias , selection modeling
full_y <- full$Effect.Size
full_v <- full$var
weightfunct(full_y, full_v)
weightfunct(full_y, full_v, steps = c(.025, .50, 1))

# source files for creating each visualization type
source("/Users/apple/Desktop/SchoolbasedMentalHealth/Scripts/viz_MARC.R")

median(full$Total.N)
# set sample sizes based on median sample size (331) and existing % weights for bar plot
#            w_j = meta-analytic weight for study j (before rescaling)
#            w_j_perc = percent weight allocated to study j

full <- full %>% 
  mutate(w_j = 1/(se^2)) %>%
  mutate(w_j_perc = w_j/sum(w_j)) %>%
  mutate(N_j = floor(w_j_perc*331))

  #needed b/c tibbles throw error in viz_forest
full <- full %>% 
  ungroup()
full <- as.data.frame(full)

# Figure 4 - Meta-Analytic Rain Cloud (MARC) Plot
#study_labels = c("Study A", "Study B", "Study C", "Study D", "Study E"), 
viz_MARC(d_j = full$Effect.Size, se_j = full$se,
         x_breaks = seq(-1, 1, 0.02), x_limit = c(-1, 1),
         multiplier = 0.06)
ggsave("./images/Fig4_MARC_4.jpeg", width = 7, height = 5, units = "in", dpi = 600)


