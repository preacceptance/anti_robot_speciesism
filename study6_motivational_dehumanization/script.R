## clear workspace
rm(list = ls()) 

## install packages
library(dplyr)
library(grid)
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('tidyverse',       
               'ggsignif',        # plotting significance bars
               'lme4',            # functions for fitting linear regression models
               'ggforce',         # make ggplot even fancier
               'ggpubr',          # arrange plots in a grid, if needed
               'ltm',           
               'tidyr',           # tools for cleaning messy data
               'stringr',         # perform string substitutions easily
               'assertthat',      # allows me to check whether a variable is a string, with is.string
               'emmeans',         # contrast analysis for regression models
               'stats',           # use function to adjust for multiple comparisons
               'filesstrings',    # create and move files
               'simr',            # power analysis for mixed models
               'compute.es',      # effect size package
               'effsize',         # another effect size package
               'pwr',             # package for power calculation
               'Hmisc',            # get p values for mixed effect model
               'DescTools',        # get Cramer's V
               'rstatix',
               'effects',
               'lavaan',
               'semTools',
               'sjstats'
)

# PROCESS Analysis (Set TRUE if you wish to run PROCESS code)
mediation <- F
if(mediation) {
  source("../process.R")
}

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data
# set working directory to current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read_csv('data.csv')
d <- d[-c(1,2),]

d |>
  mutate_if(all.is.numeric, as.numeric) -> d

## Attention Check
d |>
  filter(att_1 == 2 & att_2 == 5) -> d

nrow(d)

## Comp Check
d |>
  filter((comp == 2 & AI_type == "aging") |
         (comp == 1 & AI_type == "control") |
         (comp == 3 & AI_type == "mines") ) -> d

nrow(d)

d |>
  mutate(Condition = case_when(
    AI_type == "aging" ~ "Control-Death",
    AI_type == "control" ~ "Control",
    AI_type == "mines" ~ "Treatment"
  )) -> d

## ================================================================================================================
##                                                 DEMOGRAPHICS                 
## ================================================================================================================

prop.table(table(d[d$gender == 1 | d$gender == 2,]$gender))[[1]]
mean(as.numeric(d$age), na.rm = T)

## ================================================================================================================
##                                                 ANALYSIS                
## ================================================================================================================

cronbach.alpha(d[,c("autonomy_1", "harm_1", "die_1", "exploit_1")])
cronbach.alpha(d[,c("deteriorate_1", "being_harmed_1", "well_being_1")])

d$moral_concern <- rowMeans(d[,c("autonomy_1", "harm_1", "die_1", "exploit_1")])
d$awareness <- rowMeans(d[,c("deteriorate_1", "being_harmed_1", "well_being_1")])

d[, c("autonomy_1", "harm_1", "die_1", "exploit_1", "deteriorate_1", "being_harmed_1", "well_being_1")] |> cor()

# ANOVA
## Moral Concern
a <- aov(moral_concern ~  as.factor(Condition), data = d)
summary(a)
anova_stats(a); anova_stats(a)$partial.etasq

## Control vs. Treatment
t.test(d[d$Condition == "Control",]$moral_concern,
       d[d$Condition == "Treatment",]$moral_concern)

sd(d[d$Condition == "Control",]$moral_concern)
sd(d[d$Condition == "Treatment",]$moral_concern)

cohen.d(d[d$Condition == "Control",]$moral_concern, 
        d[d$Condition == "Treatment",]$moral_concern)

## Control-Death vs. Treatment
t.test(d[d$Condition == "Control-Death",]$moral_concern,
       d[d$Condition == "Treatment",]$moral_concern)

sd(d[d$Condition == "Control-Death",]$moral_concern)
sd(d[d$Condition == "Treatment",]$moral_concern)

cohen.d(d[d$Condition == "Control-Death",]$moral_concern, 
        d[d$Condition == "Treatment",]$moral_concern)

## Control-Death vs. Control
t.test(d[d$Condition == "Control-Death",]$moral_concern,
       d[d$Condition == "Control",]$moral_concern)

sd(d[d$Condition == "Control-Death",]$moral_concern)
sd(d[d$Condition == "Control",]$moral_concern)

cohen.d(d[d$Condition == "Control-Death",]$moral_concern, 
        d[d$Condition == "Control",]$moral_concern)

# ANOVA
## awareness
a <- aov(awareness ~  as.factor(Condition), data = d)
summary(a)
anova_stats(a); anova_stats(a)$partial.etasq

## Control vs. Treatment
t.test(d[d$Condition == "Control",]$awareness,
       d[d$Condition == "Treatment",]$awareness)

sd(d[d$Condition == "Control",]$awareness)
sd(d[d$Condition == "Treatment",]$awareness)

cohen.d(d[d$Condition == "Control",]$awareness, 
        d[d$Condition == "Treatment",]$awareness)

## Control-Death vs. Treatment
t.test(d[d$Condition == "Control-Death",]$awareness,
       d[d$Condition == "Treatment",]$awareness)

sd(d[d$Condition == "Control-Death",]$awareness)
sd(d[d$Condition == "Treatment",]$awareness)

cohen.d(d[d$Condition == "Control-Death",]$awareness, 
        d[d$Condition == "Treatment",]$awareness)

## Control-Death vs. Control
t.test(d[d$Condition == "Control-Death",]$awareness,
       d[d$Condition == "Control",]$awareness)

sd(d[d$Condition == "Control-Death",]$awareness)
sd(d[d$Condition == "Control",]$awareness)

cohen.d(d[d$Condition == "Control-Death",]$awareness, 
        d[d$Condition == "Control",]$awareness)

## ================================================================================================================
##                                              PROCESS             
## ================================================================================================================

d$cond <- as.numeric(relevel(as.factor(d$Condition), ref = "Treatment"))

if(mediation) {
  process(data = d, y = "moral_concern", x = "cond", 
          m = c("awareness"), model = 4, effsize = 1, total = 1, mcx = 1, stand = 1, 
          contrast = 1 , boot = 10000 , modelbt = 1, seed = 654321)
  
}

## ================================================================================================================
##                                              DATA VIZUALIZATION              
## ================================================================================================================

# Renaming and labeling for plots
d |>
  select(Condition, moral_concern, awareness) |>
  gather(key = "DV", value = "Value", 2:3) |>
  mutate( DV = ifelse( DV == "moral_concern", "Moral Concern", "Comprehension of Harm"), 
          Condition = case_when(
            Condition == "Treatment" ~ "Hazardous-Work",
            Condition == "Control-Death" ~ "Death-Control",
            Condition == "Control" ~ "Control"
          )) -> d_plot

# Obtain mean and standard errors for condition and measure
d_plot |>
  dplyr::group_by(`Condition`, DV) |>
  dplyr::summarize(
    avg_value = mean(Value),
    se_value = sd(Value)/sqrt(n())
  ) -> d_plot

se_width <- 1.96

# Plot Firm Liability
ggplot(data = d_plot, aes(fill=`Condition`, y=avg_value, x = DV)) +
  geom_bar(stat="identity", position="dodge", alpha=.75, width=.6) +
  geom_point(position=position_dodge(width = .6), size=.5, color="black") +
  geom_errorbar(aes(ymin=avg_value-(se_value*se_width), ymax=avg_value+(se_value*se_width)), position = position_dodge(width=.6), 
                size=.25, color="black", width=.25) +
  geom_signif(
    y_position = c(70, 70, 80, 70, 70, 80), xmin = c(0.8, 1.05, .85, 1.8, 2.05, 1.85), 
    xmax = c(.95, 1.2, 1.15, 1.95, 2.2, 2.15),
    annotation = c("ns", "**", "*", "ns","***", "***"), tip_length = 0.1, color='black', size = .25, textsize = 3.5 
  ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=12), 
        axis.title=element_text(size=10,face="bold"), legend.position = "top", legend.title = element_blank()) +
  ylab("Mean Ratings") +
  xlab("") +
  ggtitle("") +
  scale_fill_grey() +
  scale_color_grey() +
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100))-> p1

p1

ggsave("effects.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")
