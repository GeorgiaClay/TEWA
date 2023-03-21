# tewa 2022 christina gaugg und lilli hinterleithner
setwd("~/Desktop/tewa/tewa_analysis/01data")

#install packages ----
# install.packages("easystats")
# install.packages("dplyr")
# install.packages("glmmTMB")
# install.packages("ggplot2")

#read packages ----
library(car)
library(readxl)
library(dplyr)
library(easystats)
library(readxl)
library(tidyr)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(lme4)
library(sjPlot)
library(dplyr)
library(glmmTMB)

easystats::easystats_update()
ggplot2::ggplot2_update()

#DATEI importieren (und umbenennen)
df<-read_xlsx("student_data_wide Kopie.xlsx")

#DATEINKONTROLLE----
  # Anzahl der Zeilen und Spalten
  dim(df)
  
  #Datentypen, Datenstrukturen etc.
  str(df)
  
  # Variablennamen
  names(df)
  
  # Erste sechs Fälle
  head(df)
  
  # Letzte sechs Fälle
  tail(df)

#Daten vorbereiten----
# eventuell exkludieren incompleter fällen
#RECODE----
#recode fat1 und fat3 
df<-df%>%
  dplyr::mutate_at(c("fat1b","fat1d","fat3b","fat3d"),~dplyr::recode(.,"1"=7,"2"=6,"3"=5,"4"=4,"5"=3,"6"=2,"7"=1))

#recode fat2_1 und fat2_2
df<-df%>%
  dplyr::mutate_at(c("fat2_1j","fat2_1m","fat2_2j","fat2_2m"),~dplyr::recode(.,"0"=10,"1"=9,"2"=8,"3"=7,"4"=6,"5"=5,"6"=4, "7"=3,"8"=2,"9"=1, "10"=0))

#recode tsc
df<-df%>%
  dplyr::mutate_at(c("tsc2","tsc3","tsc4","tsc5","tsc6","tsc7","tsc8","tsc10","tsc11"),~dplyr::recode(.,"1"=6,"2"=5,"3"=4,"4"=3,"5"=2,"6"=1))

#recode wp
#recode in wörter schon in exel
df<-df%>%
  dplyr::mutate_at(c("WP1","WP2","WP5","WP7","WP8","WP10"),~dplyr::recode(.,"1"=6,"2"=5,"3"=4,"4"=3,"5"=2,"6"=1))

#MEAN----
#mean fat1
df<-df%>%
  mutate(FAT1=rowMeans(select(.,starts_with("fat1"))))

psych::describe(df$FAT1)
df%>%
  select(FAT1,groupid)%>%
  group_by(groupid)%>%
  report::report()

boxplot(df$FAT1~df$groupid)
boxplot(df$FAT1~df$sex)

#mean fat3
df<-df%>%
  mutate(FAT3=rowMeans(select(.,starts_with("fat3"))))

psych::describe(df$FAT3)
df%>%
  select(FAT3,groupid)%>%
  group_by(groupid)%>%
  report::report()

boxplot(df$FAT3~df$groupid)
boxplot(df$FAT3~df$sex)

#mean fat2_1 so wie lilli und chrissi
# df<-df%>%
# mutate(FAT2_1=rowMeans(select(.,starts_with("fat2_1"))))
# 
# psych::describe(df$FAT2_1)
# df%>%
#    select(FAT2_1,groupid)%>%
#    group_by(groupid)%>%
#    report::report()
# 
#  boxplot(df$FAT2_1~df$groupid)
#  boxplot(df$FAT2_1~df$sex)
# 
# # #mean fat2_2
#  df<-df%>%
#     mutate(FAT2_2=rowMeans(select(.,starts_with("fat2_2"))))
# 
#   psych::describe(df$FAT2_2)
#   df%>%
#     select(FAT2_2,groupid)%>%
#   group_by(groupid)%>%
#   report::report()
#
# boxplot(df$FAT2_2~df$groupid)
# boxplot(df$FAT2_2~df$sex)

library("dplyr")
  #mean energy (fat2_1 n, c, t, g, i) at T1
df <- df%>% 
  mutate(EN1 = rowMeans(select(.,"fat2_1n","fat2_1c","fat2_1t","fat2_1g","fat2_1l")))
psych::describe(df$EN1)

#mean energy (fat2_1 b, d, f, j, m) at T2
df <- df%>% 
  mutate(EN2 = rowMeans(select(.,"fat2_2n","fat2_2c","fat2_2t","fat2_2g","fat2_2l")))
psych::describe(df$EN2)

#mean tiredness (fat2_2 n, c, t, g, i) at T1
df <- df%>% 
  mutate(TE1 = rowMeans(select(.,"fat2_1b","fat2_1d","fat2_1f","fat2_1j","fat2_1m")))
psych::describe(df$TE1)

#mean tiredness (fat2_1 b, d, f, j, m) at T2
df <- df%>% 
  mutate(TE2 = rowMeans(select(.,"fat2_2b","fat2_2d","fat2_2f","fat2_2j","fat2_2m")))
psych::describe(df$TE2)


#mean tsc
df<-df%>%
  mutate(TSC=rowMeans(select(.,starts_with("tsc"))))

psych::describe(df$TSC)
df%>%
  select(TSC,groupid)%>%
  group_by(groupid)%>%
  report::report()

boxplot(df$TSC~df$groupid)
boxplot(df$TSC~df$sex)

#mean WP
df<-df%>%
  mutate(mean_WP=rowMeans(select(.,starts_with("WP"))))

psych::describe(df$WP1)
df%>%
  select(WP1,groupid)%>%
  group_by(groupid)%>%
  report::report()

boxplot(df$mean_WP~df$groupid)
boxplot(df$mean_WP~df$sex)


#DESKRIPTIVE Skalen und DENOGRAPHISCHE daten----
#deskriptives zeug----
# Prüfen der teilnehmer·innen zahl (1=feamle, 2=male,999=anders)
table(df$groupid)
# Muttersprache 
table(df$sprache)
# Alter
report::report(df$age)
#studierend? (1=ja, 2=no)
table(df$study)
#akademisches Elternteil (1=yes 2=no 3=weiß nicht)
table(df$study_parents)

#deskriptive daten für skalen
df%>% 
  select("FAT1","FAT3","EN1","EN2","TE1","TE2","mean_WP","TSC")%>%
  report::report_table()

#deskriptive daten für Skalen nach Gruppen (high/low incentives)
#group 1 - low incentives
df%>% 
  select("groupid","FAT1","FAT3","EN1","EN2","TE1","TE2","mean_WP","TSC")%>%
  filter(groupid==1)%>%
  report::report_table()
#group 2 - high incentives
df%>% 
  select("groupid","FAT1","FAT3","EN1","EN2","TE1","TE2","mean_WP","TSC")%>%
  filter(groupid==2)%>%
  report::report_table()

#RELIABILITY (CRONBACHS ALPHA) ----
#rel fat1
df%>%
  select(starts_with("fat1"))%>%
  psych::alpha()

#rel fat3
df%>%
  select(starts_with("fat3"))%>%
  psych::alpha()

#rel fat 2_1
#vorher aufteilen in tiredness und energy
#energy EN1
df%>%
  select("fat2_1n","fat2_1c", "fat2_1t", "fat2_1g","fat2_1l")%>%
  psych::alpha()

#tiredess
df%>%
  select("fat2_1b","fat2_1d", "fat2_1f", "fat2_1j","fat2_1m")%>%
  psych::alpha()

#rel fat 2_2
# vorher aufteilen in tiredness und energy
#energy
df%>%
  select("fat2_2n", "fat2_2c", "fat2_2t", "fat2_2g", "fat2_2l")%>%
  psych::alpha()

#tiredess
df%>%
  select("fat2_2b","fat2_2d", "fat2_2f", "fat2_2j","fat2_2m")%>%
  psych::alpha()

#rel tsc 
df%>%
  select(starts_with("tsc"))%>%
  psych::alpha()

#rel WP
df%>%
  select(starts_with("WP"))%>%
  psych::alpha()


# t Test für die Kontrolle der Gruppeneinteilung <-  mit FAT auch noch kontrolieren ----
t.test(df$mean_WP~df$groupid)
report::report(t.test(df$mean_WP~df$groupid))

t.test(df$TSC~df$groupid)
report::report(t.test(df$TSC~df$groupid))

t.test(df$FAT1~df$groupid)
report::report(t.test(df$FAT1~df$groupid))

t.test(df$FAT3~df$groupid)
report::report(t.test(df$FAT3~df$groupid))

#Skalenanalyse (weiterführen)----
df%>%
  select("FAT1","FAT3",groupid)%>%
  filter(groupid==1)%>%
  report::report_table()

df%>%
  select("FAT1","FAT3",groupid)%>%
  filter(groupid==2)%>%
  report::report_table()

df%>%
  select("mean_WP",groupid)%>%
  filter(groupid==1)%>%
  report::report_table()

df%>%
  select("mean_WP",groupid)%>%
  filter(groupid==2)%>%
  report::report_table()

library("dplyr")
#StroopScores----
test <- df %>%
  select(starts_with("StroopScorec"))

help("%>%")

test<-df%>%
  select(starts_with("Stroopscorec"))

#summe der korrekten incongruenten versuche
df<-df%>%
  mutate(sum_inc=rowSums(select(.,starts_with("Stroopscorei"))))

#summer der korrekten kongruenten versuche 
df <- df%>% 
  mutate(sum_con = rowSums(select(.,starts_with("StroopScorec"))))

# summer ALLER korrekten versuche
df <- df%>% 
  mutate(sum_all = rowSums(select(.,"sum_inc","sum_con")))


df%>%
  select("groupid","sum_inc")%>%
  group_by(groupid)%>%
  report::report()

df$sum_inc

df%>%
  select("groupid","sum_con")%>%
  group_by(groupid)%>%
  report::report()

df%>%
  select("groupid","sum_all")%>%
  group_by(groupid)%>%
  report::report()

#summer aller korrekten versuche by block
df <- df%>%mutate(sum_B1 = rowSums(select(.,"StroopScorei","StroopScorec")))
df <- df%>%mutate(sum_B2 = rowSums(select(.,"StroopScorei1","StroopScorec1")))
df <- df%>%mutate(sum_B3= rowSums(select(.,"StroopScorei2","StroopScorec2")))
df <- df%>%mutate(sum_B4 = rowSums(select(.,"StroopScorei3","StroopScorec3")))
df <- df%>%mutate(sum_B5 = rowSums(select(.,"StroopScorei4","StroopScorec4")))
df <- df%>%mutate(sum_B6 = rowSums(select(.,"StroopScorei5","StroopScorec5")))
df <- df%>%mutate(sum_B7= rowSums(select(.,"StroopScorei6","StroopScorec6")))
df <- df%>%mutate(sum_B8 = rowSums(select(.,"StroopScorei7","StroopScorec7")))
df <- df%>%mutate(sum_B9 = rowSums(select(.,"StroopScorei8","StroopScorec8")))
df <- df%>%mutate(sum_B10 = rowSums(select(.,"StroopScorei9","StroopScorec9")))
df <- df%>%mutate(sum_B11 = rowSums(select(.,"StroopScorei10","StroopScorec10")))
df <- df%>%mutate(sum_B12 = rowSums(select(.,"StroopScorei11","StroopScorec11")))
df <- df%>%mutate(sum_B13 = rowSums(select(.,"StroopScorei12","StroopScorec12")))
df <- df%>%mutate(sum_B14 = rowSums(select(.,"StroopScorei13","StroopScorec13")))
df <- df%>%mutate(sum_B15 = rowSums(select(.,"StroopScorei14","StroopScorec14")))

#mean rate der korrenten versuche per block 
#compute mean rate of correct trials by block
df <- df%>%mutate(p_B1 = (StroopScorepi+StroopScorepc)/2) 
df <- df%>%mutate(p_B2 = (StroopScorepi1+StroopScorepc1)/2)
df <- df%>%mutate(p_B3 = (StroopScorepi2+StroopScorepc2)/2)
df <- df%>%mutate(p_B4 = (StroopScorepi3+StroopScorepc3)/2)
df <- df%>%mutate(p_B5 = (StroopScorepi4+StroopScorepc4)/2)
df <- df%>%mutate(p_B6 = (StroopScorepi5+StroopScorepc5)/2)
df <- df%>%mutate(p_B7 = (StroopScorepi6+StroopScorepc6)/2)
df <- df%>%mutate(p_B8 = (StroopScorepi7+StroopScorepc7)/2)
df <- df%>%mutate(p_B9 = (StroopScorepi8+StroopScorepc8)/2)
df <- df%>%mutate(p_B10 = (StroopScorepi9+StroopScorepc9)/2)
df <- df%>%mutate(p_B11 = (StroopScorepi10+StroopScorepc10)/2)
df <- df%>%mutate(p_B12 = (StroopScorepi11+StroopScorepc11)/2)
df <- df%>%mutate(p_B13 = (StroopScorepi12+StroopScorepc12)/2)
df <- df%>%mutate(p_B14 = (StroopScorepi13+StroopScorepc13)/2)
df <- df%>%mutate(p_B15 = (StroopScorepi14+StroopScorepc14)/2)

df<-df%>%
  mutate(sum_con=rowSums(select(.,starts_with("StroopScorec"))))

df<-df%>%
  mutate(sum_all=rowSums(select(.,"sum_con","sum_inc")))

df%>%
  select(sum_all,groupid)%>%
  group_by(groupid)%>%
  report::report()

#inc;con Gruppen

df%>%
  select(sum_con,groupid)%>%
  group_by(groupid)%>%
  report::report()

df%>%
  select(sum_inc,groupid)%>%
  group_by(groupid)%>%
  report::report()

#exportieren und sichern
# writexl::write_xlsx(df,"prepared_data.xlsx")

#ANOVA----
#mixed-anova fatigue-short-scale
#get data
data<-df[c("subjectid","groupid","FAT1","FAT3")] 

#change to long-format
test <- data %>% 
  gather(key = "time", value = "score", FAT1,FAT3) %>%
  convert_as_factor(groupid, time)

#get summary statistics
test %>% 
  group_by(time, groupid) %>%
  get_summary_stats(score, type = "mean_sd")

#plot
ggboxplot(test, x = "time", y = "score",color = "groupid", palette = "jco",size=1.5,fill="orange") 

#berechnen der ANOVA (pes=partial eta square)
#Interpretation des Partial Eta Squared
# η2 = 0.01 indicates a small effect. η2 = 0.06 indicates a medium effect. η2 = 0.14 indicates a large effect.
res.aov <- anova_test(data = test, dv = score, wid = subjectid,between = groupid, within = time,effect.size = "pes")
#get results
get_anova_table(res.aov) 

#mixed-anova tiredness-scale
#get data
data<-df[c("subjectid","groupid","TE1","TE2")] 

#change to long-format
#change to long-format
test <- data %>% 
  gather(key = "time", value = "score", TE1,TE2) %>%
  convert_as_factor(groupid, time)

#get summary statistics
test %>% 
  group_by(time, groupid) %>%
  get_summary_stats(score, type = "mean_sd")

#plot
ggboxplot(test, x = "time", y = "score",color = "groupid", palette = "jco",size=1.5,) 

#outliers 
test%>%
  group_by(time, groupid)%>%
  identify_outliers(score)

#normalverteilung
# test%>%
#   group_by(time, groupid)%>%
#   shapiro_test(score)

#normal QQPlot
ggqqplot(test, "score", ggtheme = theme_bw())+
  facet_grid(time~groupid)

#homogenität by levene test
test%>%group_by(time)%>%
  levene_test(score~groupid)
# zeigt uns dass varianzen homogen sind -> varianzhomogenität


#compute ANOVA (pes=partial eta square)
res.aov <- anova_test(data = test, dv = score, wid = subjectid,between = groupid, within = time,effect.size = "pes") 
#get results 
get_anova_table(res.aov) 

#mixed-anova energy-scale
#get data
data<-df[c("subjectid","groupid","EN1","EN2")]

#change to long-format
test <- data %>% 
  gather(key = "time", value = "score", EN1,EN2) %>%
  convert_as_factor(groupid, time)

#get summary statistics
test %>% 
  group_by(time, groupid) %>%
  get_summary_stats(score, type = "mean_sd")

#plot
ggboxplot(test, x = "time", y = "score",color = "groupid", palette = "jco",size=1.5) 

#mauchlys test of sphericity
res.aov <- anova_test(data = test, dv = score, wid = subjectid,between = groupid, within = time,effect.size = "pes") #compute ANOVA (pes=partial eta square)
#get results
get_anova_table(res.aov) 

#mixed model----
#get data long format
dfl<-read_csv("EDIN_online_1_long.csv")
# mixed model berechnung
dfl$level<-dfl$block/10

d <- dfl %>%
  select(subject, TP, FAT1, group, level)
m1<-lmer(TP~FAT1+group*poly(level,2,raw=TRUE)+(level| subject),data=d)
summary(m1)

fm <- fortify.merMod(m1)

d %>%
  group_by(group, level) %>%
  summarise(TP = mean(TP))  %>%
  ggplot(aes(x = level, y = TP, color = group)) + 
  geom_line()

sjPlot::tab_model(m1)
performance::icc(m1)
sjPlot::set_theme(base = theme_classic(base_size=20))
sjPlot::plot_model(m1, type = "pred", terms = c("level [all]","group"),  title="")

report::report(m1)


m1<-lmer(TP~group*poly(level,2,raw=TRUE)+(level| subject),data=d)
summary(m1)
m2 <- lmer(TP~FAT1+group*poly(level,2,raw=TRUE)+(1| subject),data=d)
summary(m1)
anova(m2, m1)

check_model(m1)

# Extract the prediction data frame
pred.mm <- ggpredict(m1, terms = c("level [all]", "group"))  # this gives overall predictions for the model
ggpredict(m1, terms = c("level [all]", "group"))
# Plot the predictions 

ggplot(pred.mm) + 
  geom_line(aes(x = x, y = predicted)) +          # slope
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
              fill = "lightgrey", alpha = 0.5)+  # error band
  geom_point(data = d,                      # adding the raw data (scaled values)
             aes(x = level, y = TP, colour = group)) + 
  labs(x = "Body Length (indexed)", y = "Test Score", 
       title = "Body length does not affect intelligence in dragons") + 
  theme_minimal()

example.data <- d
example.data <- example.data %>%
  rename(performance = TP,
         block = level)
