#loading packages
packages <- c("readxl", "ggpmisc", "tidyverse", "rlang", "ggpubr", "modelr", 'patchwork', 'writexl')
lapply(packages, library, character.only = T)

setwd("L:\\Tovi\\Cedric_Kouam\\carbs_survival\\Carbs Tests")
carbdata <- read_excel("carbsabs_test.xlsx", sheet = "19Jan23")


#creating data frame for the replicates
absorbance1 <-  carbdata$stdabs1
glucose1 <- carbdata$stdmass1
carbs1 <- na.omit(data.frame(absorbance1, glucose1))
carbsm1 <- aggregate(absorbance1 ~ glucose1, data = carbs1, FUN = mean)

absorbance2 <-  carbdata$stdabs2
glucose2 <- carbdata$stdmass2
carbs2 <- na.omit(data.frame(absorbance2, glucose2))
carbsm2 <- aggregate(absorbance2 ~ glucose2, data = carbs2, FUN = mean)

my.formula <- y~x


#Plotting results from the replicates
fri1 <- ggplot(data = carbs1, aes(x = absorbance1, y = glucose1))+
  labs(x= "absorbances (nm)", y= "Glucose (ug)", title = "Anthrone Calibration Plate 1")+
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = my.formula)+
  stat_regline_equation(label.x = 0.25, label.y = 40)+
  stat_cor(label.x = 0.25, label.y = 35)+
  geom_point(color= "Blue")
ggsave("30nov22glutcalicurve.jpg", dpi = 300, height = 6, width = 8)
fri1

fri2 <- ggplot(data = carbs2, aes(x = absorbance2, y = glucose2))+
  labs(x= "absorbances (nm)", y= "Glucose (ug)", title = "Anthrone Calibration Plate 2")+
  geom_smooth(method = "lm", color = "Orange", se = FALSE, formula = my.formula)+
  stat_regline_equation(label.x = 0.25, label.y = 40)+
  stat_cor(label.x = 0.25, label.y = 35)+
  geom_point(color= "Green")
ggsave("30nov22glucalicurve2.jpg", dpi = 300, height = 6, width = 8)
fri2

frim1 <- ggplot(data = carbsm1, aes(x = absorbance1, y = glucose1))+
  labs(x= "absorbances (nm)", y= "Glucose (ug)", title = "Plate 1 Anthrone Calibration Average")+
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = my.formula)+
  stat_regline_equation(label.x = 0.25, label.y = 40)+
  stat_cor(label.x = 0.25, label.y = 35)+
  geom_point(color= "Blue")
ggsave("30nov22glucalicurvemean.jpg", dpi = 300, height = 6, width = 8)
frim1

frim2 <- ggplot(data = carbsm2, aes(x = absorbance2, y = glucose2))+
  labs(x= "absorbances (nm)", y= "Glucose (ug)", title = "Plate 2 Anthrone Calibration Average")+
  geom_smooth(method = "lm", color = "orange", se = FALSE, formula = my.formula)+
  stat_regline_equation(label.x = 0.25, label.y = 40)+
  stat_cor(label.x = 0.25, label.y = 35)+
  geom_point(color= "Green")
ggsave("30nov22glucalicurvemean2.jpg", dpi = 300, height = 6, width = 8)
frim2

#First create your unknown data frame

unknown.df1 <- na.omit(data.frame(mosqid1 = carbdata$mosqid1,
                                absorbance1 = carbdata$mosqabs1))
unknown.df1

unknown.df2 <- na.omit(data.frame(mosqid2 = carbdata$mosqid2,
                                 absorbance2 = carbdata$mosqabs2))
unknown.df2
#Aggregate the unknown data frame

unknown.abs1 <- aggregate(absorbance1 ~ mosqid1, data = unknown.df1,
                          FUN = mean)
unknown.abs1$absorbance1 <- round(unknown.abs1$absorbance1, 2)

unknown.abs1

unknown.abs2 <- aggregate(absorbance2 ~ mosqid2, data = unknown.df2,
                         FUN = mean)
unknown.abs2$absorbance2 <- round(unknown.abs2$absorbance2, 2)

unknown.abs2
#make sure your unknown values have the same name as the values in your model
#eg. unknown df values are "yvalues" like in the original df.(very important)
#samples.abs <- data.frame(absorbance = unknown.abs$Abs)
#samples.abs2 <- data.frame(absorbance2 = unknown.abs2$Abs2)

#linear model
carb.lm1 <- lm(formula = glucose1 ~ absorbance1, data = carbdata)

carb.lm2 <- lm(formula = glucose2 ~ absorbance2, data = carbdata)

#Predict glucose mass (ug) of your unknown and name the predcited value "var="

glucose.values1 <- add_predictions(unknown.abs1, carb.lm1, var = "glucose(ug)")
view(glucose.values1)

glucose.values2 <- add_predictions(unknown.abs2, carb.lm2, var = "glucose(ug)")
view(glucose.values2)



#you could also use predict.lm function to find predicted values
#e.g carbs.values2 <- predict.lm(carbs.lm, unknown.abs2)

#plot the unknown glucose mass based on the absorbance

friuk1 <- ggplot()+
  labs(x= "absorbances (nm)", y= "glucose (ug)", 
       title = "Standard and Unknown Regression Plate 1",
       color="Legend")+
  geom_smooth(method = "lm", data = carbsm1,lty = "dashed",
              aes(x = absorbance1, y= glucose1),
              color = "black", se = FALSE, formula = my.formula)+
  stat_regline_equation(data = carbsm1,
                        aes(x = absorbance1, y= glucose1),label.x = 0.4, label.y = 40)+
  stat_cor(data = carbsm1,
           aes(x = absorbance1, y= glucose1),label.x = 0.4, label.y = 35)+
  geom_point(data = carbsm1, aes(x = absorbance1, y = glucose1, 
                                 color= "standard points"))+
  geom_point(data = glucose.values1, aes(x = absorbance1, y=`glucose(ug)`,
                                         color= "unknown samples"))
ggsave("30nov22glu_unknown_reg1.jpg", dpi = 300, height = 6, width = 8)
friuk1

friuk2 <- ggplot()+
  labs(x= "absorbances (nm)", y= "glucose (ug)", 
       title = "Standard and Unknown Regression Plate 2",
       color="Legend")+
  geom_smooth(method = "lm", data = carbsm2,lty = "dashed",
              aes(x = absorbance2, y= glucose2),
              color = "black", se = FALSE, formula = my.formula)+
  stat_regline_equation(data = carbsm2,
                        aes(x = absorbance2, y= glucose2),label.x = 0.4, label.y = 40)+
  stat_cor(data = carbsm2,
           aes(x = absorbance2, y= glucose2),label.x = 0.4, label.y = 35)+
  geom_point(data = carbsm2, aes(x = absorbance2, y = glucose2, 
                                color= "standard points"))+
  geom_point(data = glucose.values2, aes(x = absorbance2, y=`glucose(ug)`,
                                         color= "unknown samples"))
ggsave("30nov22glu_unknown_reg2.jpg", dpi = 300, height = 6, width = 8)
friuk2

(Weds|Wedsm|weds3)

#create the data frame with your glucose concentrations and glucose mass per mosq
glucose.values1$`glucose_diluconc(ug/uL)` <- glucose.values1$`glucose(ug)`/150
glucose.values1

glucose.values1$`glucose_conc(ug/uL)/mosq` <- glucose.values1$`glucose_diluconc(ug/uL)`*10
glucose.values1

glucose.values1$`glucose(ug)/mosq` <- round(glucose.values1$`glucose_conc(ug/uL)/mosq`*180, 0)
glucose.values1

glucose.values2$`glucose_diluconc(ug/uL)` <- glucose.values2$`glucose(ug)`/150
glucose.values2

glucose.values2$`glucose_conc(ug/uL)/mosq` <- glucose.values2$`glucose_diluconc(ug/uL)`*10
glucose.values2

glucose.values2$`glucose(ug)/mosq` <- round(glucose.values2$`glucose_conc(ug/uL)/mosq`*180, 0)
glucose.values2

#export your results in excel format
write_xlsx(glucose.values1,"L:\\Tovi\\Cedric_Kouam\\carbs_survival\\rdm3rep1glucosevalues1.xlsx")
write_xlsx(glucose.values2,"L:\\Tovi\\Cedric_Kouam\\carbs_survival\\rdm3rep1glucosevalues2.xlsx")

#Plot a histogram of unknown samples using ggplot

mosq.glu1 <- ggplot(glucose.values1, aes(x = mosqid1 , y = `glucose(ug)/mosq`))+
  geom_bar(fill='blue', stat = "identity")+
  labs(x = "Individual Mosquitoes", y = "Glucose mass(ug)/mosq",
       title = "Glucose Mass per Individual Mosquito Plate 1")+
  theme(axis.text.x = element_text(face = 'bold', size = 12, angle = 45))+
  geom_text(aes(label = `glucose(ug)/mosq`), vjust = -0.2, size = 5)
ggsave("30nov22_glu.massbymosq.jpg", dpi=300, height=6, width=8)
mosq.glu1

mosq.glu2 <- ggplot(glucose.values2, aes(x = mosqid2 , y = `glucose(ug)/mosq`))+
  geom_bar(fill='blue', stat = "identity")+
  labs(x = "Individual Mosquitoes", y = "Glucose mass(ug)/mosq",
       title = "Glucose Mass per Individual Mosquito Plate 2")+
  theme(axis.text.x = element_text(face = 'bold', size = 12, angle = 45))+
  geom_text(aes(label = `glucose(ug)/mosq`), vjust = -0.2, size = 5)
ggsave("30nov22_glu.massbymosq2.jpg", dpi=300, height=6, width=8)
mosq.glu2

(fri1|frim1|friuk1)/(fri2|frim2|friuk2)
(mosq.glu1|mosq.glu2)

(fri|frim)/(friuk|mosq.glu)
ggsave("25may22pilotglu.jpg", dpi = 300, height = 6, width = 8)














#12-13April22 data
setwd("L:\\Tovi\\Cedric_Kouam\\RData&codes\\Rdata\\Carbs")
Carbsdata1 <- read_excel("CarbohydrateAssayData_12Apr22_13Apr22.xlsx", sheet = "12Apr22")
Carbsdata2 <- read_excel("CarbohydrateAssayData_12Apr22_13Apr22.xlsx", sheet = "13Apr22")

#creating data frame for the replicates
absorbance1 <-  Carbsdata1$stdabs
glucose1 <- Carbsdata1$stdmass
carbs1 <- data.frame(absorbance1, glucose1)
carbsm1 <- aggregate(absorbance1 ~ glucose1, data = carbs1, FUN = mean)

absorbance2 <-  Carbsdata2$stdabs
glucose2 <- Carbsdata2$stdmass
carbs2 <- data.frame(absorbance2, glucose2)
carbsm2 <- aggregate(absorbance2 ~ glucose2, data = carbs2, FUN = mean)

my.formula <- y~x


#Plotting results from the replicates
Tues <- ggplot(data = carbs1, aes(x = absorbance1, y = glucose1))+
  labs(x= "absorbances (nm)", y= "Glucose (ug)", title = "12Apr22 Anthrone Calibration")+
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = my.formula)+
  stat_regline_equation(label.x = 0.5, label.y = 150)+
  stat_cor(label.x = 0.5, label.y = 125)+
  geom_point(color= "Blue")
Tues
  ggsave("glucalicurve1.jpg", dpi = 300, height = 6, width = 8)

Tuesm <- ggplot(data = carbsm1, aes(x = absorbance1, y = glucose1))+
  labs(x= "absorbances (nm)", y= "Glucose (ug)", title = "12Apr22 Anthrone Calibration Average")+
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = my.formula)+
  stat_regline_equation(label.x = 0.5, label.y = 150)+
  stat_cor(label.x = 0.5, label.y = 125)+
  geom_point(color= "Blue")
Tuesm
  ggsave("glucalicurvemean1.jpg", dpi = 300, height = 6, width = 8)

Weds <- ggplot(data = carbs2, aes(x= absorbance2, y= glucose2))+
  geom_point(color= "Red")+
  labs(x= "absorbances (nm)", y= "Glucose (ug)", title = "13Apr22 Anthrone Calibration")+
  geom_smooth(method = "lm", se = FALSE, formula = my.formula)+
  stat_regline_equation(label.x = 0.5, label.y = 60)+
  stat_cor(label.x = 0.5, label.y = 50)
Weds
ggsave("glucalicurve2.jpg", dpi = 300, height = 6, width = 8)

Wedsm <- ggplot(data = carbsm2, aes(x= absorbance2, y= glucose2))+
  geom_point(color= "Red")+
  labs(x= "absorbances (nm)", y= "Glucose (ug)", title = "13Apr22 Anthrone Calibration Average")+
  geom_smooth(method = "lm", se = FALSE, formula = my.formula)+
  stat_regline_equation(label.x = 0.5, label.y = 60)+
  stat_cor(label.x = 0.5, label.y = 50)
Wedsm
ggsave("glucalicurvemean2.jpg", dpi = 300, height = 6, width = 8)

#Plotting the calibration for both replicates into one plot
BothReps <- ggplot()+
  geom_point(data = carbsm1,aes(absorbance1, glucose1, color = "1st Rep"))+
  geom_point(data = carbsm2, aes(absorbance2, glucose2, color = "2nd Rep"))+
  geom_smooth(data = carbsm1, aes(absorbance1, glucose1), method = "lm", se = F, formula = NULL,
              position = "identity", color = "yellow")+
  geom_smooth(data = carbsm2, aes(absorbance2, glucose2), method = "lm", se = F, formula = NULL,
              color = "orange")+
  labs(title = 'Two replicates of Glucose Assay Mean Calibration', y = 'glucose(ug)', x = 'absorbance',
       color = 'reps')+
  stat_regline_equation(aes(absorbance1, glucose1), data = carbsm1,label.x = 0.5, label.y = 100)+
  stat_regline_equation(aes(absorbance2, glucose2), data = carbsm2, label.x = 1.25, label.y = 25)

BothReps
#First create your unknown data frame

unknown.df2 <- na.omit(data.frame(mosqid = Carbsdata2$mosqid,
                                  volume = Carbsdata2$volume,
                                  absorbance2 = Carbsdata2$mosqabs))
unknown.df2
#Aggregate the unknown data frame

unknown.abs2 <- aggregate(absorbance2 ~ mosqid + volume, data = unknown.df2,
                          FUN = mean)
unknown.abs2

#make sure your unknown values have the same name as the values in your model
#eg. unknown df values are "yvalues" like in the original df.(very important)
#samples.abs <- data.frame(absorbance = unknown.abs$Abs)
#samples.abs2 <- data.frame(absorbance2 = unknown.abs2$Abs2)

#linear model
carbs.lm2 <- lm(formula = glucose2 ~ absorbance2, data = carbsm2)

#Predict glucose mass (ug) of your unknown and name the predcited value "var="

glucose.values2 <- add_predictions(unknown.abs2, carbs.lm2, var = "glucose(ug)")
view(glucose.values2)
#you could also use predict.lm function to find predicted values
#e.g carbs.values2 <- predict.lm(carbs.lm, unknown.abs2)

#plot the unknown glucose mass based on the absorbance

weds3 <- ggplot()+
  labs(x= "absorbances (nm)", y= "glucose (ug)", 
       title = "Standard and Unknown Regression",
       color="Legend")+
  geom_smooth(method = "lm", data = carbsm2,lty = "dashed",
              aes(x = absorbance2, y= glucose2),
              color = "black", se = FALSE, formula = my.formula)+
  stat_regline_equation(data = carbsm2,
                        aes(x = absorbance2, y= glucose2),label.x = 0.5, label.y = 60)+
  stat_cor(data = carbsm2,
           aes(x = absorbance2, y= glucose2),label.x = 0.5, label.y = 50)+
  geom_point(data = carbsm2, aes(x = absorbance2, y = glucose2, 
                                         color= "standard points"))+
  geom_point(data = glucose.values2, aes(x = absorbance2, y=`glucose(ug)`,
                                        color= "unknown samples"))
weds3
ggsave("glu_unknown_reg2.jpg", dpi = 300, height = 6, width = 8)

(Weds|Wedsm|weds3)

#create the data frame with your glucose concentrations and glucose mass per mosq
glucose.values2$`glucose_diluconc(ug/uL)` <- glucose.values2$`glucose(ug)`/glucose.values2$volume
glucose.values2

glucose.values2$`glucose_conc(ug/uL)/mosq` <- glucose.values2$`glucose_diluconc(ug/uL)`*10
glucose.values2

glucose.values2$`glucose(ug)/mosq` <- round(glucose.values2$`glucose_conc(ug/uL)/mosq`*180, 0)
glucose.values2

#export your results in excel format
write_xlsx(glucose.values2,"L:\\Tovi\\Cedric_Kouam\\RData&codes\\Rdata\\Carbs\\glucosevalues.xlsx")

#Plot a histogram of unknown samples using ggplot

mosq.glu <- ggplot(glucose.values2, aes(x = mosqid , y = `glucose(ug)/mosq`))+
  geom_bar(fill='blue', stat = "identity")+
  labs(x = "Individual Mosquitoes", y = "Glucose mass(ug)/mosq",
       title = "Glucose Mass per Individual Mosquito")+
  theme(axis.text.x = element_text(face = 'bold', size = 12, angle = 45))+
  geom_text(aes(label = `glucose(ug)/mosq`), vjust = -0.2, size = 5)
ggsave("19may22_glu.massbymosq.jpg", dpi=300, height=6, width=8)
mosq.glu

(Weds|Wedsm)/(weds3|mosq.glu)
ggsave("19may22pilotglu.jpg", dpi = 300, height = 6, width = 8)




