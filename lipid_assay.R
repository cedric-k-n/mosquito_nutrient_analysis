#loading packages
packages <- c("readxl", "ggpmisc", "tidyverse", "rlang", "ggpubr", "modelr", 'patchwork', 'writexl')
lapply(packages, library, character.only = T)
setwd("L:/Tovi/Cedric_Kouam/lipid_survival")

lipdata <- read_excel("lipabs.xlsx", sheet = "rdm6rep2")
#glydata <- read_excel('lipid_standard_test.xlsx', sheet = "30jan23abs")
#creating data frame for the replicates
absorbance1 <-  lipdata$stdabs1
lipid1 <- lipdata$stdmass1
lip1 <- na.omit(data.frame(absorbance1, lipid1))
lipm1 <- aggregate(absorbance1 ~ lipid1, data = lip1, FUN = mean)

absorbance2 <-  lipdata$stdabs2
lipid2 <- lipdata$stdmass2
lip2 <- na.omit(data.frame(absorbance2, lipid2))
lipm2 <- aggregate(absorbance2 ~ lipid2, data = lip2, FUN = mean)

absorbance3 <-  lipdata$stdabs3
lipid3 <- lipdata$stdmass3
lip3 <- na.omit(data.frame(absorbance3, lipid3))
lipm3 <- aggregate(absorbance3 ~ lipid3, data = lip3, FUN = mean)


#abSq1 <- absorbance1*absorbance1

my.formula <- y~x
#my.formula <- y~x + I(x^2)


#Plotting results from the replicates
fri1 <- ggplot(data = lip1, aes(x = absorbance1, y = lipid1))+
  labs(x= "absorbances (nm)", y= "Lipid (ug)", title = "Vanillin Calibration Plate 1")+
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = my.formula)+
  stat_regline_equation(label.x = 0.25, label.y = 30)+
  stat_cor(label.x = 0.25, label.y = 25)+
  geom_point(color= "Blue")
ggsave("rdm6rep2lipcalicurve1.jpg", dpi = 300, height = 6, width = 8)
fri1

fri2 <- ggplot(data = lip2, aes(x = absorbance2, y = lipid2))+
  labs(x= "absorbances (nm)", y= "Lipid (ug)", title = "Vanillin Calibration Plate 2")+
  geom_smooth(method = "lm", color = "orange", se = FALSE, formula = my.formula)+
  stat_regline_equation(label.x = 0.25, label.y = 30)+
  stat_cor(label.x = 0.25, label.y = 25)+
  geom_point(color= "green")
ggsave("rdm6rep2lipcalicurve2.jpg", dpi = 300, height = 6, width = 8)
fri2

fri3 <- ggplot(data = lip3, aes(x = absorbance3, y = lipid3))+
  labs(x= "absorbances (nm)", y= "Lipid (ug)", title = "Vanillin Calibration Plate 3")+
  geom_smooth(method = "lm", color = "yellow", se = FALSE, formula = my.formula)+
  stat_regline_equation(label.x = 0.25, label.y = 30)+
  stat_cor(label.x = 0.25, label.y = 25)+
  geom_point(color= "red")
ggsave("rdm6rep2lipcalicurve3.jpg", dpi = 300, height = 6, width = 8)
fri3

frim1 <- ggplot(data = lipm1, aes(x = absorbance1, y = lipid1))+
  labs(x= "absorbances (nm)", y= "Lipid (ug)", title = "Plate 1 Vanillin Calibration Average")+
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = my.formula)+
  stat_regline_equation(label.x = 0.25, label.y = 30)+
  stat_cor(label.x = 0.25, label.y = 25)+
  geom_point(color= "Blue")
ggsave("rdm6rep2lipcalicurvemean1.jpg", dpi = 300, height = 6, width = 8)
frim1

frim2 <- ggplot(data = lipm2, aes(x = absorbance2, y = lipid2))+
  labs(x= "absorbances (nm)", y= "Lipid (ug)", title = "Plate 2 Vanillin Calibration Average")+
  geom_smooth(method = "lm", color = "orange", se = FALSE, formula = my.formula)+
  stat_regline_equation(label.x = 0.25, label.y = 30)+
  stat_cor(label.x = 0.25, label.y = 25)+
  geom_point(color= "Green")
ggsave("rdm6rep2lipcalicurvemean2.jpg", dpi = 300, height = 6, width = 8)
frim2

frim3 <- ggplot(data = lipm3, aes(x = absorbance3, y = lipid3))+
  labs(x= "absorbances (nm)", y= "lipid (ug)", title = "Plate 3 Vanillin Calibration Average")+
  geom_smooth(method = "lm", color = "yellow", se = FALSE, formula = my.formula)+
  stat_regline_equation(label.x = 0.25, label.y = 30)+
  stat_cor(label.x = 0.25, label.y = 25)+
  geom_point(color= "red")
ggsave("rdm6rep2lipcalicurvemean3.jpg", dpi = 300, height = 6, width = 8)
frim3

#First create your unknown data frame

unknown.df1 <- na.omit(data.frame(mosqid1 = lipdata$mosqid1,
                                  absorbance1 = lipdata$mosqabs1))
unknown.df1

unknown.df2 <- na.omit(data.frame(mosqid2 = lipdata$mosqid2,
                                  absorbance2 = lipdata$mosqabs2))
unknown.df2

unknown.df3 <- na.omit(data.frame(mosqid3 = lipdata$mosqid3,
                                  absorbance3 = lipdata$mosqabs3))
unknown.df3
#Aggregate the unknown data frame

unknown.abs1 <- aggregate(absorbance1 ~ mosqid1, data = unknown.df1,
                          FUN = mean)
unknown.abs1$absorbance1 <- round(unknown.abs1$absorbance1, 2)

unknown.abs1

unknown.abs2 <- aggregate(absorbance2 ~ mosqid2, data = unknown.df2,
                          FUN = mean)
unknown.abs2$absorbance2 <- round(unknown.abs2$absorbance2, 2)

unknown.abs2

unknown.abs3 <- aggregate(absorbance3 ~ mosqid3, data = unknown.df3,
                          FUN = mean)
unknown.abs3$absorbance3 <- round(unknown.abs3$absorbance3, 2)

unknown.abs3
#make sure your unknown values have the same name as the values in your model
#eg. unknown df values are "yvalues" like in the original df.(very important)
#samples.abs <- data.frame(absorbance = unknown.abs$Abs)
#samples.abs2 <- data.frame(absorbance2 = unknown.abs2$Abs2)

#linear model
lip.lm1 <- lm(formula = lipid1 ~ absorbance1, data = lipdata)
#GlySq1  <- lm(formula = lipid1 ~ absorbance1, abSq1, data = glydata) 

lip.lm2 <- lm(formula = lipid2 ~ absorbance2, data = lipdata)

lip.lm3 <- lm(formula = lipid3 ~ absorbance3, data = lipdata)

#Predict lipid mass (ug) of your unknown and name the predcited value "var="

lipid.values1 <- add_predictions(unknown.abs1, lip.lm1, var = "lipid (ug)")
view(lipid.values1)

lipid.values2 <- add_predictions(unknown.abs2, lip.lm2, var = "lipid (ug)")
view(lipid.values2)

lipid.values3 <- add_predictions(unknown.abs3, lip.lm3, var = "lipid (ug)")
view(lipid.values3)

#you could also use predict.lm function to find predicted values
#e.g carbs.values2 <- predict.lm(carbs.lm, unknown.abs2)

#plot the unknown lipid mass based on the absorbance

friuk1 <- ggplot()+
  labs(x= "absorbances (nm)", y= "lipid (ug)", 
       title = "Standard and Unknown Regression Plate 1",
       color="Legend")+
  geom_smooth(method = "lm", data = lipm1,lty = "dashed",
              aes(x = absorbance1, y= lipid1),
              color = "black", se = FALSE, formula = my.formula)+
  stat_regline_equation(data = lip1,
                        aes(x = absorbance1, y= lipid1),label.x = 0.4, label.y = 30)+
  stat_cor(data = lipm1,
           aes(x = absorbance1, y= lipid1),label.x = 0.4, label.y = 25)+
  geom_point(data = lipm1, aes(x = absorbance1, y = lipid1, 
                               color= "standard points"))+
  geom_point(data = lipid.values1, aes(x = absorbance1, y=`lipid (ug)`,
                                          color= "unknown samples"))
ggsave("rdm6rep2lip_unknown_reg1.jpg", dpi = 300, height = 6, width = 8)
friuk1

friuk2 <- ggplot()+
  labs(x= "absorbances (nm)", y= "lipid (ug)", 
       title = "Standard and Unknown Regression Plate 2",
       color="Legend")+
  geom_smooth(method = "lm", data = lipm2,lty = "dashed",
              aes(x = absorbance2, y= lipid2),
              color = "black", se = FALSE, formula = my.formula)+
  stat_regline_equation(data = lipm2,
                        aes(x = absorbance2, y= lipid2),label.x = 0.4, label.y = 30)+
  stat_cor(data = lipm2,
           aes(x = absorbance2, y= lipid2),label.x = 0.4, label.y = 25)+
  geom_point(data = lipm2, aes(x = absorbance2, y = lipid2, 
                               color= "standard points"))+
  geom_point(data = lipid.values2, aes(x = absorbance2, y=`lipid (ug)`,
                                          color= "unknown samples"))
ggsave("rdm6rep2lip_unknown_reg2.jpg", dpi = 300, height = 6, width = 8)
friuk2

friuk3 <- ggplot()+
  labs(x= "absorbances (nm)", y= "lipid (ug)", 
       title = "Standard and Unknown Regression Plate 3",
       color="Legend")+
  geom_smooth(method = "lm", data = lipm3,lty = "dashed",
              aes(x = absorbance3, y= lipid3),
              color = "black", se = FALSE, formula = my.formula)+
  stat_regline_equation(data = lipm3,
                        aes(x = absorbance3, y= lipid3),label.x = 0.4, label.y = 30)+
  stat_cor(data = lipm3,
           aes(x = absorbance3, y= lipid3),label.x = 0.4, label.y = 25)+
  geom_point(data = lipm3, aes(x = absorbance3, y = lipid3, 
                               color= "standard points"))+
  geom_point(data = lipid.values3, aes(x = absorbance3, y=`lipid (ug)`,
                                          color= "unknown samples"))
ggsave("rdm6rep2lip_unknown_reg3.jpg", dpi = 300, height = 6, width = 8)
friuk3

(Weds|Wedsm|weds3)

#create the data frame with your lipid concentrations and lipid mass per mosq

lipid.values1$`lipid_diluconc(ug/uL)` <- lipid.values1$`lipid (ug)`/200
lipid.values1

lipid.values1$`lipid_conc(ug/uL)/mosq` <- lipid.values1$`lipid_diluconc(ug/uL)`*10
lipid.values1

lipid.values1$`lipid(ug)/mosq` <- round(lipid.values1$`lipid_conc(ug/uL)/mosq`*180, 0)
lipid.values1

lipid.values2$`lipid_diluconc(ug/uL)` <- lipid.values2$`lipid (ug)`/200
lipid.values2

lipid.values2$`lipid_conc(ug/uL)/mosq` <- lipid.values2$`lipid_diluconc(ug/uL)`*10
lipid.values2

lipid.values2$`lipid(ug)/mosq` <- round(lipid.values2$`lipid_conc(ug/uL)/mosq`*180, 0)
lipid.values2

lipid.values3$`lipid_diluconc(ug/uL)` <- lipid.values3$`lipid (ug)`/200
lipid.values3

lipid.values3$`lipid_conc(ug/uL)/mosq` <- lipid.values3$`lipid_diluconc(ug/uL)`*10
lipid.values3

lipid.values3$`lipid(ug)/mosq` <- round(lipid.values3$`lipid_conc(ug/uL)/mosq`*180, 0)
lipid.values3

#export your results in excel format
write_xlsx(lipid.values1,"L:\\Tovi\\Cedric_Kouam\\lipid_survival\\rdm6rep2lipidvalues1.xlsx")
write_xlsx(lipid.values2,"L:\\Tovi\\Cedric_Kouam\\lipid_survival\\rdm6rep2lipidvalues2.xlsx")
write_xlsx(lipid.values3,"L:\\Tovi\\Cedric_Kouam\\lipid_survival\\rdm6rep2lipidvalues3.xlsx")

#Plot a histogram of unknown samples using ggplot

mosq.lip1 <- ggplot(lipid.values1, aes(x = mosqid1 , y = `lipid(ug)/mosq`))+
  geom_bar(fill='blue', stat = "identity")+
  labs(x = "Individual Mosquitoes", y = "Lipid mass(ug)/mosq",
       title = "Lipid Mass per Individual Mosquito Plate 1")+
  theme(axis.text.x = element_text(face = 'bold', size = 12, angle = 45))+
  geom_text(aes(label = `lipid(ug)/mosq`), vjust = -0.2, size = 5)
ggsave("rdm6rep2_lip.massbymosq1.jpg", dpi=300, height=6, width=8)
mosq.lip1

mosq.lip2 <- ggplot(lipid.values2, aes(x = mosqid2 , y = `lipid(ug)/mosq`))+
  geom_bar(fill='blue', stat = "identity")+
  labs(x = "Individual Mosquitoes", y = "Lipid mass(ug)/mosq",
       title = "Lipid Mass per Individual Mosquito Plate 2")+
  theme(axis.text.x = element_text(face = 'bold', size = 12, angle = 45))+
  geom_text(aes(label = `lipid(ug)/mosq`), vjust = -0.2, size = 5)
ggsave("rdm6rep2_lip.massbymosq2.jpg", dpi=300, height=6, width=8)
mosq.lip2

mosq.lip3 <- ggplot(lipid.values3, aes(x = mosqid3 , y = `lipid(ug)/mosq`))+
  geom_bar(fill='blue', stat = "identity")+
  labs(x = "Individual Mosquitoes", y = "lipid mass(ug)/mosq",
       title = "lipid Mass per Individual Mosquito Plate 3")+
  theme(axis.text.x = element_text(face = 'bold', size = 12, angle = 45))+
  geom_text(aes(label = `lipid(ug)/mosq`), vjust = -0.2, size = 5)
ggsave("rdm6rep2_lipid.massbymosq3.jpg", dpi=300, height=6, width=8)
mosq.lip3

(fri1|frim1|friuk1)/(fri2|frim2|friuk2)
(mosq.glu1|mosq.glu2)

(fri3|frim3|friuk3)/(mosq.lip3)

(fri|frim)/(friuk|mosq.glu)
ggsave("25may22pilotglu.jpg", dpi = 300, height = 6, width = 8)