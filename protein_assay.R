#loading packages
packages <- c("readxl", "ggpmisc", "tidyverse", "rlang", "ggpubr", "modelr", "writexl", "patchwork")
lapply(packages, library, character.only = T)
setwd("L:\\Tovi\\Cedric_Kouam//Protein_Survival\\")
Prot.data <- read_excel("proteinabs.xlsx", sheet = "pro_01mar23")

#creating data frame for the first replicate
absorbance <-  Prot.data$Abs
protein <- Prot.data$Pro_Std
std.pro.df <- na.omit(data.frame(protein, absorbance))
std.pro.df.mean <- aggregate(absorbance ~ protein, std.pro.df, FUN = mean)
my.formula <- y~x

#Plotting results from the first replicate of 12Apr22
pro.plot <- ggplot(data = std.pro.df, aes(x = absorbance, y = protein))+
  labs(x= "absorbances (nm)", y= "protein concentration (ug/uL)", title = "Protein Calibration")+
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = my.formula, lty ="dashed")+
  stat_regline_equation(label.x = 0.4, label.y = 0.85)+
  stat_cor(label.x = 0.4, label.y = 0.8)+
  geom_point(color= "Blue")
  ggsave("18may22_stdprotein.jpg", dpi=300, height=6, width=8)
pro.plot

pro.plot2 <- ggplot(data = std.pro.df.mean, aes(x = absorbance, y = protein))+
  labs(x= "absorbances (nm)", y= "protein concentration (ug/uL)", title = "Protein Calibration Average")+
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = my.formula, lty ="dashed")+
  stat_regline_equation(label.x = 0.4, label.y = 0.85)+
  stat_cor(label.x = 0.4, label.y = 0.8)+
  geom_point(color= "Blue")
ggsave("18may22_stdproteinmean.jpg", dpi=300, height=6, width=8)
pro.plot2


#First create your unknown data frame
unknown.abs <- na.omit(data.frame(MosqID = Prot.data$MosqID,
                                 absorbance = Prot.data$MosqAbs))
unknown.abs


#Aggregate the unknown data frame
protein.unknown.df <- aggregate(absorbance ~ MosqID, data = unknown.abs,
                         FUN = mean)
protein.unknown.df


#make sure your unknown values have the same name as the values in your model
#Not very necessary here but see the example below for just in case
#eg. unknown df values are "yvalues" like in the original df.(very important)
#samples.abs <- data.frame(absorbance = protein.unknown.df$absorbance)

#linear model
protein.lm <- lm(formula = protein ~ absorbance, data = std.pro.df.mean)


#Predict protein conc (ug/uL) of your unknown and give variable name
protein.values <- add_predictions(protein.unknown.df, protein.lm, 
                                  var = "Unknown Conc(ug/uL)")
protein.values
#or you can also predict using predict.lm function
#protein.values <- predict.lm(protein.lm, protein.unknown.df,var = "Unknown Conc(ug/uL)")

#add a column for the protein mass/mosq by * by the initial vol. 180uL
protein.values$`protein mass(ug)/mosq` <- round(protein.values$`Unknown Conc(ug/uL)`*180, 0)
protein.values
#export your data as excel
write_xlsx(protein.values,"L:\\Tovi\\Cedric_Kouam//RData&codes\\Exp_Samp_Trial\\proteinvalues.xlsx")

#add the predicted points on your protein calibration curve
pro.plot3 <- ggplot()+
  labs(x= "absorbances (nm)", y= "protein concentration (ug/uL)", 
       title = "Protein Calibration Average with unknown ",
       color="Legend")+
  geom_smooth(method = "lm", data = std.pro.df.mean,lty = "dashed",
              aes(x = absorbance, y= protein),
              color = "black", se = FALSE, formula = my.formula)+
  stat_regline_equation(data = std.pro.df.mean,
                        aes(x = absorbance, y= protein),label.x = 0.4, label.y = 0.85)+
  stat_cor(data = std.pro.df.mean,
           aes(x = absorbance, y= protein),label.x = 0.4, label.y = 0.8)+
  geom_point(data = std.pro.df.mean, aes(x = absorbance, y = protein, 
            color= "standard points"))+
  geom_point(data = protein.values, aes(x = absorbance, y=`Unknown Conc(ug/uL)`,
            color= "unknown samples"))
ggsave("18may22_samples.std.jpg", dpi=300, height=6, width=8)
pro.plot3
#add annotations for standard points


#Plot a histogram of unknown samples using ggplot

mosq.prot <- ggplot(protein.values, aes(x = MosqID, y = `protein mass(ug)/mosq`))+
             geom_bar(fill='blue', stat = "identity")+
             labs(x = "Individual Mosquitoes", y = "Protein mass(ug)/mosq",
             title = "Protein Mass per Individual Mosquito")+
             theme(axis.text.x = element_text(face = 'bold', size = 12, angle = 45))+
             geom_text(aes(label = `protein mass(ug)/mosq`), vjust = -0.2, size = 5)
            ggsave("18may22_pro.massbymosq.jpg", dpi=300, height=6, width=8)
mosq.prot

(pro.plot|pro.plot2)/(pro.plot3|mosq.prot)
ggsave("18may22pilotprotein.jpg", dpi = 300, height = 6, width = 8)
