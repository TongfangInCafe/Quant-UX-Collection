#read the dataset into an R variable using the read.csv(file) function
dataTwoWayInteraction <- read.csv("dataset_ANOVA_TwoWayInteraction.csv")
#display the data
dataTwoWayInteraction

#use anova(object) to test the omnibus hypothesis
#Are main effects or interaction effects present in the independent variables?
anova(lm(StressReduction ~ Treatment * Gender, dataTwoWayInteraction))


#use subset(data, condition) to divide the original dataset along the treatment groups
#medical subset
dataMedical <- subset(dataTwoWayInteraction, Treatment == "medical")
#mental subset
dataMental <- subset(dataTwoWayInteraction, Treatment == "mental")
#physical subset
dataPhysical <- subset(dataTwoWayInteraction, Treatment == "physical")

#run ANOVA on the treatment subsets to investigate the impacts of gender within each
#medical
anova(lm(StressReduction ~ Gender, dataMedical))

anova(lm(StressReduction ~ Gender, dataPhysical))