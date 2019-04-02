
pokemon_dataset = read.csv("pokemon_modified.csv")
combat = read.csv("combats.csv")

head(pokemon_dataset) #orginal dataset

rownames(pokemon_dataset) = pokemon_dataset[ ,2]
pokemon_dataset = pokemon_dataset[ ,-c(1:2)] 
summary_poke = summary(pokemon_dataset)
pokemon_dataset = pokemon_dataset[ ,-c(1:2)]
pokemon_dataset[,8:27] = ifelse(pokemon_dataset[,8:27]==TRUE,1,0)

pokemon_dataset[1,]
#for (i in 1:len(combat)){
combat_tr =data.frame(pokemon_dataset[combat[,1],],pokemon_dataset[combat[,2],])
#}
Winner = rep(0, nrow(combat))
Winner[combat$Winner ==  combat$First_pokemon] = 1
head(Winner)
combat_tr = data.frame(combat_tr, Winner)
rownames(combat_tr) = c(1:nrow(combat))

pokemon_dataset[,8:27] = ifelse(pokemon_dataset[,8:27]==TRUE,1,0)
write.csv(combat_tr, file = "combat_tr.csv", row.names = FALSE)

#--------------------#

##### Problem 8 (c) ##
##### (c).i ##########
college_summary = summary(dataset_college)
college_summary
##### (c).ii #########
pairs(dataset_college)
par(mfrow = c(1, 1))
plot(dataset_college[,6:5], main = "Top25perc versus Top10perc")
plot(dataset_college$perc.alumni, dataset_college$Expend, main = "perc.alumni versus Expend", 
      xlab = "perc.alumni", ylab = "Expend")
##### (c).iii ########
plot(dataset_college$Private, dataset_college$Outstate, 
     main = "Boxplot of Private versus Outstate", xlab = "Private", ylab = "Outstate")
##### (c).iv #########
Elite = rep("No", nrow(dataset_college))
fix(Elite)
Elite[dataset_college$Top10perc > 50] = "Yes"
fix(Elite)
Elite = as.factor(Elite)

dataset_college = data.frame(dataset_college, Elite)
college_summary = summary(dataset_college)
college_summary
plot(dataset_college$Elite, dataset_college$Outstate, main = "Boxplot of Elite versus Outstate",
     xlab = "Elite", ylab = "Outstate")
##### (c).v ##########
par(mfrow = c(2, 3))
hist(dataset_college$Top25perc, breaks = 5, main ="Histogram of Top25perc with breaks = 5"
     ,xlab = "Top25perc")
hist(dataset_college$Top25perc, breaks = 15, main ="Histogram of Top25perc with breaks = 15"
     ,xlab = "Top25perc")
hist(dataset_college$Top25perc, breaks = 30, main ="Histogram of Top25perc with breaks = 30"
     ,xlab = "Top25perc")
hist(dataset_college$perc.alumni, breaks = 25, col = 4, main ="Histogram of perc.alumni with breaks = 25"
     , xlab = "perc.alumni")
hist(dataset_college$Expend, breaks = 25, col = 5, main ="Histogram of Expend with breaks = 25"
     , xlab = "Expend")
hist(dataset_college$S.F.Ratio, breaks = 25, col = 6, main ="Histogram of S.F. Ratio with breaks = 25"
     , xlab = "S.F. Ratio")

##### (c).vi #########
private_elite = rep("No", nrow(dataset_college))
private_elite[dataset_college$Private == "Yes" & dataset_college$Elite == "Yes"] = "Yes"
private_elite = as.factor(private_elite)
dataset_college = data.frame(dataset_college, private_elite)

p_e_outstate = rep(0, nrow(dataset_college))
p_e_outstate[dataset_college$private_elite == "Yes"] = 
  dataset_college$Outstate[dataset_college$private_elite == "Yes"]

others_outstate = rep(0, nrow(dataset_college))
others_outstate[dataset_college$private_elite == "No"] = 
  dataset_college$Outstate[dataset_college$private_elite == "No"]

total_outstate_pe = sum(p_e_outstate)
mean_outstate_pe = total_outstate_pe/65
total_outstate_others = sum(others_outstate)
mean_outstate_others = total_outstate_others/712

sort(p_e_outstate)
grep(20100,dataset_college$Outstate)
grep(19900,dataset_college$Outstate)
grep(19840,dataset_college$Outstate)
row.names(dataset_college)

college_summary = summary(dataset_college)
college_summary
#--------------------#


##### Problem 10 (a) #
library(MASS)
data(Boston)
Boston
fix(Boston)
?Boston
#--------------------#

##### Problem 10 (b) #
pairs(Boston)
par(mfrow = c(2, 2))
plot(Boston$lstat, Boston$medv, xlab = "lstat", ylab = "medv", main = "lstat versus medv")
plot(Boston$lstat, Boston$rm, xlab = "lstat", ylab = "rm", main = "lstat versus rm")
plot(Boston$medv, Boston$rm, xlab = "medv", ylab = "rm", main = "medv versus rm")
plot(Boston$dis, Boston$nox, xlab = "dis", ylab = "nox", main = "dis versus nox")
class(Boston$black)
#--------------------#

##### Problem 10 (c) #
par(mfrow = c(1, 2))
plot(Boston$medv, Boston$crim, xlab = "medv", ylab = "crim", main = "medv versus crim")
plot(Boston$dis, Boston$crim, xlab = "dis", ylab = "crim", main = "dis versus crim")
par(mfrow = c(2, 2))
plot(Boston$nox, Boston$crim, xlab = "nox", ylab = "crim", main = "nox versus crim")
plot(Boston$age, Boston$crim, xlab = "age", ylab = "crim", main = "age versus crim")
plot(Boston$rad, Boston$crim, xlab = "rad", ylab = "crim", main = "rad versus crim")
plot(Boston$tax, Boston$crim, xlab = "tax", ylab = "crim", main = "tax versus crim")
#--------------------#

##### Problem 10 (d) #
par(mfrow = c(1, 3))
hist(Boston$crim, breaks = 30, xlab = "crim", col = 1)
hist(Boston$tax, breaks = 30, xlab = "tax", col = 1)
hist(Boston$ptratio, breaks = 30, xlab = "ptratio", col = 1)
#--------------------#

##### Problem 10 (e) #
chas = rep("No", nrow(Boston))
chas[Boston$chas == 1] = "Yes"
chas = as.factor(chas)
summary(chas)
#--------------------#

##### Problem 10 (f) #
summary(Boston)
#--------------------#

##### Problem 10 (g) #
subset(Boston, medv == min(Boston$medv))
summary(Boston)
#--------------------#

##### Problem 10 (h) #
rm_over7 = rep("No", nrow(Boston))
rm_over7[Boston$rm >= 7] = "Yes"
rm_over7 = as.factor(rm_over7)
rm_over8 = rep("No", nrow(Boston))
rm_over8[Boston$rm >= 8] = "Yes"
rm_over8 = as.factor(rm_over8)
summary(rm_over7)
summary(rm_over8)
summary(subset(Boston, Boston$rm >= 8))
#--------------------#

