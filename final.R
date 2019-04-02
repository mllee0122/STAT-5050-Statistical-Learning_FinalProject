# input data
pokemon = read.csv("data/pokemon_modified.csv")
combats = read.csv("data/combats.csv")
# 
rownames(pokemon) = pokemon[,2]
pokemon = pokemon[,-c(1:4)]
# pokemon[,8:27] = ifelse(pokemon[,8:27]==TRUE,1,0)

combats_train = data.frame(pokemon[combats[,1],],pokemon[combats[,2],])
Winner = ifelse(combats$Winner==combats$First_pokemon,TRUE,FALSE)
combats_train = data.frame(combats_train,Winner)
rownames(combats_train) = c(1:nrow(combats_train))
write.csv(combats_train, file = "data/combats_train.csv",row.names=FALSE)

combats_test = combats_train[-c(1:40000),]
combats_train = combats_train[1:40000,]
# 
# linear_model = lm(Winner~.,data=combats_train)
# y = predict(linear_model, newdata = combats_test)
# y = ifelse(y>0.5, TRUE,FALSE)
# linear_model_MSE = sum((combats_test$Winner-y)^2)/5000
# linear_model_MSE
# 
# linear_model_tab = table(combats_test$Winner, y)
# 
# linear_model_auc = (linear_model_tab["FALSE", "FALSE"] + linear_model_tab["TRUE", "TRUE"])/sum(linear_model_tab)
# linear_model_sens = linear_model_tab["FALSE", "FALSE"]/sum(linear_model_tab[,"FALSE"])
# linear_model_spec = linear_model_tab["TRUE", "TRUE"]/sum(linear_model_tab[,"TRUE"])

# pokemon_train = pokemon[1:718,]
# pokemon_test = pokemon[-c(1:718),]
# 
# linear_model = lm(Legendary~.,data=pokemon_train)
# y = predict(linear_model, newdata = pokemon_test)
# y = ifelse(y>0.5, TRUE,FALSE)
# linear_model_MSE = sum((pokemon_train$Legendary-y)^2)/5000
# linear_model_MSE
# 
# linear_model_tab = table(pokemon_test$Legendary, y)
# 
# linear_model_auc = (linear_model_tab["FALSE", "FALSE"] + linear_model_tab["TRUE", "TRUE"])/sum(linear_model_tab)
# linear_model_sens = linear_model_tab["FALSE", "FALSE"]/sum(linear_model_tab[,"FALSE"])
# linear_model_spec = linear_model_tab["TRUE", "TRUE"]/sum(linear_model_tab[,"TRUE"])

# sample = sample()
pokemon_train = pokemon[1:718,]
pokemon_test = pokemon[-c(1:718),]

linear_model = lm(Sp..Def~.,data=pokemon_train)
y = predict(linear_model, newdata = pokemon_test)
# y = ifelse(y>0.5, TRUE,FALSE)
linear_model_MSE = mean((pokemon_test$Sp..Atk-y)^2)
linear_model_MSE

# linear_model_tab = table(pokemon_test$Mega, y)
# 
# linear_model_auc = (linear_model_tab["FALSE", "FALSE"] + linear_model_tab["TRUE", "TRUE"])/sum(linear_model_tab)
# linear_model_sens = linear_model_tab["FALSE", "FALSE"]/sum(linear_model_tab[,"FALSE"])
# linear_model_spec = linear_model_tab["TRUE", "TRUE"]/sum(linear_model_tab[,"TRUE"])


