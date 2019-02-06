dataset <- read.csv("/Users/michaelspainhour/Desktop/Scientific/FEV.csv",stringsAsFactors=FALSE)
fev_and_smoke <- data.frame(dataset$fev, dataset$smoke, stringsAsFactors=FALSE)
fev_and_smoke$dataset.smoke[fev_and_smoke$dataset.smoke=="current smoker"] <- 1
fev_and_smoke$dataset.smoke[fev_and_smoke$dataset.smoke=="non-current smoker"] <- 0
 fev_and_smoke$dataset.smoke <- as.numeric(as.character(fev_and_smoke$dataset.smoke))
cor(fev_and_smoke$dataset.fev, fev_and_smoke$dataset.smoke)
cor(dataset$height, dataset$fev)
scatter.smooth(x=dataset$height, y=dataset$fev, main='Height and FEV', xlab='Height', ylab='FEV')
plot(dataset$height, dataset$fev, main='Height and FEV', abline(lm(fev~height, data=dataset)), xlab='Height', ylab='FEV')
cor(dataset$age, dataset$fev)
plot(dataset$age, dataset$fev, main='Age and FEV', abline(lm(fev~age, data=dataset)), xlab='Age', ylab='FEV', col='red')
hist(dataset$fev, main='FEV Frequencies', xlab='FEV', col='blue')
hist(dataset$height, main='Height Frequencies', xlab='Height in inches', col='red')
smoker_count <- ldply(dataset$smoke, function(c) sum(c=="smoker"))
non_smoker_count <- ldply(dataset$smoke, function(c) sum(c=="non-current smoker"))
smoker_nonsmoker_freqs <- table(dataset$smoke)
smoker_dataframe <- data.frame(smokers=c('current smoker', 'non-current smoker'), freqs=c(smoker_nonsmoker_freqs[["current smoker"]][1], smoker_nonsmoker_freqs[["non-current smoker"]][1]))
pie(smoker_dataframe$freqs, smoker_dataframe$smokers, main='Smokers and Non-Smokers', col=rainbow(2))

