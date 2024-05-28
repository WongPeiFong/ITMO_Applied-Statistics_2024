library (ade4)
library (vegan)
data (doubs)
help (doubs)
doubs.spe <- doubs$poi
doubs.env <- doubs$mil
doubs.spa <- doubs$xy
str(doubs)
summary(doubs$fish)
summary(doubs$env)
summary(doubs$xy)
summary(doubs$species)

model.matrix <- cbind(doubs$env, doubs$xy)

rda_result <- rda(doubs$fish ~ doubs$env)

partial_rda_result <- rda(doubs$fish ~ doubs$env + doubs$xy)

anova(rda_result, partial_rda_result)
