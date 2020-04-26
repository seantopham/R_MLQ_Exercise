## 2 ##### Test Structure of 360 Leadership Scale https://www.youtube.com/watch?v=rpFL5hI5qzw includes reliability, factor analyses, and descriptive table  

#Uses mlq.csv and mlq_meta.csv
# Constructs Measured:
# ac - Attributed Charisma
# mbep - Passive managment by exception
# cr - Contigent Reward
# ic - Individualized Considertion
# is - Intellectual Stimulation
# mbea - Active managment by exception

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr, psych, Hmisc, lavaan, lm.beta)

head(mlq)
dim(mlq)
View(mlq)
sapply(mlq, function(X) sum(is.na(X))) #check for missing data
#Create quick reference lists; using the "v" is arbitrary
v <- list()
v$mlq_items <- mlq_meta$id #the mlq_meta is really not necessary, these list variables can be created in other ways and the mlq_meta is not used anywhere else in this analysis below
v$mlq_scales <- unique(mlq_meta$scale)
v$mlq_transactional <- c("mbep", "mbea", "cr")
v$mlq_transformational <- c("ac", "is", "ic")
v$mlq_outcome <- "goal"
#Exploratory Factor Analysis; there are 6 proposed factors in the mlq.csv
?scree
scree((mlq[,v$mlq_items])) #Produces a scree plot to visually see the factor sctructure; scree() is in the psych package; rule of thumb is eigen value of 1 or more should be retained.
?fa.parallel
fa.parallel((mlq[,v$mlq_items])) #More safisticated version than scree plot because it also compares the eigen values to random data. Notice triangle it is getting 6 factors with this data 
#Examine 6 factor Exploratory Factor Analysis
fac1 <- factanal(mlq[,v$mlq_items], 6, rotation = "promax") #puting factanal() in an element allows you to call fac1$xxxxx and get specifics
print(fac1, cutoff = .30)
facload <- round(unclass(fac1$loadings), 2) #Factor loadings
print(facload)
#Confirmatory Factor Analysis NOTE: Uses lavaan package
# library("lavaan")
models <- ls()
fits <- ls()

# 1 factor model
models$m1 <- 
  ' global =~ mbea1 + mbea2 + mbea3 + mbep1 + mbep2 + mbep3 + cr1 + cr2 + cr3 + ac1 + ac2 + ac3 + is1 + is2 + is3 + ic1 + ic2 + ic3'
# 2 factor model
models$m2 <-
  ' transformational =~ ac1 + ac2 + ac3 + is1 + is2 + is3 + ic1 + ic2 + ic3
          transactional =~ ac1 + ac2 + ac3 + is1 + is2 + is3 + ic1 + ic2 + ic3'
# 6 factor model
models$m3 <-
  ' mbea =~ mbea1 + mbea2 + mbea3
          mbep =~ mbep1 + mbep2 + mbep3
          cr =~ cr1 + cr2 + cr3
          ac =~ ac1 + ac2 + ac3
          is =~ is1 + is2 + is3
          ic =~ ic1 + ic2 + ic3'

fits$m1 <- lavaan::cfa(models$m1, data = mlq)
fits$m2 <- lavaan::cfa(models$m2, data = mlq)
fits$m3 <- lavaan::cfa(models$m3, data = mlq)

summary(fits$m3, fit.measures = TRUE)
standardizedsolution(fits$m3) #items are loading above .6 or .7 thats good

# Below is the fit data that can be reported in a table and such. cfi rule of thumbs is at least above .9 ideally above .95
?fitmeasures #in lavaan package
fitmeasures(fits$m1, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
fitmeasures(fits$m2, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
fitmeasures(fits$m3, c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

# Does the same thing as the 3 individually run fitmeasures above but puts it into a nice single table. Fucking thing below will not work!
v$fitindicies <- c("npar", "chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
round(sapply(fits, function(X) fitmeasures(X)[v$fitindicies]), 3)

# Reliability Analysis

keys.list <- list(mbea=c("mbea1","mbea2","mbea3"),
                  mbep=c("mbep1","mbep2","mbep3"),cr=c("cr1","cr2","cr3"),
                  ac=c("ac1","ac2","ac3"), is=c("is1","is2","is3"), ic=c("ic1","ic2","ic3")) #Creating keys list
scoreItems(keys.list, mlq[, v$mlq_items])
scores <- scoreItems(keys.list, mlq[, v$mlq_items])
summary(scores)
describe(scores$scores)
round(scores$alpha, 2) #Cronbachs Alpha; rule of thumb above .8 pretty good, above .7 is still ok, .6 and .5 will probably not sit well with reviewers 
round(scores$item.cor, 2)

# Descriptive Statistics and Correlations

#Getting rid of people/cases with missing goal data for simplicity sake of below in this descriptive table example
mlq$missing_count <- apply(mlq[,c(v$mlq_scales, v$mlq_outcome)], 1, function(x) sum(is.na(x))) #creating missing count column
mlq_c <- mlq[mlq$missing_count == 0, ] #retaining only those rows with 0 in missing count column

desc <- list()
desc$cor <- cor(mlq_c[,c(v$mlq_scales, "goal")])
desc$mean <- sapply(mlq_c[,c(v$mlq_scales, "goal")], mean)
desc$sd <- sapply(mlq_c[,c(v$mlq_scales, "goal")], sd)

desc$tab <- data.frame(mean = desc$mean, sd = desc$sd, desc$cor) # List created above made into dataframe

rtab <- round(desc$tab, 2)
print(rtab)

# Regression Models
v$mlq_transactional
v$mlq_transformational

fits$transactional <- lm(goal ~ mbep + mbea + cr, mlq) #Regression goal is the outcome measure ~ means predicted by
fits$transformtional <- lm(goal ~ ac + is + ic, mlq)
fits$all <- lm(goal ~ mbep + mbea + cr + ac + is + ic, mlq)
?lapply
lapply(fits, summary)
summary(fits$transactional)$adj.r.squared #Review the adjusted r-squared of each particular model
summary(fits$transformtional)$adj.r.squared
summary(fits$all)$adj.r.squared
# below should return each adjusted r squared at once but does NOT work. Use above individual method
lapply(fits, function(x) summary(x)$adj)

#Does adding transactional or transformational add prediction
anova(fits$transactional, fits$all)
anova(fits$transformtional, fits$all) #pay attention to significance codes. Adding transformational to transactional did significantly improve prediction. The *** or ** or * are the significance level
summary(fits$all)

#Standardized betas
library("lm.beta") #below uses the lm.beta package
summary(lm.beta::lm.beta(fits$all)) #shows that the main significant predictors were mbep and mbea
