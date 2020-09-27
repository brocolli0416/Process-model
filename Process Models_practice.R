install.packages("lavaan")
install.packages("semPlot")
install.packages("magrittr")

library(tidyverse) # Tidyverse!
library(lavaan) # This is our path modelling and SEM package
library(semPlot) # Makes nice plots for lavaan
library(magrittr)
library(foreign)
library(dplyr)

interest_data = read.spss("interest.sav", to.data.frame=TRUE)

doctor_model = '
  social.dominance ~ a*worry + b*sociability
  analytical.reasoning ~ c*math + d*reading
  doctor ~ e*social.dominance + f*analytical.reasoning

  worry.thru.dominance:=a*e
  sociability.thru.dominance:=b*e
  math.thru.reasoning:=c*f
  reading.thru.reasoning:=d*f
'

doctor_analysis = sem(model=doctor_model, data = interest_data)
doctor_analysis %>%
  semPaths(whatLabels = 'est',
           rotation = 2,
           nCharNodes = 0,
           label.cex = 2
           )

doctor_analysis %>%
  summary(fit.measures = TRUE, # I don't know why the function doesn't print fit stats by default, but you have to specifically ask for them
          standardized = TRUE)

## Nested model where "worry" is 0

doctor_nstd_model = '
social.dominance ~ 0*worry + b*sociability
analytical.reasoning ~ c*math + d*reading
doctor ~ e*social.dominance + f*analytical.reasoning

sociability.thru.dominance:=b*e
math.thru.reasoning:=c*f
reading.thru.reasoning:=d*f
'

doctor_nstd_analysis = sem(model=doctor_nstd_model, data = interest_data)
doctor_nstd_analysis %>%
  semPaths(whatLabels = 'est',
           rotation = 2,
           nCharNodes = 0,
           label.cex = 2
  )

doctor_nstd_analysis %>%
  summary(fit.measures = TRUE, # I don't know why the function doesn't print fit stats by default, but you have to specifically ask for them
          standardized = TRUE)
##Compare the two models
anova(doctor_analysis, doctor_nstd_analysis)
### No difference observed when "worry" is omitted

doctor_nnstd_model = '
  social.dominance ~ a*sociability
  analytical.reasoning ~ b*math + c*reading
  doctor ~ d*social.dominance + e*analytical.reasoning + f*education

  education ~~ math
  education ~~ reading
  math ~~ reading
  
  sociability.thru.dominance:=a*d
  math.thru.reasoning:=b*e
  reading.thru.reasoning:=c*e
'

doctor_nnstd_analysis = sem(model = doctor_nnstd_model, data = interest_data)
doctor_nnstd_analysis %>%
  semPaths(whatLabels = 'est',
           rotation = 2,
           nCharNodes = 0,
           label.cex = 2
  )

anova(doctor_analysis, doctor_nnstd_analysis)

abs(AIC(doctor_analysis) - AIC(doctor_nnstd_analysis))
AIC(doctor_analysis)
AIC(doctor_nnstd_analysis)

abs(AIC(doctor_analysis) - AIC(doctor_nstd_analysis))
AIC(doctor_analysis)
AIC(doctor_nstd_analysis)

tli(doctor_analysis)
