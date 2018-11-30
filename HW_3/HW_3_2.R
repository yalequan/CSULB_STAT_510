# (a) Fit the quadratic model to the data, then conduct a test to determine if incidence rate is curvilinearly related to estimated rate. (Use α = .05.)

INFECTION <- read_csv("Documents/School/CSULB/CSULB - M.A.S./STAT 510 - Regression/HW_3/INFECTION.csv")

Rate = INFECTION$RATE
Estimation = INFECTION$EST

quadratic_model = lm(Rate ~ Estimation + I(Estimation^2))
summary(quadratic_model)

#(b) Construct a scatterplot for the data. Locate
#the data point for Botulism on the graph.
#What do you observe?


library(ggplot2)
ggplot(INFECTION, aes(x = Estimation, y = Rate)) + geom_point()

#(c) Repeat part a, but omit the data point for
#Botulism from the analysis. Has the fit of the
#model improved? Explain.

INFECTION_SUBSET = subset(INFECTION, INFECTION !="Botulism", select = c(INFECTION, RATE, EST))
View(INFECTION_SUBSET)

Rate_Subset = INFECTION_SUBSET$RATE
Estimation_Subset = INFECTION_SUBSET$EST

quadratic_model_subset = lm(Rate_Subset ~ Estimation_Subset + I(Estimation_Subset^2))
summary(quadratic_model_subset)
