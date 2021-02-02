# Testing Freshman and Others difference
# 
source("TidyAndAnalysisCAPS.R")

caps <- caps %>%
  mutate(isFreshman = (Class == "Freshman"))

caps.f <- caps[caps$isFreshman,]
caps.n <- caps[!caps$isFreshman,]

# Comparison between freshman and other
mean.f <- mean(covid.f$awareness)
mean.o <- mean(covid.o$awareness)
sd.f <- sd(covid.f$awareness)
sd.o <- sd(covid.o$awareness)
n.f <-length(covid.f$awareness)
n.o <-length(covid.o$awareness)

# Now construct the test statistic
se <-sqrt((sd.f^2)/n.f + (sd.o^2)/n.o)
t <-(mean.f-mean.o)/se
print(2*((1-pt(abs(t), df=min(n.f, n.o)))))

# Abstraction using functions
t_test <- function(data_a, data_b){
  # Remove NA from vector
  data_a <- data_a[!is.na(data_a)]
  data_b <- data_b[!is.na(data_b)]
  
  mean.1 <- mean(data_a)
  cat("Mean of A: ", mean.1, "\n")
  mean.2 <- mean(data_b)
  cat("Mean of B: ", mean.2, "\n")
  sd.1 <- sd(data_a)
  sd.2 <- sd(data_b)
  n.1 <-length(data_a)
  n.2 <-length(data_b)
  
  # Now construct the test statistic
  se <-sqrt((sd.1^2)/n.1 + (sd.2^2)/n.2)
  t <-(mean.1-mean.2)/se
  cat("t-score: ", t, "\n")
  p <- 2*((1-pt(abs(t), df=n.1+n.2-2)))
  cat("P(|t| >= ", abs(t), ") =", p ,"\n")
}

# testing all variables between freshman vs others
t_test(caps.f$`Serious Issue`, caps.n$`Serious Issue`)
t_test(caps.f$`Personal Mental Health`, caps.n$`Personal Mental Health`)
t_test(caps.f$`Stress Level`, caps.n$`Stress Level`)
t_test(caps.f$`Serious Issue`, caps.n$`Serious Issue`)
t_test(caps.f$Informed, caps.n$Informed)
t_test(caps.f$`Academic Performance`, caps.n$`Academic Performance`)
t_test(caps.f$`CAPS Adequacy`, caps.n$`CAPS Adequacy`)
t_test(caps.f$`CAPS Usage`, caps.n$`CAPS Usage`)
t_test(caps.f$Funding, caps.n$Funding)
t_test(caps.f$`No-show Fee`, caps.n$`No-show Fee`)
t_test(caps.f[caps.f$ASUN != 0,]$`ASUN`, caps.n[caps.n$ASUN != 0,]$`ASUN`)


caps.on <- caps[caps$Online == 1,]
caps.off <- caps[caps$Online == 0,]

# testing all variables between online vs offline
t_test(caps.off$`Serious Issue`, caps.on$`Serious Issue`)
t_test(caps.off$`Personal Mental Health`, caps.on$`Personal Mental Health`)
t_test(caps.off$`Stress Level`, caps.on$`Stress Level`)
t_test(caps.off$Informed, caps.on$Informed)
t_test(caps.off$`Academic Performance`, caps.on$`Academic Performance`)
t_test(caps.off$`CAPS Adequacy`, caps.on$`CAPS Adequacy`)
t_test(caps.off$Funding, caps.on$Funding)
t_test(caps.off$`No-show Fee`, caps.on$`No-show Fee`)
t_test(caps.off[caps.off$ASUN != 0,]$`ASUN`, caps.on[caps.on$ASUN != 0,]$`ASUN`)

caps.know <- caps[caps$`CAPS Usage` == 'Yes',]
caps.dk <- caps[!(caps$`CAPS Usage` == 'Yes'),]

# Knowledge
t_test(caps.know$`Serious Issue`, caps.dk$`Serious Issue`)
t_test(caps.know$`Personal Mental Health`, caps.dk$`Personal Mental Health`)
t_test(caps.know$`Stress Level`, caps.dk$`Stress Level`)
t_test(caps.know$Informed, caps.dk$Informed)
t_test(caps.know$`Academic Performance`, caps.dk$`Academic Performance`)
t_test(caps.know$`CAPS Adequacy`, caps.dk$`CAPS Adequacy`)
t_test(caps.know$Funding, caps.dk$Funding)
t_test(caps.know$`No-show Fee`, caps.dk$`No-show Fee`)
t_test(caps.know[caps.know$ASUN != 0,]$`ASUN`, caps.dk[caps.dk$ASUN != 0,]$`ASUN`)

caps.no0 <- caps[caps$ASUN != 0,]
cor(complete.cases(caps$`Stress Level`), 
    complete.cases(caps$`Personal Mental Health`))

lmod <- lm(`Academic Performance` ~ `Stress Level`, caps)
summary(lmod)

lmod <- lm(`ASUN` ~ Informed, caps.no0)
summary(lmod)

lmod <- lm(`CAPS Adequacy` ~ Informed, caps)
summary(lmod)

lmod <- lm(ASUN ~ `CAPS Adequacy`, caps.no0)
summary(lmod)

ggplot(data = caps, aes(x = Informed, y=`CAPS Adequacy`)) +
  geom_point()

lmod <- lm(ASUN ~ Funding, caps.no0)
summary(lmod)

lmod <- lm(ASUN ~ `No-show Fee`, caps.no0)
summary(lmod)

lmod <- lm(`Serious Issue` ~ Funding, caps)
summary(lmod)

caps.female <- caps[caps$Gender == "Female",]
caps.male <- caps[caps$Gender == "Male",]

# testing all variables between male vs female
t_test(caps.female$`Serious Issue`, caps.male$`Serious Issue`)
t_test(caps.female$`Personal Mental Health`, caps.male$`Personal Mental Health`)
t_test(caps.female$`Stress Level`, caps.male$`Stress Level`)
t_test(caps.female$Informed, caps.male$Informed)
t_test(caps.female$`Academic Performance`, caps.male$`Academic Performance`)
t_test(caps.female$`CAPS Adequacy`, caps.male$`CAPS Adequacy`)
t_test(caps.female$Funding, caps.male$Funding)
t_test(caps.female$`No-show Fee`, caps.male$`No-show Fee`)
t_test(caps.female[caps.female$ASUN != 0,]$`ASUN`, caps.male[caps.male$ASUN != 0,]$`ASUN`)
