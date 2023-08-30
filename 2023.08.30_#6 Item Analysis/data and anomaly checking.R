library(tidyverse)
library(gridExtra)

# read data
dat <- readxl::read_excel("resource/Grit - Studi Kasus.xlsx")

# check data & anomaly
dim(dat)
names(dat)
str(dat)

  ## KM
  par(mfrow = c(3,3))
  for (i in 6:13) {
    hist(unlist(dat[,i]), main = names(dat[,i]), xlab = "Skor")
  }
  plot.new()
  
  ## PU
  par(mfrow = c(3,4))
  for (i in 14:24) {
    hist(unlist(dat[,i]), main = names(dat[,i]), xlab = "Skor")
  }
  plot.new()
  
  ## AD
  par(mfrow = c(3,5))
  for (i in 25:37) {
    hist(unlist(dat[,i]), main = names(dat[,i]), xlab = "Skor")
  } # AD3 contains 8 value, should be removed
  plot.new()
  
  ## Cons
  par(mfrow = c(2,3))
  for (i in 38:43) {
    hist(unlist(dat[,i]), main = names(dat[,i]), xlab = "Skor")
  }
  plot.new()
  
  ## AD3: anomaly detected
  par(mfrow = c(1,1))
  hist(unlist(dat[,"AD3"]), main = "AD3", xlab = "Skor")
  
  ## remove row with value 8 in AD3
  dat <- dat[-which(dat[,"AD3"] == 8),]

  ## remove NAs
  dim(dat)
  dat <- drop_na(dat)
  dim(dat)

# 3 rows removed
# write.csv(dat, "data grid cleaned.csv")

  
  
# CFA
# Factors
Factor1 =~ lambda_1_1*CO1 + lambda_1_2*CO2 + lambda_1_3*CO3 + lambda_1_4*CO4 + lambda_1_5*CO5 + lambda_1_6*CO6 + lambda_1_7*CO7 + lambda_1_8*CO8
Factor2 =~ lambda_2_1*PE1 + lambda_2_2*PE2 + lambda_2_3*PE3 + lambda_2_4*PE4 + lambda_2_5*PE5 + lambda_2_6*PE6 + lambda_2_7*PE7 + lambda_2_8*PE8 + lambda_2_9*PE9 + lambda_2_10*PE10 + lambda_2_11*PE11
Factor3 =~ lambda_3_1*AD1 + lambda_3_2*AD2 + lambda_3_3*AD3 + lambda_3_4*AD4 + lambda_3_5*AD5 + lambda_3_6*AD6 + lambda_3_7*AD7 + lambda_3_8*AD8 + lambda_3_9*AD9 + lambda_3_10*AD10 + lambda_3_11*AD11 + lambda_3_12*AD12 + lambda_3_13*AD13

cfa <- lavaan::cfa(
  "CO =~ lambda_1_1*CO1 + lambda_1_2*CO2 + lambda_1_3*CO3 + lambda_1_4*CO4 + lambda_1_5*CO5 + lambda_1_6*CO6 + lambda_1_7*CO7 + lambda_1_8*CO8
  PE =~ lambda_2_1*PE1 + lambda_2_2*PE2 + lambda_2_3*PE3 + lambda_2_4*PE4 + lambda_2_5*PE5 + lambda_2_6*PE6 + lambda_2_7*PE7 + lambda_2_8*PE8 + lambda_2_9*PE9 + lambda_2_10*PE10 + lambda_2_11*PE11
  AD =~ lambda_3_1*AD1 + lambda_3_2*AD2 + lambda_3_3*AD3 + lambda_3_4*AD4 + lambda_3_5*AD5 + lambda_3_6*AD6 + lambda_3_7*AD7 + lambda_3_8*AD8 + lambda_3_9*AD9 + lambda_3_10*AD10 + lambda_3_11*AD11 + lambda_3_12*AD12 + lambda_3_13*AD13",
  data = dat,
  estimator = "DWLS"
)

cfa_plot <- semPlot::semPaths(cfa,
                  whatLabels = "std",
                  rotation = 2,
                  sizeMan = 5,
                  node.width = 1,
                  edge.label.cex = .7)

semptools::set_cfa_layout(cfa_plot,
                          indicator_order = c("CO1","CO2","CO3","CO4","CO5","CO6","CO7","CO8","PE1","PE2","PE3","PE4","PE5","PE6","PE7","PE8","PE9","PE10","PE11","AD1","AD2","AD3","AD4","AD5","AD6","AD7","AD8","AD9","AD10","AD11","AD12","AD13"),
                          indicator_factor = c("CO","CO","CO","CO","CO","CO","CO","CO","PE","PE","PE","PE","PE","PE","PE","PE","PE","PE","PE","AD","AD","AD","AD","AD","AD","AD","AD","AD","AD","AD","AD","AD"),
                          fcov_curve = 3)


# Get factor loadings data
summary(cfa, standardized = TRUE)


relCO <- alpha(dat[1:8])
relCO$total$raw_alpha
relCO$item.stats
