
library(dplyr)
library(reshape2)
library(repeated)
library(gnlm)
library(MASS) 
library(lme4)
library(dplyr)
library(ggeffects)
library(data.table)   
library(table1) 
source("~/Library/CloudStorage/OneDrive-UniversityofPittsburgh/dissertation_all/simulation/part2_2023/MyFunctions.R") 
 


NLS_1966_to_1976.raw <- fread("Library/CloudStorage/OneDrive-UniversityofPittsburgh/dissertation_all/simulation/part2_2023/NLS_1966_to_1976.csv")
NLS_1966_to_1976 =NLS_1966_to_1976.raw

colname=read.csv("/Users/yang/Library/CloudStorage/OneDrive-UniversityofPittsburgh/dissertation_all/simulation/part2_2023/application_NLS/NLS_1966_to_1976 (2)/NLS_1966_to_1976_name.csv") 
colname$name=row.names(colname)
## check for labels :/Users/yang/Library/CloudStorage/OneDrive-UniversityofPittsburgh/dissertation_all/simulation/part2_2023/application_NLS/NLS_1966_to_1976 (2)/NLS_1966_to_1976_label.cdb

colnames(NLS_1966_to_1976)=c("ID","age_66","race","south_66","smsa_66","nearc2_66","nearc4_66","wage_66","highest_grade_66",
                             "kkw_66","fatheduc_66","motheduc_66","wage_67","highest_grade_67","wage_68","highest_grade_68",
                             "IQ_68","IQ_test_68","wage_69","highest_grade_69","wage_70","highest_grade_70","wage_71","highest_grade_71",
                             "wage_73","highest_grade_73","wage_75","highest_grade_75","wage_76","highest_grade_76")

summary(NLS_1966_to_1976)

# recode -5 (NON-INTERVIEW) and -4(VALID SKIP) as missing
NLS_1966_to_1976 <- NLS_1966_to_1976 %>% mutate_all(~ifelse(. == -5, NA, .))

NLS_1966_to_1976 <- NLS_1966_to_1976 %>% mutate_all(~ifelse(. == -4, NA, .))
num_missing <- colSums(is.na(NLS_1966_to_1976))
table1(~age_66+factor(race)+factor(race)+factor(fatheduc_66>12)+factor(motheduc_66>12)+factor(smsa_66)+IQ_68 | factor(highest_grade_66>12),data=NLS_1966_to_1976)

NLS_1966_to_1976_nonmissing_all <- NLS_1966_to_1976[complete.cases(NLS_1966_to_1976[,c("wage_66","highest_grade_66",
                                                                                       "fatheduc_66","motheduc_66","wage_67","highest_grade_67","wage_68","highest_grade_68",
                                                                                       "kkw_66","wage_69","highest_grade_69","wage_70","highest_grade_70","wage_71","highest_grade_71",
                                                                                       "wage_73","highest_grade_73","wage_75","highest_grade_75","wage_76","highest_grade_76")]),]

NLS_1966_to_1976_nonmissing_all_outcome <- NLS_1966_to_1976[complete.cases(NLS_1966_to_1976[,c("wage_66","highest_grade_66",
                                                                                               "wage_67","highest_grade_67","wage_68","highest_grade_68",
                                                                                               "wage_69","highest_grade_69","wage_70","highest_grade_70","wage_71","highest_grade_71",
                                                                                               "wage_73","highest_grade_73","wage_75","highest_grade_75","wage_76","highest_grade_76")]),]

num_missing2 <- colSums(is.na(NLS_1966_to_1976_nonmissing_all_outcome))
table1(~age_66+factor(race)+factor(race)+factor(fatheduc_66>12)+factor(motheduc_66>12)+factor(smsa_66)+IQ_68 | factor(highest_grade_66>12),data=NLS_1966_to_1976_nonmissing_all_outcome)

library(forcats)
sama.66=as.factor(NLS_1966_to_1976_nonmissing_all_outcome$smsa_66)
NLS_1966_to_1976_nonmissing_all_outcome$sama.66.2level= sama.66 %>% fct_collapse(yes = c(1,2))
table1(~factor(sama.66.2level)  | factor(highest_grade_66>12),data=NLS_1966_to_1976_nonmissing_all_outcome)

# Replace missing values with mean
NLS_1966_to_1976_nonmissing_all_outcome_imputed <- as.data.frame(NLS_1966_to_1976_nonmissing_all_outcome)
for (i in 1:ncol(NLS_1966_to_1976_nonmissing_all_outcome)) {
  var <- NLS_1966_to_1976_nonmissing_all_outcome_imputed[,i]
  var_mean <- mean(var, na.rm = TRUE)
  var[is.na(var)] <- var_mean
  NLS_1966_to_1976_nonmissing_all_outcome_imputed[,i] <- var
} 

num_missing3 <- colSums(is.na(NLS_1966_to_1976_nonmissing_all_outcome_imputed))

data.long=data.table::melt(as.data.table(NLS_1966_to_1976_nonmissing_all_outcome_imputed),id.vars=c("ID","age_66","race","south_66","smsa_66","nearc2_66","nearc4_66",
                                                                                                    "kkw_66","fatheduc_66","motheduc_66","IQ_68","IQ_test_68"),measure= patterns("wage_", "highest_grade_"), variable.name = "year", value.name = c("wage","highest_grade"),variable.factor=FALSE)

#recode year
data.long$year.new = case_when(
  data.long$year == 1 ~ 1966,
  data.long$year == 2 ~ 1967,
  data.long$year == 3 ~ 1968,
  data.long$year == 4 ~ 1969,
  data.long$year == 5 ~ 1970,
  data.long$year == 6 ~ 1971,
  data.long$year == 7 ~ 1973,
  data.long$year == 8 ~ 1975,
  data.long$year == 9 ~ 1976,
)

# dicotomous treatment (education) : education beyond high school as the treatment
data.long$college=ifelse(data.long$highest_grade>12,1,0)
table(data.long$college)


data.long$fatheduc_66.college=ifelse(data.long$fatheduc_66 >12,1,0)
table(data.long$fatheduc_66.college)
data.long$motheduc_66.college=ifelse(data.long$motheduc_66>12,1,0)
table(data.long$motheduc_66.college)

data.long$smsa_66.binary=ifelse(data.long$fatheduc_66 <3,1,0)
table(data.long$smsa_66.binary)

data.long$white=ifelse(data.long$race==1,1,0) 
data.long$black=ifelse(data.long$race==2,1,0)
table(data.long$white)
table(data.long$black)

data.long$age.new=data.long$age+data.long$year.new -rep(1966,nrow(data.long ))

# create binary IV 
# PRESENCE OF ACCREDITED 4 YEAR COLLEGE 
# 935       1 YES, PRIVATE ONLY
# 456       2 YES, PUBLIC ONLY
# 2224       3 YES, BOTH PUBLIC AND PRIVATE 4 YEAR COLLEGE
# 1610       4 NO 4 YEAR COLLEGE
data.long$nearc4_66.binary=ifelse(data.long$nearc4_66<4,1,0)
table(data.long$nearc4_66.binary)


# z(IV): nearc4_66.binary (presence of a nearby 4-year college as an instrument)
# a(trt): college (education beyond high school as the treatment)
# y(outcome): wage (earnings, INCOME FROM WAGES & SALARY)
# y(Y.binary): wage >median wage
# x(covaraites): year, age_66, race, smsa_66(Standard Metropolitan Statistical Areas), kkw_66(KNOWLEDGE OF WORLD OF WORK: a general test of work-related abilities) or IQ_66, fatheduc_66, motheduc_66
# x(covaraites): year, age_66,white,black,smsa_66.binary(1:smsa),IQ_66,fatheduc_66.college,motheduc_66.college 
#x=cbind(data.long$year.new ,data.long$age_66 , data.long$white ,data.long$black , data.long$smsa_66.binary ,data.long$kkw_66 , data.long$fatheduc_66.college ,data.long$motheduc_66 , )

data.long$year.new.scale=scale(data.long$year.new)
data.long$age.new.scale=scale(data.long$age.new)
data.long$IQ_68.scale=scale(data.long$IQ_68)






############################################################################

############################################################################
x=cbind(c=rep(1,nrow(data.long)),year=scale(data.long$year.new) ,age=scale(data.long$age.new) , white=data.long$white ,black=data.long$black , smsa=data.long$smsa_66.binary ,
        IQ=scale(data.long$IQ_68) , fatheduc=data.long$fatheduc_66.college ,motheduc=data.long$motheduc_66.college   ) 


## MLE
# part 1
# alpha: p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9]; 
# zeta: p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18] 

mu_trtment =function(parameter1) {
  alpha=c(parameter1[1],parameter1[2],parameter1[3],parameter1[4],parameter1[5],parameter1[6],parameter1[7],parameter1[8],parameter1[9])
  zeta=c(parameter1[10],parameter1[11],parameter1[12],parameter1[13],parameter1[14],parameter1[15],parameter1[16],parameter1[17],parameter1[18])
  result=data.long$nearc4_66.binary*tanh(x %*% alpha) + 1/(2*(exp(x %*% zeta )-1) ) * 
    (exp(x %*% zeta )*(2-tanh(x %*% alpha))+tanh(x %*% alpha)-sqrt(  (exp(x %*% zeta)*(tanh(x %*% alpha)-2)-tanh(x %*% alpha))^2 +
                                                                       4 * exp(x %*% zeta) * (1-tanh(x %*% alpha)) * (1-exp(x %*% zeta))  ))
  return(result)
}
trt.gnlr=gnlm::gnlr(data.long$college, dist="binomial", mu=mu_trtment, pmu=c(rep(0.01 ,9),rep(0.01,9)) ) 
parameter1=trt.gnlr$coefficients
output.d.rb=gnlmm(data.long$college, dist="binomial", mu=mu_trtment, nest=data.long$ID, points=10, pmu=parameter1,psd=0.001,scale = "identity")

 
# part 2
# beta: p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9]; 
# eta: p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18]  
# continuous outcome
# mu2=function(p) data.long$nearc4_66*tanh(x %*% c(output.d.rb$coefficients[1],output.d.rb$coefficients[2],output.d.rb$coefficients[3],output.d.rb$coefficients[4],output.d.rb$coefficients[5],output.d.rb$coefficients[6],output.d.rb$coefficients[7],output.d.rb$coefficients[8],output.d.rb$coefficients[9]))*(x %*%c(p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9])) + x %*%c(p[10],p[11],p[12],p[13],p[14],p[15],p[16],p[17],p[18])


# create binary outcome
y.cutoff = median(data.long$wage)
Y.binary = as.numeric(data.long$wage > y.cutoff)
table(Y.binary)
data.long$Y.binary=Y.binary

mu_outcome=function(parameter2) {
  beta=c(parameter2[1],parameter2[2],parameter2[3],parameter2[4],parameter2[5],parameter2[6],parameter2[7],parameter2[8],parameter2[9])
  eta=c(parameter2[10],parameter2[11],parameter2[12],parameter2[13],parameter2[14],parameter2[15],parameter2[16],parameter2[17],parameter2[18])
  alpha.mle=c(output.d.rb$coefficients[1],output.d.rb$coefficients[2],output.d.rb$coefficients[3],output.d.rb$coefficients[4],output.d.rb$coefficients[5],output.d.rb$coefficients[6],output.d.rb$coefficients[7],output.d.rb$coefficients[8],output.d.rb$coefficients[9])
  result=data.long$nearc4_66.binary*tanh(x %*% alpha.mle)*tanh(x %*% beta) + 1/(2*(exp(x %*% eta )-1) ) * 
    (exp(x %*% eta )*(2-tanh(x %*% beta) * tanh(x %*% alpha.mle)) +tanh(x %*% beta) *tanh(x %*% alpha.mle)-sqrt(  (exp(x %*% eta)*(tanh(x %*% beta) *tanh(x %*% alpha.mle)-2)-tanh(x %*% beta) *tanh(x %*% alpha.mle))^2+4 * exp(x %*% eta) * (1-tanh(x %*% beta) *tanh(x %*% alpha.mle)) * (1-exp(x %*% eta))  ))
  return(result)
}



y.gnlr=gnlm::gnlr(Y.binary, dist="binomial", mu=mu_outcome, pmu=c(rep(0.15 ,9),rep(0.01,9)) ) 
#mean(  tanh(x %*% y.gnlr$coefficients[1:9]) ) 
parameter2=y.gnlr$coefficients
output.y.rb=gnlmm(Y.binary, dist="binomial", mu=mu_outcome, nest=data.long$ID, points=10, pmu=parameter2,psd=0.001,scale = "identity")
beta.mle=c(output.y.rb$coefficients[1],output.y.rb$coefficients[2],output.y.rb$coefficients[3],output.y.rb$coefficients[4],output.y.rb$coefficients[5],output.y.rb$coefficients[6],output.y.rb$coefficients[7],output.y.rb$coefficients[8],output.y.rb$coefficients[9])
delta.est.rb = mean(  tanh(x %*% beta.mle) )
delta.est.rb







## DB
#######################################################
# Multiply robust estimation
#######################################################  

# 1.1 Estimate f(Z\mid X) using glm
data1966=data.long[data.long$year.new==1966,]
est.fz=glm( nearc4_66.binary~ year.new.scale+age.new.scale+white+black+smsa_66.binary+IQ_68.scale+fatheduc_66.college+motheduc_66.college, family=binomial(link = "logit"),data=data1966)
data1966$f.z.x.est=predict(est.fz, type = "response")
data.long=merge(data.long,data1966[,c("ID","f.z.x.est")],by="ID")


## 1.2 Estimate p0.d.est
alpha.est = output.d.rb$coefficients[1:9]
zeta.est  = output.d.rb$coefficients[10:18]
atanh.delta.d.est = x %*% alpha.est
delta.d.est = tanh(atanh.delta.d.est)
lopop.d.est = x %*% zeta.est
p0.d.est    = mapply(getProbScalarRDiff, atanh.delta.d.est, lopop.d.est)[1,]
data.long$p0.d.est=p0.d.est
 
### 1.3 Estimate p0.y.est 
beta.est = output.y.rb$coefficients[1:9]
eta.est  = output.y.rb$coefficients[10:18]
delta.est = tanh(x %*% beta.est)
delta.y.est = delta.est * delta.d.est
lopop.y.est = x %*% eta.est
p0.y.est    = mapply(getProbScalarRDiff, atan(delta.y.est), lopop.y.est)[1,]
data.long$p0.y.est= p0.y.est
mean(Y.binary[data.long$nearc4_66.binary==0]-p0.y.est[data.long$nearc4_66.binary==0]) 

### 3.Bounded DR estimation of beta  
dr.beta.objective = function(beta) {
  obj=t(1/tanh(x %*% beta)) %*% ((Y.binary - data.long$college * tanh(x %*% beta) - p0.y.est +  tanh(x %*% beta) * p0.d.est) * 
                                   (2 * data.long$nearc4_66.binary - 1)/data1966$f.z.x.est)
  return(sum(obj^2))
} 
startpars =  rep(0.15 ,9)
opt = Optimize(dr.beta.objective, startpars)
b.beta.est = opt$par
b.delta.est = tanh(x %*% b.beta.est) 
 
### 4. TR estimation of Delta
b.Delta.est = mean( b.delta.est ) 

 

### naive
wt=rep(1,nrow(data.long))
naive = sum(Y.binary[data.long$college==1]*wt[data.long$college==1])/sum(wt[data.long$college==1]) - sum(Y.binary[data.long$college==0]*wt[data.long$college==0])/sum(wt[data.long$college==0])



## TSLS
stageI  = lm(college ~ nearc4_66.binary + year.new.scale+age.new.scale+white+black+smsa_66.binary+IQ_68.scale+fatheduc_66.college+motheduc_66.college  ,  data = data.long)
Ahat    = predict(stageI )
stageII = lm(Y.binary ~ Ahat +  year.new.scale+age.new.scale+white+black+smsa_66.binary+IQ_68.scale+fatheduc_66.college+motheduc_66.college , data = data.long)
stageII$coefficients["Ahat"]

# remove newly created columns
data.long=data.long[,1:28]






boot.results=NULL
## sampling subjects
for (boot in 1:1000) {
  
  if (boot%%10==0) {print(boot)}
  tryCatch({
    index = sample(1:nrow(NLS_1966_to_1976_nonmissing_all_outcome_imputed), nrow(NLS_1966_to_1976_nonmissing_all_outcome_imputed), replace=TRUE)
  
  data.long.boot = data.long[data.long$ID %in% index,]
  
  x=cbind(c=rep(1,nrow(data.long.boot)),year=scale(data.long.boot$year.new) ,age=scale(data.long.boot$age.new) , white=data.long.boot$white ,black=data.long.boot$black , smsa=data.long.boot$smsa_66.binary ,
          IQ=scale(data.long.boot$IQ_68) , fatheduc=data.long.boot$fatheduc_66.college ,motheduc=data.long.boot$motheduc_66.college   ) 
   
  mu_trtment =function(parameter1) {
    alpha=c(parameter1[1],parameter1[2],parameter1[3],parameter1[4],parameter1[5],parameter1[6],parameter1[7],parameter1[8],parameter1[9])
    zeta=c(parameter1[10],parameter1[11],parameter1[12],parameter1[13],parameter1[14],parameter1[15],parameter1[16],parameter1[17],parameter1[18])
    result=data.long.boot$nearc4_66.binary*tanh(x %*% alpha) + 1/(2*(exp(x %*% zeta )-1) ) * 
      (exp(x %*% zeta )*(2-tanh(x %*% alpha))+tanh(x %*% alpha)-sqrt(  (exp(x %*% zeta)*(tanh(x %*% alpha)-2)-tanh(x %*% alpha))^2 +
                                                                         4 * exp(x %*% zeta) * (1-tanh(x %*% alpha)) * (1-exp(x %*% zeta))  ))
    return(result)
  }
  trt.gnlr=gnlm::gnlr(data.long.boot$college, dist="binomial", mu=mu_trtment, pmu=c(rep(0.01 ,9),rep(0.01,9)) ) 
  parameter1=trt.gnlr$coefficients
  output.d.rb=gnlmm(data.long.boot$college, dist="binomial", mu=mu_trtment, nest=data.long.boot$ID, points=10, pmu=parameter1,psd=0.001,scale = "identity")
  
  
  # part 2 
  # create binary outcome
  y.cutoff = median(data.long.boot$wage)
  Y.binary = as.numeric(data.long.boot$wage > y.cutoff)
  table(Y.binary)
  data.long.boot$Y.binary=Y.binary
  
  mu_outcome=function(parameter2) {
    beta=c(parameter2[1],parameter2[2],parameter2[3],parameter2[4],parameter2[5],parameter2[6],parameter2[7],parameter2[8],parameter2[9])
    eta=c(parameter2[10],parameter2[11],parameter2[12],parameter2[13],parameter2[14],parameter2[15],parameter2[16],parameter2[17],parameter2[18])
    alpha.mle=c(output.d.rb$coefficients[1],output.d.rb$coefficients[2],output.d.rb$coefficients[3],output.d.rb$coefficients[4],output.d.rb$coefficients[5],output.d.rb$coefficients[6],output.d.rb$coefficients[7],output.d.rb$coefficients[8],output.d.rb$coefficients[9])
    result=data.long.boot$nearc4_66.binary*tanh(x %*% alpha.mle)*tanh(x %*% beta) + 1/(2*(exp(x %*% eta )-1) ) * 
      (exp(x %*% eta )*(2-tanh(x %*% beta) * tanh(x %*% alpha.mle)) +tanh(x %*% beta) *tanh(x %*% alpha.mle)-sqrt(  (exp(x %*% eta)*(tanh(x %*% beta) *tanh(x %*% alpha.mle)-2)-tanh(x %*% beta) *tanh(x %*% alpha.mle))^2+4 * exp(x %*% eta) * (1-tanh(x %*% beta) *tanh(x %*% alpha.mle)) * (1-exp(x %*% eta))  ))
    return(result)
  }
   
  y.gnlr=gnlm::gnlr(Y.binary, dist="binomial", mu=mu_outcome, pmu=c(rep(0.1 ,9),rep(0.01,9)) ) 
  #mean(  tanh(x %*% y.gnlr$coefficients[1:9]) )
  parameter2=y.gnlr$coefficients
  output.y.rb=gnlmm(Y.binary, dist="binomial", mu=mu_outcome, nest=data.long.boot$ID, points=10, pmu=parameter2,psd=0.001,scale = "identity")
  beta.mle=c(output.y.rb$coefficients[1],output.y.rb$coefficients[2],output.y.rb$coefficients[3],output.y.rb$coefficients[4],output.y.rb$coefficients[5],output.y.rb$coefficients[6],output.y.rb$coefficients[7],output.y.rb$coefficients[8],output.y.rb$coefficients[9])
  delta.est.rb = mean(  tanh(x %*% beta.mle) )
   
  
  ## DB
  #######################################################
  # Multiply robust estimation
  #######################################################  
  
  # 1.1 Estimate f(Z\mid X) using glm
  data1966=data.long.boot[data.long.boot$year.new==1966,]
  est.fz=glm( nearc4_66.binary~ year.new.scale+age.new.scale+white+black+smsa_66.binary+IQ_68.scale+fatheduc_66.college+motheduc_66.college, family=binomial(link = "logit"),data=data1966)
  data1966$f.z.x.est=predict(est.fz, type = "response")
  data.long.boot=merge(data.long.boot,data1966[,c("ID","f.z.x.est")],by="ID")
  
  
  ## 1.2 Estimate p0.d.est
  alpha.est = output.d.rb$coefficients[1:9]
  zeta.est  = output.d.rb$coefficients[10:18]
  atanh.delta.d.est = x %*% alpha.est
  delta.d.est = tanh(atanh.delta.d.est)
  lopop.d.est = x %*% zeta.est
  p0.d.est    = mapply(getProbScalarRDiff, atanh.delta.d.est, lopop.d.est)[1,]
  data.long.boot$p0.d.est=p0.d.est
  
  ### 1.3 Estimate p0.y.est 
  beta.est = output.y.rb$coefficients[1:9]
  eta.est  = output.y.rb$coefficients[10:18]
  delta.est = tanh(x %*% beta.est)
  delta.y.est = delta.est * delta.d.est
  lopop.y.est = x %*% eta.est
  p0.y.est    = mapply(getProbScalarRDiff, atan(delta.y.est), lopop.y.est)[1,]
  data.long.boot$p0.y.est= p0.y.est
  mean(Y.binary[data.long.boot$nearc4_66.binary==0]-p0.y.est[data.long.boot$nearc4_66.binary==0]) 
  
  ### 3.Bounded DR estimation of beta  
  dr.beta.objective = function(beta) {
    obj=t(1/tanh(x %*% beta)) %*% ((Y.binary - data.long.boot$college * tanh(x %*% beta) - p0.y.est +  tanh(x %*% beta) * p0.d.est) * 
                                     (2 * data.long.boot$nearc4_66.binary - 1)/data.long.boot$f.z.x.est)
    return(sum(obj^2))
  } 
  startpars =  rep(0.15 ,9)
  opt = Optimize(dr.beta.objective, startpars)
  b.beta.est = opt$par
  b.delta.est = tanh(x %*% b.beta.est) 
  
  ### 4. TR estimation of Delta
  b.Delta.est = mean( b.delta.est ) 
  
  
  
  ### naive
  wt=rep(1,nrow(data.long.boot))
  naive = sum(Y.binary[data.long.boot$college==1]*wt[data.long.boot$college==1])/sum(wt[data.long.boot$college==1]) - sum(Y.binary[data.long.boot$college==0]*wt[data.long.boot$college==0])/sum(wt[data.long.boot$college==0])
  
  
  
  ## TSLS
  stageI  = lm(college ~ nearc4_66.binary + year.new.scale+age.new.scale+white+black+smsa_66.binary+IQ_68.scale+fatheduc_66.college+motheduc_66.college, data = data.long.boot)
  Ahat    = predict(stageI)
  stageII = lm(Y.binary ~ Ahat +  year.new.scale+age.new.scale+white+black+smsa_66.binary+IQ_68.scale+fatheduc_66.college+motheduc_66.college, data = data.long.boot)
  delta.2sls= stageII$coefficients["Ahat"]
  
  
  
  
  boot.results.temp=data.frame(delta.mle=delta.est.rb,  delta.db=b.Delta.est,
                               delta.naive=naive,  delta.2sls=delta.2sls)
  boot.results =rbind(boot.results.temp,boot.results )
 }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

write.csv(boot.results,"/Users/yang/Library/CloudStorage/OneDrive-UniversityofPittsburgh/dissertation_all/part2_v2/non_linear_mixed_effect_model/NLS_bootstrap_results_022023.csv",row.names = FALSE)


summary(boot.results)


sd(boot.results$delta.mle)
sd(boot.results$delta.db)
sd(boot.results$delta.naive)
sd(boot.results$delta.2sls)
 














