library(cem)
cem <- df
cem <- select(cem, salary, w, l, g, gs, sv, ipouts, h, er, bb, so, baopp, era, treated)
cem <- data.frame(na.omit(cem))

cem$treated = recode(cem$throws, "'L'=1; 'R'=0;", as.factor.result=FALSE)

tr <- which(cem$treated==1)
ct <- which(cem$treated==0)

mean(cem$salary[tr]) - mean(cem$salary[ct])
# -700.6434

cem$league = recode(cem$lgID, "'NL'=1; 'AL'=2;", as.factor.result=FALSE)
cem$lgID <- NULL

cem$ba <- recode(cem$baopp, ".000:.100= 1; .151:.200 =3; .201:.250=4; .251:.300 =5; .301:.350 =6; .351:.400 =6; .401:.500 =7; .501:.700 =8")
cem$baopp <- NULL

cem$ERA <- recode(cem$era, ".000:.1= 1; 1.01:2.00 =2; 2.01:3=3; 3.01:4 =4; 4.01:5 =5; 5.01:10 =6")
cem$era <- NULL
cem$games <- recode(cem$g, "1:10= 1; 11:20 =2; 21:30=3; 31:40 =4; 41:50 =6; 51:80 =6")
cem$g <- NULL
cem$loss <- recode(cem$l, "0:2= 1; 2:5 =2; 6:10=3; 10:18 =4")
cem$l <- NULL
cem$walks <- recode(cem$bb, "0:5= 1; 6:10 =2; 11:15=3; 16:20 =4; 25:30 =5; 31:88=6")
cem$bb <- NULL

mat <- cem(treatment = "treated", data = cem, drop = "salary", keep.all=TRUE)
est <- att(mat, salary ~ treated, data = cem)
est

# Treatment effect estimation for data:

# G0  G1
# All       604 256
# Matched   105  61
# Unmatched 499 195

# Linear regression model estimated on matched data only

# Coefficients:
#  Estimate Std. Error t value   p-value    
# (Intercept)  1172616     139604  8.3996 2.024e-14 ***
#  treated       150375     230296  0.6530    0.5147  
