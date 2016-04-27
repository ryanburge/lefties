df <- select(df, year, player_id, teamID, lgID, salary, w, l, g, gs, cg, sho, sv, ipouts, h, er, bb, so, baopp, era, ibb, wp, hbp, bk, bfp, gf, r, sh, sf, nameGiven, throws)

reg1 <- lm(salary ~ era + w + l + g + ipouts + throws + baopp + so + bb , data=df)
summary(reg1)

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2801563.4   583014.3   4.805 1.80e-06 ***
# era             1875.1    22269.7   0.084 0.932915    
# w             139291.6    74154.5   1.878 0.060641 .  
# l             371822.9    67913.1   5.475 5.64e-08 ***
# g             -32995.3     6125.3  -5.387 9.11e-08 ***
# ipouts           339.9     3342.7   0.102 0.919032    
# throwsR      -176206.0   237314.7  -0.742 0.457974    
# baopp       -2486503.4  1927267.8  -1.290 0.197314    
# so             26083.3     7856.4   3.320 0.000935 ***
# bb            -62288.0    13925.8  -4.473 8.68e-06 ***
