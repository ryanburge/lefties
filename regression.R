df <- select(df, year, player_id, teamID, lgID, salary, w, l, g, gs, cg, sho, sv, ipouts, h, er, bb, so, baopp, era, ibb, wp, hbp, bk, bfp, gf, r, sh, sf, nameGiven, throws)

reg1 <- lm(salary ~ era + w + l + g + ipouts + throws, data=df)
summary(reg1)
