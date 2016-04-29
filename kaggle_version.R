library(lubridate)
library(dotwhisker)
library(broom)
library(dplyr)
library(ggplot2)
library(cem)
library(dplyr)
library(car)

master <- read.csv("D:/bball/player.csv", stringsAsFactors = FALSE)
salary <- read.csv("D:/bball/salary.csv", stringsAsFactors = FALSE)
pitching <- read.csv("D:/bball/pitching.csv", stringsAsFactors = FALSE)

master$date <- as.Date(master$final_game)
master$year <- year(master$date)
master <- subset(master, master$year >=2005)
pitching <- subset(pitching, pitching$year >=2005)
salary <- subset(salary, year >=2005)
salary$year <- salary$year
salary$player_id <- salary$player_id
df <- merge(salary, pitching, by=c("year", "player_id"))
df <- merge(df, master, by=c("year", "player_id"))
df$throw <- df$throws.y
head(df)


df$lgID <- df$league_id.x
righties <- subset(df, df$throw =="R")
lefties <- subset(df, df$throw =="L")
mean(lefties$salary)
mean(righties$salary)


rht <- aggregate(righties$salary, list(righties$year), mean, na.rm = TRUE)
lft <- aggregate(lefties$salary, list(lefties$year), mean, na.rm = TRUE)
rht$throw <- 'right'
lft$throw <- 'left'
histogram<-rbind(rht,lft)
handPalette <- c("#daa520", "#228b22")
l_al <- subset(lefties, lgID =="AL")
lal <- aggregate(l_al$salary, list(l_al$year), mean, na.rm = TRUE)
l_nl <- subset(lefties, lgID =="NL")
lnl <- aggregate(l_nl$salary, list(l_nl$year), mean, na.rm = TRUE)
lnl$league <- "NL"
lal$league <- "AL"
leftleague<-rbind(lnl,lal)
leaguePalette <- c("#ff0000", "#0000cd")
r_al <- subset(righties, lgID =="AL")
ral <- aggregate(r_al$salary, list(r_al$year), mean, na.rm = TRUE)
r_nl <- subset(righties, lgID =="NL")
rnl <- aggregate(r_nl$salary, list(r_nl$year), mean, na.rm = TRUE)
rnl$league <- "NL"
ral$league <- "AL"
rightleague<-rbind(rnl,ral)

ggplot(histogram, aes(x=Group.1, y = x/1000)) + geom_bar(aes(fill=throw),stat="identity", position= "dodge") + xlab("Season") + ylab("Salary (in thousands)") + scale_fill_manual(values=handPalette)

ggplot(leftleague, aes(x=Group.1, y = x/1000)) + geom_bar(aes(fill=league),stat="identity", position= "dodge") + xlab("Season") + ylab("Salary (in thousands)") + ggtitle("Lefties Salaries") + scale_fill_manual(values=leaguePalette)

ggplot(rightleague, aes(x=Group.1, y = x/1000)) + geom_bar(aes(fill=league),stat="identity", position= "dodge") + xlab("Season") + ylab("Salary (in thousands)") + ggtitle("Righties Salaries") + scale_fill_manual(values=leaguePalette)


p <- ggplot(df, aes(salary/1000, era))
p + geom_point(aes(colour = df$throw)) + xlim(5000, 25000) + ylim(0, 10) + scale_color_manual(values = c("#daa520", "#228b22")) + xlab("Salary (in thousands)") + ylab("ERA") +  theme(legend.title=element_blank())


reg1 <- lm(salary ~ era + w + l + g + ipouts + throws + baopp + so + bb , data=df)
summary(reg1)

dwplot(reg1)
dwplot(reg1) + geom_vline(xintercept = 0, colour = "grey60", linetype = 2)

cem <- select(df, salary, w, l, g, gs, sv, ipouts, h, er, bb, so, baopp, era, throws, lgID)
cem <- data.frame(na.omit(cem))

cem$treated = recode(cem$throws, "'L'=1; 'R'=0;", as.factor.result=FALSE)

tr <- which(cem$treated==1)
ct <- which(cem$treated==0)

mean(cem$salary[tr]) - mean(cem$salary[ct])

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
summary(est)