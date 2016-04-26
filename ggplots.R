righties <- subset(df, df$throws =="R")
lefties <- subset(df, df$throws =="L")

mean(lefties$salary)
mean(righties$salary)

rht$throw <- 'right'
lft$throw <- 'left'
histogram<-rbind(rht,lft)

handPalette <- c("#daa520", "#228b22")

ggplot(histogram, aes(x=Group.1, y = x/1000)) + geom_bar(aes(fill=throw),stat="identity", position= "dodge") + xlab("Season") + ylab("Salary (in thousands)") + scale_fill_manual(values=handPalette)


l_al <- subset(lefties, lgID =="AL")
lal <- aggregate(l_al$salary, list(l_al$year), mean, na.rm = TRUE)
l_nl <- subset(lefties, lgID =="NL")
lnl <- aggregate(l_nl$salary, list(l_nl$year), mean, na.rm = TRUE)

lnl$league <- "NL"
lal$league <- "AL"
leftleague<-rbind(lnl,lal)

leaguePalette <- c("#ff0000", "#0000cd")

ggplot(leftleague, aes(x=Group.1, y = x/1000)) + geom_bar(aes(fill=league),stat="identity", position= "dodge") + xlab("Season") + ylab("Salary (in thousands)") + ggtitle("Lefties Salaries") + scale_fill_manual(values=leaguePalette)

r_al <- subset(righties, lgID =="AL")
ral <- aggregate(r_al$salary, list(r_al$year), mean, na.rm = TRUE)
r_nl <- subset(righties, lgID =="NL")
rnl <- aggregate(r_nl$salary, list(r_nl$year), mean, na.rm = TRUE)
rnl$league <- "NL"
ral$league <- "AL"
rightleague<-rbind(rnl,ral)

ggplot(rightleague, aes(x=Group.1, y = x/1000)) + geom_bar(aes(fill=league),stat="identity", position= "dodge") + xlab("Season") + ylab("Salary (in thousands)") + ggtitle("Righties Salaries") + scale_fill_manual(values=leaguePalette)


p <- ggplot(df, aes(salary/1000, era))
p + geom_point(aes(colour = df$throw)) + xlim(5000, 25000) + scale_color_manual(values = c("#daa520", "#228b22")) + xlab("Salary (in thousands)") + ylab("ERA") +  theme(legend.title=element_blank())

