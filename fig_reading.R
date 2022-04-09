# Load data
library(idar)
data(reading)

# Html colors
transp_pink = "#EB078E50"
transp_purple = "#6A5ACD50"
mypink = "#EB078E"
mypurple = "#6A5ACD"

# Empty graph
par(mar=c(4,4,2,2))
plot(NA, main = "Reading ability assessment", xlab=" ", ylab=" ",
     xlim = c(5.9, 12.1), ylim = c(30,75),cex.lab=1.5)
mtext("Age", side = 1, line = 2.5, cex = 1.25)
mtext("Score", side = 2, line = 2.5, cex = 1.25)
grid()

# Add points for control group
index = reading$group == "Control"
points(reading$age[index], reading$score[index], pch = 19, col = transp_pink, cex = 1.25)
points(reading$age[index], reading$score[index], pch = 1, col = mypink, cex = 1.25)

# Add points for treatment group
points(reading$age[!index], reading$score[!index], pch = 15, col = transp_purple, cex = 1.25)
points(reading$age[!index], reading$score[!index], pch = 0, col = mypurple, cex = 1.25)

# Manual legend
text(x = 10.55, y = 38, pos = 4, "Control", cex = 1.15)
text(x = 10.55, y = 35, pos = 4, "Treatement", cex = 1.15)
points(10.35, 38.3, pch = 19, col = transp_pink, cex = 1.5)
points(10.35, 38.3, pch = 1, col = mypink, cex = 1.5)
points(10.35, 35.3, pch = 15, col = transp_purple, cex = 1.5)
points(10.35, 35.3, pch = 0, col = mypurple, cex = 1.5)

