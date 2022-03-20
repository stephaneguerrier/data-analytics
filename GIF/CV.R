library(idar)
data(reading)
reading$age_minus_6 = reading$age - 6
# Fit linear regression model
mod3 = lm(score ~ age_minus_6 + group:age_minus_6, data = reading)
summary(mod3)

mypurple = rgb(106, 90, 205,maxColorValue=255)
cruk_blue = rgb(55,56,149,maxColorValue=255)
cruk_lightblue = rgb(6, 188, 241,maxColorValue=255)
cruk_pink = rgb(235,7,142,maxColorValue=255)
cruk_yellow = rgb(240,178,0,maxColorValue=255)
cruk_yellow = rgb(240,178,0,maxColorValue=255)
beamer_gray = gray(.35)

plot0 = function(){
  par(mar=c(4,4,2,2))
  plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = c(5.9, 12.1), ylim = c(30,75),
       col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
  mtext("Age", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
  mtext("Score", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

  grid()
  points(age1, mu1, pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
  points(age1, mu1, pch = 1, col = cruk_pink, cex = 1.25)
  points(age2, mu2, pch = 15, col = paste0(mypurple, 50), cex = 1.25)
  points(age2, mu2, pch = 0, col = mypurple, cex = 1.25)

  age_x = seq(from = 6, to = 13)
  lines(age_x, mod3$coefficients[1] + (age_x - 6)*mod3$coefficients[2], lty = 2, col = cruk_pink)
  lines(age_x, mod3$coefficients[1] + (age_x - 6)*mod3$coefficients[2] +  (age_x - 6)*mod3$coefficients[3], lty = 2, col = mypurple)

  text(x = 10.55, y = 43, pos = 4, "Control", cex = 1.15, col = beamer_gray)
  text(x = 10.55, y = 40, pos = 4, "Treatement", cex = 1.15, col = beamer_gray)
  text(x = 10.55, y = 37, pos = 4, "Full fit", cex = 1.15, col = beamer_gray)

  points(10.35, 43.3, pch = 19, col = paste0(cruk_pink, 50), cex = 1.5)
  points(10.35, 43.3, pch = 1, col = cruk_pink, cex = 1.5)
  points(10.35, 40.3, pch = 15, col = paste0(mypurple, 50), cex = 1.5)
  points(10.35, 40.3, pch = 0, col = mypurple, cex = 1.5)

  lines(c(10.2, 10.5), c(37.35, 37.35), lty = 2, col = mypurple)
  lines(c(10.2, 10.5), c(36.65, 36.65), lty = 2, col = cruk_pink)

}


plot1 = function(){
  par(mar=c(4,4,2,2))
  plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = c(5.9, 12.1), ylim = c(30,75),
       col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
  mtext("Age", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
  mtext("Score", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

  grid()
  points(age1, mu1, pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
  points(age1, mu1, pch = 1, col = cruk_pink, cex = 1.25)
  points(age2, mu2, pch = 15, col = paste0(mypurple, 50), cex = 1.25)
  points(age2, mu2, pch = 0, col = mypurple, cex = 1.25)

  age_x = seq(from = 6, to = 13)
  lines(age_x, mod3$coefficients[1] + (age_x - 6)*mod3$coefficients[2], lty = 2, col = cruk_pink)
  lines(age_x, mod3$coefficients[1] + (age_x - 6)*mod3$coefficients[2] +  (age_x - 6)*mod3$coefficients[3], lty = 2, col = mypurple)

  points(reading$age[id %in% id_test], reading$score[id %in% id_test],
         pch = 19, col = "#FF00004C", cex = 2.5)


  mod_train = lm(score ~ age_minus_6 + group:age_minus_6, data = reading[!(id %in% id_test),])
  lines(age_x, mod_train$coefficients[1] + (age_x - 6)*mod_train$coefficients[2], lty = 1, lwd = 1.5, col = cruk_pink)
  lines(age_x, mod_train$coefficients[1] + (age_x - 6)*mod_train$coefficients[2] +  (age_x - 6)*mod_train$coefficients[3], lty = 1, lwd = 1.5,col = mypurple)


  text(x = 10.55, y = 43, pos = 4, "Control", cex = 1.15, col = beamer_gray)
  text(x = 10.55, y = 40, pos = 4, "Treatement", cex = 1.15, col = beamer_gray)
  text(x = 10.55, y = 37, pos = 4, "Full fit", cex = 1.15, col = beamer_gray)
  text(x = 10.55, y = 34, pos = 4, "Partial fit", cex = 1.15, col = beamer_gray)
  text(x = 10.55, y = 31, pos = 4, "Excluded points", cex = 1.15, col = beamer_gray)

  points(10.35, 43.3, pch = 19, col = paste0(cruk_pink, 50), cex = 1.5)
  points(10.35, 43.3, pch = 1, col = cruk_pink, cex = 1.5)
  points(10.35, 40.3, pch = 15, col = paste0(mypurple, 50), cex = 1.5)
  points(10.35, 40.3, pch = 0, col = mypurple, cex = 1.5)
  points(10.35, 31.3, pch = 19, col = "#FF00004C", cex = 2.5)

  lines(c(10.2, 10.5), c(37.35, 37.35), lty = 2, col = mypurple)
  lines(c(10.2, 10.5), c(36.65, 36.65), lty = 2, col = cruk_pink)

  lines(c(10.2, 10.5), c(34.35, 34.35), lty = 1, lwd = 1.5, col = mypurple)
  lines(c(10.2, 10.5), c(33.65, 33.65), lty = 1, lwd = 1.5, col = cruk_pink)
}

plot2 = function(){
  plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = c(5.9, 12.1), ylim = c(30,75),
       col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
  mtext("Age", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
  mtext("Score", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

  grid()
  points(age1, mu1, pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
  points(age1, mu1, pch = 1, col = cruk_pink, cex = 1.25)
  points(age2, mu2, pch = 15, col = paste0(mypurple, 50), cex = 1.25)
  points(age2, mu2, pch = 0, col = mypurple, cex = 1.25)

  age_x = seq(from = 6, to = 13)
  lines(age_x, mod3$coefficients[1] + (age_x - 6)*mod3$coefficients[2], lty = 2, col = cruk_pink)
  lines(age_x, mod3$coefficients[1] + (age_x - 6)*mod3$coefficients[2] +  (age_x - 6)*mod3$coefficients[3], lty = 2, col = mypurple)

  points(reading$age[!(id %in% id_test)], reading$score[!(id %in% id_test)],
         pch = 15, col = "#FFFFFFB2", cex = 1.4)


  mod_train = lm(score ~ age_minus_6 + group:age_minus_6, data = reading[!(id %in% id_test),])
  lines(age_x, mod_train$coefficients[1] + (age_x - 6)*mod_train$coefficients[2], lty = 1, lwd = 1.5, col = cruk_pink)
  lines(age_x, mod_train$coefficients[1] + (age_x - 6)*mod_train$coefficients[2] +  (age_x - 6)*mod_train$coefficients[3], lty = 1, lwd = 1.5,col = mypurple)

  text(x = 10.55, y = 43, pos = 4, "Control", cex = 1.15, col = beamer_gray)
  text(x = 10.55, y = 40, pos = 4, "Treatement", cex = 1.15, col = beamer_gray)
  text(x = 10.55, y = 37, pos = 4, "Full fit", cex = 1.15, col = beamer_gray)
  text(x = 10.55, y = 34, pos = 4, "Partial fit", cex = 1.15, col = beamer_gray)

  points(10.35, 43.3, pch = 19, col = paste0(cruk_pink, 50), cex = 1.5)
  points(10.35, 43.3, pch = 1, col = cruk_pink, cex = 1.5)
  points(10.35, 40.3, pch = 15, col = paste0(mypurple, 50), cex = 1.5)
  points(10.35, 40.3, pch = 0, col = mypurple, cex = 1.5)

  lines(c(10.2, 10.5), c(37.35, 37.35), lty = 2, col = mypurple)
  lines(c(10.2, 10.5), c(36.65, 36.65), lty = 2, col = cruk_pink)

  lines(c(10.2, 10.5), c(34.35, 34.35), lty = 1, lwd = 1.5, col = mypurple)
  lines(c(10.2, 10.5), c(33.65, 33.65), lty = 1, lwd = 1.5, col = cruk_pink)


}


plot3 = function(){
  plot(NA, main="Reading ability assessment",xlab=" ",ylab=" ", xlim = c(5.9, 12.1), ylim = c(30,75),
       col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
  mtext("Age", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
  mtext("Score", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

  grid()
  points(age1, mu1, pch = 19, col = paste0(cruk_pink, 50), cex = 1.25)
  points(age1, mu1, pch = 1, col = cruk_pink, cex = 1.25)
  points(age2, mu2, pch = 15, col = paste0(mypurple, 50), cex = 1.25)
  points(age2, mu2, pch = 0, col = mypurple, cex = 1.25)

  age_x = seq(from = 6, to = 13)
  lines(age_x, mod3$coefficients[1] + (age_x - 6)*mod3$coefficients[2], lty = 2, col = cruk_pink)
  lines(age_x, mod3$coefficients[1] + (age_x - 6)*mod3$coefficients[2] +  (age_x - 6)*mod3$coefficients[3], lty = 2, col = mypurple)

  points(reading$age[!(id %in% id_test)], reading$score[!(id %in% id_test)],
         pch = 15, col = "#FFFFFFB2", cex = 1.4)


  mod_train = lm(score ~ age_minus_6 + group:age_minus_6, data = reading[!(id %in% id_test),])
  lines(age_x, mod_train$coefficients[1] + (age_x - 6)*mod_train$coefficients[2], lty = 1, lwd = 1.5, col = cruk_pink)
  lines(age_x, mod_train$coefficients[1] + (age_x - 6)*mod_train$coefficients[2] +  (age_x - 6)*mod_train$coefficients[3], lty = 1, lwd = 1.5,col = mypurple)

  err = 0
  for (i in 1:length(id_test)){
    if (reading$group[id == id_test[i]] == "Control"){
      lines(c(reading$age[id == id_test[i]], reading$age[id == id_test[i]]),
            c(reading$score[id == id_test[i]],
              mod_train$coefficients[1] + (reading$age[id == id_test[i]] - 6)*mod_train$coefficients[2]),
            col = "red2", lwd = 2)
      err = err + diff(c(reading$score[id == id_test[i]], mod_train$coefficients[1] + (reading$age[id == id_test[i]] - 6)*mod_train$coefficients[2]))^2
    }else{
      lines(c(reading$age[id == id_test[i]], reading$age[id == id_test[i]]),
            c(reading$score[id == id_test[i]],
              mod_train$coefficients[1] + (reading$age[id == id_test[i]] - 6)*(mod_train$coefficients[2] + mod_train$coefficients[3])),
            col = "red2", lwd = 2)
      err = err + diff(c(reading$score[id == id_test[i]], mod_train$coefficients[1] + (reading$age[id == id_test[i]] - 6)*(mod_train$coefficients[2] + mod_train$coefficients[3])))^2
    }

  }

  text(x = 10.55, y = 43, pos = 4, "Control", cex = 1.15, col = beamer_gray)
  text(x = 10.55, y = 40, pos = 4, "Treatement", cex = 1.15, col = beamer_gray)
  text(x = 10.55, y = 37, pos = 4, "Full fit", cex = 1.15, col = beamer_gray)
  text(x = 10.55, y = 34, pos = 4, "Partial fit", cex = 1.15, col = beamer_gray)
  text(x = 10.55, y = 31, pos = 4, "Error", cex = 1.15, col = beamer_gray)

  points(10.35, 43.3, pch = 19, col = paste0(cruk_pink, 50), cex = 1.5)
  points(10.35, 43.3, pch = 1, col = cruk_pink, cex = 1.5)
  points(10.35, 40.3, pch = 15, col = paste0(mypurple, 50), cex = 1.5)
  points(10.35, 40.3, pch = 0, col = mypurple, cex = 1.5)

  lines(c(10.2, 10.5), c(37.35, 37.35), lty = 2, col = mypurple)
  lines(c(10.2, 10.5), c(36.65, 36.65), lty = 2, col = cruk_pink)

  lines(c(10.2, 10.5), c(34.35, 34.35), lty = 1, lwd = 1.5, col = mypurple)
  lines(c(10.2, 10.5), c(33.65, 33.65), lty = 1, lwd = 1.5, col = cruk_pink)
  lines(c(10.2, 10.5), c(31, 31), lty = 1, lwd = 2, col = "red")

  text(7,73, paste("Mean squared error = ", round(err/6, 2)), col = "red")
  err/6
}


plot_mse = function(){
  plot(NA, main="10-fold Cross Validation",xlab=" ",ylab=" ", xlim = c(1,10), ylim = c(0, 30),
       col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
  mtext("Split", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
  mtext("Out-of-sample prediction error (Mean square error)", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

  grid()

  lines(myerr, col = "red", type = "b", pch = 16, cex = 2)
}

plot_mse_last = function(){
  plot(NA, main="10-fold Cross Validation",xlab=" ",ylab=" ", xlim = c(1,10), ylim = c(0, 30),
       col.axis=beamer_gray,col.main=beamer_gray,col.lab=beamer_gray,cex.lab=1.5)
  mtext("Split", side = 1, line = 2.5, col = beamer_gray, cex = 1.25)
  mtext("Out-of-sample prediction error (Mean square error)", side = 2, line = 2.5, col = beamer_gray, cex = 1.25)

  grid()

  lines(myerr, col = "red", type = "b", pch = 16, cex = 2)

  merr = mean(myerr)

  abline(h = merr, col = "red", lty = 2)
  text(5, 15, paste("Estimated CV error = ", round(merr, 2)), col = "red", cex = 1.25)
}

set.seed(189)
ord = sample(reading$id)

group = reading$group
id = reading$id
age1 = reading$age[group == "Control"]
age2 = reading$age[group == "Treatment"]
mu1 = reading$score[group == "Control"]
mu2 = reading$score[group == "Treatment"]
myerr = NULL


library("gifski")
png_path <- file.path(tempdir(), "frame%03d.png")
png(png_path, units="px", width=2000*2, height=2000*1.13, res=300)
par(ask = FALSE, mar = c(3,0.75,0.75,0.75), mfrow = c(1, 2))
nb = 32

plot0()
plot_mse()

for (k in 1:10){
  index = k
  test_index = (1:6)+(index-1)*6
  id_test = ord[test_index]
  id_train = ord[-test_index]
  plot1()
  plot_mse()

  plot2()
  plot_mse()

  a = plot3()
  myerr = c(myerr, a)
  plot_mse()
}


plot3()
plot_mse_last()

dev.off()


png_files <- sprintf(png_path, 1:nb)
gif_file <- tempfile(fileext = ".gif")
library(gifski)
gifski(png_files, "GIF/CV.gif", delay = 0.75, loop = TRUE, progress = TRUE, width = 1200,
       height = 600)
unlink(png_files)
utils::browseURL(gif_file)

