## finding outliers

final_test1 <- read.delim("F:/Research/To_Qi/final_test1.txt")
linear <- final_test1$V5
cnn <- final_test1$predi_value
plot(1:length(linear),cnn-linear)
abline(h = 1.5, col = "red")
abline(h = -0.5, col = "red")

upper <- which((cnn - linear) > 1.5)
upper


lower <- which((cnn - linear) < -0.5)
lower
