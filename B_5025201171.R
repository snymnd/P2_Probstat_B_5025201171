data0 <- c(78,75,67,77,70,72,78,74,77)
data1 <- c(100,95,70,90,90,90,89,90,100)
data=data.frame(data0, data1)
data

# 1.a Standar Deviasi data selisih pasangann pengamatan
sd_selisih = sd(data1 - data0)
sd_selisih

#1.b nilai t (p-value)
#install.packages('BSDA')
library(BSDA)
#karena 2 data berpasangan maka diuji dengan t dependent
t.test(data1, data0, paired = TRUE)



#1.c
var(data0)
var(data1)
# hasil diatas menunjukan varians kedua data berbeda maka 
# var.equal pada fungsi t.test = FALSE
t.test(data1, data0,
       mu = 0.5, paired = TRUE, var.equal= FALSE,
       conf.level = 0.95)

#Dari hasil Diatas didaptkan nilai p-value = 7.499e-05 = 0.00007499
# yang artinya nilai p-value berada kurang dari significance level alpha = 0.05
# Dengan begitu kita dapa menolak hipotesis 0 (H0/Hypothesis null)
# Dan menyimpulkan bahwa rata-rata kadar saturasi sebelum dan setelah aktivitas A
# berbeda secara signifikan dengan p-value 7.499e-05.
# Serta secara yakin bahwa rata-rata perbedaannya berada pada confident interfal 11.334 - 21.11064
# Dengan rata-rata perbedaan 16.22222

# http://www.sthda.com/english/wiki/paired-samples-t-test-in-r

#install.packages('BSDA')
library(BSDA)
#2.a
# dik: meanX = 20000, n = 100, std_dev = 3900, asumsi alpha = 0.05
# H0 <= 20000, H1 > 20000, karena di soal dikatakan "lebih dari" tetapi hipotesis null (H0) harus bernilai sama dengan (=), maka hipotesis soal menjadi hipotesis alternatif (H1)
mean_X <- 23500
sd_X <- 3900 
zsum.test(mean.x=mean_X, sigma.x=sd_X, 
          n.x = 100, alternative = "greater", 
          mu = 2000, conf.level = 0.95)

#hasil diatas menunjukkan nilai p = 2.2e-16 = 2.2x10^-16
#yang berarti nilai p berada di bawah alpha = 0.05, maka H0 ditolak
#dan dapat disimpulkan bahwa mobil dikemudikan rata-rata lebih dari 20000 km per tahun (sesuai klaim soal)
#dengan kesimpulan tersebut saya SETUJU pada klaim soal.

#2.b
# dari soal, didapatkan bahwa H0(null) <= 2000, H1 (alternatif) > 20000
# dan output menghasilkan nilai p = 2.2e-16 yang menandakan bahwa nilai p kurang dari alpha sehingga hipotesis nulll ditolak
# alternative hypothesis: true mean is greater than 2000 menunjukan Hipotesis alternatif yang akan diujikan
# confident interfal berada dari 22858.51 sampai tak hingga
# dan rata-rata nilai dari sample yang dihitung adalah 23500

#2.c
#hasil diatas menunjukkan nilai p = 2.2e-16 = 2.2x10^-16
#yang berarti nilai p berada di bawah alpha = 0.05, maka H0 ditolak
#dan dapat disimpulkan bahwa mobil dikemudikan rata-rata lebih dari 20000 km per tahun (sesuai klaim soal)

#https://www.rdocumentation.org/packages/PASWR/versions/1.1/topics/zsum.test
#https://rpubs.com/databee/696450

# 3.a tentukan H0 dan H1
# Pada soal ditanyakan "apakah ada perbedaan pada rata-ratanya (dari 2 populasi)?
# maka H0 : mean1 = mean2 : mean1 - mean2 = 0(selisih)
# H1 : mean1 != mean2 : mean1 - mean2 != 0(selisih)

# 3.b hitung sample statistik
# asumsi : varians diaggap sama, data Independen/tidak berpasangan
# x = bandung, y= bali
tsum.test(mean.x = 3.64, s.x = 1.67, n.x = 19,
          mean.y = 2.79, s.y = 1.32, n.y = 27,
          alternative = "two.sided", var.equal = TRUE, 
          conf.level = 0.95)

# 3.c lakukan uji statistik (df = 2) 


# 3.d nilai keritis
crit_val = qt(0.05, 2, lower.tail = FALSE)
crit_val

# 3.e
# keputusan: pada 3.b nilai p-value = 0.06049 yang artinya p-value > alpha
# maka hipotesis null(H0) tidak ditolak = diterima

# 3.f
# kesimpulam : karena H0 diterima maka mean1 = mean2 tidak ada perbedaan rata-rata sari 2 populasi yang digunakan



# https://rpubs.com/databee/696450


#4 Dik: 
my_data <- read.table("datano4.txt",h=T)
attach(my_data)
names(my_data)

my_data$Group <- as.factor(my_data$Group)
my_data$Group = factor(my_data$Group,labels = c("grup 1", "grup 2", "grup 3"))

class(my_data$Group)

#4.a
group1 <- subset(my_data, Group == "grup 1")
group2 <- subset(my_data, Group == "grup 2")
group3 <- subset(my_data, Group == "grup 3")

qqnorm(group1$Length)
qqline(group1$Length)

qqnorm(group2$Length)
qqline(group2$Length)

qqnorm(group3$Length)
qqline(group3$Length)

#4b
bartlett.test(Length ~ Group, data = my_data)
# asumsi alpha = 0.05
# Didapatkan nilai p-value = 0.8054, dengan nilai p-value yang melebihi alpha
# maka dapat disimpulkan bahwa H0 Dapat diterima disimpulkan pebedaan panjang antara ke3 grup sama

# 4.c
model_1 = lm(Length ~ Group, data = my_data)
anova(model_1)

#4.d
# dari c, didapatkan nilai p = 0.8054, nilai p > dari alpha = 0.05, maka H0 diterima

#4.e
TukeyHSD(aov(model_1))

#4.f
install.packages("ggplot2")
library("ggplot2")

ggplot(my_data, aes(x = Group, y = Length)) +
  geom_boxplot(fill = "red", colour = "black") +
  scale_x_discrete() + xlab("Kucing") +
  ylab("panjang")

# 5
install.packages("multcompView")
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

#5.a
mydata <- read.csv(file.choose())

qplot(x = Temp, y = Light, geom = "point", data = mydata) +
  facet_grid(.~Glass, labeller = label_both)

#5.b
mydata$Glass <- as.factor(mydata$Glass)
mydata$Temp_Factor <- as.factor(mydata$Temp)
str(mydata)

mydataaov <- aov(Light ~ Glass*Temp_Factor, data = mydata)
summary(mydataaov)

#5.c
data_summary <- group_by(mydata, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))

print(data_summary)

#5.d
tukey <- TukeyHSD(mydataaov)
print(tukey)

#5.f
tukey.cld <- multcompLetters4(mydataaov, tukey)
print(tukey.cld)



