# P2_Probstat_B_5025201171

## 1
``` r
data0 <- c(78,75,67,77,70,72,78,74,77)
data1 <- c(100,95,70,90,90,90,89,90,100)
data=data.frame(data0, data1)
data
```

### 1.a Standar Deviasi data selisih pasangann pengamatan
```r
sd_selisih = sd(data1 - data0)
sd_selisih
```
![image](https://user-images.githubusercontent.com/70748569/170881290-c7a53fee-082e-4410-a5ea-dd08e46f9746.png)

### 1.b nilai t (p-value)
```r
#1.b nilai t (p-value)
#install.packages('BSDA')
library(BSDA)
#karena 2 data berpasangan maka diuji dengan t dependent
t.test(data1, data0, paired = TRUE)
```
![image](https://user-images.githubusercontent.com/70748569/170881321-b4bd1ce0-73ee-4497-851c-65ecd049b6a0.png)

### 1.c
var(data0)
var(data1)
hasil diatas menunjukan varians kedua data berbeda maka 
var.equal pada fungsi t.test = FALSE
```r
t.test(data1, data0,
       mu = 0.5, paired = TRUE, var.equal= FALSE,
       conf.level = 0.95)
```
![image](https://user-images.githubusercontent.com/70748569/170881342-7735543e-0ef8-4e0c-95c2-457d748e6542.png)
![image](https://user-images.githubusercontent.com/70748569/170881353-d39cba73-0989-438b-a1ce-b56bcfcab0dd.png)

Dari hasil Diatas didaptkan nilai p-value = 7.499e-05 = 0.00007499
yang artinya nilai p-value berada kurang dari significance level alpha = 0.05
Dengan begitu kita dapa menolak hipotesis 0 (H0/Hypothesis null)
Dan menyimpulkan bahwa rata-rata kadar saturasi sebelum dan setelah aktivitas A
berbeda secara signifikan dengan p-value 7.499e-05.
Serta secara yakin bahwa rata-rata perbedaannya berada pada confident interfal 11.334 - 21.11064
Dengan rata-rata perbedaan 16.22222

## 2
### 2.a
dik: meanX = 20000, n = 100, std_dev = 3900, asumsi alpha = 0.05
H0 <= 20000, H1 > 20000, karena di soal dikatakan "lebih dari" tetapi hipotesis null (H0) harus bernilai sama dengan (=), maka hipotesis soal menjadi hipotesis alternatif (H1)
```r
mean_X <- 23500
sd_X <- 3900 
zsum.test(mean.x=mean_X, sigma.x=sd_X, 
          n.x = 100, alternative = "greater", 
          mu = 2000, conf.level = 0.95)
```
![image](https://user-images.githubusercontent.com/70748569/170881397-0b0f6b73-3da7-4dfc-98e9-60250beec129.png)

hasil diatas menunjukkan nilai p = 2.2e-16 = 2.2x10^-16
yang berarti nilai p berada di bawah alpha = 0.05, maka H0 ditolak
#dan dapat disimpulkan bahwa mobil dikemudikan rata-rata lebih dari 20000 km per tahun (sesuai klaim soal)
#dengan kesimpulan tersebut saya SETUJU pada klaim soal.

### 2.b
dari soal, didapatkan bahwa H0(null) <= 2000, H1 (alternatif) > 20000
dan output menghasilkan nilai p = 2.2e-16 yang menandakan bahwa nilai p kurang dari alpha sehingga hipotesis nulll ditolak
alternative hypothesis: true mean is greater than 2000 menunjukan Hipotesis alternatif yang akan diujikan
confident interfal berada dari 22858.51 sampai tak hingga dan rata-rata nilai dari sample yang dihitung adalah 23500

### 2.c
hasil diatas menunjukkan nilai p = 2.2e-16 = 2.2x10^-16 yang berarti nilai p berada di bawah alpha = 0.05, maka H0 ditolak dan dapat disimpulkan bahwa mobil dikemudikan rata-rata lebih dari 20000 km per tahun (sesuai klaim soal)

## 3
### 3.a tentukan H0 dan H1
Pada soal ditanyakan "apakah ada perbedaan pada rata-ratanya (dari 2 populasi)?
maka H0 : mean1 = mean2 : mean1 - mean2 = 0(selisih)
H1 : mean1 != mean2 : mean1 - mean2 != 0(selisih)

### 3.b hitung sample statistik
asumsi : varians diaggap sama, data Independen/tidak berpasangan
x = bandung, y= bali

```r
tsum.test(mean.x = 3.64, s.x = 1.67, n.x = 19,
          mean.y = 2.79, s.y = 1.32, n.y = 27,
          alternative = "two.sided", var.equal = TRUE, 
          conf.level = 0.95)
```
![image](https://user-images.githubusercontent.com/70748569/170881555-6d038737-77e1-41c5-b0fc-f409981466c5.png)

### 3.d nilai kritis
![image](https://user-images.githubusercontent.com/70748569/170881608-5fc20b38-98ee-4b4f-930e-6f9e0ab2c6e6.png)

### 3.e
keputusan: pada 3.b nilai p-value = 0.06049 yang artinya p-value > alpha
maka hipotesis null(H0) tidak ditolak = diterima

### 3.f
kesimpulam : karena H0 diterima maka mean1 = mean2 tidak ada perbedaan rata-rata sari 2 populasi yang digunakan

## 4
inisiasi data
```r
my_data <- read.table("datano4.txt",h=T)
attach(my_data)
names(my_data)

my_data$Group <- as.factor(my_data$Group)
my_data$Group = factor(my_data$Group,labels = c("grup 1", "grup 2", "grup 3"))

class(my_data$Group)
```
### 4.a 
Assig data ke tiap-tiap grup
```r
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
```
![image](https://user-images.githubusercontent.com/70748569/170881816-3c773c45-e8b8-4301-8aa9-b2abee8587f3.png)

![image](https://user-images.githubusercontent.com/70748569/170881829-4073de81-0e32-4c10-aba1-d4d6d1bf573d.png)

![image](https://user-images.githubusercontent.com/70748569/170881856-905e6e6d-1c65-4c16-a4f0-5717581f7ca9.png)

### 4b
```r
bartlett.test(Length ~ Group, data = my_data)
```
![image](https://user-images.githubusercontent.com/70748569/170881906-cb72a3fe-f281-4a05-aa65-26896020efb2.png)

asumsi alpha = 0.05
Didapatkan nilai p-value = 0.8054, dengan nilai p-value yang melebihi alpha
maka dapat disimpulkan bahwa H0 Dapat diterima disimpulkan pebedaan panjang antara ke3 grup sama

### 4.c
![image](https://user-images.githubusercontent.com/70748569/170881919-87ca8fb3-155d-4ab7-8edd-4d1528c96cc8.png)


### 4.d
dari c, didapatkan nilai p = 0.8054, nilai p > dari alpha = 0.05, maka H0 diterima


### 4.e
![image](https://user-images.githubusercontent.com/70748569/170881948-f97024b1-6e3b-4852-8375-2e3d27132603.png)

### 4.f
![image](https://user-images.githubusercontent.com/70748569/170881993-2f4bb69a-efa8-4780-b8c7-ac2acf03baa4.png)

![image](https://user-images.githubusercontent.com/70748569/170881979-9584e0ab-0bf2-487e-ad29-62644f3dbea4.png)

