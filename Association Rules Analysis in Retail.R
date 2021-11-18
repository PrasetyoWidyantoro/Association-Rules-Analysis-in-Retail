#Association Rules Analysis in Retail
#Market Basket Analysis

#Menggunakan library arules
library(arules)

#Membaca transaksi dari file data_transaksi.txt
transaksi <- read.transactions(file="data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)

#Menampilkan jumlah kombinasi dari produk yang terdapat pada daftar transaksi yang ada
inspect(apriori(transaksi, parameter = list(support=.1, minlen=2, target='frequent itemsets')))

#Membaca transaksi dari file data_transaksi2.txt
transaksi <- read.transactions(file="data_transaksi2.txt", format="single", sep="\t", cols=c(1,2), skip=1)

#Menampilkan jumlah kombinasi dari produk yang terdapat pada daftar transaksi yang ada
inspect(apriori(transaksi, parameter = list(support=.03, minlen=2, target='frequent itemsets')))

transaksi_tabular <- read.csv("data_transaksi.txt", sep="\t")

#Menampilkan variable transaksi_tabular dengan fungsi print
print(transaksi_tabular)

transaksi <- read.transactions(file="data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)
transaksi@itemInfo

#Menampilkan Daftar Kode Transaksi
transaksi@itemsetInfo

#Tampilan Transaksi dalam bentuk Matrix
transaksi@data

#itemFrequency
itemFrequency(transaksi, type="absolute")

#Statistika 3 Teratas
data_item <- itemFrequency(transaksi, type="absolute")

#Melakukan sorting pada data_item
data_item <- sort(data_item, decreasing = TRUE)

#Mengambil 3 item pertama
data_item <- data_item[1:3]

#Luaran  Statistik  3 teratas Sebagai File
#Konversi data_item menjadi data frame dengan kolom Nama_Produk dan Jumlah
data_item <- data.frame("Nama Produk"=names(data_item), "Jumlah"=data_item, row.names=NULL)

#Menulis File Statistik Top 3
write.csv(data_item, file="top3_item_retail.txt", eol = "\r\n")

#Tampilan item frequency plot
itemFrequencyPlot(transaksi)

#Melihat Itemset per Transaksi dengan Inspect
#Menggunakan inspect terhadap transaksi
inspect(transaksi)

#Menghasilkan association rules dan disimpan sebagai variable mba
mba <- apriori(transaksi)

#Melihat isi dari rules dengan menggunakan fungsi inspect
inspect(mba)

#Filter rhs dengan item "Sirup" dan tampilkan
inspect(subset(mba, rhs %in% "Sirup"))

#Filter LHS
inspect(subset(mba, lhs %in% "Gula"))

#Filter LHS dan RHS
inspect(subset(mba, lhs %in% "Pet Food" & rhs %in% "Sirup"))

#Menghasilkan Rules dengan Parameter Support dan Confidence
mba <- apriori(transaksi,parameter = list(supp = 0.1, confidence = 0.5))


#Inspeksi Rules Yang Dihasilkan
inspect(mba)

#Filter LHS dan RHS (2)
inspect(subset(mba, lhs %in% "Teh Celup" | rhs %in% "Teh Celup"))

#Filter berdasarkan Lift
mba <- apriori(transaksi,parameter = list(supp = 0.1, confidence = 0.5))
inspect(subset(mba, (lhs %in% "Teh Celup" | rhs %in% "Teh Celup") & lift>1))

#3Rekomendasi - Filter dengan %ain%
inspect(subset(mba, (lhs %ain% c("Pet Food", "Gula" ))))

#Visualisasi Rules dengan Graph
plot(subset(mba, lift>1.1), method="graph")

