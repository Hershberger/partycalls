
hou_data <- data.table(
  lop = c(rep(1, 4245), rep(0, 9308), rep(1, 6123), rep(0, 1090)),
  call = c(rep(1, 4245 + 9308), rep(0, 6123 + 1090)))

sen_data <- data.table(
  lop = c(rep(1, 2063), rep(0, 5233), rep(1, 4876), rep(0, 1851)),
  call = c(rep(1, 2063 + 5233), rep(0, 4876 + 1851)))


cor.test(hou_data[, lop], hou_data[, call]) # -.52, -.50
cor.test(sen_data[, lop], sen_data[, call]) # -.45, -.43
