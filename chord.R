library(circlize)
data <- matrix(c(
  1, 1, 1, 1, 1, 1,
  1, 1, 0, 0, 1, 0,
  1, 1, 0, 0, 0, 1,
  1, 1, 0, 0, 0, 0,
  1, 1, 0, 0, 1, 0,
  1, 0, 0, 0, 0, 0,
  1, 1, 0, 1, 0, 1,
  1, 0, 0, 0, 0, 1,
  1, 1, 0, 0, 0, 1,
  1, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 1,
  1, 1, 0, 0, 0, 1,
  1, 1, 0, 0, 0, 1,
  1, 0, 0, 0, 0, 1,
  1, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 1, 1,
  0, 0, 0, 1, 0, 0
), nrow = 18, ncol = 6, byrow = TRUE)

colnames(data) <- c("eGFR", "CKD", "Microalbumin", "Neurotisism", "Depression", "CVD")
rownames(data) <- c("rs11208648_G", "rs1260326_T", "rs1919127_C", "rs12987055_C", "rs3749147_A", "rs17616248_C", "rs7832606_T", "rs2954029_T", "rs2954033_A", "rs1728765_G", "rs67801643_T", "rs58542926_T", "rs10415849_T", "rs17216525_T", "rs429358_C", "rs9617963_A", "rs9625965_C", "rs3761472_G")

circos.par("track.height" = 0.5)
circos.par("cell.padding" = c(0, 0, 0, 0))

# 绘制 Chord diagram
chordDiagram(data)

# 添加标题
title("Schematic overview of SNP-disease associations")

# 调整标题位置
circos.trackPlotRegion


pdf("Schematic overview of SNP-disease associations.pdf", width = 8, height = 6)
#首先对布局进行设置，设置扇形间的距离
circos.par(gap.after = c(rep(3, nrow(data)-1), 10, rep(3, ncol(data)-1), 10))
#调整扇形顺序与原图保持一致
orderlist <- c(rev(rownames(mat)), colnames(mat))

circos.clear()
dev.off()
