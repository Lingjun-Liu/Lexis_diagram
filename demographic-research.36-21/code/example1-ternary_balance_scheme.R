
# 【绘制三元平衡方案的莱克斯曲面】               #
# 我们使用从CIE-Lch颜色空间派生的离散三元平衡方案颜色刻度，
# 绘制了跨时期和年龄的三种死亡原因比例的莱克斯曲面。
# 我们还制作了该图的一个版本，其中包含连续的三元刻度和叠加的总体死亡率等高线。

# 初始化 --------------------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(ggtern)
library(colorspace)

# 数据 --------------------------------------------------------------------

# 按年份、年龄和性别划分的3种选定死亡原因
# 在所有死亡原因中的比例
cod3 <- read_csv("/Users/tuo/Desktop/Lexis/demographic-research.36-21/data/cod3.csv")

# 死亡率的莱克斯曲面
mx <- read_csv("/Users/tuo/Desktop/Lexis/demographic-research.36-21/data/mx.csv")

# 三元几何的函数 ------------------------------------------

#' 分段三元图中的质心坐标
#'
#' @param k 分段三元图中的行数
#'
#' @return 行j中区域i的三元质心坐标矩阵。
#'
#' @details 我们在一个分割成k^2个相等大小的规则三角形的三元图上操作。
#'   规则三角形由行j和行成员i索引。
#'   详见S. H. Derakhshan和C. V. Deutsch (2009)的论文：三元混合物的色标。
TernaryCentroidCoord <- function (k) {
  centroids <- matrix(nrow = k^2, ncol = 5,
                      dimnames = list(NULL, c("j", "i", "p1", "p2", "p3")))
  for (j in 1:k) {
    for (i in 1:(2*k - 2*j + 1)) {
      p1 = (6*k - 6*j - 3*i + 4 + i%%2) / (6*k)
      p2 = (6*j - 2 - 2*i%%2) / (6*k)
      p3 = (3*i - 2 + i%%2) / (6*k)
      centroids[i+(j-1)*(2*k-j+1),] = c(j, i, p1, p2, p3)
    }
  }
  return(centroids)
}

#' 三元坐标中两点之间的距离
#'
#' @param p,c 长度为3的三元坐标向量
#'
#' @return p和c之间的距离。
TernaryDistance <- function(p, c) {
  q = p-c
  d = -q[2]*q[3] - q[3]*q[1] - q[1]*q[2]
  return(d)
}

#' 对于三元坐标p，返回集合C中最近的坐标
#'
#' @param p 长度为3的三元坐标向量
#' @param C 三元坐标矩阵
#' @param index 返回匹配的行索引而不是匹配本身
#'
#' @return 在C中与p距离最小的三元坐标或这些坐标的行索引。
TernaryNearest <- function (p, C, index = FALSE) {
  i <- nnet::which.is.max(# 随机打破平局
    apply(C, 1, function (x) -TernaryDistance(p, x))
  )
  # 返回索引或值
  return(ifelse(index, i, C[i,]))
}

#' 给定分段三元图中的质心的顶点坐标
#'
#' @param p 长度为3的三元坐标向量
#' @param i 质心的行成员索引
#' @param k 质心的行索引
#'
#' @return 每个质心周围顶点的矩阵。
#'
#' @details 我们在一个分割成k^2个相等大小的规则三角形的三元图上操作。
#'   规则三角形由行j和行成员i索引。
#'   详见S. H. Derakhshan和C. V. Deutsch (2009)的论文：三元混合物的色标。
TernaryVerticesFromCentroid <- function (p, i, k) {
  term1 = ((-1)^(i %% 2) * 2) / (3*k)
  term2 = ((-1)^(i %% 2)) / (3*k)
  A = c(p[1] - term1, p[2] + term2, p[3] + term2)
  B = c(p[1] + term2, p[2] - term1, p[3] + term2)
  C = c(p[1] + term2, p[2] + term2, p[3] - term1)
  return(matrix(c(A, B, C), 3, 3, byrow = TRUE))
}

# 从三元组成中派生混合颜色的函数 ------------

#' 添加极坐标向量
#'
#' @param .phi 以弧度为单位的极角
#' @param .r   半径
#'
#' @details 每个向量(phi_i, r_i)表示为极坐标形式的复数
#'   z_i = r_i*exp(i phi_i)。合向量P = sum_i(z_i)具有
#'   极角Arg(P)和半径abs(P)。
#'
#' @return 极坐标中向量的和(r, theta)。
AddPolVec <- function (.phi, .r) {
  z = complex(argument = .phi, modulus = .r)
  resultant = sum(z)
  return(list(phi = Arg(resultant), r = abs(resultant)))
}

#' 返回2-单纯形的三元平衡方案颜色
#'
#' 输入向量的3个元素映射到3种原色。这些
#' 颜色根据输入向量中元素的比例相互混合。
#'
#' @param .simplex2 [0,1]中的3元素比例向量
#' @param .L        混合颜色的亮度
#' @param .C        可能的最大色度
#' @param .H        第一种原色的色相（角度）
#' @param .rev      反转原色向量
#' @param .contrast 增加原色和混合物之间的色度和亮度对比
#'   [0=无额外对比，1=最大额外对比]
#'
#' @details 颜色混合在CIE-LCH颜色空间中进行。亮度
#'   根据\code{.L}中的规范保持恒定，其中较高的值对应于较浅的颜色。
#'   色度取决于向量元素的比例。比例越平等，色度越低，反之亦然。
#'   只有当向量的单个元素等于向量的总和时，才能达到\code{.C}中指定的最大色度值，
#'   也就是说，如果该元素完全主导了向量。
#'   每个向量元素被分配一种原色。第一个原色（第一个向量元素的原色）的色相
#'   由\code{.H}的值确定，这是一个角度值。其他两个原色是通过在圆的
#'   周长上（"色轮"）找到均匀间隔的位置而得出的。
#'   改变\code{.H}的值会改变分配给向量每个元素的原色。
#'
#' @result 表示混合颜色的十六进制代码字符串。
MixTernBalance <- function (.simplex2, .l = 80, .c = 140, .h = 90, .rev = FALSE,
                            .contrast = 0) {
  
  # 生成原色，从[0, 360)中的值H开始，然后
  # 在色轮的周长上选取两个等距点
  primaries = (.h + c(0, 120, 240)) %% 360
  if (.rev == TRUE) primaries <- rev(primaries)
  
  # 根据组比例缩放色度：[0, .C]
  chroma <- .simplex2*.c
  
  # 使用极坐标LCH坐标中的向量加法混合组颜色
  mixed_coord <- AddPolVec(.phi = primaries*pi/180, # 将度转换为弧度
                           .r = chroma)
  
  # 增强平衡与不平衡混合物的亮度和色度对比
  contrast_factor = scales::rescale(mixed_coord$r,
                                    from = c(0, .c), to = c(1-.contrast, 1))
  l = contrast_factor*.l
  C = contrast_factor*mixed_coord$r
  
  # 转换为十六进制RGB
  mixed_hex_rgb <-
    colorspace::hex(colorspace::polarLAB(L = l,
                                         C = C,
                                         # 将弧度转换为度
                                         H = mixed_coord$phi*180/pi),
                    fixup = TRUE)
  
  return(mixed_hex_rgb)
  
}

# 绘制三元平衡方案 ---------------------------------------------

k = 5   # 三元刻度上的间隔数（高k用于平滑的颜色过渡）
l = 90  # 颜色混合的亮度
c = 140 # 颜色混合的色度
h = 320 # 颜色混合的初始色相
contrast = 0.5 # 引入混合颜色之间的亮度和额外的色度对比

# 我们将数据映射到三元图中的一组点上，并为每个点导出混合颜色。
as_tibble(TernaryCentroidCoord(k)) %>%
  rowwise() %>%
  mutate(rgb = MixTernBalance(c(p1, p2, p3), .l = l, .c = c, .h = h, .contrast = contrast)) %>%
  ungroup() -> ternary_centroids

# 我们获取按年份、年龄和性别划分的各种死亡原因的死亡份额[0,1]，
# 并导出十六进制代码形式的混合颜色。然后ggplot直接使用这些颜色
# 作为Lexis曲面中每个瓦片的填充颜色。
cod3 %>%
  group_by(year, age, sex) %>%
  # 将每个数据点量化为最近的三元质心并获取
  # 相应的颜色混合
  mutate(
    rgb = ternary_centroids$rgb[
      TernaryNearest(px, ternary_centroids[,3:5], index = TRUE)
    ]
  ) %>%
  ungroup() -> cod3_mix

# 绘制Lexis曲面
plot_tern_balance <-
  ggplot() +
  # 三元组成
  geom_tile(aes(x = year+.5, width = 1,
                y = age_start + age_width/2, height = age_width,
                fill = rgb), data = filter(cod3_mix, sex == "total")) +
  # 网格
  geom_hline(yintercept = seq(10, 100, 10),
             alpha = 0.2, lty = "dotted") +
  geom_vline(xintercept = seq(1930, 1990, 10),
             alpha = 0.2, lty = "dotted") +
  geom_abline(intercept = seq(-100, 100, 10)-1930,
              alpha = 0.2, lty = "dotted") +
  # 刻度
  scale_fill_identity() +
  scale_x_continuous("年份", expand = c(0.02, 0),
                     breaks = seq(1930, 1990, 10)) +
  scale_y_continuous("年龄", expand = c(0, 0),
                     breaks = seq(0, 100, 10)) +
  # 坐标
  coord_equal() +
  # 主题
  theme_void() +
  theme(
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(),
    axis.text.x = element_text()
  )
plot_tern_balance
#ggsave("./out/fig/tern_balance_raw.pdf", plot_tern_balance,
#       width = 5, height = 7)

# 绘制三元平衡方案图例 --------------------------------------

# 我们生成图例，一个颜色编码的三元图，分段为行
# j_1, ... j_k。每行由i个三角形平铺而成。给定每个三角形(ji)的质心，
# 我们计算用于绘图的顶点的三元坐标。

# 计算三元质心坐标及其相应的混合颜色，用于具有k行的三元图。
as_tibble(TernaryCentroidCoord(k)) %>%
  rowwise() %>%
  mutate(rgb = MixTernBalance(c(p1, p2, p3), .l = l, .c = c, .h = h, .contrast = contrast)) %>%
  ungroup() -> ternary_centroids

# 对于具有k行的三元图中的每个子三角形，计算顶点的三元坐标
# 对于具有k行的三元图中的每个子三角形，计算顶点的三元坐标
library(sysfonts)
library(showtextdb)
library(showtext)
showtext.auto()
ternary_centroids %>%
  group_by(j, i) %>%
  group_modify(~{
    vertices = TernaryVerticesFromCentroid(p = c(.$p1, .$p2, .$p3),
                                           i = .$i, k = k)
    # 复制第一个顶点以闭合路径
    vertices = rbind(vertices, vertices[1,])
    vertices = as.data.frame(vertices)
    colnames(vertices) <- c("v1", "v2", "v3")
    return(vertices)
  }) %>% 
  ungroup() %>%
  mutate(id = sort(rep(1:(k^2), 4))) -> ternary_vertices

plot_tern_balance_lgnd <-
  ggplot(ternary_vertices) +
  geom_polygon(aes(x = v1, y = v2, z = v3,
                   group = id,
                   fill = factor(id)), colour = "white") +
  # 刻度
  scale_fill_manual(values = ternary_centroids$rgb, guide = FALSE) +
  scale_L_continuous("外部因素", breaks = seq(0, 1, length.out = k+1)) +
  scale_T_continuous("肿瘤", breaks = seq(0, 1, length.out = k+1)) +
  scale_R_continuous("其他", breaks = seq(0, 1, length.out = k+1)) +
  # 坐标
  coord_tern() +
  # 箭头
  Larrowlab("% 外部因素") +
  Tarrowlab("% 肿瘤") +
  Rarrowlab("% 其他") +
  # 主题
  theme_classic() +
  theme(
    tern.axis.arrow.show = TRUE,
    tern.axis.ticks.length.major = unit(12, "pt"),
    tern.axis.text = element_text(size = 12, colour = "black"),
    tern.panel.background = element_blank(), # 禁用裁剪区域
    tern.axis.title.T = element_text(),
    tern.axis.title.L = element_text(hjust = 0.2, vjust = 1, angle = -60),
    tern.axis.title.R = element_text(hjust = 0.8, vjust = 0.6, angle = 60)
  )
plot_tern_balance_lgnd
#ggsave("./out/fig/tern_balance_lgnd_raw.pdf", plot_tern_balance_lgnd,
#       width = 5, height = 7)

# 绘制带等高线的三元平衡方案 -------------------------------

# 与之前的程序相同，但我们使用更连续的颜色刻度，
# 并叠加总体死亡率作为等高线。

k = 30
l = 90
c = 140
h = 320
contrast = 0.3

as_tibble(TernaryCentroidCoord(k)) %>%
  rowwise() %>%
  mutate(rgb = MixTernBalance(c(p1, p2, p3), .l = l, .c = c, .h = h, .contrast = contrast)) %>%
  ungroup() -> ternary_centroids

cod3 %>%
  group_by(year, age, sex) %>%
  mutate(rgb = ternary_centroids$rgb[TernaryNearest(px, ternary_centroids[,3:5], index = TRUE)]) %>%
  ungroup() -> cod3_mix

# 绘制Lexis曲面
plot_tern_balance_cont <-
  ggplot() +
  # 三元组成
  geom_tile(aes(x = year+.5, width = 1,
                y = age_start + age_width/2, height = age_width,
                fill = rgb), data = filter(cod3_mix, sex == "total")) +
  # 网格
  geom_hline(yintercept = seq(10, 100, 10),
             alpha = 0.2, lty = "dotted") +
  geom_vline(xintercept = seq(1930, 1990, 10),
             alpha = 0.2, lty = "dotted") +
  geom_abline(intercept = seq(-100, 100, 10)-1930,
              alpha = 0.2, lty = "dotted") +
  # 死亡率等高线
  stat_contour(aes(x = year+.5, y = age_start+age_width/2, z = mx),
               breaks = c(.0001, .0003, .0005,
                          .001, .003, .005,
                          .01, .03, .05,
                          .1, .3),
               colour = "#545454", lwd = .3,
               data = filter(mx,sex == "total")) +
  # 刻度
  scale_fill_identity() +
  scale_x_continuous("年份", expand = c(0.02, 0),
                     breaks = seq(1930, 1990, 10)) +
  scale_y_continuous("年龄", expand = c(0, 0),
                     breaks = seq(0, 100, 10)) +
  # 坐标
  coord_equal() +
  # 主题
  theme_void() +
  theme(
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(),
    axis.text.x = element_text()
  )
plot_tern_balance_cont
#ggsave("./out/fig/tern_balance_cont_raw.pdf", plot_tern_balance_cont,
#       width = 5, height = 7)

# 绘制带等高线的三元平衡方案图例 ------------------------
# 确保ggtern包已正确加载
library(ggtern)

# 绘制带等高线的三元平衡方案图例
as_tibble(TernaryCentroidCoord(k)) %>%
  rowwise() %>%
  mutate(rgb = MixTernBalance(c(p1, p2, p3), .l = l, .c = c, .h = h, .contrast = contrast)) %>%
  ungroup() -> ternary_centroids

# 创建用于图例的数据集 - 使用原始三元坐标而不是顶点
tern_legend_data <- ternary_centroids %>%
  select(j, i, p1, p2, p3, rgb)

# 绘制三元图图例
plot_tern_balance_cont_lgnd <- 
  ggtern(data = tern_legend_data, aes(x = p1, y = p2, z = p3, color = rgb)) +
  geom_point(size = 5) +
  scale_color_identity() +
  # 添加比例尺
  scale_L_continuous("外部因素", breaks = seq(0, 1, .2)) +
  scale_T_continuous("肿瘤", breaks = seq(0, 1, .2)) +
  scale_R_continuous("其他", breaks = seq(0, 1, .2)) +
  # 箭头标签
  Larrowlab("% 外部因素") +
  Tarrowlab("% 肿瘤") +
  Rarrowlab("% 其他") +
  # 主题设置
  theme_classic() +
  theme(
    tern.axis.arrow.show = TRUE,
    tern.axis.ticks.length.major = unit(12, "pt"),
    tern.axis.text = element_text(size = 12, colour = "black"),
    tern.panel.background = element_blank(),
    tern.axis.title.T = element_text(),
    tern.axis.title.L = element_text(hjust = 0.2, vjust = 1, angle = -60),
    tern.axis.title.R = element_text(hjust = 0.8, vjust = 0.6, angle = 60)
  )

# 显示图表
print(plot_tern_balance_cont_lgnd)

#ggsave(file = "./out/fig/tern_balance_lgnd_cont_raw.pdf", plot_tern_balance_cont_lgnd,
#       width = 5, height = 5)