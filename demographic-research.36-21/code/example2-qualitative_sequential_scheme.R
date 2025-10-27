####################################################
# 绘制定性顺序方案的Lexis Surface图 #
####################################################

# 我们绘制一个Lexis Surface图，展示不同时期和年龄段最常见的死亡原因及其在所有死亡中的占比，
# 使用定性-顺序方案的颜色比例尺。


# 初始化 --------------------------------------------------------------------

library(readr)     # 用于读取CSV文件
library(dplyr)     # 用于数据处理和管道操作
library(tidyr)     # 用于数据整形
library(ggplot2)   # 用于数据可视化
library(colorspace) # 用于颜色空间转换和处理

# 数据 --------------------------------------------------------------------

# 按年份、年龄和性别的死亡率
mx <- read_csv("/Users/tuo/Desktop/Lexis/demographic-research.36-21/data/mx.csv")

# 5种选定死亡原因在所有死亡原因中的占比
# 按年份、年龄和性别分类
cod5 <- read_csv("/Users/tuo/Desktop/Lexis/demographic-research.36-21/data/cod5.csv")

# cod5 到 cod5_mode 的处理 ---------------------------------------------
# 这是一个筛选操作，目的是在每个年份-性别-年龄组合中
# 只保留占比(px)最高的那个死亡原因
# 
# 例如：如果1950年30岁男性的死亡原因中，
# 循环系统疾病占40%，肿瘤占30%，其他原因占比更低，
# 那么只保留循环系统疾病这一行数据

cod5 %>%
  group_by(year, sex, age) %>%  # 按年份、性别和年龄分组
  filter(px == max(px)) %>%     # 筛选出每组中占比最高的死亡原因
  ungroup() -> cod5_mode        # 解除分组并存储结果

# 定性顺序颜色混合 ----------------------------------------------------
#
# 步骤1：为5种主要死亡原因定义基础颜色
# 每种死亡原因分配一个独特的颜色，以便在可视化中区分
cpal <- c(
  "Circulatory diseases" = "#CB040C",  # 循环系统疾病 - 红色
  "Neoplasms"            = "#B05D00",  # 肿瘤 - 褐色
  "Infections"           = "#1E6617",  # 感染 - 绿色
  "External"             = "#A5147B",  # 外部原因 - 紫色
  "Other"                = "#18657A"   # 其他 - 蓝色
)

# 步骤2：定义占比区间和对应的颜色强度
# 由于px值表示死亡原因占比(0-1之间)，我们需要：
# a) 将连续的占比值划分为离散区间，便于可视化
# b) 为每个区间定义相应的颜色强度（通过alpha值）
#
# 例如：px = 0.75的死亡原因会被归类到第3个区间(0.6-0.8)
# 并使用第3个alpha值(0.8)来混合颜色
breaks <- c(0.2, 0.4, 0.6, 0.8, 1)      # 定义4个占比区间: 0.2-0.4, 0.4-0.6, 0.6-0.8, 0.8-1
alphas <- seq(0.2, 1, length.out = length(breaks)-1)  # 为每个区间定义alpha值: 0.2, 0.46, 0.73, 1

# 步骤3：定义颜色混合函数
# 该函数将根据alpha值将基础颜色与白色混合：
# - alpha越大，原始颜色成分越多，颜色越深
# - alpha越小，白色成分越多，颜色越浅
# 
# 这样可以在保持颜色类别区分的同时，通过颜色深浅表示占比大小
#' 将颜色与白色混合
#'
#' @details 这是与白色背景的alpha混合。alpha混合在LAB色彩空间中进行，
#'   确保在感知上平衡的结果。
#'
#' @param .rgb   RGB十六进制值向量
#' @param .alpha 范围在[0,1]内的alpha值
#'
#' @return 一个经过alpha混合的RGB十六进制值向量。
MixWithWhite <- function (.rgb, .alpha) {
  # 根据.alpha将.rgb基础颜色与白色混合
  # .alpha=1表示全部使用原始颜色，.alpha=0表示全部使用白色
  result <- mixcolor(.alpha, sRGB(1, 1, 1), hex2RGB(.rgb), where = "LAB")
  return(hex(result)) # 将结果转换为RGB十六进制字符串
}

# 步骤4：应用颜色混合处理
# 将cod5_mode数据框转换为包含混合颜色信息的新数据框cod5_mode_mix
# 添加3个新列：
# - base_col: 基础颜色
# - px_disc: 离散化后的占比区间编号(1-4)
# - mix_col: 最终混合颜色，用于图表填充
# 获取混合后的颜色
cod5_mode %>%
  mutate(
    # 为每行添加对应的基础颜色
    base_col = cpal[cod],
    # 将px值离散化为宽度为1/5的区间
    # 输出是一个整数，表示区间的位置(1,2,3,4)
    px_disc = cut(px, breaks,
                  labels = FALSE, include.lowest = TRUE),
    # 将每个基础颜色与白色混合
    mix_col = MixWithWhite(.rgb = base_col, .alpha = alphas[px_disc])
  ) -> cod5_mode_mix

# 绘制最突出死亡原因的Lexis Surface图 ---------------------
# Lexis Surface图绘制 -------------------------------------------------
# 这里使用已处理的数据创建最终的可视化
# - X轴: 年份(year) 
# - Y轴: 年龄(age_start)
# - 颜色: 混合后的颜色(mix_col)
#   * 色相区分不同死亡原因类别
#   * 深浅表示该原因的占比大小
plot_qual_seq <-
  cod5_mode_mix %>%
  filter(sex == "total") %>%  # 仅筛选总体数据（不区分性别）
  # 将方块与网格对齐
  mutate(
    year = year + 0.5,          # 调整年份位置到中点
    age_start = age_start+age_width/2  # 调整年龄区间的起始位置到中点
  ) %>%
  ggplot() +
  # 带颜色的Lexis Surface图
  geom_tile(aes(x = year, width = 1,
                y = age_start, height = age_width,
                fill = mix_col)) +
  # Lexis网格线
  geom_hline(yintercept = seq(10, 100, 10),    # 水平网格线（年龄轴）
             alpha = 0.2, lty = "dotted") +
  geom_vline(xintercept = seq(1930, 1990, 10), # 垂直网格线（年份轴）
             alpha = 0.2, lty = "dotted") +
  geom_abline(intercept = seq(-100, 100, 10)-1930,  # 对角线（同一出生队列）
              alpha = 0.2, lty = "dotted") +
  # 比例尺设置
  scale_fill_identity() +  # 使用我们混合后的实际颜色值
  scale_x_continuous("年份", expand = c(0.02, 0),  # X轴标签和扩展设置
                     breaks = seq(1930, 1990, 10)) +
  scale_y_continuous("年龄", expand = c(0, 0),    # Y轴标签和扩展设置
                     breaks = seq(0, 100, 10)) +
  # 坐标设置
  coord_equal() +  # 确保X轴和Y轴单位相等，保持Lexis图的几何特性
  # 主题设置
  theme_void() +   # 使用最小化主题作为基础
  theme(
    axis.text = element_text(colour = "black"),  # 设置轴文本为黑色
    axis.text.y = element_text(),                # 启用Y轴文本
    axis.text.x = element_text()                 # 启用X轴文本
  )
plot_qual_seq
# 保存图表到PDF文件
#ggsave("./out/fig/qual_seq_raw.pdf", plot_qual_seq,
#       width = 5, height = 7)

# 绘制定性顺序图例 --------------------------------------
# 基于我们定义的基础颜色、分割点和alpha级别，我们为图表绘制图例。

# 将每个基础颜色与每个alpha级别混合
lgnd_mixed <- t(sapply(cpal, function (x) MixWithWhite(x, alphas)))
# 转换为长格式，带有ggplot绘图的位置信息
lgnd_data <- expand.grid(x = 1:nrow(lgnd_mixed),
                         y = 1:ncol(lgnd_mixed))
lgnd_data$col <- as.vector(lgnd_mixed)  # 添加混合后的颜色列

# 绘制定性顺序图例
plot_qual_seq_lgnd <-
  ggplot(lgnd_data) +
  geom_tile(aes(x = x, y = y, fill = col),  # 创建填充色块
            colour = "white", lwd = 1) +     # 设置色块边框
  scale_fill_identity() +                    # 使用实际颜色值
  scale_x_continuous(breaks = 1:length(cpal),  # X轴为死亡原因类别
                     labels = names(cpal),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = 1:(length(alphas)+1)-0.5,  # Y轴为占比百分比
                     labels = breaks*100,
                     expand = c(0,0)) +
  coord_fixed(4) +  # 固定坐标比例，使图例更紧凑
  theme_void() +    # 使用最小化主题
  theme(
    plot.margin = unit(rep(10, 4), "pt"),  # 设置图例周围的边距
    axis.text = element_text(colour = "black"),  # 设置轴文本为黑色
    axis.text.y = element_text(),                # 启用Y轴文本
    axis.text.x = element_text(vjust = 0.5, hjust = 1, angle = 90)  # 旋转X轴文本以防重叠
  )
plot_qual_seq_lgnd
# 保存图例到PDF文件
#ggsave("./out/fig/qual_seq_lgnd_raw.pdf", plot_qual_seq_lgnd,
#       width = 2, height = 4)