#########################################################################
# 绘制带有主要死亡原因轮廓的小多倍Lexis表面图                           #
#########################################################################

# 本代码的目标：
# 1. 分析不同死亡原因在不同年龄和时期的分布模式
# 2. 识别每个年龄-时期组合中的主要死亡原因
# 3. 通过Lexis表面图可视化这些模式，并用轮廓线突出显示主要死亡原因区域

# 第一部分：初始化环境 ---------------------------------------------------

library(readr)    # 用于高效读取CSV文件
library(raster)   # 处理和分析栅格数据
library(dplyr)    # 提供数据处理和管道操作
library(ggplot2)  # 生成高质量图形

# 第二部分：数据导入 ---------------------------------------------------

# 读取数据集：包含按年份、年龄和性别划分的10种选定死亡原因占所有死亡的比例
# 数据结构：cod10包含列：year(年份), age(年龄组), age_width(年龄组宽度), 
#          sex(性别), cod(死亡原因), px(死亡原因比例)
cod10 <- read_csv("/Users/tuo/Desktop/Lexis/demographic-research.36-21/data/cod10.csv")

# 第三部分：数据处理 - 创建详细的Lexis表面数据 --------------------------

# 步骤1：创建完整的1x1年龄-时期网格
# 原始数据可能有不规则的年龄组，这里我们创建均匀的1岁间隔网格
left_join(
  # 创建完整的年龄-时期-性别-死亡原因组合网格
  expand.grid(year      = unique(cod10$year),        # 所有年份
              age_start = 0:104,                     # 单岁年龄，从0到104岁
              age_width = 1,                         # 设定统一的年龄间隔为1岁
              sex       = unique(cod10$sex),         # 所有性别类别
              cod       = unique(cod10$cod),         # 所有死亡原因
              stringsAsFactors = FALSE               # 不将字符串转换为因子
  ),
  # 从原始数据中选择需要的列，排除age和age_width这两列，因为我们将使用新创建的值
  select(cod10, -age, -age_width)
) %>%
  # 步骤2：数据准备和缺失值处理
  # 按性别、年份、死亡原因和起始年龄排序，为后续填充缺失值做准备
  arrange(sex, year, cod, age_start) %>%
  # 使用前值填充(LOCF)处理缺失值：对于细分后没有具体数值的年龄段，
  # 使用同一组内前一个有效值填充
  mutate(px = zoo::na.locf(.$px)) -> cod10_1x1  # 结果存入新的数据框cod10_1x1

# 步骤3：识别每个年龄-时期-性别组合中的主要死亡原因
cod10_1x1 %>%
  # 按性别、年份和起始年龄分组，在每个组内比较不同死亡原因
  group_by(sex, year, age_start) %>%
  # 标记比例最高的死亡原因(值为1)，其他为NA
  mutate(cod_mode = ifelse(px == max(px), 1, NA)) %>%
  ungroup() -> cod10_mode  # 结果存入新的数据框cod10_mode

# 第四部分：为主要死亡原因区域生成地理空间轮廓 -------------------------

# 使用地理空间技术创建轮廓线，突出显示每种死亡原因在何时何地是主要原因
cod10_mode %>% 
  # 按性别和死亡原因分组处理
  group_by(sex, cod) %>%
  do(
    {
      # 步骤1：将数据转换为矩阵形式[年龄,时期]，便于栅格处理
      M <-
        matrix(
          .$cod_mode,                          # 使用之前标记的主要死亡原因数据
          nrow = n_distinct(.$age_start),      # 矩阵行数 = 不同年龄数
          ncol = n_distinct(.$year),           # 矩阵列数 = 不同年份数
          dimnames = list(unique(.$age_start), # 行名 = 年龄值
                          unique(.$year))      # 列名 = 年份值
        )
      # 步骤2：翻转矩阵，使年龄轴方向正确
      M <- apply(M, 2, rev)  # 沿列(第2维)应用rev函数，反转每列
      
      # 步骤3：将矩阵转换为地理空间栅格对象
      # 这使我们能够使用地理空间功能来生成轮廓
      R <- raster(M,
                  xmn = min(.$year),          # 栅格x轴最小值 = 最早年份
                  xmx = max(.$year)+1,        # 栅格x轴最大值 = 最晚年份+1
                  ymn = min(.$age_start),     # 栅格y轴最小值 = 最小年龄
                  ymx = max(.$age_start)+1)   # 栅格y轴最大值 = 最大年龄+1
      
      # 步骤4：生成轮廓多边形并转换为数据框
      # 将值为1的相邻单元格合并为多边形(dissolve=TRUE)
      outline <- fortify(rasterToPolygons(R, dissolve = TRUE))
      
      # 步骤5：提取多边形坐标点数据，用于绘图
      data.frame(x = outline$lon, y = outline$lat, group = outline$group)
    }
  ) -> cod10_mode_outline  # 结果存入新的数据框cod10_mode_outline

# 第五部分：创建可视化图表 ----------------------------------------------

plot_small_multiples <-
  ggplot() +
  # 步骤1：绘制Lexis表面热图，表示不同死亡原因的比例
  # 仅使用总体(sex="total")数据，不分男女
  geom_tile(aes(x = year+0.5, y = age_start+age_width/2,  # 定位到单元格中心
                width = 1, height = age_width,            # 设置单元格尺寸
                fill = cut_interval(px, length = 0.1)),   # 将比例值分为等长区间着色
            data = filter(cod10, sex == "total")) +
  
  # 步骤2：添加主要死亡原因区域的轮廓线
  geom_path(aes(x = x, y = y, group = group),  # 使用之前生成的轮廓线数据
            data = filter(cod10_mode_outline, sex == "total"),
            lwd = 0.3) +  # 设置线宽为0.3
  
  # 步骤3：设置颜色比例尺
  scale_fill_brewer(type = "seq", palette = "PuBuGn",  # 使用蓝绿色渐变色系
                    guide = guide_legend(reverse = TRUE)) +  # 反转图例顺序
  
  # 步骤4：设置坐标轴
  scale_x_continuous("Year", expand = c(0.02, 0),       # X轴标签及边距
                     breaks = seq(1940, 2000, 20)) +    # X轴刻度，每20年一个
  scale_y_continuous("Age", expand = c(0, 0),           # Y轴标签及边距
                     breaks = seq(0, 100, 20)) +        # Y轴刻度，每20岁一个
  
  # 步骤5：按死亡原因分面显示
  facet_wrap(~ cod, ncol = 5, as.table = TRUE) +  # 每行显示5个死亡原因
  
  # 步骤6：添加Lexis网格辅助线
  # 水平线表示同年龄，每20岁一条
  geom_hline(yintercept = seq(20, 100, 20),
             alpha = 0.2, lty = "dotted") +
  # 垂直线表示同年份，每20年一条
  geom_vline(xintercept = seq(1940, 1980, 20),
             alpha = 0.2, lty = "dotted") +
  # 对角线表示同一出生队列，每20年一条
  geom_abline(intercept = seq(-100, 100, 20)-1940,
              alpha = 0.2, lty = "dotted") +
  
  # 步骤7：设置坐标系和主题
  coord_equal() +  # 使用等比例坐标系，确保年龄和年份单位长度相同
  theme_void() +   # 使用极简主题，去除背景元素
  theme(
    axis.text   = element_text(colour = "black"),  # 设置坐标轴文本为黑色
    axis.text.y = element_text(),                  # 显示Y轴文本
    axis.text.x = element_text(),                  # 显示X轴文本
    panel.spacing = unit(0, "cm")                  # 设置面板间距为0
  )
plot_small_multiples

# 步骤8：保存图表（注释掉的代码）
# 如需生成PDF文件，取消下面注释
#ggsave("./out/fig/small_multiples_raw.pdf", plot_small_multiples,
#       width = 13, height = 8)  # 保存为宽13英寸、高8英寸的PDF文件