###########################
# 绘制按年龄划分的面积图 #
###########################

# 第一部分：初始化环境 ---------------------------------------------------
library(readr)    # 用于高效读取CSV文件
library(dplyr)    # 提供数据处理和管道操作
library(ggplot2)  # 生成高质量图形

# 第二部分：数据导入与处理 -----------------------------------------------

# 读取数据集：包含按年份、年龄和性别划分的5种选定死亡原因占所有死亡的比例
# 数据结构：cod5包含列：year(年份), age(年龄组), sex(性别), cod(死亡原因), px(死亡原因比例)
cod5 <- read_csv("/Users/tuo/Desktop/Lexis/demographic-research.36-21/data/cod5.csv")

# 步骤1：确保年龄组按正确顺序排列
# 将age变量转换为有序因子，以便在绘图时保持正确的年龄组顺序
lev_age <- c("<1","1-4","5-9","10-14","15-19","20-24","25-29",
             "30-34","35-39","40-44","45-49","50-54","55-59",
             "60-64","65-69","70-74","75-79","80-84","85-89",
             "90-94","95-99","100+")
cod5 <- mutate(cod5, age = factor(age, lev_age, ordered = TRUE))

# 步骤2：控制死亡原因的顺序
# ggplot会按数据框中的出现顺序绘制面积，因此重新排序数据框非常重要
# 这影响面积图中各死亡原因的堆叠顺序
lev_cod <- c("Circulatory diseases",  # 循环系统疾病
             "Neoplasms",             # 肿瘤
             "Infections",            # 感染
             "External",              # 外部原因
             "Other")                 # 其他
cod5 %>%
  # 将cod转换为因子，并指定因子水平顺序
  mutate(cod = factor(cod, lev_cod)) %>%
  # 按年份、性别和年龄分组
  group_by(year, sex, age) %>% 
  # 在每个分组内按cod排序，确保绘图时的堆叠顺序
  arrange(cod) -> cod5

# 第三部分：创建可视化图表 -----------------------------------------------

# 步骤1：为每种死亡原因分配颜色
# 在面积图中，相邻颜色需要有足够对比度，因此我们同时变化色调和亮度
# 例如，浅绿色旁边是紫色，再旁边是浅蓝色
cpal <- c(
  "Circulatory diseases" = "#FB8072",  # 循环系统疾病：淡红色
  "Neoplasms"            = "#FFFFB3",  # 肿瘤：淡黄色
  "Infections"           = "#54D39E",  # 感染：绿色
  "External"             = "#D086B5",  # 外部原因：紫色
  "Other"                = "#80B1D3"   # 其他：淡蓝色
)

# 步骤2：为每个年龄类别绘制一个面积图
plot_agewise_area <-
  # 只使用总体(sex="total")数据，不分男女
  ggplot(filter(cod5, sex == "total")) +
  # 绘制面积图，x轴为年份，y轴为比例，填充色按死亡原因区分
  geom_area(aes(x = year, y = px, fill = cod),
            show.legend = FALSE) +  # 不显示图例，因为颜色含义可从各面板中理解
  
  # 添加时期网格线，每10年一条
  geom_vline(xintercept = seq(1930, 1999, 10),
             colour = "black", linewidth = 0.4, alpha = 0.2, lty = 3) +
  
  # 设置颜色比例尺，使用之前定义的颜色方案
  scale_fill_manual("", values = cpal) +
  
  # 设置X轴，表示年份，每10年一个刻度
  scale_x_continuous("Year", expand = c(0, 0),
                     breaks = seq(1930, 1990, 10)) +
  
  # 设置固定比例的坐标系，使图形更美观
  coord_fixed(ratio = 5) +
  
  # 按年龄分面，将不同年龄组的图形垂直排列
  # as.table=FALSE使得年龄组从下到上按从老到年轻的顺序排列
  facet_grid(age~., as.table = FALSE) +
  
  # 设置主题为极简风格
  theme_void() +
  theme(
    axis.text    = element_text(colour = "black"),  # 坐标轴文本为黑色
    axis.text.x  = element_text(),                  # 显示X轴文本
    strip.text.y = element_text(angle = 0, hjust = 1),  # 调整Y轴分面标签
    panel.spacing = unit(0, "cm")                   # 设置面板间距为0（更新了过时的panel.margin）
  )
plot_agewise_area
# 保存图表（注释掉的代码）
# 如需生成PDF文件，取消下面注释
#ggsave("./out/fig/agewise_area_raw.pdf", plot_agewise_area,
#       width = 5, height = 7)  # 保存为宽5英寸、高7英寸的PDF文件