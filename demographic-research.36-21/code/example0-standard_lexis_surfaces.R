##############################################
# 绘制Lexis surface的死亡率性别比               #
##############################################

# 本代码的目标：
# 1. 从人类死亡率数据库(HMD)获取英格兰和威尔士的死亡率数据
# 2. 计算男性与女性死亡率的比值（性别比）
# 3. 通过Lexis表面图可视化不同年龄和时期的死亡率性别差异

# 第一部分：初始化环境 ---------------------------------------------------
#install.packages("HMDHFDplus")
library(HMDHFDplus)
library(HMDHFDplus)   # 用于从人类死亡率数据库下载数据(替代已停止维护的hmdget包)
library(ggplot2)  # 用于数据可视化
library(dplyr)    # 用于数据处理和管道操作
library(tidyr)    # 用于数据重塑(长宽格式转换)
library(readr)    # 用于高效读写CSV文件
library(sysfonts) # 用于字体管理
library(showtextdb) # 用于字体数据库
library(showtext)   # 用于在R图形中使用系统字体
showtext.auto()   # 激活showtext功能，允许使用系统字体

# 第二部分：数据获取 -----------------------------------------------------
# 安装并加载HMDHFDplus包


# HMD数据库凭证（从环境变量读取；请在 .Renviron 中设置 HMD_USERNAME/HMD_PASSWORD）
hmd_user <- Sys.getenv("HMD_USERNAME", unset = NA)
hmd_pass <- Sys.getenv("HMD_PASSWORD", unset = NA)
if (is.na(hmd_user) || is.na(hmd_pass) || hmd_user == "" || hmd_pass == "") {
  stop("未检测到HMD凭证。请在项目根目录创建 .Renviron（或使用用户级 ~/.Renviron），设置 HMD_USERNAME 和 HMD_PASSWORD。参见 README。")
}

# 步骤1：从人类死亡率数据库下载英格兰和威尔士的死亡率数据
# country = "GBRTENW" 表示英国的英格兰和威尔士地区
# item = "Mx_1x1" 表示需要1岁年龄间隔、1年时间间隔的死亡率数据
# mx表示死亡率(mortality rate)
enwamx <- readHMDweb(CNTRY = "GBRTENW", item = "Mx_1x1",
                     username = hmd_user, password = hmd_pass)

# 步骤2：重塑数据格式，将宽格式转为长格式
# 原始数据中Female和Male是两个独立的列
# 转换后Sex成为一个变量列，mx(死亡率)成为对应的值列

# 整理数据格式以匹配原代码预期
enwamx <- enwamx %>%
  pivot_longer(cols = c(Female, Male), names_to = "Sex", values_to = "mx") %>%
  select(Year, Age, Sex, mx)

# 使用sprintf格式化死亡率数值，保留5位小数
write_csv(mutate(enwamx, mx = sprintf("%1.5f", mx)), file = "/Users/tuo/Desktop/Lexis/demographic-research.36-21/data/enwamx.csv")

# 第三部分：数据处理 - 计算性别比 ----------------------------------------

enwamx %>%
  # 步骤1：将数据从长格式转回宽格式，以便计算性别比
  # 这样每一行包含相同年份和年龄的男性和女性死亡率
  pivot_wider(names_from = Sex, values_from = mx) %>%
  
  # 步骤2：计算男性死亡率除以女性死亡率的比值
  # na_if(Male/Female, Inf)将无穷大值（当Female=0时）转换为NA
  mutate(mx_sex_ratio = na_if(Male/Female, Inf)) %>%
  
  # 步骤3：数据筛选，仅保留1905年及以后的数据，且年龄不超过100岁
  # 这可能是因为早期数据质量问题或分析关注点
  filter(Year >= 1905, Age <= 100) -> dat

# 第四部分：数据分类 - 将连续的性别比转换为分类变量 ---------------------

# 步骤1：定义性别比的断点和标签
# 断点定义了不同程度的性别死亡率差异
# 小于1的值表示女性死亡率高于男性，大于1的值表示男性死亡率高于女性
breaks <- c(0, 1/2 , 100/175, 100/150, 100/125, 100/101,
            101/100, 125/100, 150/100, 175/100, 2/1, Inf)

# 为每个区间定义描述性标签
# 注意部分标签后添加了空格，这是为了确保因子水平唯一性
labels <- c(">100%女性\n死亡率过剩",
            "75到100%",
            "50到75%",
            "25到50%",
            "1到25%",
            "死亡率相等",
            # 下面的标签末尾有空格，确保与女性过剩的标签区分
            "1到25% ",
            "25到50% ",
            "50到75% " ,
            "75到100% ",
            ">100%男性\n死亡率过剩")

# 步骤2：将连续的性别比值转换为离散分类变量
dat %>%
  # 使用cut函数将连续变量分割成离散区间
  # include.lowest=TRUE确保包含最小值
  mutate(mx_sex_ratio_disc =
           cut(mx_sex_ratio,
               breaks, labels,
               include.lowest = TRUE)) -> dat

# 第五部分：创建Lexis表面图 ----------------------------------------------

plot_lexis_surface <- ggplot(dat) +
  # 步骤1：使用geom_raster创建热图
  # x轴为年份，y轴为年龄，颜色表示性别比分类
  # +0.5确保像素中心对齐在格点上，而不是左下角
  geom_raster(aes(x = Year+0.5, y = Age+0.5,
                  fill = mx_sex_ratio_disc)) +
  
  # 步骤2：添加Lexis网格辅助线
  # 水平线表示同一年龄，每10岁一条
  geom_hline(yintercept = seq(10, 100, 10),
             alpha = 0.2, lty = "dotted") +
  # 垂直线表示同一时间点，每10年一条
  geom_vline(xintercept = seq(1910, 1990, 10),
             alpha = 0.2, lty = "dotted") +
  # 对角线表示同一出生队列，每10年一条
  # 公式中intercept值调整使对角线能表示正确的出生队列
  geom_abline(intercept = seq(-100, 100, 10)-1910,
              alpha = 0.2, lty = "dotted") +
  
  # 步骤3：设置图表外观和比例尺
  # 使用ColorBrewer的发散色调色板(RdBu)，表示女性过剩和男性过剩
  # drop=FALSE确保即使数据中没有出现某些分类，图例也会显示所有分类
  scale_fill_brewer(name = NULL, type = "div", palette = 5, drop = FALSE) +
  # 设置x轴(年份)的标签和刻度
  scale_x_continuous("年份", expand = c(0.02, 0),
                     breaks = seq(1900, 2010, 10)) +
  # 设置y轴(年龄)的标签和刻度
  scale_y_continuous("年龄", expand = c(0, 0),
                     breaks = seq(0, 100, 10)) +
  # 反转图例顺序，使得男性过剩在上，女性过剩在下
  guides(fill = guide_legend(reverse = TRUE)) +
  
  # 步骤4：设置坐标系和主题
  # 等比例坐标确保Lexis图形不会变形，1年宽度=1岁高度
  coord_equal() +
  # 使用极简主题，移除大部分背景元素
  theme_void() +
  # 自定义主题元素，确保坐标轴文本可见
  theme(
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(),
    axis.text.x = element_text()
  )

# 显示绘制的图表
plot_lexis_surface

# 保存图表（已注释掉）
# 如需生成PDF文件，取消下面注释
#ggsave("./out/fig/lexis_surface_raw.pdf", plot_lexis_surface,
#       width = 7, height = 7)  # 保存为7x7英寸的PDF文件