###############################
# 死亡原因数据准备脚本 #
###############################

# 脚本概述:
# 本脚本处理法国历史死亡原因数据（1925-1999年），完成以下任务:
# 1. 加载按时期、年龄、性别和死亡原因分类的原始死亡数据
# 2. 计算每个时期-年龄-性别组合中各种死亡原因的占比
# 3. 创建三个不同粒度的死亡原因数据集(包含10种、5种和3种主要死亡原因)
# 4. 准备总体死亡率数据
#
# 这些处理后的数据将用于创建论文中的Lexis Surface图表。

# 一、初始化环境 ------------------------------------------------------------

library(readr)    # 用于读取CSV文件
library(dplyr)    # 用于数据处理和转换
library(tidyr)    # 用于数据重塑（宽转长、长转宽）

# 二、数据导入与预处理 -----------------------------------------------------

# 加载ICD-9疾病分类代码和标签
cbook_cod <- read_csv("/Users/tuo/Desktop/Lexis/demographic-research.36-21/data/cod_names.csv", skip = 13)

# 定义年龄层级顺序（确保数据按正确的年龄顺序处理）
lev_age <- c("<1","1-4","5-9","10-14","15-19","20-24","25-29",
             "30-34","35-39","40-44","45-49","50-54","55-59",
             "60-64","65-69","70-74","75-79","80-84","85-89",
             "90-94","95-99","100+")

# 三、基础死亡原因比例数据处理 ----------------------------------------------

# 步骤1: 读取原始死亡数据并进行基础预处理
read_csv("/Users/tuo/Desktop/Lexis/demographic-research.36-21/data/ined-cod-fra-1925-1999-counts.csv", skip = 19) %>%
  filter(age != "total") %>%                  # 移除总计行
  # 转换分类变量为有序因子
  mutate(
    age = factor(age, levels = lev_age, ordered = TRUE),   # 确保年龄正确排序
    cod = factor(cod, labels = cbook_cod$short)            # 使用简短标签代替代码
  ) %>%
  # 添加数值年龄起点和年龄组宽度（用于Lexis图）
  arrange(age) %>%
  group_by(year, sex, cod) %>%
  mutate(
    age_start = c(0, 1, seq(5, 100, 5)),     # 每个年龄组的起始年龄
    age_width = c(diff(age_start), 5)        # 每个年龄组的宽度
  ) %>% ungroup() %>%
  # 步骤2: 计算每种死亡原因占当年、该性别、该年龄组总死亡的比例
  group_by(year, age, sex) %>%
  select(year, age, age_start, age_width, sex, cod, dx) %>%
  mutate(px = dx / dx[cod == "Total"]) %>%   # px: 该死亡原因占总死亡的比例
  ungroup() %>%
  # 移除"Total"类别（因为我们已计算了比例）
  filter(cod != "Total") %>% 
  droplevels() -> cod_prop

# 步骤3: 处理比例和不等于1的问题
# 注: 由于原始数据的限制，各死亡原因比例之和可能不等于1
# 解决方案: 创建"Other"类别，包含所有未明确分类的死亡比例
cod_prop %>%
  group_by(year, age, age_start, age_width, sex) %>%
  summarise(cod = "Other", px = 1 - sum(px)) %>%  # 计算剩余比例
  bind_rows(select(cod_prop, -dx), .) %>%         # 合并原有数据和"Other"类别
  ungroup() %>%
  arrange(year, age, sex) -> cod_prop

# 四、创建不同粒度的死亡原因数据集 ------------------------------------------

# 数据集1: 10种主要死亡原因
# 包含9种具体死亡原因和1个"其他"类别

# 选择我们关注的9种主要死亡原因
lab_cod_10 <- cbook_cod$short[c(2, 3, 9, 10, 11, 16, 17, 18, 19)]

# 处理并创建10种死亡原因数据集
cod_prop %>%
  # 筛选出目标死亡原因
  filter(cod %in% lab_cod_10) %>%
  # 转为宽格式以便计算
  spread(key = cod, value = px) %>%
  # 计算"其他"类别的比例（确保总和为1）
  mutate(Other = 1-rowSums(.[lab_cod_10])) %>%
  # 转回长格式
  gather(key = "cod", value = "px",
         c(all_of(lab_cod_10), "Other")) %>%
  arrange(sex, year, age, cod) -> cod_prop10

# 保存10种死亡原因数据集
write_csv(mutate(cod_prop10, px = sprintf("%1.5f", px)), 
          file = "/Users/tuo/Desktop/Lexis/demographic-research.36-21/data/cod10.csv")

# 数据集2: 5种主要死亡原因
# 包含4种具体死亡原因和1个"其他"类别

# 选择4种主要死亡原因
lab_cod_5 <- cbook_cod$short[c(2, 3, 9, 19)]

# 处理并创建5种死亡原因数据集
cod_prop %>%
  filter(cod %in% lab_cod_5) %>%
  spread(key = cod, value = px) %>%
  mutate(Other = 1-rowSums(.[lab_cod_5])) %>%
  gather(key = "cod", value = "px",
         c(all_of(lab_cod_5), "Other")) %>%
  arrange(sex, year, age, cod) -> cod_prop5

# 保存5种死亡原因数据集
write_csv(mutate(cod_prop5, px = sprintf("%1.5f", px)), 
          file = "/Users/tuo/Desktop/Lexis/demographic-research.36-21/data/cod5.csv")

# 数据集3: 3种主要死亡原因
# 包含2种具体死亡原因和1个"其他"类别

# 选择2种主要死亡原因
lab_cod_3 <- cbook_cod$short[c(3, 19)]

# 处理并创建3种死亡原因数据集
cod_prop %>%
  filter(cod %in% lab_cod_3) %>%
  spread(key = cod, value = px) %>%
  mutate(Other = 1-rowSums(.[lab_cod_3])) %>%
  gather(key = "cod", value = "px",
         c(all_of(lab_cod_3), "Other")) %>%
  arrange(sex, year, age, cod) -> cod_prop3

# 保存3种死亡原因数据集
write_csv(mutate(cod_prop3, px = sprintf("%1.5f", px)), 
          file = "/Users/tuo/Desktop/Lexis/demographic-research.36-21/data/cod3.csv")

# 五、准备总死亡率数据 -----------------------------------------------------

# 读取总死亡率数据并进行处理
read_csv("/Users/tuo/Desktop/Lexis/demographic-research.36-21/data/ined-cod-fra-1925-1999-rates.csv", skip = 19) %>%
  filter(age != "total", cod == "000*-999*") %>%  # 筛选总死亡率数据
  select(-cod) %>%                                # 移除不需要的列
  # 应用因子以确保正确排序
  mutate(
    age = factor(age, levels = lev_age, ordered = TRUE)
  ) %>%
  # 添加数值年龄起点和宽度（同样用于Lexis图）
  arrange(age) %>%
  group_by(year, sex) %>%
  mutate(
    age_start = c(0, 1, seq(5, 100, 5)),
    age_width = c(diff(age_start), 5)
  ) %>% 
  ungroup() %>%
  mutate(mx = mx/1E5) -> mx  # 将死亡率从每10万人转换为比例

# 保存总死亡率数据
write_csv(mutate(mx, mx = sprintf("%1.5f", mx)), 
          file = "/Users/tuo/Desktop/Lexis/demographic-research.36-21/data/mx.csv")