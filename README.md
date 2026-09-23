<div align="center">

# EasyR

**面向实际工作的无代码 R 数据分析工作台**

导入数据、清洗整理、统计绘图、建立模型、生成报告，全程通过界面操作。

![R](https://img.shields.io/badge/R-Shiny-276DC3?logo=r&logoColor=white)
![R tests](https://github.com/Linklee-sad/EasyR/actions/workflows/r-tests.yml/badge.svg)
![License](https://img.shields.io/badge/License-MIT-059669.svg)
![UI](https://img.shields.io/badge/UI-中文%20%7C%20English-2563EB)
![Data](https://img.shields.io/badge/Data-CSV%20%7C%20Excel-059669)
![Mode](https://img.shields.io/badge/Mode-Local--first-7C3AED)

</div>

---

EasyR 是一个模块化的 R Shiny 应用，适合希望使用 R 完成数据分析、但不想为每一步编写代码的用户。它以项目为单位组织工作：从 CSV、Excel、Yahoo Finance 或 FRED 获取数据，完成清洗、可视化和建模，然后导出图像、表格与报告。

## 主要功能

| 模块 | 能力 |
| --- | --- |
| 项目 | 创建新项目，保存和恢复 `.easyr` 项目，保留数据集、清洗状态和分析参数 |
| 数据导入 | CSV 批量导入、Excel 工作表、编码与分隔符、首行标题开关、空白表头自动修复 |
| 数据整理 | 分列处理缺失值、文字清理、筛选、排序、类型转换、行列增删改查、撤销与恢复 |
| 统计绘图 | 描述统计、10 类 ggplot2 图形、交互式 3D 散点图、统一图像编辑和 PNG 导出 |
| 函数绘图 | 独立入口、GeoGebra 风格输入侧栏、逐函数定义域、普通函数和参数曲线、零点标记与 PNG 导出 |
| 概率教学 | 18 种概率分布、随机抽样、大数定律、中心极限定理和可编辑教学图 |
| 统计建模 | 线性回归、逻辑回归、高级正则化回归、决策树、随机森林、支持向量机、主成分分析和 K-means 聚类 |
| 时间序列 | Yahoo/FRED 数据、趋势、ACF、STL、GARCH、ARIMA/ETS 预测和预测区间 |
| AI 助手 | 数据处理建议、算法参数推荐、Markdown 增强报告和长内容分页 |
| 原理演示 | 用分步交互图解释线性／逻辑回归、决策树、随机森林、SVM、PCA、K-means 和时间序列 |

## 快速开始

### 在线部署

[![Deploy to Posit Connect Cloud Free](https://img.shields.io/badge/Deploy-Posit%20Connect%20Cloud%20Free-447099?style=for-the-badge&logo=posit&logoColor=white)](https://connect.posit.cloud/)
[![Deploy to Render](https://render.com/images/deploy-to-render-button.svg)](https://render.com/deploy?repo=https://github.com/Linklee-sad/EasyR)

**Posit Connect Cloud Free（推荐）**：点击第一个按钮并登录，选择 **Publish → Shiny → Public GitHub repository**，然后填写：

- Repository：`https://github.com/Linklee-sad/EasyR`
- Branch：`main`
- Primary file：`app.R`

本仓库的 `manifest.json` 已包含 R 与软件包依赖。免费方案会生成公开访问链接；开启自动重新发布后，仓库更新可以触发重新构建。

**Render Free**：点击第二个按钮可以从本仓库创建独立的 Render Web Service。仓库中的 `Dockerfile` 会安装 R、EasyR 及全部运行依赖，`render.yaml` 会配置免费实例、端口、健康检查和后续自动部署。Render 会要求用户登录自己的账号并确认套餐。

在线实例设置了 `EASYR_HOSTED=1`。在此模式下，AI API Key 只存在当前会话，不允许写入服务器配置文件。上传的数据和 `.easyr` 项目仍可能包含敏感内容；公开部署时应使用非敏感数据，且不要把 API Key 写入仓库、Dockerfile 或 `render.yaml`。容器文件系统属于临时运行环境，长期项目请下载到自己的设备保存。

### 环境要求

- R 4.2 或更高版本（推荐）
- RStudio（推荐，但不是必需）
- 首次运行时用于安装依赖的网络连接

### 运行应用

克隆或下载项目：

```bash
git clone https://github.com/Linklee-sad/EasyR.git
cd EasyR
```

在 RStudio 中打开 `app.R`，点击 **Run App**。项目中的 `.Rprofile` 会让应用默认在系统浏览器中打开。

也可以从终端启动：

```bash
Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
```

首次启动会自动安装缺少的依赖。依赖保存在项目内的 `.R-library/`，不会提交到 GitHub。后续启动会直接复用。

如果只想安装依赖：

```bash
Rscript setup.R
```

## 使用流程

1. 在开始界面选择 **创建新项目**、打开已有 `.easyr` 项目、载入示例数据，或直接打开独立函数绘图工具。
2. 从左侧导入 CSV／Excel；多个 CSV 可以一次选择，Excel 每次导入一个工作簿。
3. 导入完成后自动进入 **数据工作区 → 数据浏览**，查看表格、字段类型和缺失情况。
4. 切换到 **整理数据**，完成缺失值处理、筛选、排序和编辑。
5. 使用顶部功能进入绘图、建模、聚类或时间序列分析。
6. 下载 CSV、PNG、预测结果和报告，或将当前状态保存为 `.easyr` 项目。

顶部状态栏可以在多个数据集之间切换。每个数据集分别保留自己的清洗版本，分析模块始终读取当前选中的处理结果。

## 数据导入与整理

EasyR 支持：

- 单个或批量 CSV，UTF-8／GB18030 编码及逗号、分号、制表符分隔；
- `.xlsx` 和 `.xls` 工作簿及工作表选择；
- 由用户决定第一行是否为字段名；
- 第一列表头为空时自动命名为“序号”，其他空白或重复字段自动生成唯一名称；
- 100 MB、500 MB 和 1000 MB 的单文件限制；
- 多数据集并存、切换和移除；
- 按字段设置不同的缺失值策略。

缺失值策略包括保留、删除行、均值、中位数、众数、0、向前填充、向后填充和线性插值。整理工作台还支持条件删除／保留行、字段排序、类型转换、单元格编辑、字段和行操作、内容查找、逐步撤销以及 UTF-8 BOM CSV 导出。

## 可视化

所有二维图形使用 ggplot2，并共用标题、轴标签、主题、配色、透明度、点大小、线宽、字号、导出尺寸和 DPI 设置。

| 图形 | 可用功能 |
| --- | --- |
| 直方图 | 分箱、均值／中位数参考线、分组叠加与分面 |
| 密度图 | 分组密度、参考线与分面 |
| 散点图 | 分组着色、线性拟合和 95% 均值置信带 |
| 折线趋势图 | 数值／日期横轴、分组、数据点、LOESS 趋势和置信带 |
| Q-Q 图 | 正态分位数参照、分组和分面 |
| ECDF | 经验累计比例、分组比较和分面 |
| 相关性热图 | 多字段、Pearson／Spearman、系数标签和相关矩阵 |
| 箱线图 | 分组、原始点叠加和横向显示 |
| 小提琴图 | 分组、内嵌箱线图和原始点 |
| 类别频数图 | 前 20 类、频数标签和横向显示 |
| 3D 散点图 | Plotly 旋转、缩放、悬停、分组着色和当前视角截图 |

大型 3D 图最多显示 50,000 个均匀抽取的点，以保持浏览器流畅；原始数据不会被修改。

开始界面的独立函数绘图工具不需要数据集。左侧可以连续输入 `sin(x)`、`x^2` 等普通函数，并为每个函数单独设置定义域；选择“参数曲线”后，可用 `x(t)=3*cos(t)`、`y(t)=2*sin(t)` 绘制椭圆、圆、螺线和李萨如曲线。参数曲线使用等比例坐标，避免几何图形变形。

## 分析与建模

| 功能 | 输出 |
| --- | --- |
| 线性回归 | `lm()` 系数、置信区间、标准化系数、R²、F 检验、VIF、诊断图和专业解读 |
| 逻辑回归 | 二分类、正类选择、系数、优势比与置信区间、预测概率、ROC/AUC 和阈值分析 |
| 高级回归 | 多项式与交互项、岭回归、Lasso、Elastic Net、交叉验证选 λ、系数路径与变量筛选 |
| 决策树 | 回归／分类、剪枝参数、树结构图、变量重要性、统一测试集评估和预测结果 |
| 模型比较 | 5／10 折交叉验证、统一折划分、分类／回归排行榜、折间波动图和完整结果导出 |
| 随机森林 | 回归／分类、独立测试集、OOB 指标、混淆矩阵、变量重要性和预测结果 |
| 支持向量机 | 线性／RBF／多项式／Sigmoid 核、分类／回归指标和支持向量统计 |
| PCA | 方差解释率、碎石图、得分、载荷、单位圆和结果数据 |
| K-means | 聚类中心、组内平方和、肘部图、轮廓系数和聚类标签 |
| 时间序列 | 变换、趋势、ACF、STL、GARCH、ARIMA／ETS 验证与未来预测 |

模型页面提供本地专业解读和结果导出。专业结论描述统计关系和预测表现，不自动代表因果关系；重要分析仍应结合研究设计、重复验证和领域知识。

## 时间序列数据

时间序列模块可以直接加入在线数据集：

- **Yahoo Finance**：通过 `quantmod` 下载股票、ETF 和指数数据；
- **FRED**：通过官方 Series Observations API 下载宏观经济序列，需要用户自己的 FRED API Key。

下载结果会进入顶部数据集列表，可以继续清洗、绘图和建模。时间序列分析支持水平值、一阶差分、百分比变化和对数收益率，并提供 GARCH 条件波动率与 ARIMA／ETS 预测可视化。

## AI 数据顾问

AI 功能支持 OpenAI、DeepSeek、Google Gemini、Anthropic Claude、通义千问（Qwen）和兼容第三方接口。用户只需要填写供应商、模型、API URL（第三方）和 API Key；EasyR 会自动使用各内置供应商对应的接口格式与认证方式。

AI 可以：

- 根据数据结构与用户目标提出清洗和分析路线；
- 为随机森林、SVM、PCA、K-means 和时间序列推荐参数；
- 根据本地计算结果生成 Markdown 专业报告；
- 将长报告按标题和完整段落分页；
- 下载完整 `.md` 文件。

### 隐私设计

- 默认只发送字段结构、统计摘要、相关性和模型计算结果；
- 原始逐行数据默认不发送；
- 每次发送前需要用户主动勾选同意；
- API Key 默认只存在当前会话内；
- 用户可以选择将连接配置保存在本机系统配置目录；
- `.easyr` 项目文件不会保存 API Key、AI 输入或临时上传路径；
- 本地保存的 API Key 是可读取文本，不应在公共电脑上启用。

连接第三方模型可能产生对应供应商的费用。

## 项目保存与恢复

`.easyr` 文件保存：

- 全部数据集和当前数据集；
- 每个数据集的整理版本与缺失值规则；
- 支持的绘图和分析参数。

恢复项目后，模型计算结果需要重新运行。项目文件包含数据内容，请按数据敏感级别妥善保存。

## 项目结构

```text
EasyR/
├── app.R                         # Shiny 入口与模块连接
├── setup.R                       # 自动检查和安装依赖
├── .Rprofile                     # 默认使用系统浏览器
├── R/
│   ├── home.R                    # 开始界面
│   ├── import.R                  # CSV / Excel 与多数据集管理
│   ├── project.R                 # .easyr 保存和恢复
│   ├── preview.R                 # 数据预览和字段概况
│   ├── workbench.R               # 数据清洗与编辑
│   ├── plotting.R                # ggplot2 / Plotly 绘图工作台
│   ├── function_plotter.R        # 独立函数与参数曲线绘图工具
│   ├── plot_editor.R             # 通用图像编辑和导出
│   ├── regression.R              # 线性回归
│   ├── logistic_regression.R     # 逻辑回归
│   ├── advanced_regression.R     # 多项式、交互项和正则化回归
│   ├── model_evaluation.R        # 统一模型评估、ROC/AUC 和阈值分析
│   ├── model_comparison.R        # 交叉验证和模型比较
│   ├── random_forest.R           # 随机森林
│   ├── decision_tree.R           # 决策树
│   ├── svm.R                     # 支持向量机
│   ├── pca.R                     # 主成分分析
│   ├── kmeans.R                  # K-means
│   ├── timeseries.R              # 时间序列、GARCH 与预测
│   ├── distributions.R           # 概率分布教学
│   ├── ai.R                      # AI 配置、建议与报告
│   ├── algorithm_tutorials.R     # 通用交互教程
│   ├── random_forest_tutorial.R  # 随机森林原理演示
│   └── i18n.R                    # 中英文界面
└── *_test.R                      # 模块和集成测试
```

每个分析功能都采用 Shiny 模块组织，UI 与服务器逻辑分别封装，通过响应式数据连接，避免依赖全局用户数据。

## 依赖

启动脚本会自动安装以下 R 包：

`shiny` · `readxl` · `DT` · `ggplot2` · `plotly` · `jsonlite` · `httr2` · `commonmark` · `randomForest` · `e1071` · `cluster` · `quantmod` · `tseries`

## 测试

运行基础集成测试：

```bash
Rscript smoke_test.R
```

运行全部测试：

```bash
for test_file in *_test.R; do
  Rscript "$test_file" || exit 1
done
```

测试覆盖导入、项目恢复、清洗、空字段边界、10 类 ggplot2 图形、函数与参数曲线、3D 绘图、图像导出、概率分布、AI 请求与脱敏、全部模型、交互教程、GARCH 和时间序列预测。

## 当前边界

- 应用以本地 R 会话为运行环境，多个大型数据集会累计占用内存；
- Excel 解压后的内存占用可能明显大于文件大小；
- GitHub Pages 无法运行 Shiny，在线部署需要 Posit Connect、shinyapps.io、Shiny Server 或容器服务；
- Yahoo Finance 数据可能延迟或临时不可用，FRED 数据可能在发布后修订；
- AI 与在线数据功能需要网络，其他清洗、绘图和本地建模功能可以离线使用。

## 路线图

- [x] CSV／Excel 与多数据集工作区
- [x] 项目保存与恢复
- [x] 数据清洗、编辑、撤销与导出
- [x] ggplot2、Plotly 3D 和统一图像编辑器
- [x] 线性／逻辑回归、决策树、随机森林、SVM、PCA、K-means
- [x] 时间序列、GARCH 和预测
- [x] AI 参数建议与 Markdown 报告
- [x] 中英文界面和交互式原理演示
- [ ] 固定依赖版本
- [ ] GitHub Actions 持续集成
- [ ] 在线部署模板

## 参与开发

欢迎通过 Issue 描述问题、数据场景或功能建议。提交改动时建议：

1. 将功能放入独立的 `R/*.R` 模块；
2. 保持按钮式操作和中英文文本一致；
3. 为数据边界或核心计算补充测试；
4. 运行全部 `*_test.R` 后再提交。

更完整的流程见 [CONTRIBUTING.md](CONTRIBUTING.md)。安全问题请按 [SECURITY.md](SECURITY.md) 私下报告。

## 参考

- [Shiny Modules](https://shiny.posit.co/r/articles/improve/modules/)
- [ggplot2 Reference](https://ggplot2.tidyverse.org/reference/)
- [R `lm`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html)
- [quantmod `getSymbols`](https://www.quantmod.com/documentation/getSymbols.html)
- [FRED Series Observations API](https://fred.stlouisfed.org/docs/api/fred/series_observations.html)
- [tseries GARCH](https://search.r-project.org/CRAN/refmans/tseries/html/garch.html)

## License

EasyR 使用 [MIT License](LICENSE)。你可以使用、复制、修改、发布和分发本项目，但需要保留原始版权和许可声明。
