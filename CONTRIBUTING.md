# 参与 RBench 开发

感谢你愿意改进 RBench。提交代码前，请先确认改动适合“通过界面完成 R 数据分析”的产品方向，并尽量保持操作简单、结果可解释。

## 提交问题

创建 Issue 前请先搜索是否已有相同问题。Bug 报告应包含：

- 操作系统、R 版本和 RBench 版本或提交；
- 可以复现问题的最小操作步骤；
- Console 中的完整错误信息；
- 数据的字段类型和大致规模。

请勿上传真实 API Key、私人数据、客户数据或包含个人信息的截图。可以使用内置 `iris` 数据或构造最小示例。

## 本地开发

```bash
git clone https://github.com/Linklee-sad/RBench.git
cd RBench
Rscript setup.R
Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
```

## 代码约定

- 新功能优先放在独立的 `R/*.R` 模块中；
- Shiny 功能使用 `xxx_ui(id)` 与 `xxx_server(id, ...)` 分离界面和服务器逻辑；
- 不在全局环境保存用户数据、API Key 或模型结果；
- 用户可见文本同时加入 `R/i18n.R`；
- 错误信息应告诉用户如何修正输入；
- 图形优先复用通用 ggplot2 编辑器；
- 不提交 `.easyr`、`.RData`、`.Renviron`、API Key 或真实业务数据。

## 测试

为核心计算、边界输入或修复的问题添加对应测试，然后运行：

```bash
for test_file in *_test.R; do
  Rscript "$test_file" || exit 1
done
```

## Pull Request

PR 描述应说明问题、最终行为和验证方式。一个 PR 尽量只处理一个主题。界面改动建议附上匿名示例截图。
