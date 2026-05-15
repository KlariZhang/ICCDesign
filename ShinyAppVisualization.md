# ShinyApp可视化 修改记录

本文档记录本次根据已有 ICCDesign 程序完成 Shiny 开发时所做的主要代码修改。

## 1. 新增 Shiny 应用

新增文件：

- `R/shiny-app.R`

主要内容：

- 新增 `run_icc_app()`，作为用户启动 Shiny 应用的导出函数。
- 新增 `icc_shiny_app()`，用于构建 Shiny app 对象。
- 实现完整 UI 和 server 逻辑，包含三个主要页面：
  - `ICC analysis`：进行 ICC 计算。
  - `Sample size`：进行样本量和功效分析。
  - `Data and guide`：展示数据预览、数据诊断、ICC 类型选择指南和可靠性分级标准。
- 支持三种数据输入方式：
  - 示例数据。
  - 上传 CSV/TSV/TXT 文件。
  - 直接粘贴表格数据。
- 支持用户选择 ICC 设计参数：
  - 是否所有受试者使用相同评分者。
  - 评分者效应类型：random / fixed。
  - 评分单位：single / average。
  - 一致性目标：absolute / consistency。
  - alpha、rho0、是否删除缺失值、是否包含 subject-rater interaction。
- 展示 ICC 分析结果：
  - ICC 点估计。
  - 置信区间。
  - 可靠性评级。
  - F 检验 p 值。
  - ANOVA 组件。
  - 标准化分析报告。
- 支持下载：
  - ICC 分析报告 `.txt`。
  - 样本量设计结果 `.csv`。
- 增加响应式 CSS，使界面在桌面和较窄屏幕下均可使用。

## 2. 导出 Shiny 启动函数

修改文件：

- `NAMESPACE`

修改内容：

- 增加：

```r
export(run_icc_app)
```

作用：

- 允许用户安装或加载包后直接运行：

```r
ICCDesign::run_icc_app()
```

## 3. 新增 Shiny 启动函数文档

新增文件：

- `man/run_icc_app.Rd`

主要内容：

- 记录 `run_icc_app()` 的用法、参数和功能说明。
- 参数包括：
  - `host`
  - `port`
  - `launch.browser`
  - `...`

作用：

- 避免导出函数缺少帮助文档。
- 让 `R CMD check` 能通过文档检查。

## 4. 修复 `icc_calc()` 的本地开发调用问题

修改文件：

- `R/main-interface.R`

修改内容：

- 在数据预处理后增加错误检查：

```r
if (!is.null(data_summary$error_msg)) {
  stop(data_summary$error_msg, call. = FALSE)
}
```

- 在设计参数校验后增加错误检查：

```r
if (!design_check$is_valid) {
  stop(design_check$error_msg, call. = FALSE)
}
```

- 将原来的命名空间查找：

```r
get(mapping$icc_func_name, envir = asNamespace("ICCDesign"))
```

改为从 `icc_calc()` 当前环境中查找函数：

```r
get(
  mapping$icc_func_name,
  envir = environment(icc_calc),
  mode = "function",
  inherits = TRUE
)
```

作用：

- 原写法要求 `ICCDesign` 已作为正式包安装，否则在开发时直接 `source()` 文件会失败。
- 新写法既支持安装后的包，也支持本地开发模式。
- Shiny app 在开发态加载 R 文件后也能正常调用 `icc_calc()`。

## 5. 修复 DESCRIPTION 元数据

修改文件：

- `DESCRIPTION`

修改内容：

- 增加显式 `Author` 字段。
- 增加显式 `Maintainer` 字段。

作用：

- 当前环境下 `R CMD check` 没有自动从 `Authors@R` 生成 `Author` / `Maintainer`。
- 显式补充后，包检查可以通过 DESCRIPTION 元数据阶段。

## 6. 已完成的验证

本次修改后执行过以下验证：

```r
R CMD build --no-build-vignettes /Users/orzmr/Documents/生物统计学作业/ICCDesign
```

构建成功，生成：

```text
ICCDesign_0.1.0.tar.gz
```

随后对构建产物执行：

```r
env _R_CHECK_FORCE_SUGGESTS_=false R CMD check --output=/tmp --no-manual --no-build-vignettes /tmp/ICCDesign_0.1.0.tar.gz
```

检查结果：

```text
Status: OK
```

说明：

- 因为当前环境无法访问 CRAN/Bioconductor，`testthat` 建议依赖无法在线检查。
- 所以使用 `_R_CHECK_FORCE_SUGGESTS_=false` 跳过强制检查建议依赖。

## 7. Shiny 启动方式

安装或加载包后，可运行：

```r
ICCDesign::run_icc_app()
```

开发环境中也可以 source 所有 R 文件后运行：

```r
files <- list.files("R", full.names = TRUE)
invisible(lapply(files, source))
run_icc_app(host = "127.0.0.1", port = 3838, launch.browser = FALSE)
```

本次验证时，本地服务地址为：

```text
http://127.0.0.1:3838
```

服务已成功返回 `HTTP 200 OK`。
