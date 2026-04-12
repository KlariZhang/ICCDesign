# ----------------------------------------------------------------------------
# 1. 主要导出函数：compute_icc
# ----------------------------------------------------------------------------

#' 计算组内相关系数（ICC）
#'
#' 根据用户指定的模型（单因素/双因素/混合）、类型（绝对同意/一致性）、
#' 单位（单次评分/平均评分）以及评分者固定/随机属性，计算 ICC 的点估计、
#' 置信区间和信度评级。
#'
#' @param data 数值型矩阵或数据框，行 = 研究对象，列 = 评分者/测量次数。
#' @param model 字符型：\code{"oneway"}（单因素随机）、\code{"twoway"}（双因素随机）、
#'   \code{"mixed"}（双因素混合）。默认 \code{"twoway"}。
#' @param type 字符型：\code{"absolute"}（绝对同意）、\code{"consistency"}（一致性）。
#'   默认 \code{"absolute"}。
#' @param unit 字符型：\code{"single"}（单次评分）、\code{"average"}（平均评分）。
#'   默认 \code{"single"}。
#' @param rater_fixed 逻辑值：仅当 \code{model} 为 \code{"twoway"} 或 \code{"mixed"}
#'   时有效。\code{TRUE} 表示评分者为固定效应，\code{FALSE} 表示随机效应。
#' @param conf_level 数值型：置信区间置信度，默认 0.95。
#' @param ... 传递给内部计算函数的额外参数。
#'
#' @return 返回一个列表（类 \code{icc_result}），包含以下组件：
#'   \item{icc}{ICC 点估计值。}
#'   \item{lower_ci}{置信区间下限。}
#'   \item{upper_ci}{置信区间上限。}
#'   \item{f_statistic}{方差分析的 F 值。}
#'   \item{df1, df2}{F 分布的自由度。}
#'   \item{p_value}{检验 ICC = 0 的 p 值。}
#'   \item{icc_type}{ICC 标准符号，如 "ICC(2,1)"。}
#'   \item{reliability_rating}{基于置信区间下限的信度评级（差/中等/良好/优秀）。}
#'
#' @examples
#' # 模拟数据：30 个研究对象，3 名评分者
#' set.seed(123)
#' dat <- matrix(rnorm(90, mean = 100, sd = 15), nrow = 30, ncol = 3)
#' result <- compute_icc(dat, model = "twoway", type = "absolute", unit = "single")
#' print(result)
#'
#' @export
compute_icc <- function(data,
                        model = c("twoway", "oneway", "mixed"),
                        type = c("absolute", "consistency"),
                        unit = c("single", "average"),
                        rater_fixed = TRUE,
                        conf_level = 0.95,
                        ...) {

  # ==================== 步骤 1：输入验证 ====================
  # CRAN 要求：函数不得修改全局环境，不得产生副作用

  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("参数 'data' 必须是矩阵或数据框。")
  }

  # 处理缺失值：给出警告，然后使用行删除法
  if (any(is.na(data))) {
    warning("数据中存在缺失值，将使用行删除法（删除包含 NA 的行）。")
    data <- stats::na.omit(data)
    if (nrow(data) == 0) {
      stop("删除缺失值后无剩余数据。")
    }
  }

  # 检查样本量：至少 3 个研究对象和 2 名评分者
  if (nrow(data) < 3) {
    stop("至少需要 3 个研究对象才能计算 ICC。")
  }
  if (ncol(data) < 2) {
    stop("至少需要 2 名评分者（或 2 次重复测量）才能计算 ICC。")
  }

  # 匹配参数选项
  model <- match.arg(model)
  type <- match.arg(type)
  unit <- match.arg(unit)

  # ==================== 步骤 2：参数自动修正（符合 Koo & Li 建议） ====================

  # 2.1 随机 + 一致性 → 转为混合模型（评分者固定）
  if (model == "twoway" && !rater_fixed && type == "consistency") {
    warning("双因素随机模型与一致性类型组合在实践中通常改用混合模型（评分者固定）。",
            "已自动将 rater_fixed 设置为 TRUE，model 改为 'mixed'。")
    rater_fixed <- TRUE
    model <- "mixed"
  }

  # 2.2 混合模型但评分者为随机 → 强制改为固定
  if (model == "mixed" && !rater_fixed) {
    warning("混合模型 (mixed) 要求评分者为固定效应，已将 rater_fixed 设置为 TRUE。")
    rater_fixed <- TRUE
  }

  # 2.3 单因素模型强制绝对同意
  if (model == "oneway") {
    if (type != "absolute") {
      warning("单因素模型 (oneway) 只能使用 'absolute' 类型，已自动修正。")
      type <- "absolute"
    }
    # 单因素模型下忽略 rater_fixed 参数
  }

  # ==================== 步骤 3：确定 ICC 标准符号 ====================
  icc_label <- determine_icc_label(model, type, unit, rater_fixed, ncol(data))

  # ==================== 步骤 4：调用核心计算函数 ====================
  # 注意：compute_icc_core 目前返回模拟值，实际使用时需替换为真实算法
  results <- compute_icc_core(data, model, type, unit, rater_fixed, conf_level)

  # 确保结果中包含 conf_level（若 core 未返回则手动添加）
  if (is.null(results$conf_level)) {
    results$conf_level <- conf_level
  }

  # ==================== 步骤 5：基于置信区间下限给出信度评级 ====================
  rating <- if (results$lower_ci >= 0.90) {
    "Excellent (优秀)"
  } else if (results$lower_ci >= 0.75) {
    "Good (良好)"
  } else if (results$lower_ci >= 0.50) {
    "Moderate (中等)"
  } else {
    "Poor (差)"
  }

  results$icc_type <- icc_label
  results$reliability_rating <- rating
  class(results) <- "icc_result"

  return(results)
}


# ----------------------------------------------------------------------------
# 2. 内部辅助函数：determine_icc_label
# ----------------------------------------------------------------------------

#' 根据参数确定 ICC 标准符号
#'
#' 按照 McGraw & Wong (1996) 及 Koo & Li (2016) 的规则，
#' 将模型、类型、单位和评分者固定性映射为类似 "ICC(2,1)" 的字符串。
#'
#' @param model,type,unit, rater_fixed, k 同主函数参数
#' @return 字符型，例如 "ICC(2,1)"
#' @keywords internal
determine_icc_label <- function(model, type, unit, rater_fixed, k) {
  # 单因素随机模型 -> ICC(1)
  if (model == "oneway") {
    base <- "1"
  }
  # 双因素随机，绝对同意 -> ICC(2)
  else if (model == "twoway" && !rater_fixed && type == "absolute") {
    base <- "2"
  }
  # 双因素混合，一致性 -> ICC(3)
  else if (model == "mixed" && rater_fixed && type == "consistency") {
    base <- "3"
  }
  # 其他组合（不常见或未明确定义）
  else {
    base <- "2"
    warning(sprintf("参数组合 (model=%s, type=%s, rater_fixed=%s) 未明确定义标准ICC形式，默认使用 ICC(2) 模型。请检查设计。",
                    model, type, rater_fixed))
  }

  # 确定是单次评分 (1) 还是平均评分 (k) —— 修正：直接输出数字，不带 "k="
  unit_char <- ifelse(unit == "single", "1", as.character(k))

  paste0("ICC(", base, ", ", unit_char, ")")
}


# ----------------------------------------------------------------------------
# 3. 内部核心计算函数（占位，未实现具体算法）
# ----------------------------------------------------------------------------

#' ICC 核心计算（框架占位）
#'
#' 实际开发者应在此函数中实现：
#'   - 根据 model/type/unit/rater_fixed 构建线性混合模型或方差分析
#'   - 计算 ICC 点估计值
#'   - 利用 F 分布或 Bootstrap 方法计算置信区间
#'   - 返回包含 icc, lower_ci, upper_ci, f_statistic, df1, df2, p_value, conf_level 的列表
#'
#' @inheritParams compute_icc
#' @return 列表，包含计算结果（当前为模拟值）
#' @keywords internal
compute_icc_core <- function(data, model, type, unit, rater_fixed, conf_level) {

  # ***************** 以下为占位代码，实际使用时请替换为真正的 ICC 计算 *****************
  # CRAN 要求：不能使用 ::: 或 .Internal，推荐使用 stats::aov() 或 lme4::lmer()

  # 模拟返回结果（仅用于展示结构，实际应删除）
  result <- list(
    icc = 0.82,
    lower_ci = 0.72,
    upper_ci = 0.89,
    f_statistic = 5.43,
    df1 = nrow(data) - 1,
    df2 = (nrow(data) - 1) * (ncol(data) - 1),
    p_value = 0.001,
    conf_level = conf_level   # 添加 conf_level 以支持打印
  )

  # 实际实现时，请取消下行注释并删除上述模拟代码：
  # result <- real_icc_calculation(data, model, type, unit, rater_fixed, conf_level)

  # 若未实现，建议抛出明确错误（防止意外使用）
  # stop("compute_icc_core 尚未实现具体的 ICC 算法。")

  return(result)
}


# ----------------------------------------------------------------------------
# 4. 样本量计算函数（占位，不导出以避免 CRAN 检查问题）
# ----------------------------------------------------------------------------

#' 估算 ICC 研究所需的最小样本量（占位函数）
#'
#' 此函数应由合作同学根据功率分析实现。
#' 应返回在给定预期 ICC、评分者人数、显著性水平和功效下，所需的最少研究对象数量。
#'
#' @param expected_icc 预期 ICC 值（例如 0.75）
#' @param null_value 零假设值（例如 0.60）
#' @param k 每名研究对象的评分者人数（或重复测量次数）
#' @param alpha 显著性水平，默认 0.05
#' @param power 预期功效，默认 0.80
#' @param model,type,unit 同 \code{compute_icc}
#'
#' @return 列表，包含以下组件：
#'   \item{recommended_n}{建议的最少研究对象数量}
#'   \item{actual_power}{在建议样本量下达到的实际功效}
#'   \item{method_used}{所用功率分析方法简述}
#'
#' @keywords internal
#' @noRd
estimate_icc_sample_size <- function(expected_icc,
                                     null_value,
                                     k,
                                     alpha = 0.05,
                                     power = 0.80,
                                     model = "twoway",
                                     type = "absolute",
                                     unit = "single") {

  # 占位实现：返回经验法则建议，并发出警告
  warning("样本量计算功能尚未完整实现，当前返回经验法则建议（n >= 30, k >= 3）。")
  list(
    recommended_n = 30,
    actual_power = NA,
    method_used = "经验法则（Koo & Li, 2016）"
  )
}


# ----------------------------------------------------------------------------
# 5. 自定义打印方法
# ----------------------------------------------------------------------------

#' 打印 ICC 结果对象
#'
#' @param x \code{icc_result} 类对象
#' @param ... 其他参数（未使用）
#' @export
print.icc_result <- function(x, ...) {
  # 健壮性处理：如果 conf_level 不存在则默认为 0.95
  conf <- if (is.null(x$conf_level)) 0.95 else x$conf_level

  cat("\n===== 组内相关系数 (ICC) 结果 =====\n")
  cat("ICC 类型 :", x$icc_type, "\n")
  cat("点估计   :", round(x$icc, 4), "\n")
  cat(round(conf * 100), "% 置信区间: [", round(x$lower_ci, 4), ", ", round(x$upper_ci, 4), "]\n", sep = "")
  cat("F 检验   : F(", x$df1, ",", x$df2, ") =", round(x$f_statistic, 2), ", p =", format.pval(x$p_value), "\n")
  cat("信度评级 :", x$reliability_rating, "\n")
  cat("===================================\n")
  invisible(x)
}
