# RuleKit: سادهنویسی، ارزیابی، و وارسی قوانین سنجش داده در R
# قرارداد نتیجهگیری:
# - TRUE  = نقض/خطا
# - FALSE = عدم نقض
# - NA    = خارج از دامنه ارزیابی (مثلاً مقدارِ val یا یکی از ستونهای مرجع NA بوده یا ستون وجود نداشته)
#
# سیاست گیت:
# - اگر قانون شامل 'val' باشد: فقط وقتی val حاضر است ارزیابی میشود؛ در غیر این صورت NA.
# - اگر قانون بدون 'val' باشد: فقط وقتی همه ستونهای مرجع برای هر ردیف حاضرند ارزیابی میشود؛ در غیر این صورت NA.

# خواندن/نوشتن CSV با UTF-8
read_csv_u <- function(path) {
  read.csv(path, fileEncoding = "UTF-8-BOM", encoding = "UTF-8",
           stringsAsFactors = FALSE, check.names = FALSE)
}
write_csv_u <- function(df, path) {
  write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8")
}

# حدس جداکننده و خواندن داده
read_data_auto <- function(path) {
  sep <- if (grepl("\\.tsv$|\\.tab$|\\.txt$", tolower(path))) "\t" else ","
  read.table(path, header = TRUE, sep = sep,
             na.strings = c("NA", "", "NaN"), stringsAsFactors = FALSE, check.names = FALSE)
}

# نرمالسازی ساده قانون بدون تغییر منطق
normalize_rule <- function(rule_text) {
  if (is.null(rule_text) || is.na(rule_text)) return(rule_text)
  t <- as.character(rule_text)
  t <- gsub("\\|\\|", "|", t, perl = TRUE)
  t <- gsub("&&", "&", t, perl = TRUE)
  t <- gsub("\\s+", " ", t, perl = TRUE)
  trimws(t)
}

# استخراج نام ستونهای مرجع از یک قانون
extract_refs <- function(rule_text, data_cols, variable = NULL) {
  if (is.null(rule_text) || is.na(rule_text)) return(character(0))
  t <- as.character(rule_text)
  syms <- tryCatch(all.vars(parse(text = t)), error = function(e) character(0))
  reserved <- c("if","else","repeat","while","function","for","in","next","break",
                "TRUE","FALSE","T","F","NA","NaN","Inf","NULL","val")
  refs <- setdiff(intersect(syms, data_cols), reserved)
  # اگر قانون شامل val باشد، ستون variable هم وابسته است
  if (!is.null(variable) && grepl("\\bval\\b", t) && variable %in% data_cols) {
    refs <- unique(c(refs, variable))
  }
  refs
}

# هشدارهای نگارشی رایج
lint_textual <- function(rule_text) {
  t <- as.character(rule_text)
  warns <- character(0)
  if (grepl("(^|[^!<>])=(?!=)", t, perl = TRUE)) warns <- c(warns, "احتمال استفاده '=' بهجای '=='")
  if (grepl("\\|\\|", t)) warns <- c(warns, "بهجای '||' از '|' (برداری) استفاده کنید")
  if (grepl("&&", t)) warns <- c(warns, "بهجای '&&' از '&' (برداری) استفاده کنید")
  warns
}

# ارزیابی یک قانون روی داده با سیاست گیت
eval_rule <- function(rule_text, data, variable = NULL) {
  stopifnot(is.data.frame(data))
  n <- nrow(data)
  data_cols <- names(data)

  rule_norm <- normalize_rule(rule_text)
  # پارسکردن عبارت
  expr <- tryCatch(parse(text = rule_norm)[[1]], error = function(e) e)
  if (inherits(expr, "error")) {
    return(list(
      status = "parse-error",
      error = as.character(expr$message),
      rule = rule_norm,
      result = rep(NA, n),
      diagnostics = list(warnings = lint_textual(rule_text), refs = character(0), gated_by = "parse")
    ))
  }

  contains_val <- grepl("\\bval\\b", rule_norm)
  refs <- extract_refs(rule_norm, data_cols, variable)

  # اگر ستونهای مرجع وجود ندارند
  missing_cols <- setdiff(refs, data_cols)
  if (length(missing_cols) > 0) {
    return(list(
      status = "missing-columns",
      error = sprintf("ستون(ها)ی زیر یافت نشد: %s", paste(missing_cols, collapse = ", ")),
      rule = rule_norm,
      result = rep(NA, n),
      diagnostics = list(warnings = lint_textual(rule_text), refs = refs, gated_by = "missing_columns")
    ))
  }

  # محیط ارزیابی
  env <- list2env(as.list(data), parent = baseenv())
  if (contains_val) {
    if (is.null(variable) || !(variable %in% data_cols)) {
      return(list(
        status = "variable-not-found",
        error = "برای قانونی که شامل val است باید نام ستون variable مشخص و موجود باشد.",
        rule = rule_norm,
        result = rep(NA, n),
        diagnostics = list(warnings = lint_textual(rule_text), refs = refs, gated_by = "variable_missing")
      ))
    }
    assign("val", data[[variable]], envir = env)
  }

  # ارزیابی خام
  res_raw <- tryCatch(eval(expr, envir = env), error = function(e) e)
  if (inherits(res_raw, "error")) {
    return(list(
      status = "eval-error",
      error = as.character(res_raw$message),
      rule = rule_norm,
      result = rep(NA, n),
      diagnostics = list(warnings = lint_textual(rule_text), refs = refs, gated_by = "eval")
    ))
  }

  # تطبیق طول
  if (length(res_raw) == 1L) {
    res_raw <- rep(res_raw, n)
  } else if (length(res_raw) != n) {
    return(list(
      status = "length-mismatch",
      error = sprintf("طول خروجی %d ولی تعداد ردیفها %d است", length(res_raw), n),
      rule = rule_norm,
      result = rep(NA, n),
      diagnostics = list(warnings = lint_textual(rule_text), refs = refs, gated_by = "length")
    ))
  }

  # تبدیل به منطقی
  if (!is.logical(res_raw)) {
    res_log <- suppressWarnings(as.logical(res_raw))
  } else {
    res_log <- res_raw
  }

  # اعمال گیت روی نتیجه
  if (contains_val) {
    gate <- !is.na(data[[variable]])
  } else {
    # برای قوانین بدون val، هر ردیفی که هر یک از refs آن NA باشد، NA خواهد شد
    if (length(refs) == 0) {
      gate <- rep(TRUE, n)
    } else {
      ref_mat <- do.call(cbind, lapply(refs, function(col) !is.na(data[[col]])))
      gate <- apply(ref_mat, 1, all)
    }
  }
  result <- ifelse(gate, res_log, NA)
  list(
    status = "ok",
    error = "",
    rule = rule_norm,
    result = result,
    diagnostics = list(warnings = lint_textual(rule_text), refs = refs, gated_by = ifelse(contains_val, variable, paste(refs, collapse = ",")))
  )
}

# اعمال مجموعهای از قوانین
# انتظار قالب rules: data.frame با ستونهای:
# - name: نام یکتا برای قانون (اختیاری ولی توصیهشده)
# - variable: نام ستون هدف برای قوانین شامل val (NA برای قوانین چندستونه)
# - rule: متن قانون (عبارت R)
# - severity: سطح اهمیت (اختیاری: info/warn/error)
apply_rules <- function(rules, data) {
  stopifnot(is.data.frame(rules), is.data.frame(data))
  if (!"rule" %in% names(rules)) stop("ستون 'rule' در rules لازم است.")
  if (!"variable" %in% names(rules)) rules$variable <- NA_character_
  if (!"name" %in% names(rules)) rules$name <- paste0("rule_", seq_len(nrow(rules)))

  n <- nrow(data)
  m <- nrow(rules)
  results_df <- data.frame(row = seq_len(n), stringsAsFactors = FALSE)
  diag_list <- vector("list", m)
  names_vec <- character(m)

  for (i in seq_len(m)) {
    nm <- rules$name[i]
    var <- rules$variable[i]
    txt <- rules$rule[i]
    ev <- eval_rule(txt, data, if (!is.na(var)) var else NULL)
    colname <- if (!is.na(nm) && nzchar(nm)) nm else paste0("rule_", i)
    results_df[[colname]] <- ev$result
    diag_list[[i]] <- list(
      name = colname,
      variable = if (!is.na(var)) var else NA_character_,
      rule = ev$rule,
      status = ev$status,
      error = ev$error,
      warnings = ev$diagnostics$warnings,
      refs = ev$diagnostics$refs
    )
    names_vec[i] <- colname
  }

  # خلاصه هر قانون
  rule_summary <- data.frame(
    name = names_vec,
    violations = sapply(names_vec, function(cn) sum(results_df[[cn]] == TRUE, na.rm = TRUE)),
    valids = sapply(names_vec, function(cn) sum(results_df[[cn]] == FALSE, na.rm = TRUE)),
    nas = sapply(names_vec, function(cn) sum(is.na(results_df[[cn]]))),
    stringsAsFactors = FALSE
  )

  # شمارش نقضها در هر ردیف
  row_violation_count <- if (length(names_vec) > 0) {
    rowSums(results_df[names_vec] == TRUE, na.rm = TRUE)
  } else {
    integer(n)
  }
  results_df[["violation_count"]] <- row_violation_count

  list(
    results = results_df,
    rule_summary = rule_summary,
    diagnostics = diag_list
  )
}