# نمونه استفاده از RuleKit
# اجرای این فایل را در پوشهای انجام دهید که rulekit.R وجود دارد.
# یا مسیر کامل را به source بدهید.

# بارگذاری توابع
source("rulekit.R")

# دادهی نمونه
df <- data.frame(
  A = c(10, 200, NA, 50, 0),
  B = c(5, 100, 30, NA, 0),
  C = c(15, 300, 30, 50, 0),
  stringsAsFactors = FALSE
)

# تعریف قوانین
rules <- data.frame(
  name = c("A_in_0_100", "B_not_na", "A_le_B", "A_plus_B_eq_C"),
  variable = c("A", "B", NA, NA),
  rule = c(
    "val >= 0 & val <= 100", # قانون تکستونه با val
    "!is.na(val)",           # نباید NA باشد
    "A <= B",                # قانون چندستونه
    "A + B == C"             # قانون چندستونه
  ),
  severity = c("warn", "error", "warn", "error"),
  stringsAsFactors = FALSE
)

# اعمال قوانین
out <- apply_rules(rules, df)

# نمایش نتایج
print("نتایج سطری:")
print(out$results)

print("خلاصه قوانین:")
print(out$rule_summary)

# ذخیره نتایج در فایل (اختیاری)
# write_csv_u(out$results, "dq_results.csv")
# write_csv_u(out$rule_summary, "dq_rule_summary.csv")